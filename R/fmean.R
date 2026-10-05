#' Functional mean model
#'
#' `FMEAN()` returns an iid functional model applied to the formula's response variable as a function of age.
#' Standard deviations that cannot be estimated, such as at ages with fewer
#' than two finite residuals, are interpolated from neighbouring ages.
#' Simulations from [generate()] with `bootstrap = TRUE` resample whole years
#' of residuals, so they keep the correlation between ages. Otherwise, ages are
#' simulated independently from normal distributions, which understates the
#' uncertainty of quantities computed across ages, such as life expectancy.
#'
#' @aliases report.FMEAN
#'
#' @param formula Model specification.
#' @param ... Not used. An error is given if any arguments are supplied here,
#' so that misspelled arguments are not silently ignored.
#'
#' @return A model specification.
#'
#'
#' @author Rob J Hyndman
#' @examples
#' fmean <- norway_mortality |>
#'   dplyr::filter(Sex == "Female") |>
#'   model(mean = FMEAN(Mortality))
#' report(fmean)
#' autoplot(fmean) + ggplot2::scale_y_log10()
#' @export
FMEAN <- function(formula, ...) {
  rlang::check_dots_empty()
  fmean_model <- new_model_class("fmean", train = train_fmean)
  new_model_definition(fmean_model, !!enquo(formula))
}

train_fmean <- function(.data, ...) {
  indexvar <- index_var(.data)
  vvar <- vital_var_list(.data)
  agevar <- vvar$age
  measures <- measured_vars(.data)
  measures <- measures[!(measures %in% c(agevar, vvar$population))]
  measure <- measures[1]
  ave_measure <- .data |>
    as_tibble() |>
    group_by(!!sym(agevar)) |>
    # Infinite values (e.g. log of zero rates) are treated as missing
    summarise(.fitted = mean(.data[[measure]][is.finite(.data[[measure]])]))
  out <- .data |>
    as_tibble() |>
    left_join(ave_measure, by = agevar) |>
    mutate(
      .resid = .data[[measure]] - .fitted,
      .resid = if_else(is.finite(.resid), .resid, NA),
      .innov = .resid
    )
  sigma <- out |>
    group_by(across(all_of(agevar))) |>
    summarise(sigma = sd(.resid, na.rm = TRUE))
  sigma$sigma <- fill_by_age(sigma$sigma, sigma[[agevar]])
  out <- out |>
    as_tsibble(index = indexvar, key = all_of(agevar)) |>
    as_vital(.age = agevar) |>
    select(all_of(c(indexvar, agevar)), everything())
  model <- ave_measure |>
    rename(mean = .fitted) |>
    left_join(sigma, by = agevar)
  # Ages with no finite values take their mean from neighbouring ages
  if (!all(is.finite(model$mean))) {
    warning(
      "No finite values for ages ",
      paste(model[[agevar]][!is.finite(model$mean)], collapse = ", "),
      ". Interpolating their means from neighbouring ages.",
      call. = FALSE
    )
    model$mean <- fill_by_age(model$mean, model[[agevar]])
  }

  structure(
    list(
      fitted = out,
      model = model,
      nobs = sum(!is.na(.data[[measure]]))
    ),
    class = "FMEAN"
  )
}

# Fill missing values (e.g. standard deviations at ages with too few
# observations) by linear interpolation between neighbouring ages, using the
# nearest available value beyond the youngest or oldest of them
fill_by_age <- function(x, age) {
  ok <- is.finite(x)
  if (all(ok) || !any(ok)) {
    return(x)
  }
  if (sum(ok) == 1L) {
    x[!ok] <- x[ok]
  } else {
    x[!ok] <- stats::approx(age[ok], x[ok], xout = age[!ok], rule = 2)$y
  }
  x
}

# Resample n innovations from pool, or simulate normal innovations with
# standard deviation sigma if pool has no finite values
resample_innov <- function(pool, n, sigma) {
  pool <- pool[is.finite(pool)]
  if (length(pool) == 0L) {
    return(stats::rnorm(n, sd = sigma))
  }
  pool[sample.int(length(pool), size = n, replace = TRUE)]
}

# Resample n years of the residuals (.innov) in the fitted tsibble at the given
# ages, returning an n x ages matrix. Each row takes its residuals from one
# year, keeping the correlation between ages. Residuals missing in the year
# drawn are resampled from other years at that age, or simulated from a normal
# distribution with standard deviation sigma if there are none.
resample_years <- function(fitted, agevar, ages, n, sigma) {
  indexvar <- index_var(fitted)
  fitted <- as_tibble(fitted)
  fitted <- fitted[fitted[[agevar]] %in% ages, ]
  years <- sort(unique(fitted[[indexvar]]))
  resid <- matrix(NA_real_, length(years), length(ages))
  resid[cbind(
    match(fitted[[indexvar]], years),
    match(fitted[[agevar]], ages)
  )] <- fitted$.innov
  resid[!is.finite(resid)] <- NA
  # Only draw years with some residuals (e.g. not the first year of FNAIVE)
  resid <- resid[rowSums(!is.na(resid)) > 0, , drop = FALSE]
  out <- matrix(NA_real_, n, length(ages))
  if (NROW(resid) > 0L) {
    out[] <- resid[sample.int(NROW(resid), n, replace = TRUE), , drop = FALSE]
  }
  for (j in which(colSums(is.na(out)) > 0)) {
    miss <- is.na(out[, j])
    out[miss, j] <- resample_innov(resid[, j], sum(miss), sigma[j])
  }
  out
}

#' @rdname forecast
#' @export
forecast.FMEAN <- function(
  object,
  new_data = NULL,
  h = NULL,
  point_forecast = list(.mean = mean),
  simulate = FALSE,
  bootstrap = FALSE,
  times = 5000,
  ...
) {
  # With simulate or bootstrap, forecast.mdl_vtl_ts() uses generate() rather
  # than this method. The arguments are included so they show in the docs.
  agevar <- age_var(new_data)
  new_data |>
    left_join(object$model, by = agevar) |>
    transmute(fc = distributional::dist_normal(mean, sigma)) |>
    pull(fc)
}

#' @export
generate.FMEAN <- function(
  x,
  new_data = NULL,
  h = NULL,
  bootstrap = FALSE,
  times = 1,
  ...
) {
  agevar <- age_var(new_data)
  new_data <- new_data |>
    dplyr::left_join(x$model, by = agevar)
  if (times != length(unique(new_data$.rep))) {
    stop("`times` must equal the number of replicates (`.rep`) in `new_data`")
  }

  if (!(".innov" %in% names(new_data))) {
    if (bootstrap) {
      # One residual year for each future time and replicate, applied to all
      # ages, to keep the correlation between ages
      indexvar <- index_var(new_data)
      draw <- paste(new_data[[indexvar]], new_data$.rep)
      draws <- unique(draw)
      ages <- x$model[[agevar]]
      innov <- resample_years(
        x$fitted,
        agevar,
        ages,
        length(draws),
        x$model$sigma
      )
      new_data$.innov <- innov[cbind(
        match(draw, draws),
        match(new_data[[agevar]], ages)
      )]
    } else {
      new_data$.innov <- stats::rnorm(NROW(new_data), sd = new_data$sigma)
    }
  }

  transmute(new_data, .sim = mean + .innov)
}

#' @export
glance.FMEAN <- function(x, ...) {
  tibble(sigma2 = var(x$fitted$.resid, na.rm = TRUE))
}

#' @export
tidy.FMEAN <- function(x, ...) {
  agevar <- colnames(x$model)[1]
  # Number of observations for each age
  nobs <- as_tibble(x$fitted) |>
    group_by(across(all_of(agevar))) |>
    summarise(.n = sum(!is.na(.resid)))
  x$model |>
    left_join(nobs, by = agevar) |>
    mutate(
      term = "mean",
      estimate = mean,
      std.error = sigma / sqrt(.n),
      stat = mean / std.error,
      p.value = 2 * stats::pt(abs(stat), .n - 1, lower.tail = FALSE)
    ) |>
    select(-mean, -sigma, -.n)
}

#' @export
report.FMEAN <- function(object, ...) {
  cat("\n")
  print(object$model)
}

#' @export
model_sum.FMEAN <- function(x) {
  paste0("FMEAN")
}

#' @export
autoplot.FMEAN <- function(
  object,
  age = NULL,
  ...
) {
  model_component_plot(object, age, "mean") + ggplot2::ylab("Mean")
}

#' @export
interpolate.FMEAN <- function(object, new_data, specials, ...) {
  interpolate_fitted(object, new_data)
}

#' @export
age_components.FMEAN <- function(object, ...) {
  unnest_components(object, identity)
}

#' @export
time_components.FMEAN <- function(object, ...) {
  stop("FMEAN objects have no time components")
}
