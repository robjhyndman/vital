#' Functional naive model
#'
#' `FNAIVE()` returns a random walk functional model applied to the formula's response variable as a function of age.
#' Standard deviations that cannot be estimated, such as at ages with fewer
#' than two finite residuals, are interpolated from neighbouring ages.
#' Simulations from [generate()] with `bootstrap = TRUE` resample whole years
#' of residuals, so they keep the correlation between ages. Otherwise, ages are
#' simulated independently from normal distributions, which understates the
#' uncertainty of quantities computed across ages, such as life expectancy.
#'
#' @aliases report.FNAIVE
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
#' fnaive <- norway_mortality |>
#'   dplyr::filter(Sex == "Female") |>
#'   model(fit = FNAIVE(Mortality))
#' report(fnaive)
#' autoplot(fnaive) + ggplot2::scale_y_log10()
#' @export
FNAIVE <- function(formula, ...) {
  rlang::check_dots_empty()
  fnaive_model <- new_model_class("fnaive", train = train_fnaive)
  new_model_definition(fnaive_model, !!enquo(formula))
}

train_fnaive <- function(.data, ...) {
  indexvar <- index_var(.data)
  vvar <- vital_var_list(.data)
  agevar <- vvar$age
  measures <- measured_vars(.data)
  measures <- measures[!(measures %in% c(agevar, vvar$population))]
  measure <- measures[1]
  # Time step between observations
  step <- tsibble::default_time_units(tsibble::interval(.data))
  last_measure <- .data |>
    tsibble::as_tibble() |>
    dplyr::mutate(
      index = vctrs::vec_cast(.data[[indexvar]] + step, .data[[indexvar]])
    )
  last_measure <- last_measure[, c("index", agevar, measure)]
  colnames(last_measure) <- c(indexvar, agevar, ".fitted")
  out <- .data |>
    as_tibble() |>
    left_join(last_measure, by = c(indexvar, agevar)) |>
    mutate(
      .resid = .data[[measure]] - .fitted,
      # Infinite values (e.g. log of zero rates) are treated as missing
      .resid = if_else(is.finite(.resid), .resid, NA),
      .innov = .resid
    )
  model <- out |>
    group_by(across(all_of(agevar))) |>
    summarise(sigma = sd(.resid, na.rm = TRUE))
  model$sigma <- fill_by_age(model$sigma, model[[agevar]])
  # Random walks start from the last finite observation for each age
  obs <- as_tibble(.data)[is.finite(.data[[measure]]), c(indexvar, agevar, measure)]
  obs <- obs[order(obs[[agevar]], obs[[indexvar]]), ]
  last <- obs[!duplicated(obs[[agevar]], fromLast = TRUE), ]
  ages <- sort(unique(.data[[agevar]]))
  # Ages with no finite values take their starting values from neighbouring ages
  none <- ages[!(ages %in% last[[agevar]])]
  stale <- ages[!(ages %in% c(none, last[[agevar]][last[[indexvar]] == max(.data[[indexvar]])]))]
  if (length(none) > 0L) {
    warning(
      "No finite values for ages ",
      paste(none, collapse = ", "),
      ". Interpolating their starting values from neighbouring ages.",
      call. = FALSE
    )
    start <- last[[measure]][match(ages, last[[agevar]])]
    last <- tibble(!!agevar := ages, !!measure := fill_by_age(start, ages))
  }
  if (length(stale) > 0L) {
    warning(
      "Values are zero or missing in the final year for ages ",
      paste(stale, collapse = ", "),
      ". Using the last finite value as the starting point for these ages.",
      call. = FALSE
    )
  }
  out <- out |>
    as_tsibble(index = indexvar, key = all_of(agevar)) |>
    as_vital(.age = agevar) |>
    select(all_of(c(indexvar, agevar)), everything())

  structure(
    list(
      fitted = out,
      model = model,
      last = last[c(agevar, measure)],
      response = measure,
      nobs = sum(!is.na(.data[[measure]]))
    ),
    class = "FNAIVE"
  )
}

#' @rdname forecast
#' @export
forecast.FNAIVE <- function(
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
  indexvar <- index_var(object$fitted)
  measure <- object$response
  last <- object$last
  horizon <- match(new_data[[indexvar]], sort(unique(new_data[[indexvar]])))
  ages <- new_data[[agevar]]
  sigma <- object$model$sigma[match(ages, object$model[[agevar]])]
  distributional::dist_normal(
    last[[measure]][match(ages, last[[agevar]])],
    sigma * sqrt(horizon)
  )
}

#' @export
generate.FNAIVE <- function(
  x,
  new_data = NULL,
  h = NULL,
  bootstrap = FALSE,
  times = 1,
  ...
) {
  agevar <- age_var(new_data)
  indexvar <- index_var(x$fitted)
  h <- length(unique(new_data[[indexvar]]))
  reps <- unique(new_data[[".rep"]])
  if (times != length(reps)) {
    stop("`times` must equal the number of replicates (`.rep`) in `new_data`")
  }
  measure <- x$response
  last <- x$last
  ages <- last[[agevar]]
  sigma <- x$model$sigma[match(ages, x$model[[agevar]])]
  # Innovations for each age (one per column), ordered by horizon within
  # replicate. Bootstrapped innovations take all ages from one residual year.
  n <- h * times
  if (bootstrap) {
    innov <- resample_years(x$fitted, agevar, ages, n, sigma)
  } else {
    innov <- vapply(
      seq_along(ages),
      function(i) stats::rnorm(n, sd = sigma[i]),
      numeric(n)
    )
  }
  # Cumulate innovations over the horizon for each path (one per column)
  paths <- apply(matrix(innov, nrow = h), 2, cumsum)
  out <- tibble(
    !!agevar := rep(ages, each = n),
    .rep = rep(rep(reps, each = h), length(ages)),
    horizon = rep(seq_len(h), times * length(ages)),
    .sim = c(paths) + rep(last[[measure]], each = n)
  )
  new_data$horizon <- match(
    new_data[[indexvar]],
    sort(unique(new_data[[indexvar]]))
  )
  new_data |>
    left_join(out, by = c("horizon", agevar, ".rep")) |>
    select(-horizon)
}

#' @export
glance.FNAIVE <- function(x, ...) {
  tibble(sigma2 = var(x$fitted$.resid, na.rm = TRUE))
}

#' @export
tidy.FNAIVE <- function(x, ...) {
  tidy_coefficients(x$model)
}

#' @export
report.FNAIVE <- function(object, ...) {
  cat("\n")
  print(object$model)
}

#' @export
model_sum.FNAIVE <- function(x) {
  paste0("FNAIVE")
}

#' @export
autoplot.FNAIVE <- function(
  object,
  age = NULL,
  ...
) {
  model_component_plot(object, age, "sigma") + ggplot2::ylab("Sigma")
}

#' @export
age_components.FNAIVE <- age_components.FMEAN

#' @export
time_components.FNAIVE <- function(object, ...) {
  stop("FNAIVE objects have no time components")
}
