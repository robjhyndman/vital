#' Functional mean model
#'
#' `FMEAN()` returns an iid functional model applied to the formula's response variable as a function of age.
#'
#' @aliases report.FMEAN
#'
#' @param formula Model specification.
#' @param ... Not used.
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
  fmean_model <- new_model_class("fmean", train = train_fmean)
  new_model_definition(fmean_model, !!enquo(formula), ...)
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
  out <- out |>
    as_tsibble(index = indexvar, key = all_of(agevar)) |>
    as_vital(.age = agevar) |>
    select(all_of(c(indexvar, agevar)), everything())
  model <- ave_measure |>
    rename(mean = .fitted) |>
    left_join(sigma, by = agevar)

  structure(
    list(
      fitted = out,
      model = model,
      nobs = sum(!is.na(.data[[measure]]))
    ),
    class = "FMEAN"
  )
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
  # simulation/bootstrap not actually used here as forecast.mdl_vtl_ts
  # handles this using generate() and forecast.FMEAN is never called.
  # The arguments are included so they show in the docs
  # Similarly for h and point_forecast
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
      innov <- as_tibble(x$fitted) |>
        select(all_of(c(agevar, ".innov"))) |>
        nest_by(!!sym(agevar)) |>
        mutate(
          data = list(
            tibble(
              .innov = sample(unlist(na.omit(data)), times, replace = TRUE),
              .rep = as.character(seq_along(.innov))
            )
          )
        ) |>
        tidyr::unnest(data)
      new_data <- new_data |>
        left_join(innov, by = c(agevar, ".rep"))
    } else {
      new_data$.innov <- stats::rnorm(NROW(new_data), sd = new_data$sigma)
    }
  }

  transmute(group_by_key(new_data), ".sim" := mean + .innov)
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
  if (is.null(age)) {
    age <- age_var(first_fit(object)$data)
  }
  modelname <- attributes(object)$model
  object <- object |>
    mutate(
      out = purrr::map(object[[modelname]], function(x) {
        x$fit$model
      })
    )
  object[[modelname]] <- NULL
  object <- object |>
    tidyr::unnest("out")
  keys <- colnames(object)
  keys <- keys[!(keys %in% c("mean", "sigma", age))]
  nk <- length(keys)
  object <- object |> rename(.mean = mean)
  aes_spec <- list(x = sym(age), y = expr(.mean))
  if (nk > 0) {
    aes_spec[["colour"]] <- expr(interaction(!!!syms(keys), sep = "/"))
  }
  p <- ggplot(object, eval_tidy(expr(aes(!!!aes_spec)))) +
    geom_line() +
    ggplot2::labs(x = age, y = "Mean")
  if (nk > 1) {
    p <- p +
      ggplot2::guides(
        colour = ggplot2::guide_legend(paste0(keys, collapse = "/"))
      )
  }
  p
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
globalVariables(c(".resid", "sigma", "std.error", "stat", ".innov", ".n"))
