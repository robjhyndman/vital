#' @importFrom stats sd
train_ave <- function(.data, ...) {
  indexvar <- index(.data)
  agevar <- attributes(.data)$agevar
  measures <- measured_vars(.data)
  measures <- measures[measures != agevar]
  ave_measure <- .data |>
    as_tibble() |>
    group_by(across(all_of(agevar))) |>
    summarise(.fitted = mean(.data[[measures]], na.rm=TRUE))
  out <- .data |>
    as_tibble() |>
    left_join(ave_measure, by = agevar) |>
    mutate(.resid = .data[[measures]] - .fitted)
  sigma <- out |>
    group_by(across(all_of(agevar))) |>
    summarise(sigma = sd(.resid, na.rm=TRUE))
  out <- out |>
    as_tsibble(index = indexvar, key=agevar) |>
    as_vital(.age = agevar) |>
    select(indexvar, agevar, everything())
  model <- ave_measure |>
    rename(mean = .fitted) |>
    left_join(sigma, by = agevar)

  structure(
    list(
      fitted = out,
      model = model,
      nobs = sum(!is.na(.data[[measures]]))
    ),
    class = "model_ave"
  )
}

#' Mean models
#'
#' \code{AVERAGE()} returns an iid model applied to the formula's response variable.
#'
#' @aliases report.model_ave
#'
#' @param formula Model specification.
#' @param ... Not used.
#'
#' @return A model specification.
#'
#' @seealso
#' [Forecasting: Principles and Practices, Some simple forecasting methods (section 3.2)](https://otexts.com/fpp3/simple-methods.html)
#'
#' @examples
#' library(dplyr)
#' aus_mortality |>
#'   filter(State == "Victoria", Sex == "female") |>
#'   model(mean = AVERAGE(Mortality))
#' @export
AVERAGE <- function(formula, ...) {
  mean_model <- new_model_class("mean", train = train_ave)
  new_model_definition(mean_model, !!enquo(formula), ...)
}

#' Produce forecasts from a vital model
#'
#' The forecast function allows you to produce future predictions of a time series
#' from fitted models. If the response variable has been transformed in the
#' model formula, the transformation will be automatically back-transformed
#' (and bias adjusted if `bias_adjust` is `TRUE`). More details about
#' transformations in the fable framework can be found in
#' `vignette("transformations", package = "fable")`.
#'
#' The forecasts returned contain both point forecasts and their distribution.
#' A specific forecast interval can be extracted from the distribution using the
#' [`hilo()`] function, and multiple intervals can be obtained using [`report()`].
#' These intervals are stored in a single column using the `hilo` class, to
#' extract the numerical upper and lower bounds you can use [`unpack_hilo()`].
#'
#' @param object A mable containing one or more models.
#' @param new_data A `tsibble` containing future information used to forecast.
#' @param bootstrap If `TRUE`, then forecast distributions are computed using simulation with resampled errors.
#' @param times The number of sample paths to use in estimating the forecast distribution when `bootstrap = TRUE`.
#' @param ... Additional arguments not used.
#'#'
#' @return
#' A fable containing the following columns:
#' - `.model`: The name of the model used to obtain the forecast. Taken from
#'   the column names of models in the provided mable.
#' - The forecast distribution. The name of this column will be the same as the
#'   dependent variable in the model(s). If multiple dependent variables exist,
#'   it will be named `.distribution`.
#' - Point forecasts computed from the distribution using the functions in the
#'   `point_forecast` argument.
#' - All columns in `new_data`, excluding those whose names conflict with the
#'   above.
#'
#'
#' @importFrom fabletools forecast
#' @importFrom stats qnorm time
#' @importFrom utils tail
#'
#' @export
forecast.model_ave <- function(object, new_data, bootstrap = FALSE, times = 5000, ...) {
  h <- NROW(new_data)

  y_ave <- object$model$mean
  n <- length(object$fitted$resid)
  sigma <- object$model$sigma

  # Produce forecasts
  if (bootstrap) { # Compute prediction intervals using simulations
    sim <- map(seq_len(times), function(x) {
      generate(object, new_data, bootstrap = TRUE)[[".sim"]]
    }) %>%
      transpose() %>%
      map(as.numeric)
    distributional::dist_sample(sim)
  } else {
    fc <- rep(y_ave, h)
    se <- sigma * sqrt(1 + 1 / n)
    distributional::dist_normal(fc, se)
  }
}

#' Generate from a mean model
#'
#' @param x A mable.
#' @param new_data A `tsibble` containing future information used to forecast.
#' @param bootstrap If `TRUE`, then forecast distributions are computed using simulation with resampled errors.
#' @param ... Additional arguments not used.
#' @importFrom stats na.omit
#'
#' @export
generate.model_ave <- function(x, new_data, bootstrap = FALSE, ...) {
  f <- x$mean

  if (!(".innov" %in% names(new_data))) {
    if (bootstrap) {
      res <- residuals(x)
      new_data$.innov <- sample(na.omit(res) - mean(res, na.rm = TRUE),
                                NROW(new_data),
                                replace = TRUE
      )
    }
    else {
      new_data$.innov <- stats::rnorm(NROW(new_data), sd = x$sigma)
    }
  }

  transmute(group_by_key(new_data), ".sim" := f + !!sym(".innov"))
}

#' Interpolate missing values
#'
#' Uses a fitted model to interpolate missing values from a dataset.
#'
#' @param object A mable containing a single model column.
#' @param new_data A dataset with the same structure as the data used to fit the model.
#' @param ... Other arguments passed to interpolate methods.
#'
#' @rdname interpolate
#' @export
interpolate.model_ave <- function(object, new_data, ...) {
  # Get inputs
  y <- new_data[[measured_vars(new_data)]]
  window_size <- object$window
  miss_val <- is.na(y)

  if (!is.na(window_size)) {
    fits <- dplyr::lag(
      slide_dbl(y, mean, na.rm = TRUE, .size = window_size, .partial = TRUE)
    )[miss_val]
  }
  else {
    fits <- object$mean
  }

  new_data[[measured_vars(new_data)]][miss_val] <- fits
  new_data
}

#' Fitted values from a mean model
#'
#' @param object TBD
#' @param ... Not used.
#'
#' @export
fitted.model_ave <- function(object, ...) {
  object$fitted$.fitted
}

#' Residuals from a mean model
#'
#' @param object TBD
#' @param ... Not used.
#'
#' @export
residuals.model_ave <- function(object, ...) {
  object$fitted$.resid
}

#' Glance a average method model
#'
#' Construct a single row summary of the average method model.
#'
#' Contains the variance of residuals (`sigma2`).
#'
#' @inheritParams generics::glance
#'
#' @return A one row tibble summarising the model's fit.
#'
#' @export
glance.model_ave <- function(x, ...) {
  stop("Not sure what to put here yet")
}

#' Extract model coefficients from a mable
#'
#' This function will obtain the coefficients (and associated statistics) for
#' each model in the mable.
#'
#' @param x A mable.
#' @param ... Arguments for model methods.
#'
#' @rdname tidy
#' @export
tidy.model_ave <- function(x, ...) {
  x$model  |>
    mutate(
      term = "mean",
      estimate = mean,
      std.error = sigma / sqrt(x$nobs),
      stat = mean/std.error,
      p.value = 2 * stats::pt(abs(stat), x$nobs - 1, lower.tail = FALSE)
    ) |>
    select(-mean, -sigma)
}


#' @export
report.model_ave <- function(object, ...) {
  cat("\n")
  print(object$model)
}

#' @export
model_sum.model_ave <- function(x) {
  paste0("AVERAGE") # , ", intToUtf8(0x3BC), "=", format(x$par$estimate))
}

#' Refit a AVERAGE model
#'
#' Applies a fitted average method model to a new dataset.
#'
#' @param object A mable
#' @param new_data A dataset with the same structure as the data used to fit the model.
#' @param reestimate If `TRUE`, the mean for the fitted model will be re-estimated
#' to suit the new data.
#' @param ... Additional arguments not used.
#'
#' @examples
#' lung_deaths_male <- as_tsibble(mdeaths)
#' lung_deaths_female <- as_tsibble(fdeaths)
#'
#' fit <- lung_deaths_male %>%
#'   model(AVERAGE(value))
#'
#' report(fit)
#'
#' fit %>%
#'   refit(lung_deaths_female) %>%
#'   report()
#' @export
refit.model_ave <- function(object, new_data, reestimate = FALSE, ...) {
  # Update data for re-evaluation

  if (reestimate) {
    return(train_ave(new_data, ...))
  }

  y <- unclass(new_data)[[measured_vars(new_data)]]

  if (all(is.na(y))) {
    abort("All new observations are missing, model cannot be applied.")
  }

  n <- length(y)

  fits <- rep(object$mean, n)
  res <- y - fits
  sigma <- sd(res, na.rm = TRUE)

  object$fitted <- fits
  object$resid <- res
  object$sigma <- sigma
  object$nobs <- sum(!is.na(y))
  object
}


slide_dbl <- function (.x, .fn, ..., .size = 1, .partial = FALSE) {
    out <- numeric(if (.partial)
        length(.x)
    else length(.x) - .size + 1)
    for (i in seq_along(out)) {
        idx <- seq.int(i + .size * (-1L + !.partial) + .partial,
            i + (.size * !.partial) - 1 + .partial, by = 1L)
        idx[idx <= 0] <- NA_integer_
        out[i] <- .fn(.x[idx], ...)
    }
    out
}

globalVariables(c(".resid"))
