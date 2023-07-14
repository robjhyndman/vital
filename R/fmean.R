#' Functional mean models
#'
#' \code{FMEAN()} returns an iid functional model applied to the formula's response variable as a function of age.
#'
#' @aliases report.model_fmean
#'
#' @param formula Model specification.
#' @param ... Not used.
#'
#' @return A model specification.
#'
#'
#' @examples
#' library(dplyr)
#' aus_mortality |>
#'   filter(State == "Victoria", Sex == "female") |>
#'   model(mean = FMEAN(Mortality))
#' @export
FMEAN <- function(formula, ...) {
  fmean_model <- new_model_class("fmean", train = train_fmean)
  new_model_definition(fmean_model, !!enquo(formula), ...)
}

#' @importFrom stats sd
train_fmean <- function(.data, ...) {
  attrx <- attributes(.data)
  indexvar <- index_var(.data)
  agevar <- attrx$agevar
  measures <- measured_vars(.data)
  measures <- measures[!(measures %in% c(agevar, attrx$populationvar))]
  ave_measure <- .data |>
    as_tibble() |>
    group_by(!!sym(agevar)) |>
    summarise(.fitted = mean(.data[[measures]], na.rm=TRUE))
  out <- .data |>
    as_tibble() |>
    left_join(ave_measure, by = agevar) |>
    mutate(
      .resid = .data[[measures]] - .fitted,
      .innov = .resid
    )
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
    class = "model_fmean"
  )
}

#' Produce forecasts from a vital model
#'
#' The forecast function allows you to produce future predictions of a functional
#' time series model, where the response is a function of age.
#' If the response variable has been transformed in the
#' model formula, the transformation will be automatically back-transformed
#' and bias-adjusted. More details about
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
#' @importFrom fabletools forecast
#' @importFrom stats qnorm time
#' @importFrom utils tail
#'
#' @rdname forecast
#' @export
forecast.model_fmean <- function(object, new_data, bootstrap = FALSE, times = 5000, ...) {
  # Produce forecasts
  if (bootstrap) {
    sim <- map(seq_len(times), function(x) {
      generate(object, new_data, bootstrap = TRUE)[[".sim"]]
    }) %>%
      transpose() %>%
      map(as.numeric)
    distributional::dist_sample(sim)
  } else {
    agevar <- attributes(new_data)$agevar
    new_data |>
      left_join(object$model, by = agevar) |>
      transmute(fc = distributional::dist_normal(mean, sigma))  |>
      pull(fc)
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
generate.model_fmean <- function(x, new_data, bootstrap = FALSE, ...) {
  agevar <- attributes(new_data)$agevar
  new_data <- new_data |>
    dplyr::left_join(x$model, by = agevar)
  times <- length(unique(new_data$.rep))

  if (!(".innov" %in% names(new_data))) {
    if (bootstrap) {
      indexvar <- index_var(new_data)
      innov <- as_tibble(x$fitted) |>
        select(agevar, .innov) |>
        nest_by(!!sym(agevar)) |>
        mutate(
          data = list(
            tibble(
              .innov = sample(unlist(na.omit(data)), times, replace = TRUE),
              .rep = as.character(seq_along(.innov))
            ))
        ) |>
        tidyr::unnest(data)
      new_data <- new_data |>
        left_join(innov, by=c(agevar, ".rep"))
    }
    else {
      new_data$.innov <- stats::rnorm(NROW(new_data), sd = x$model$sigma)
    }
  }

  transmute(group_by_key(new_data), ".sim" := mean + .innov)
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
interpolate.model_fmean <- function(object, new_data, ...) {
  agevar <- attributes(new_data)$agevar
  measures <- measured_vars(new_data)
  measures <- measures[measures != agevar]
  y <- new_data[[measures]]
  miss_val <- is.na(y)
  fits <- object$fitted$.fitted

  new_data[[measures]][miss_val] <- fits[miss_val]
  new_data
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
glance.model_fmean <- function(x, ...) {
  tibble(sigma2 = var(x$fitted$.resid, na.rm=TRUE))
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
tidy.model_fmean <- function(x, ...) {
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
report.model_fmean <- function(object, ...) {
  cat("\n")
  print(object$model)
}

#' @export
model_sum.model_fmean <- function(x) {
  paste0("FMEAN")
}


#' @export
prepare_autoplot.model_fmean <- function(object, ...) {
  object$model
}

#' @export
autoplot.model_fmean <- function(object, age = "Age",...) {
  object <- object  |>
    tidyr::unnest("out")
  keys <- colnames(object)
  keys <- keys[!(keys %in% c("mean","sigma", age))]
  nk <- length(keys)
  object <- object |> rename(.mean = mean)
  aes_spec <- list(x = sym(age), y = expr(.mean))
  if(nk > 0) {
    aes_spec[["colour"]] <- expr(interaction(!!!syms(keys), sep="/"))
  }
  p <- ggplot(object, eval_tidy(expr(aes(!!!aes_spec)))) +
    geom_line() + ggplot2::labs(x = age, y = "Mean")
  if(nk > 1) {
    p <- p + ggplot2::guides(colour = ggplot2::guide_legend(paste0(keys, collapse="/")))
  }
  print(p)
}


globalVariables(c(".resid", "sigma", "std.error", "stat", ".innov"))
