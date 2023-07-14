#' Functional mean models
#'
#' \code{FMEAN()} returns an iid functional model applied to the formula's response variable as a function of age.
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
    class = "FMEAN"
  )
}

#' @rdname forecast
#' @export
forecast.FMEAN <- function(object, new_data, bootstrap = FALSE, times = 5000, ...) {
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

#' @rdname generate
#' @export
generate.FMEAN <- function(x, new_data, h = NULL, bootstrap = FALSE, times = 1, seed = NULL,  ...) {
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
#' @author Rob J Hyndman
#' @export
interpolate.FMEAN <- function(object, new_data, ...) {
  agevar <- attributes(new_data)$agevar
  measures <- measured_vars(new_data)
  measures <- measures[measures != agevar]
  y <- new_data[[measures]]
  miss_val <- is.na(y)
  fits <- object$fitted$.fitted

  new_data[[measures]][miss_val] <- fits[miss_val]
  new_data
}

#' @export
glance.FMEAN <- function(x, ...) {
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
#' @author Rob J Hyndman
#' @export
tidy.FMEAN <- function(x, ...) {
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
report.FMEAN <- function(object, ...) {
  cat("\n")
  print(object$model)
}

#' @export
model_sum.FMEAN <- function(x) {
  paste0("FMEAN")
}

#' @export
prepare_autoplot.FMEAN <- function(object, ...) {
  object$model
}

#' @export
autoplot.FMEAN <- function(object, age = "Age",...) {
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
  p
}


globalVariables(c(".resid", "sigma", "std.error", "stat", ".innov"))
