#' Functional naive model
#'
#' `FNAIVE()` returns an random walk functional model applied to the formula's response variable as a function of age.
#'
#' @aliases report.FNAIVE
#'
#' @param formula Model specification.
#' @param ... Not used.
#'
#' @return A model specification.
#'
#'
#' @author Rob J Hyndman
#' @examples
#' fnaive <- aus_mortality |>
#'   dplyr::filter(State == "Victoria", Sex == "female") |>
#'   model(fit = FNAIVE(Mortality))
#' report(fnaive)
#' autoplot(fnaive) + ggplot2::scale_y_log10()
#' @export
FNAIVE <- function(formula, ...) {
  fnaive_model <- new_model_class("fnaive", train = train_fnaive)
  new_model_definition(fnaive_model, !!enquo(formula), ...)
}

train_fnaive <- function(.data, ...) {
  attrx <- attributes(.data)
  indexvar <- index_var(.data)
  agevar <- attrx$agevar
  measures <- measured_vars(.data)
  measures <- measures[!(measures %in% c(agevar, attrx$populationvar))]
  measure <- measures[1]
  last_measure <- .data |>
    tibble::as_tibble() |>
    dplyr::mutate(index = .data[[indexvar]] + 1)
  last_measure <- last_measure[, c("index", agevar, measure)]
  colnames(last_measure) <- c(indexvar, agevar, ".fitted")
  out <- .data |>
    as_tibble() |>
    left_join(last_measure, by = c(indexvar, agevar)) |>
    mutate(
      .resid = .data[[measure]] - .fitted,
      .innov = .resid
    )
  model <- out |>
    group_by(across(all_of(agevar))) |>
    summarise(sigma = sd(.resid, na.rm = TRUE))
  out <- out |>
    as_tsibble(index = indexvar, key = agevar) |>
    as_vital(.age = agevar) |>
    select(all_of(c(indexvar, agevar)), everything())

  structure(
    list(
      fitted = out,
      model = model,
      nobs = sum(!is.na(.data[[measure]]))
    ),
    class = "FNAIVE"
  )
}

#' @rdname forecast
#' @export
forecast.FNAIVE <- function(
    object, new_data = NULL, h = NULL,
    point_forecast = list(.mean = mean),
    simulate = FALSE, bootstrap = FALSE, times = 5000, ...) {
  # simulation/bootstrap not actually used here as forecast.mdl_vtl_ts
  # handles this using generate() and forecast.LC is never called.
  # The arguments are included so they show in the docs
  # Similarly for h and point_forecast
  agevar <- attributes(new_data)$agevar
  indexvar <- index_var(object$fitted)
  h <- length(unique(new_data[[indexvar]]))
  fc <- object$fitted |>
    as_tsibble() |>
    left_join(object$model, by = agevar) |>
    group_by(!!sym(agevar)) |>
    dplyr::group_modify(function(x, ...) {
      index <- colnames(x)[[1]]
      measure <- colnames(x)[[2]]
      mean <- x[[measure]][x[[index]] == max(x[[index]])]
      sigma <- x$sigma[1]
      out <- tibble(
        horizon = seq(h),
        mean = rep(mean, h),
        sigma = sigma * sqrt(seq(h))
      ) |>
        mutate(fc = distributional::dist_normal(mean, sigma))
      out[, c("horizon", "fc")]
    })
  new_data$horizon <- new_data[[indexvar]] - min(new_data[[indexvar]]) + 1
  new_data |>
    left_join(fc, by = c("horizon", agevar)) |>
    pull(fc)
}

#' @export
generate.FNAIVE <- function(
    x, new_data = NULL, h = NULL,
    bootstrap = FALSE, times = 1, ...) {
  agevar <- attributes(new_data)$agevar
  indexvar <- index_var(x$fitted)
  h <- length(unique(new_data[[indexvar]]))
  reps <- length(unique(new_data[[".rep"]]))
  if (times != reps) {
    stop("We have a problem")
  }
  measure <- colnames(x$fitted)[3]
  setup <- x$fitted[,c(indexvar, agevar, measure, ".innov")] |>
    left_join(x$model, by = agevar) |>
    tidyr::expand_grid(.rep = unique(new_data$.rep)) |>
    group_by(!!sym(agevar), .rep)
  if(bootstrap) {
    out <- setup |>
      dplyr::group_modify(function(x, ...) {
        measure <- colnames(x)[[2]]
        innov <- sample(x$.innov, size = h, replace = TRUE)
        tibble(horizon = seq(h), .sim = tail(x[[measure]], 1) + cumsum(innov))
      })
  } else {
    out <- setup |>
      dplyr::group_modify(function(x, ...) {
        measure <- colnames(x)[[2]]
        innov <- stats::rnorm(n = h, sd = x$sigma[1])
        tibble(horizon = seq(h), .sim = tail(x[[measure]], 1) + cumsum(innov))
    })
  }
  new_data$horizon <- new_data[[indexvar]] - min(new_data[[indexvar]]) + 1
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
  NULL
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
autoplot.FNAIVE <- function(object, age = "Age", ...) {
  modelname <- attributes(object)$model
  object <- object |>
    mutate(out = purrr::map(object[[modelname]], function(x){x$fit$model}))
  object[[modelname]] <- NULL
  object <- object  |>
    tidyr::unnest("out")
  keys <- colnames(object)
  keys <- keys[!(keys %in% c("sigma", age))]
  nk <- length(keys)
  aes_spec <- list(x = sym(age), y = expr(sigma))
  if (nk > 0) {
    aes_spec[["colour"]] <- expr(interaction(!!!syms(keys), sep = "/"))
  }
  p <- ggplot(object, eval_tidy(expr(aes(!!!aes_spec)))) +
    geom_line() +
    ggplot2::labs(x = age, y = "Sigma")
  if (nk > 1) {
    p <- p + ggplot2::guides(colour = ggplot2::guide_legend(paste0(keys, collapse = "/")))
  }
  p
}


globalVariables(c(".resid", "sigma", "std.error", "stat", ".innov", "fit", "horizon"))
