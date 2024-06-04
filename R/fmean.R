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
#' fmean <- aus_mortality |>
#'   dplyr::filter(State == "Victoria", Sex == "female") |>
#'   model(mean = FMEAN(Mortality))
#' report(fmean)
#' autoplot(fmean) + ggplot2::scale_y_log10()
#' @export
FMEAN <- function(formula, ...) {
  fmean_model <- new_model_class("fmean", train = train_fmean)
  new_model_definition(fmean_model, !!enquo(formula), ...)
}

train_fmean <- function(.data, ...) {
  attrx <- attributes(.data)
  indexvar <- index_var(.data)
  agevar <- attrx$agevar
  measures <- measured_vars(.data)
  measures <- measures[!(measures %in% c(agevar, attrx$populationvar))]
  measure <- measures[1]
  ave_measure <- .data |>
    as_tibble() |>
    group_by(!!sym(agevar)) |>
    summarise(.fitted = mean(.data[[measure]], na.rm=TRUE))
  out <- .data |>
    as_tibble() |>
    left_join(ave_measure, by = agevar) |>
    mutate(
      .resid = .data[[measure]] - .fitted,
      .innov = .resid
    )
  sigma <- out |>
    group_by(across(all_of(agevar))) |>
    summarise(sigma = sd(.resid, na.rm=TRUE))
  out <- out |>
    as_tsibble(index = indexvar, key=agevar) |>
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
forecast.FMEAN <- function(object, new_data = NULL, h = NULL,
    point_forecast = list(.mean = mean),
    simulate = FALSE, bootstrap = FALSE, times = 5000, ...) {
  # simulation/bootstrap not actually used here as forecast.mdl_vtl_ts
  # handles this using generate() and forecast.LC is never called.
  # The arguments are included so they show in the docs
  # Similarly for h and point_forecast
  agevar <- attributes(new_data)$agevar
  new_data |>
    left_join(object$model, by = agevar) |>
    transmute(fc = distributional::dist_normal(mean, sigma))  |>
    pull(fc)
}

#' @export
generate.FMEAN <- function(x, new_data = NULL, h = NULL,
    bootstrap = FALSE, times = 1,  ...) {
  agevar <- attributes(new_data)$agevar
  new_data <- new_data |>
    dplyr::left_join(x$model, by = agevar)
  if(times != length(unique(new_data$.rep)))
    stop("We have a problem")

  if (!(".innov" %in% names(new_data))) {
    if (bootstrap) {
      indexvar <- index_var(new_data)
      innov <- as_tibble(x$fitted) |>
        select(all_of(c(agevar, ".innov"))) |>
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

#' @export
glance.FMEAN <- function(x, ...) {
  tibble(sigma2 = var(x$fitted$.resid, na.rm=TRUE))
}

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
autoplot.FMEAN <- function(object, age = "Age",...) {
  modelname <- attributes(object)$model
  object <- object |>
    mutate(out = purrr::map(object[[modelname]], function(x){x$fit$model}))
  object[[modelname]] <- NULL
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

#' @export
interpolate.FMEAN <- function (object, new_data, specials, ...) {
  attrx <- attributes(new_data)
  keyvar <- key_vars(new_data)
  agevar <- attrx$agevar
  timevar <- attrx$index
  measures <- measured_vars(new_data)
  measures <- measures[!(measures %in% c(agevar, attrx$populationvar))]
  measure <- measures[1]
  fits <- fitted(object) |> select(.fitted)
  output <- as_tibble(new_data) |>
    dplyr::left_join(as_tibble(fits), by = c(agevar, timevar))
  missing <- is.na(output[[measure]])
  output[[measure]][missing] <- output$.fitted[missing]
  output$.fitted <- NULL
  return(vital(output, key = unique(c(keyvar, agevar)),
               index = timevar, .age = agevar))
}

globalVariables(c(".resid", "sigma", "std.error", "stat", ".innov"))
