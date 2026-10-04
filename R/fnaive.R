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
#' fnaive <- norway_mortality |>
#'   dplyr::filter(Sex == "Female") |>
#'   model(fit = FNAIVE(Mortality))
#' report(fnaive)
#' autoplot(fnaive) + ggplot2::scale_y_log10()
#' @export
FNAIVE <- function(formula, ...) {
  fnaive_model <- new_model_class("fnaive", train = train_fnaive)
  new_model_definition(fnaive_model, !!enquo(formula), ...)
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
    dplyr::mutate(index = .data[[indexvar]] + step)
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
  out <- out |>
    as_tsibble(index = indexvar, key = all_of(agevar)) |>
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
  fitted <- as_tibble(object$fitted)
  measure <- colnames(fitted)[3]
  # Random walks start from the last observation for each age
  last <- fitted[fitted[[indexvar]] == max(fitted[[indexvar]]), ]
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
  measure <- colnames(x$fitted)[3]
  fitted <- as_tibble(x$fitted)
  # Random walks start from the last observation for each age
  last <- fitted[fitted[[indexvar]] == max(fitted[[indexvar]]), ]
  ages <- last[[agevar]]
  sigma <- x$model$sigma[match(ages, x$model[[agevar]])]
  # Innovations for each age, ordered by horizon within replicate
  n <- h * times
  innov <- vapply(
    seq_along(ages),
    function(i) {
      if (!bootstrap) {
        return(stats::rnorm(n, sd = sigma[i]))
      }
      pool <- fitted$.innov[fitted[[agevar]] == ages[i]]
      pool <- pool[!is.na(pool)]
      if (length(pool) == 0L) {
        return(rep(NA_real_, n))
      }
      pool[sample.int(length(pool), size = n, replace = TRUE)]
    },
    numeric(n)
  )
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

globalVariables(c("fit", "horizon"))
