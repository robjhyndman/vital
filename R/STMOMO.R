#' Generalized APC stochastic mortality model
#'
#' A Generalized Age-Period-Cohort (GAPC) stochastic mortality model, as defined
#' in Villegas et al. (2018). The StMoMo package is used to fit the model.
#' `GAPC()` returns a GAPC model applied to the formula's response variable as a function of age.
#' The model will optionally call \code{\link[stMoMo]{genWeightMat}} with arguments `clip` and `zeroCohorts`.
#' All other arguments are passed to \code{\link[StMoMo]{StMoMo}}.
#'
#' @aliases report.GAPC
#' @param formula Model specification
#' @param use_weights If `TRUE`, will call \code{\link[stMoMo]{genWeightMat}} with arguments `clip` and `zeroCohorts`.
#' @param clip Passed to \code{\link[stMoMo]{genWeightMat}()}
#' @param zeroCohorts Passed to \code{\link[stMoMo]{genWeightMat}()}
#' @param ... All other arguments passed to \code{\link[StMoMo]{StMoMo}()}
#'
#' @references Villegas, A.M., Millossovich, P., and Kaishev, V.K. (2018).
#' StMoMo: An R package for stochastic mortality modelling.
#' *Journal of Statistical Software*, **84**(3), 1-38.
#' <doi:10.18637/jss.v084.i03>
#' @author Rob J Hyndman
#' @return A model specification.
#'
#' @examples
#' # CBD model
#' f2 <- function(x, ages) x - mean(ages)
#' cbd <- aus_mortality |>
#'   dplyr::filter(State == "Victoria", Sex == "female") |>
#'   model(cbd = GAPC(Mortality, link = "log",
#'     staticAgeFun = FALSE, periodAgeFun = c("1", f2))
#'   )
#' report(cbd)
#' @export
GAPC <- function(formula, use_weights = TRUE, clip = 0, zeroCohorts = NULL, ...) {
  stmomo_model <- new_model_class("stmomo", train = train_stmomo)
  new_model_definition(stmomo_model, !!enquo(formula),
    use_weights = use_weights,
    clip = clip, zeroCohorts = zeroCohorts, ...
  )
}

train_stmomo <- function(.data, sex = NULL, specials,
                         use_weights = TRUE, clip = 0, zeroCohorts = NULL, ...) {
  # Variable names
  indexvar <- index_var(.data)
  vvar <- vital_var_list(.data)
  measures <- measured_vars(.data)
  measures <- measures[!(measures %in% c(vvar$age, vvar$population))]
  measures <- measures[1]

  # Compute StMoMo model
  model <- StMoMo::StMoMo(...)
  data2 <- vital_to_stmomo(.data)
  if (model$link == "logit" & any(data2$Dxt / data2$Ext > 1)) {
    stop("Mortality rates must be less than 1 for logit link.
    Perhaps you need to use initial rather than central population values.")
  }

  if (use_weights) {
    wxt <- StMoMo::genWeightMat(ages = data2$ages,
      years = data2$years,
      clip = clip, zeroCohorts = zeroCohorts
    )
    miss <- is.na(data2$Dxt / data2$Ext)
    wxt[miss] <- 0
    data2$Dxt[miss] <- 0
    data2$Ext[miss] <- 1
  } else {
    wxt <- NULL
  }
  out <- StMoMo::fit(model,
    Dxt = data2$Dxt, Ext = data2$Ext,
    ages = data2$ages, years = data2$years, wxt = wxt, verbose = FALSE
  )
  out$data$series <- sex
  out$data$label <- "vital"

  structure(
    list(
      model = out,
      fitted = fitted(out),
      nobs = sum(!is.na(.data[[measures]]))
    ),
    class = "GAPC"
  )
}

#' @rdname forecast
#' @export

forecast.GAPC <- function(
    object, new_data = NULL, h = NULL, point_forecast = list(.mean = mean),
    simulate = FALSE, bootstrap = FALSE, times = 5000, ...) {
  # Uncertainty does not work here. Users should call with simulate = TRUE for PI
  warning("Use simulate = TRUE to get distributional forecasts")
  indexvar <- index_var(new_data)
  h <- length(unique(new_data[[indexvar]]))
  pred <- forecast(object$model, h = h)
  df <- as.data.frame(pred$rates) |>
    mutate(age = pred$ages) |>
    pivot_longer(-age, names_to = "year", values_to = ".mean")
  df <- df[, c("year", "age", ".mean")]
  colnames(df)[1:2] <- colnames(new_data)
  df$Year <- as.numeric(df$Year)
  if(any(sort(unique(df$Year)) != sort(unique(new_data$Year)))) {
    stop("Years don't match")
  }
  if(any(sort(unique(df$Age)) != sort(unique(new_data$Age)))) {
    stop("Ages don't match")
  }
  left_join(new_data, df, by = c("Age", "Year")) |>
    dplyr::pull(.mean) |>
    distributional::dist_degenerate()
}

#' @export
generate.GAPC <- function(
    x, new_data = NULL, h = NULL,
    bootstrap = FALSE, times = 1, ...) {
  agevar <- age_var(new_data)
  indexvar <- index_var(new_data)
  h <- length(unique(new_data[[indexvar]]))
  if (times != length(unique(new_data$.rep))) {
    stop("We have a problem")
  }
  pred <- simulate(x$model, nsim = times, h = max(2, h))
  df <- as.data.frame(pred$rates) |>
    mutate(age = pred$ages) |>
    pivot_longer(-age, names_to = "year", values_to = ".sim") |>
    mutate(
      .rep = stringr::str_extract(year, "\\d*$"),
      year = as.numeric(stringr::str_remove(year, "\\.\\d*$"))
    )
  df <- df[, c(".rep", "year", "age", ".sim")]
  colnames(df)[1:3] <- colnames(new_data)
  left_join(new_data, df, by = c(".rep", "Year", "Age"))
}

#' @export
glance.GAPC <- function(x, ...) {
  tibble(
    loglik = x$model$loglik,
    deviance = x$model$deviance,
    nobs = x$model$nobs,
    npar = x$model$npar
  )
}

#' @export
tidy.GAPC <- function(x, ...) {
  return(NULL)
}

#' @export
report.GAPC <- function(object, ...) {
  print(object$model)
}

#' @export
model_sum.GAPC <- function(x) {
  paste0("GAPC")
}

# Convert vital object to a StMoMoData object
vital_to_stmomo <- function(.data) {
  # Variable names
  indexvar <- index_var(.data)
  vvar <- vital:::vital_var_list(.data)
  if (!("deaths" %in% names(vvar))) {
    stop("Deaths variable is required")
  }
  if (!("population" %in% names(vvar))) {
    stop("Population variable is required")
  }
  ages <- sort(unique(.data[[vvar$age]]))
  years <- sort(unique(.data[[indexvar]]))
  .data <- dplyr::arrange(as_tibble(.data), !!rlang::sym(indexvar), !!rlang::sym(vvar$age))
  Dxt <- round(matrix(.data[[vvar$deaths]], nrow = length(ages), ncol = length(years)))
  Ext <- matrix(.data[[vvar$population]], nrow = length(ages), ncol = length(years))
  list(Dxt = Dxt, Ext = Ext, ages = ages, years = years)
}
