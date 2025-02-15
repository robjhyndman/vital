#' Generalized APC stochastic mortality model
#'
#' A Generalized Age-Period-Cohort (GAPC) stochastic mortality model, as defined in the StMoMo package.
#' `GAPC()` returns a GAPC model applied to the formula's response
#' variable as a function of age.
#'
#' @aliases report.GAPC
#' @param formula Model specification.
#' @param ... Arguments passed to StMoMo::StMoMo()
#'
#' @references Villegas, A.M., Millossovich, P., and Kaishev, V.K. (2018).
#' StMoMo: An R package for stochastic mortality modelling.
#' *Journal of Statistical Software*, **84**(3), 1-38.
#' <doi:10.18637/jss.v084.i03>
#' @author Rob J Hyndman
#' @return A model specification.
#'
#' @examples
#' f2 <- function(x, ages) x - mean(ages)
#' cbd <- aus_mortality |>
#'   dplyr::filter(State == "Victoria", Sex == "female") |>
#'   model(cbd = GAPC(Mortality, link = "logit", staticAgeFun = FALSE,
#'   periodAgeFun = c("1", f2)))
#' report(cbd)
#' @export
GAPC <- function(formula, ...) {
  stmomo_model <- new_model_class("stmomo", train = train_stmomo)
  new_model_definition(stmomo_model, !!enquo(formula), ...)
}

train_stmomo <- function(.data, sex = NULL, specials, scale = FALSE, ...) {
  # Variable names
  indexvar <- index_var(.data)
  vvar <- vital_var_list(.data)
  measures <- measured_vars(.data)
  measures <- measures[!(measures %in% c(vvar$age, vvar$population))]
  measures <- measures[1]

  # Compute StMoMo model
  model <- StMoMo::StMoMo(...)
  data2 <- vital_to_stmomo(.data)
  out <- StMoMo::fit(model, Dxt = data2$Dxt, Ext=data2$Ext, ages = data2$ages, years = data2$years)

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
  indexvar <- index_var(new_data)
  h <- length(unique(new_data[[indexvar]]))
  pred <- forecast(object$model, h=h)
  df <- as.data.frame(pred$rates) |>
    mutate(age = pred$ages) |>
    pivot_longer(-age, names_to = "year", values_to = ".mean")
  df <- df[,c("year","age",".mean")]
  colnames(df)[1:2] <- colnames(new_data)
  df$Year <- as.numeric(df$Year)
  new_data <- left_join(new_data, df, by = c("Age","Year"))
  dist <- as.character(object$model$fittingModel$family)[1]
  if(dist == "poisson") {
    return(distributional::dist_poisson(new_data$.mean))
  } else if(dist == "binomial") {
    return(distributional::dist_bernoulli(new_data$.mean))
  } else {
    stop("Unknown distribution")
  }
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
  pred <- simulate(x$model, nsim=times, h=max(2,h))
  df <- as.data.frame(pred$rates) |>
    mutate(age = pred$ages) |>
    pivot_longer(-age, names_to = "year", values_to = ".sim") |>
    mutate(
      .rep = stringr::str_extract(year, "\\d*$"),
      year = as.numeric(stringr::str_remove(year, "\\.\\d*$"))
    )
  df <- df[,c(".rep","year","age",".sim")]
  colnames(df)[1:3] <- colnames(new_data)
  new_data <- left_join(new_data, df, by = c(".rep", "Year", "Age"))
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
  if(!("deaths" %in% names(vvar))) {
    stop("Deaths variable is required")
  }
  if(!("population" %in% names(vvar))) {
    stop("Population variable is required")
  }
  ages <- sort(unique(.data[[vvar$age]]))
  years <- sort(unique(.data[[indexvar]]))
  .data <- dplyr::arrange(as_tibble(.data), !!rlang::sym(indexvar), !!rlang::sym(vvar$age))
  Dxt <- matrix(.data[[vvar$deaths]], nrow = length(ages), ncol = length(years))
  Ext <- matrix(.data[[vvar$population]], nrow = length(ages), ncol = length(years))
  list(Dxt = Dxt, Ext = Ext, ages = ages, years = years)
}
