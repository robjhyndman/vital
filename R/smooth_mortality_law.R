#' Function to smooth mortality rates using using MortalityLaw package
#'
#' This smoothing function allows smoothing of a variable in a vital object using MortalityLaw package.
#' The vital object is returned along with some additional columns containing
#' information about the smoothed variable: usually `.smooth` containing the
#' smoothed values, and `.smooth_se` containing the corresponding standard errors.
#'
#' `smooth_mortality_law()` smoothes mortality rates using specified mortality law and optimisation method.
#' @param .data A vital object
#' @param .var name of variable to smooth
#' @param law name of mortality law. For available mortality laws, users can check the \code{\link{MortalityLaws::availableLaws}}
#' function to learn about the available options.
#' @param opt.method optimisation method. The models can be fitted under the maximum likelihood methodology
#' or by selecting a loss function to be optimised. See the implemented loss function by running the \code{\link{MortalityLaws::xavailableLF}} function.
#' @return vital with added columns containing smoothed values and their standard errors
#' @keywords smooth, mortality
#' @rdname smooth_law_vital
#' @author Sixian Tang and Rob J Hyndman
#' @examples
#' library(MortalityLaws)
#' norway_mortality |> smooth_mortality_law(Mortality)
#'
#' @export
smooth_mortality_law <- function(.data, .var, law = "gompertz", opt.method = "LF2") {
  smooth_law_vital(.data, {{ .var }}, smooth_mortality_law_x, law, opt.method)
}

smooth_mortality_law_x <- function(data, var, age, law, opt.method) {
  y <- data[[var]]
  x <- data[[age]]
  smooth.fit <- MortalityLaws::MortalityLaw(x = x, mx = y, law = law, opt.method = opt.method)
  # Mean squared error
  residual_variance <- sum((smooth.fit$residuals)^2, na.rm = TRUE) / (length(smooth.fit$fitted.values) - length(smooth.fit$coefficients))
  standard_error <- sqrt(residual_variance)
  out <- tibble(
    age = x,
    .smooth = smooth.fit$fitted.values,
    .smooth_se = .smooth * standard_error
  )
  colnames(out)[1] <- age

  # Extract estimated parameters
  #param_estimates <- tibble(t(smooth.fit$coefficients))
  #colnames(param_estimates) <- names(smooth.fit$coefficients)

  return(out)
}

smooth_law_vital <- function(.data, .var, smooth_fn, law, opt.method) {
  if(rlang::quo_is_missing(enquo(.var))) {
    stop("Please specify which variable to smooth. .var is missing with no default.")
  }
  # Index variable
  index <- tsibble::index_var(.data)
  # Keys including age
  keys <- tsibble::key_vars(.data)
  attrx <- vital_var_list(.data)
  age <- attrx$age
  if (is.null(age)) {
    stop("No age variable found")
  }
  pop <- attrx$population
  # Drop Age as a key and nest results
  keys_noage <- keys[!(keys %in% c(age, "AgeGroup", "Age_Group"))]
  # Turn .var into character
  resp <- names(eval_select(enquo(.var), data = .data))
  nested_data <- tidyr::nest(as_tibble(.data), .by = tidyr::all_of(c(index, keys_noage)))
  smooth <- purrr::map(nested_data[["data"]],
                       \(x) smooth_fn(x, resp, age, law, opt.method))
  nested_data$sm <- smooth
  nested_data$data <- NULL
  out <- tsibble::as_tibble(nested_data) |>
    tidyr::unnest(cols = sm) |>
    left_join(as_tibble(.data), by = c(index, keys_noage, age))
  cols <- c(colnames(.data), ".smooth", ".smooth_se")
  out[cols] |>
    tsibble::as_tsibble(index = index, key = all_of(keys)) |>
    as_vital(
      index = index,
      key = all_of(keys),
      .age = age,
      .sex = attrx$sex,
      .population = attrx$population,
      .deaths = attrx$deaths,
      .births = attrx$births,
      reorder = TRUE
    )
}


