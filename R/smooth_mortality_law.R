#' Function to smooth mortality rates using MortalityLaw package
#'
#' This smoothing function allows smoothing of a variable in a vital object using
#' the MortalityLaw package.
#' The vital object is returned along with some additional columns containing
#' information about the smoothed variable: `.smooth` containing the
#' smoothed values, and `.smooth_se` containing the corresponding standard errors.
#' If `.data` has deaths and population variables (see [vital_vars()]), the law
#' is fitted to these; otherwise it is fitted to `.var`.
#'
#' @param .data A vital object
#' @param .var name of variable to smooth. This should contain mortality rates.
#' @param law name of mortality law. For available mortality laws, users can check the \code{\link[MortalityLaws]{availableLaws}}. Argument ignored if a custom law supplied.
#' function to learn about the available options.
#' @param ... Additional arguments are passed to \code{\link[MortalityLaws]{MortalityLaw}}.
#' @return vital with added columns containing smoothed values and their standard errors
#' @keywords smooth
#' @author Sixian Tang and Rob J Hyndman
#' @examples
#' norway_mortality |> smooth_mortality_law(Mortality)
#' @export
smooth_mortality_law <- function(.data, .var, law = "gompertz", ...) {
  smooth_vital(
    .data,
    {{ .var }},
    smooth_fn = smooth_mortality_law_x,
    deaths = vital_var_list(.data)$deaths,
    law = law,
    ...
  )
}

smooth_mortality_law_x <- function(
  data,
  var,
  age_spacing = 1,
  age,
  popvar = NULL,
  deaths = NULL,
  ...
) {
  # Call MortalityLaws, using Dx and Ex if both are available
  if (!is.null(deaths) && !is.null(popvar)) {
    smooth.fit <- MortalityLaws::MortalityLaw(
      x = data[[age]],
      Dx = data[[deaths]],
      Ex = data[[popvar]],
      ...
    )
  } else {
    smooth.fit <- MortalityLaws::MortalityLaw(
      x = data[[age]],
      mx = data[[var]],
      ...
    )
  }
  # Mean squared error
  n <- length(smooth.fit$fitted.values)
  p <- length(smooth.fit$coefficients)
  residual_variance <- sum((smooth.fit$residuals)^2, na.rm = TRUE) / (n - p)
  # Construct output as a tibble
  out <- tibble(
    age = data[[age]],
    .smooth = smooth.fit$fitted.values,
    .smooth_se = .smooth * sqrt(residual_variance) / sqrt(n)
  )
  colnames(out)[1] <- age
  return(out)
}
