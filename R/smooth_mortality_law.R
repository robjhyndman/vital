#' Function to smooth mortality rates using MortalityLaw package
#'
#' This smoothing function allows smoothing of a variable in a vital object using
#' the MortalityLaw package.
#' The vital object is returned along with some additional columns containing
#' information about the smoothed variable: `.smooth` containing the
#' smoothed values, and `.smooth_se` containing the corresponding standard errors.
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
    law = law,
    ...
  )
}

smooth_mortality_law_x <- function(
  data,
  var,
  age_spacing = 1,
  age,
  pop = NULL,
  ...
) {
  # Get Ex and Dx variables
  Ex <- if (!is.null(pop)) data[[pop]] else NULL
  Dx_name <- vital_vars(data)["deaths"]
  Dx <- if (!is.null(Dx_name)) data[[Dx_name]] else NULL
  # Need both Ex and Dx
  if (is.null(Dx) | is.null(Ex)) {
    Dx <- Ex <- NULL
  }
  # Call MortalityLaws
  smooth.fit <- MortalityLaws::MortalityLaw(
    x = data[[age]],
    Dx = Dx,
    Ex = Ex,
    mx = data[[var]],
    ...
  )
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
