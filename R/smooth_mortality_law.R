#' Function to smooth mortality rates using MortalityLaw package
#'
#' This smoothing function allows smoothing of a variable in a vital object using
#' the MortalityLaw package.
#' The vital object is returned along with some additional columns containing
#' information about the smoothed variable: `.smooth` containing the
#' smoothed values, and `.smooth_se` containing the corresponding standard errors.
#' If `.data` has deaths and population variables (see [vital_vars()]), the law
#' is fitted to these; otherwise it is fitted to `.var`.
#' The standard errors are approximate: they are the smoothed values multiplied
#' by the residual standard deviation of the log rates and by \eqn{\sqrt{p/n}},
#' where \eqn{p} is the number of parameters of the law and \eqn{n} is the
#' number of ages with positive rates.
#'
#' @param .data A vital object
#' @param .var name of variable to smooth. This should contain mortality rates.
#' @param law name of mortality law. See \code{\link[MortalityLaws]{availableLaws}}
#' for the available options. Argument ignored if a custom law supplied.
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
    age_spacing = 1,
    smooth_fn = smooth_mortality_law_x,
    deaths = vital_var_list(.data)$deaths,
    law = law,
    ...
  )
}

# The law is fitted at the observed ages, so age_spacing is not used
smooth_mortality_law_x <- function(
  data,
  var,
  age_spacing,
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
  # Residual variance on the log scale, from ages with finite log rates
  log_resid <- log(data[[var]]) - log(smooth.fit$fitted.values)
  log_resid <- log_resid[is.finite(log_resid)]
  n <- length(log_resid)
  p <- length(smooth.fit$coefficients)
  residual_variance <- sum(log_resid^2) / (n - p)
  # Approximate standard errors, using the average leverage p/n of the fit
  out <- tibble(
    age = data[[age]],
    .smooth = smooth.fit$fitted.values,
    .smooth_se = .smooth * sqrt(residual_variance * p / n)
  )
  colnames(out)[1] <- age
  return(out)
}
