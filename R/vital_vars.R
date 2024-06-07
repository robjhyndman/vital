#' Return vital variables
#'
#' A vital object is a special case of a tsibble object with additional attributes
#' identifying the age, sex, deaths, births and population variables.
#' `vital_vars()` returns a character vector the names of the vital variables.
#'
#' @param x A tsibble object.
#' @return A character vector of the names of the vital variables.
#' @examples
#' vital_vars(aus_mortality)
#'
#' @export
vital_vars <- function(x) {
  attributes(x)$vital
}

# Simpler to work with a list for internal use
vital_var_list <- function(x) {
  as.list(vital_vars(x))
}

age_var <- function(x) {
  vital_var_list(x)$age
}
sex_var <- function(x) {
  vital_var_list(x)$sex
}
