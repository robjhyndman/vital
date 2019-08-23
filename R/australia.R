#' Australian fertility data
#'
#' \code{aus_fertility} is an annual `tsibble` covering the years 1921-2002 with three values:
#' \tabular{ll}{
#'     Fertility: \tab Fertility rate per thousand women \cr
#'     Exposure:  \tab Population of women at 30 June each year\cr
#'     Births:    \tab Number of births \cr
#' }
#'
#' The data is disaggregated using one key:
#' \tabular{ll}{
#'     Age: \tab Age of mother at time of birth\cr
#' }
#'
#' @source
#' Australian Human Mortality Database
#'
#' @name aus_fertility
#' @format Time series of class `tsibble`
#' @keywords datasets
#' @examples
#'
#' aus_fertility
#'
NULL

#' Australian mortality data
#'
#' \code{aus_mortality} is an annual `tsibble` with three values:
#' \tabular{ll}{
#'     Mortality: \tab Mortality rate \cr
#'     Exposure:  \tab Population at 30 June each year\cr
#'     Deaths:    \tab Number of deaths \cr
#' }
#'
#' The data is disaggregated using three keys:
#' \tabular{ll}{
#'     Age:   \tab Age at death\cr
#'     Sex:   \tab male or female \cr
#'     State: \tab State of Australia\cr
#' }
#'
#' @source
#' Australian Human Mortality Database
#'
#' @name aus_mortality
#' @format Time series of class `tsibble`
#' @keywords datasets
#' @examples
#'
#' aus_mortality
#'
NULL
