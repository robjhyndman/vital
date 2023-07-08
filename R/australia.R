#' Australian fertility data
#'
#' \code{aus_fertility} is an annual `life_tsibble` covering the years 1921-2002 with three values:
#' \tabular{ll}{
#'     Fertility: \tab Fertility rate per 1000 women \cr
#'     Exposure:  \tab Population of women at 30 June each year\cr
#'     Births:    \tab Number of births \cr
#' }
#'
#' The data is disaggregated using one key:
#' \tabular{ll}{
#'     Age: \tab Age of mother at time of birth\cr
#' }
#' The extreme age groups (15 and 49) also include a few younger and older mothers respectively.
#'
#' @source
#' Australian Human Mortality Database
#'
#' @name aus_fertility
#' @format Time series of class `life_tsibble`
#' @keywords datasets
#' @examples
#' library(ggplot2)
#' aus_fertility
#' aus_fertility |>
#'   rainbow_plot(Fertility) +
#'   ylab("Fertility rate")
#'
NULL

#' Australian mortality data
#'
#' \code{aus_mortality} is an annual `life_tsibble` with three values:
#' \tabular{ll}{
#'     Mortality: \tab Mortality rate \cr
#'     Exposure:  \tab Population at 30 June each year\cr
#'     Deaths:    \tab Number of deaths \cr
#' }
#'
#' The data is disaggregated using four keys:
#' \tabular{ll}{
#'     Age:   \tab Age at death\cr
#'     Sex:   \tab male or female \cr
#'     State: \tab State of Australia\cr
#'     Code: \tab Short code for state\cr
#' }
#' The age group 100 also includes people who died aged older than 100.
#'
#' @source
#' Australian Human Mortality Database
#'
#' @name aus_mortality
#' @format Time series of class `life_tsibble`
#' @keywords datasets
#' @examples
#' library(ggplot2)
#' aus_mortality
#' aus_mortality |>
#'   dplyr::filter(State=="Victoria", Sex != "total") |>
#'   rainbow_plot(Exposure/1000) +
#'   ylab("Population at 30 June (thousands)")
#'
NULL
