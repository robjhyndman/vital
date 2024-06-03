#' Australian fertility data
#'
#' `aus_fertility` is an annual `vital` object covering the years 1921-2002 with three values:
#' \tabular{ll}{
#'     Fertility: \tab Fertility rate per woman \cr
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
#' Australian Human Mortality Database. <https://aushd.org>
#'
#' @name aus_fertility
#' @format Time series of class `vital`
#' @keywords datasets
#' @examples
#' library(ggplot2)
#' aus_fertility
#' aus_fertility |>
#'   autoplot(Fertility) +
#'   ylab("Fertility rate")
#'
NULL

#' Australian mortality data
#'
#' `aus_mortality` is an annual `vital` with three values:
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
#' The data up to 1970 were taken from the Australian Demographic Data Bank
#' (\url{https://pkg.robjhyndman.com/addb/}). From 1971, the data come from the
#' Australian Human Mortality Database (\url{https://aushd.org}). There may be
#' some discontinuities introduced due to different methods being used to prepare
#' the data before and after 1971. Note that "ACTOT" includes both the ACT and overseas territories
#' and is only available up to 2003. The data exclusively from the ACT begins in 1971.
#'
#' @source
#' Australian Human Mortality Database
#'
#' @name aus_mortality
#' @format Time series of class `vital`
#' @keywords datasets
#' @examples
#' library(ggplot2)
#' aus_mortality
#' aus_mortality |>
#'   dplyr::filter(State=="Victoria", Sex != "total") |>
#'   autoplot(Exposure) +
#'   ylab("Population at 30 June (thousands)")
#'
NULL
