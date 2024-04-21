#' Norwegian mortality and births data
#'
#' `norway_births` is an annual `vital` object covering the years 1846-2022, as provided
#' by the Human Mortality Database on 21 April 2024.
#' @source
#' Human Mortality Database <https://mortality.org>
#'
#' @name norway_births
#' @format Time series of class `vital`
#' @keywords datasets
#' @examples
#' library(ggplot2)
#' norway_births
#' norway_births |>
#'   autoplot(Births)
#'
NULL

#' Norwegian mortality data
#'
#' `norway_mortality` is an annual `vital` covering the years 1846-2022, as provided
#' by the Human Mortality Database on 21 April 2024.
#' @source Human Mortality Database <https://mortality.org>
#'
#' @name norway_mortality
#' @format Time series of class `vital`
#' @keywords datasets
#' @examples
#' library(ggplot2)
#' norway_mortality
#' norway_mortality |>
#'   dplyr::filter(Age < 85, Year < 1900, Sex != "Total") |>
#'   autoplot(Mortality) +
#'   scale_y_log10()
#'
NULL
