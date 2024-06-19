#' Norwegian mortality and births data
#'
#' `norway_births` is an annual `vital` object covering the years 1900-2022, as provided
#' by the Human Mortality Database on 21 April 2024.
#' @source
#' Human Mortality Database <https://mortality.org>
#'
#' @name norway_births
#' @format Time series of class `vital`
#' @keywords datasets
#' @examples
#' library(ggplot2)
#' # Births
#' norway_births
#' norway_births |>
#'   autoplot(Births)
#' # Deaths
#' norway_mortality
#' norway_mortality |>
#'   dplyr::filter(Age < 85, Year < 1950, Sex != "Total") |>
#'   autoplot(Mortality) +
#'   scale_y_log10()
#' # Fertility
#' norway_fertility
#' norway_fertility |>
#'   autoplot(Fertility)
#'
NULL

#' Norwegian fertility data
#'
#' `norway_fertality` is an annual `vital` covering the years 1967-2022, as provided
#' by the Human Fertility Database on 21 April 2024.
#' @source Human Fertility Database <https://www.humanfertility.org>
#'
#' @name norway_fertility
#' @rdname norway_births
#'
NULL

#' Norwegian mortality data
#'
#' `norway_mortality` is an annual `vital` covering the years 1900-2022, as provided
#' by the Human Mortality Database on 21 April 2024.
#'
#' @name norway_mortality
#' @rdname norway_births
#'
NULL


