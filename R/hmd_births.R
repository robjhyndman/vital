#' Read data directly from HMD and construct a `vital` object for use in other functions
#'
#' `read_hmd_births` reads single year Births data from the Human Mortality Database (HMD
#' <https://www.mortality.org>) and constructs a `vital` object suitable
#' for use in other functions. This function uses [HMDHFDplus::readHMDweb()]
#' to download the required data. To read handle Deaths, Population, Exposure,
#' and Death Rates, use either `read_hmd()` or `read_hmd_files()`.
#'
#' In order to read the data, users are required to create an account with the
#' HMD website (<https://www.mortality.org>), and obtain a valid username and password.
#'
#' @param country Directory abbreviation from the HMD. For instance, Australia = "AUS".
#' @param username HMD username (case-sensitive)
#' @param password HMD password (case-sensitive)
#'
#' @return `read_hmd_births` returns a `vital` object with the following variables:
#' \item{Year}{Numerical vector containing year of observation}
#' \item{Sex}{Character vector taking values "Female", "Male", "Total"}
#' \item{Births}{Total live births}
#'
#' @author Rob J Hyndman
#' @examples
#' \dontrun{
#' norway <- read_hmd_births(
#'   country = "NOR",
#'   username = "Nora.Weigh@mymail.com",
#'   password = "FF!5xeEFa6"
#' )
#' }
#' @export

read_hmd_births <- function(country, username, password) {
  data <- HMDHFDplus::readHMDweb(
    country,
    item = "Births",
    username = username, password = password, fixup = TRUE
  ) |>
  wrangle_births()
}

#' Read data from files downloaded from HMD and construct a `vital` object for use in other functions
#'
#' `read_hmd_births_file` reads single year births data from the appropriate file
#' downloaded from the Human Mortality Database (HMD <https://www.mortality.org>)
#' and constructs a `vital` object suitable for use in other functions. This
#' function uses [HMDHFDplus::readHMD()] to parse the files.
#'
#' @param filename Name of the file containing the data.
#' @return `read_hmd_births_file` returns a `vital` object containing the following variables:
#' \item{Year}{Numerical vector containing year of observation}
#' \item{Sex}{Character vector taking values "Female", "Male", "Total"}
#' \item{Births}{Total live births}
#' @author Rob J Hyndman
#' @examples
#' \dontrun{
#' read_hmd_births_file("Births.txt")
#' }
#' @keywords manip
#' @export
#'
read_hmd_births_file <- function(filename) {
  data <- HMDHFDplus::readHMD(filename, fixup = TRUE) |>
    wrangle_births()
}

wrangle_births <- function(object) {
  object |>
    dplyr::select(Year:Total) |>
    tidyr::pivot_longer(Female:Total, names_to = "Sex", values_to = "Births") |>
    tsibble::as_tsibble(index = Year, key = Sex) |>
    dplyr::arrange(Sex, Year) |>
    as_vital(.sex = "Sex", .deaths = deaths, .population = population)
}

globalVariables(c("Female","Total","Sex"))
