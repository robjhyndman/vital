#' Read old-age mortality from Kannisto-Thatcher (K-T) database and construct a `vital` object for use in other functions
#'
#' `read_ktdb` reads old-age mortality data classified by sex, age, year of birth, and calendar year for more than 30 countries.
#' The series is available in Kannisto-Thatcher (K-T) database (<https://www.demogr.mpg.de/cgi-bin/databases/ktdb/datamap.plx>)
#' and constructs a `vital` object suitable for use in other functions.
#'
#' @param country Directory abbreviation from the K-T database.
#' @param triangle Lexis triangle number, 1 (default) is lower triangle, 2 is upper triangle.
#' @return `read_ktdb` returns a `vital` object combining the downloaded data.
#'
#' @author Sixian Tang
#' @examples
#' \dontrun{
#' australia <- read_ktdb(country = "Australia")
#' }
#'
#' @export
read_ktdb <- function(country, triangle = 1) {

  # check if country code is available
  ctrylookup <- getktdbcountries()

  # get country
  if (missing(country) || !(country %in% ctrylookup$Country)) {
    if (missing(country)) {
      cat("\nCountry missing\n")
    } else {
      cat("\nCountry not found\n")
    }

    if (interactive()) {
      country <- utils::select.list(choices = ctrylookup$Country, multiple = FALSE, title = "Select Country Code")
    } else {
      stop("Country should be one of these:\n", paste(ctrylookup$Country, collapse = ",\n"))
    }
  }
  stopifnot(length(country) == 1)
  CountryID <- which(ctrylookup$Country == country)

  # read ktdb data
  xpath <- "/html/body/table"
  url <- paste0("https://www.demogr.mpg.de/cgi-bin/databases/ktdb/record.plx?CountryID=", CountryID)
  html <- rvest::read_html(url)
  links <- rvest::html_attr(rvest::html_elements(rvest::html_element(html, xpath = xpath), "a"), "href")[1:2]
  data_male <- utils::read.csv(paste0("https://www.demogr.mpg.de/", links[1]), header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  data_female <- utils::read.csv(paste0("https://www.demogr.mpg.de/", links[2]), header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  data_male <- data_male |> dplyr::mutate(Sex = "Male")
  data_female <- data_female |> dplyr::mutate(Sex = "Female")
  data <- dplyr::bind_rows(data_male, data_female) |> dplyr::filter(Triangle == triangle) |> dplyr::arrange(Year, Sex, Age)
  ktdb_to_vital(data)
}
#'
#'
#'#' Read old-age mortality data from files downloaded from K-T database
#'
#' `read_ktdb_file` reads old-age mortality data from a file downloaded from
#' K-T database (<https://www.demogr.mpg.de/cgi-bin/databases/ktdb/datamap.plx>)
#' and constructs a `vital` object suitable for use in other functions.
#' If two files are provided, the function will treat them as data for each gender,
#' returning a combined dataset. If only one file is provided, the function will
#' assume that it represents data for a single gender.
#'
#' @param file1 Name of the first file containing data downloaded from the K-T database.
#' @param file2 Name of the second file containing data downloaded from the K-T database.
#' @param triangle Lexis triangle number, 1 (default) is lower triangle, 2 is upper triangle.
#' @param male_first Indicator of whether file1 is for males. Default is TRUE.
#' @return `read_ktdb_file` returns a `vital` object combining the downloaded data.
#'
#' @author Sixian Tang
#' @examples
#' \dontrun{
#' # File downloaded from the K-T database
#' australia_male <- read_ktdb_file("maustl.txt")
#' }
#'
#' @export
read_ktdb_file <- function(file1, file2 = NULL, triangle = 1, male_first = TRUE) {
  data1 <- utils::read.csv(file1, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  if (!is.null(file2)) {
    data2 <- utils::read.csv(file2, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
    if (male_first == TRUE) {
      data_male <- data1 |> dplyr::mutate(Sex = "Male")
      data_female <- data2 |> dplyr::mutate(Sex = "Female")
    } else {
      data_male <- data2 |> dplyr::mutate(Sex = "Male")
      data_female <- data1 |> dplyr::mutate(Sex = "Female")
    }
    data <- dplyr::bind_rows(data_male, data_female) |> dplyr::filter(Triangle == triangle) |> dplyr::arrange(Year, Sex, Age)
  } else {
    if (male_first == TRUE) {
      data <- data1 |> dplyr::mutate(Sex = "Male") |> dplyr::filter(Triangle == triangle) |> dplyr::arrange(Year, Sex, Age)
    } else {
      data <- data1 |> dplyr::mutate(Sex = "Female") |> dplyr::filter(Triangle == triangle) |> dplyr::arrange(Year, Sex, Age)
    }
  }
  ktdb_to_vital(data)
}
#'
#'
# Get ktdb country list
getktdbcountries <- function () {
  xpath <- "/html/body"
  grab_url <- "https://www.demogr.mpg.de/cgi-bin/databases/ktdb/datamap.plx"
  html <- rvest::read_html(grab_url)
  country_names <- rvest::html_text2(rvest::html_elements(rvest::html_elements(html, xpath = xpath), "a"))[8:42]
  # Create a tibble with country names
  tab_main <- dplyr::mutate(tsibble::tibble(Country = country_names))
  return(tab_main)
}
#'
#'
#
ktdb_to_vital <- function(ktdb_data) {
  # Convert data into a vital object
  vital_data <- as_vital(ktdb_data, index = c("Year"), key = c("Sex", "Age"), .age = "Age", .sex = "Sex", .deaths = "Deaths", .population = "Population", .drop = TRUE)
  return(vital_data)
}

globalVariables(c("Triangle"))
