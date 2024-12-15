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
  read_ktdb_file(
    male = paste0("https://www.demogr.mpg.de/", links[1]),
    female = paste0("https://www.demogr.mpg.de/", links[2])
  )
}

#'
#'
#' #' Read old-age mortality data from files downloaded from K-T database
#'
#' `read_ktdb_file` reads old-age mortality data from a file downloaded from
#' K-T database (<https://www.demogr.mpg.de/cgi-bin/databases/ktdb/datamap.plx>)
#' and constructs a `vital` object suitable for use in other functions.
#' If two files are provided, the function will treat them as data for each gender,
#' returning a combined dataset. If only one file is provided, the function will
#' assume that it represents data for a single gender.
#'
#' @param male File containing male mortality downloaded from the K-T database.
#' @param female File containing female mortality downloaded from the K-T database.
#' @param triangle Lexis triangle number, 1 (default) is lower triangle, 2 is upper triangle.
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
read_ktdb_file <- function(male = NULL, female = NULL, triangle = 1) {
  # Read files
  if (!is.null(male)) {
    data_male <- utils::read.csv(male, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE) |>
      dplyr::mutate(Sex = "Male")
  }
  if (!is.null(female)) {
    data_female <- utils::read.csv(female, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE) |>
      dplyr::mutate(Sex = "Female")
  }
  # Combine data
  if (!is.null(male) & !is.null(female)) {
    data <- dplyr::bind_rows(data_male, data_female)
  } else if (!is.null(female)) {
    data <- data_female
  } else {
    data <- data_male
  }
  # Filter on triangle
  data <- data |> dplyr::filter(Triangle == triangle)
  # Fix class of each column
  data$Population <- as.numeric(data$Population) |> suppressWarnings()
  data$Deaths <- as.numeric(data$Deaths) |> suppressWarnings()
  # Convert to vital
  ktdb_to_vital(data)
}
#'
#'
# Get ktdb country list
getktdbcountries <- function() {
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
  vital_data <- as_vital(ktdb_data,
    index = c("Year"), key = c("Sex", "Age"),
    .age = "Age", .sex = "Sex", .deaths = "Deaths", .population = "Population",
    reorder = TRUE
  )
  return(vital_data)
}

globalVariables(c("Triangle"))
