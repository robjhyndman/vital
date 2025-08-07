#' Read old-age mortality from Kannisto-Thatcher (K-T) database and construct a `vital` object for use in other functions
#'
#' `read_ktdb` reads old-age mortality data classified by sex, age, year of birth, and calendar year for more than 30 countries.
#' The series is available in Kannisto-Thatcher (K-T) database (<https://www.demogr.mpg.de/cgi-bin/databases/ktdb/datamap.plx>)
#' and constructs a `vital` object suitable for use in other functions.
#'
#' @param country Country name or country code as specified by the KT database. For instance, Australian
#' data can be obtained using \code{country = "Australia"} or \code{country = 1}.
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
  # Get country code
  if (is.numeric(country)) {
    country <- round(country)
    if (country < 1 | country > 36) stop("Unknown country code")
  } else if (country %in% countries$Country) {
    country <- countries$ktdb_number[countries$Country == country]
  } else {
    stop("Unknown country")
  }

  links <- countries[countries$ktdb_number == country, ]
  # read ktdb data
  read_ktdb_files(
    male = paste0(
      "https://www.demogr.mpg.de/databases/ktdb/",
      links$ktdb_male[1]
    ),
    female = paste0(
      "https://www.demogr.mpg.de/databases/ktdb/",
      links$ktdb_female[1]
    )
  )
}

#'
#'
#' Read old-age mortality data from files downloaded from K-T database
#'
#' `read_ktdb_files` reads old-age mortality data from files downloaded from
#' K-T database (<https://www.demogr.mpg.de/cgi-bin/databases/ktdb/datamap.plx>)
#' and constructs a `vital` object suitable for use in other functions.
#' If two files are provided, the function will treat them as data for each gender,
#' returning a combined dataset. If only one file is provided, the function will
#' assume that it represents data for a single gender.
#'
#' @param male File containing male mortality downloaded from the K-T database.
#' @param female File containing female mortality downloaded from the K-T database.
#' @param triangle Lexis triangle number, 1 (default) is lower triangle, 2 is upper triangle.
#' @return `read_ktdb_files` returns a `vital` object combining the downloaded data.
#'
#' @author Sixian Tang
#' @examples
#' \dontrun{
#' # File downloaded from the K-T database
#' australia_male <- read_ktdb_files("maustl.txt")
#' }
#'
#' @export
read_ktdb_files <- function(male = NULL, female = NULL, triangle = 1) {
  # Read files
  if (!is.null(male)) {
    data_male <- utils::read.csv(
      male,
      header = TRUE,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ) |>
      dplyr::mutate(Sex = "Male")
  }
  if (!is.null(female)) {
    data_female <- utils::read.csv(
      female,
      header = TRUE,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ) |>
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

ktdb_to_vital <- function(ktdb_data) {
  # Convert data into a vital object
  vital_data <- as_vital(
    ktdb_data,
    index = c("Year"),
    key = c("Sex", "Age"),
    .age = "Age",
    .sex = "Sex",
    .deaths = "Deaths",
    .population = "Population",
    reorder = TRUE
  )
  return(vital_data)
}

globalVariables(c("Triangle"))
