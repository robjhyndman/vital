#' Read Short-Term Mortality Fluctuations data from the Human Mortality Database
#'
#' `read_stmf` reads weekly mortality data from the Short-term Mortality Fluctuations (STMF)
#' series available in the Human Mortality Database (HMD) <https://www.mortality.org/Data/STMF>,
#' and constructs a `vital` object suitable for use in other functions.
#' The HMD requires a login to download STMF data.
#'
#' @param country Country name or country code as specified by the HMD. For instance, Australian
#' data can be obtained using \code{country = "Australia"} or \code{country = "AUS"}.
#' @param username HMD username (case-sensitive)
#' @param password HMD password (case-sensitive)
#' @return A `vital` object combining the downloaded data.
#'
#' @author Sixian Tang
#' @seealso [read_stmf_files()] for reading STMF files that have already been downloaded.
#' @examples
#' \dontrun{
#' norway <- read_stmf(
#'   country = "NOR",
#'   username = "Nora.Weigh@mymail.com",
#'   password = "FF!5xeEFa6"
#' )
#' }
#'
#' @export
read_stmf <- function(country, username, password) {
  # Get country code
  if (!(country %in% countries$stmf_code)) {
    if (country %in% countries$Country) {
      name <- country
      country <- countries$stmf_code[countries$Country == name]
      if (is.na(country)) {
        stop("No STMF data available for ", name)
      }
    } else {
      stop("Unknown country")
    }
  }
  stopifnot(length(country) == 1)

  # read STMF data
  url <- paste0(
    "https://www.mortality.org/File/GetDocument/Public/STMF/Outputs/",
    country,
    "stmfout.csv"
  )
  read_stmf_files(hmd_download(url, username, password))
}

# Log in to the HMD and download a file to a temporary file, returning its path
hmd_download <- function(url, username, password) {
  login_url <- "https://www.mortality.org/Account/Login"
  session <- rvest::session(login_url)
  form <- rvest::html_form(session)[[1]]
  form$action <- login_url
  form$url <- login_url
  form <- rvest::html_form_set(
    form,
    Email = username,
    Password = password,
    `__RequestVerificationToken` = form$fields[["__RequestVerificationToken"]]$value
  ) |>
    suppressWarnings()
  session <- rvest::session_submit(session, form)
  response <- rvest::session_jump_to(session, url)$response
  # Without a successful login, the HMD returns its login page instead
  if (grepl("html", response$headers[["content-type"]], fixed = TRUE)) {
    stop(
      "Unable to download data from the HMD. Check your username and password.",
      call. = FALSE
    )
  }
  file <- tempfile(fileext = ".csv")
  writeBin(response$content, file)
  file
}

#' Read STMF data from files downloaded from HMD
#'
#' `read_stmf_files` reads weekly mortality data from a file downloaded
#' from the Short-term Mortality Fluctuations (STMF) series available in the
#' Human Mortality Database (HMD) <https://www.mortality.org/Data/STMF>,
#' and constructs a `vital` object suitable for use in other functions.
#'
#' @param file Name of a file containing data downloaded from the HMD.
#'
#' @return `read_stmf_files` returns a `vital` object combining the downloaded data.
#'
#' @author Rob J Hyndman
#' @seealso [read_stmf()] for downloading and reading STMF data directly from the HMD.
#' @examples
#' \dontrun{
#' # File downloaded from the Human Mortality Database STMF series
#' # (https://www.mortality.org/Data/STMF)
#' mortality <- read_stmf_files("AUSstmfout.csv")
#' }
#' @keywords manip
#' @export
#'

read_stmf_files <- function(file) {
  data <- utils::read.csv(
    file,
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  data$Sex[data$Sex == "b"] <- "both"
  data$Sex[data$Sex == "f"] <- "female"
  data$Sex[data$Sex == "m"] <- "male"
  stmf_to_vital(data)
}

stmf_to_vital <- function(stmf_data) {
  # Death counts are followed by death rates for the same age groups
  age_groups <- c("0-14", "15-64", "65-74", "75-84", "85+", "Total")
  colnames(stmf_data) <- c(
    "CountryCode",
    "Year",
    "Week",
    "Sex",
    paste0("Deaths_", age_groups),
    paste0("Mortality_", age_groups),
    "Split",
    "SplitSex",
    "Forecast"
  )

  # One row for each age group, with its death count and rate
  formatted_data <- stmf_data[c(
    "Year",
    "Week",
    "Sex",
    paste0("Deaths_", age_groups),
    paste0("Mortality_", age_groups)
  )] |>
    tidyr::pivot_longer(
      -c(Year, Week, Sex),
      names_to = c(".value", "Age_group"),
      names_sep = "_"
    )

  # Create YearWeek column
  formatted_data <- formatted_data |>
    dplyr::mutate(
      YearWeek = tsibble::make_yearweek(year = Year, week = Week)
    ) |>
    dplyr::select(-Year, -Week) # Remove Year and Week columns

  # Convert the formatted data into a tsibble (or vital object as needed)
  vital_data <- as_vital(
    formatted_data,
    index = c("YearWeek"),
    key = c("Sex", "Age_group"),
    .sex = "Sex",
    .deaths = "Deaths"
  )

  return(vital_data)
}
