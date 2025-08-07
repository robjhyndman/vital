#' Read Short-Term Mortality Fluctuations data from the Human Mortality Database
#'
#' `read_stmf` reads weekly mortality data from the Short-term Mortality Fluctuations (STMF)
#' series available in the Human Mortality Database (HMD) <https://www.mortality.org/Data/STMF>),
#' and constructs a `vital` object suitable for use in other functions.
#'
#' @param country Country name or country code as specified by the HMD. For instance, Australian
#' data can be obtained using \code{country = "Australia"} or \code{country = "AUS"}.
#' @return A `vital` object combining the downloaded data.
#'
#' @author Sixian Tang
#' @examples
#' \dontrun{
#' norway <- read_stmf(country = "NOR")
#' }
#'
#'
#' @export
read_stmf <- function(country) {
  # Get country code
  if (!(country %in% countries$stmf_code)) {
    if (country %in% countries$Country) {
      country <- countries$stmf_code[countries$Country == country]
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
  read_stmf_files(url)
}

#' Read STMF data from files downloaded from HMD
#'
#' `read_stmf_files` reads weekly mortality data from a file downloaded
#' from the Short-term Mortality Fluctuations (STMF) series available in the
#' Human Mortality Database (HMD) <https://www.mortality.org/Data/STMF>),
#' and constructs a `vital` object suitable for use in other functions.
#'
#' @param file Name of a file containing data downloaded from the HMD.
#'
#' @return `read_stmf_files` returns a `vital` object combining the downloaded data.
#'
#' @author Rob J Hyndman
#' @examples
#' \dontrun{
#' # File downloaded from the [Human Mortality Database STMF series]
#' (https://www.mortality.org/Data/STMF)
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
  # Rename the second set of age columns by adding ".1" suffix
  age_groups <- c("0-14", "15-64", "65-74", "75-84", "85+", "Total")
  age_rate_groups <- paste0(age_groups, ".1") # Create names for death rates

  # Rename columns for the data frame
  colnames(stmf_data) <- c(
    "CountryCode",
    "Year",
    "Week",
    "Sex",
    age_groups,
    age_rate_groups,
    "Split",
    "SplitSex",
    "Forecast"
  )

  # Initialize an empty data frame to store results
  formatted_data <- data.frame()

  # Loop through each age group to create a structured data frame
  for (i in seq_along(age_groups)) {
    # Combine death counts and rates into a single data frame
    combined_data <- stmf_data |>
      dplyr::select(
        Year,
        Week,
        Sex,
        all_of(age_groups[i]),
        all_of(age_rate_groups[i])
      ) |>
      dplyr::rename(
        Deaths = all_of(age_groups[i]),
        Mortality = all_of(age_rate_groups[i])
      ) |>
      dplyr::mutate(Age_group = age_groups[i]) |>
      dplyr::select(Year, Week, Sex, Age_group, Deaths, Mortality)

    # Bind the combined data to the main data frame
    formatted_data <- dplyr::bind_rows(formatted_data, combined_data)
  }

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

globalVariables(c("Week", "Age_group"))
