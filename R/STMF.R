#' Read Short-Term Mortality Fluctuations data from HMD and construct a `vital` object for use in other functions
#'
#' `read_stmf` reads weekly mortality data from the Short-term Mortality Fluctuations (STMF)
#' series available in the Human Mortality Database (HMD) <https://www.mortality.org/Data/STMF>)
#' and constructs a `vital` object suitable for use in other functions.
#'
#' @param country Directory abbreviation from the HMD. For instance, Australia = "AUS".
#' @return `read_stmf_data` returns a `vital` object combining the downloaded data.
#'
#' @author Sixian Tang
#' @examples
#' \dontrun{
#' norway <- read_stmf_data(country = "NOR")
#' }
#'
#'
#' @export
read_stmf_data <- function(country) {

  # check if country code is available
  ctrylookup <- getSTMFcountries()

  # get country
  if (missing(country) || !(country %in% ctrylookup$CNTRY)) {
    if (missing(country)) {
      cat("\nCountry missing\n")
    } else {
      cat("\nCountry not found\n")
    }

    if (interactive()) {
      country <- utils::select.list(choices = ctrylookup$CNTRY, multiple = FALSE, title = "Select Country Code")
    } else {
      stop("Country should be one of these:\n", paste(ctrylookup$CNTRY, collapse = ",\n"))
    }
  }
  stopifnot(length(country) == 1)

  # read STMF data
  url <- paste0("https://www.mortality.org/File/GetDocument/Public/STMF/Outputs/", country, "stmfout.csv")
  data <- utils::read.csv(url, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  data$Sex[data$Sex == "b"] <- "both"
  data$Sex[data$Sex == "f"] <- "female"
  data$Sex[data$Sex == "m"] <- "male"

  stmf_to_vital(data)
}
#'
#' Get STMF country list
getSTMFcountries <- function () {
  xpath <- "/html/body/div[1]/div/div/div/div/div[2]/table/tbody"
  html <- rvest::read_html("https://www.mortality.org/Data/STMF")
  links <- rvest::html_attr(rvest::html_elements(rvest::html_element(html, xpath = xpath),
                                   "a"), "href")
  cntry_names <- rvest::html_text2(rvest::html_elements(rvest::html_element(html,
                                                       xpath = xpath), "a"))
  # Create a tibble with country names and links
  tab_main <- dplyr::mutate(tsibble::tibble(Country = cntry_names),
                     # Extract country codes from the links
                     CNTRY = sub("/File/GetDocument/Public\\\\STMF\\\\Outputs\\\\(\\w+)stmfout\\.csv", "\\1", links))
  return(tab_main)
}
#'
#'
stmf_to_vital <- function(stmf_data) {
  # Rename the second set of age columns by adding ".1" suffix
  age_groups <- c("0-14", "15-64", "65-74", "75-84", "85+", "Total")
  age_rate_groups <- paste0(age_groups, ".1")  # Create names for death rates

  # Rename columns for the data frame
  colnames(stmf_data) <- c("CountryCode", "Year", "Week", "Sex", age_groups, age_rate_groups, "Split", "SplitSex", "Forecast")

  # Initialize an empty data frame to store results
  formatted_data <- data.frame()

  # Loop through each age group to create a structured data frame
  for (i in seq_along(age_groups)) {
    # Combine death counts and rates into a single data frame
    combined_data <- stmf_data |>
      dplyr::select(Year, Week, Sex, all_of(age_groups[i]), all_of(age_rate_groups[i])) |>
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
    dplyr::mutate(YearWeek = tsibble::make_yearweek(year = Year, week = Week)) |>
    dplyr::select(-Year, -Week)  # Remove Year and Week columns

  # Convert the formatted data into a tsibble (or vital object as needed)
  vital_data <- as_vital(formatted_data, index = c("YearWeek"), key = c("Sex", "Age_group"), .age = "Age_group", .sex = "Sex", .deaths = "Deaths")

  return(vital_data)
}

globalVariables(c("Week","Age_group"))
