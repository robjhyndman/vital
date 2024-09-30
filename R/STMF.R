#' Read Short-Term Mortality Fluctuations data from HMD and construct a `vital` object for use in other functions
#'
#' `read_stmf` reads weekly mortality data from the Short-term Mortality Fluctuations (STMF)
#' series available in the Human Mortality Database (HMD) <https://www.mortality.org/Data/STMF>)
#' and constructs a `vital` object suitable for use in other functions.
#'
#' @param CNTRY Directory abbreviation from the HMD. For instance, Australia = "AUS".
#' @return `read_stmf_data` returns a `vital` object combining the downloaded data.
#'
#' @author Sixian Tang
#' @examples
#' \dontrun{
#' norway <- read_stmf_data(CNTRY = "NOR")
#' }
#'
#'
#' @export
read_stmf_data <- function(CNTRY) {

  # check if country code is available
  ctrylist    <- getSTMFcountries()

  ctrylookup  <- ctrylist |>
    dplyr::select(-"link")

  # get CNTRY
  if (missing(CNTRY)){
    cat("\nCNTRY missing\n")
    if (interactive()){
      CNTRY <- select.list(choices = ctrylookup$CNTRY, multiple = FALSE, title = "Select Country Code")
    } else {
      stop("CNTRY should be one of these:\n",paste(ctrylookup$CNTRY, collapse = ",\n"))
    }
  }
  if (!(CNTRY %in% ctrylookup$CNTRY)){
    cat("\nCNTRY not found\n")
    if (interactive()){
      CNTRY <- select.list(choices = ctrylookup$CNTRY, multiple = FALSE, title = "Select Country Code")
    } else {
      stop("CNTRY should be one of these:\n",paste(ctrylookup$CNTRY, collapse = ",\n"))
    }
  }
  stopifnot(length(CNTRY) == 1)

  # read STMF data
  url <- paste0("https://www.mortality.org/File/GetDocument/Public/STMF/Outputs/", CNTRY, "stmfout.csv")
  data <- read.csv(url, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

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
  tab_main <- dplyr::mutate(tsibble::tibble(Country = cntry_names, link = links),
                     # Extract country codes from the links
                     CNTRY = sub("/File/GetDocument/Public\\\\STMF\\\\Outputs\\\\(\\w+)stmfout\\.csv", "\\1", .data$link))
  return(tab_main)
}
#'
#'
#' @export
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
      dplyr::select(Year, Week, Sex, all_of(age_groups[i]), all_of(age_rate_groups[i]), Forecast) |>
      dplyr::rename(
        Death_counts = all_of(age_groups[i]),
        Death_rates = all_of(age_rate_groups[i])
      ) |>
      dplyr::mutate(Age_group = age_groups[i]) |>
      dplyr::select(Year, Week, Sex, Age_group, Death_counts, Death_rates, Forecast)

    # Bind the combined data to the main data frame
    formatted_data <- dplyr::bind_rows(formatted_data, combined_data)
  }

  # Create YearWeek column in the format YYYYWW
  formatted_data <- formatted_data |>
    dplyr::mutate(YearWeek = as.integer(sprintf("%d%02d", Year, Week))) |>
    dplyr::select(-Year, -Week)  # Remove Year and Week columns

  # Arrange the data by YearWeek, Sex, and Age_group
  formatted_data <- formatted_data |>
    dplyr::arrange(YearWeek, Sex, Age_group)

  # Convert the formatted data into a tsibble (or vital object as needed)
  vital_data <- as_vital.data.frame(formatted_data, index = c("YearWeek"), key = c("Sex", "Age_group"))

  return(vital_data)
}
#'
#'
#' @export
as_vital.data.frame <- function(x, key = NULL, index,
    .age = NULL, .sex = NULL, .deaths = NULL, .births = NULL, .population = NULL,
    reorder = TRUE,
    ...) {
  tsibble::as_tsibble(x, key = !!rlang::enquo(key), index = !!rlang::enquo(index), ...) |>
    vital::as_vital(
      .age = .age, .sex = .sex,
      .deaths = .deaths, .births = .births,
      .population = .population,
      reorder = reorder
    )
}

