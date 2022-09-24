#' Read data from the Human Mortality Database
#'
#' Download and merge data from the Human Mortality Database (\url{http://mortality.org}).
#' It returns births, deaths, population, exposure, and death rates for single
#' years of age (i.e., the 1x1 files). For other types of data extraction,
#' use the `HMDHFDplus` package.
#'
#' @details Before running this function,
#' you need to register at the "former" HMD website (https://former.mortality.org),
#' and use \code{\link{hmd_session}()} to register your username and password.
#' Usernames and passwords for the new HMD site (https://mortality.org) will not
#' work. This is a temporary patch until an API is released for the new site.
#'
#' @param country Character string containing the country code. It may be
#' a named or unnamed character vector. If the vector is named, the name is used
#' as the label.
#'
#' @author Emi Tanaka and Rob Hyndman
#'
#' @return A tsibble with an annual index, and Age, Sex and Country keys.
#'
#' @seealso \code{\link[HMDHFDplus]{readHMD}}.
#'
#' @examples
#' \dontrun{
#' aus <- read_hmd("AUS")
#' }
#' @export

read_hmd <- function(country) {
  stats <- c(
    "birth", "death", "exposure_to_risk",
    "population", "death_rate"
  )

  # Read data
  df <- hmd_data(
    country = country,
    stats = c(
      "birth", "death", "exposure_to_risk",
      "population", "death_rate"
    )
  )

  # If population data, drop duplicates
  #  if(type=="Population") {
  #    df <- df[,!grepl("2", colnames(df))]
  #    colnames(df) <- gsub("[[:digit:]]+", "", colnames(df))
  #  }

  # Turn it into a tsibble
  key_var <- stats::na.omit(colnames(df)[match(c("Age", "Sex"), colnames(df))])
  df <- as_tsibble(df, index = Year, key = !!key_var)

  # Reorder columns
  vars <- stats::na.omit(colnames(df)[match(
    c("Year", "Age", "OpenInterval", "Sex"),
    colnames(df)
  )])
  df <- select(df, !!vars, tidyselect::everything())

  return(df)
}

utils::globalVariables("Year")
