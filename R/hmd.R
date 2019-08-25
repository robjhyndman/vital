#' Read text file from HMD
#'
#' Read text files from the Human Mortality Database (\url{http://mortality.org}).
#' It currently handles births, deaths, population, exposure, death rates,
#' life tables and life expectancy files.
#' It will not work with cohort data or lexis triangles.
#'
#' @param file A path to a file, or a connection.
#'
#' @return A tsibble with an annual index and (possibly) Age and Sex keys.
#'
#' @seealso \code{\link[HMDHFDplus]{readHMD}}.
#'
#' @examples
#' \dontrun{
#' aus <- read_hmd("AUS_Mx_1x1.txt")
#' }
#' @export

read_hmd <- function(file) {
  # Type of data from first line of file
  firstline <- scan(file, nlines=1, what="character", sep=",", strip.white=TRUE)
  if(any(grepl("Births",firstline)))
    type <- "Births"
  else if(any(grepl("Deaths",firstline)))
    type <- "Deaths"
  else if(any(grepl("Population",firstline)))
    type <- "Population"
  else if(any(grepl("Exposure",firstline)))
    type <- "Exposure"
  else if(any(grepl("Death rates",firstline)))
    type <- "Mortality"
  else if(any(grepl("Life tables",firstline)))
    type <- "Life_tables"
  else if(any(grepl("Life expectancy",firstline)))
    type <- "Life_expectancy"
  else
    stop("Unknown file type")

  # Avoid CRAN check errors
  Male <- Female <- Total <- Sex <- Year <- NULL

  # Read data
  df <- HMDHFDplus::readHMD(file)

  # If population data, drop duplicates
  if(type=="Population") {
    df <- df[,!grepl("2", colnames(df))]
    colnames(df) <- gsub("[[:digit:]]+", "", colnames(df))
  }

  # pivot long if contains sexes
  if("Female" %in% colnames(df))
    df <- gather(df, Male, Female, Total, key=Sex, value={{type}})

  # Turn it into a tsibble
  key_var <- stats::na.omit(colnames(df)[match(c("Age","Sex"), colnames(df))])
  df <- as_tsibble(df, index=Year, key=!!key_var)

  # Reorder columns
  vars <- stats::na.omit(colnames(df)[match(c("Year", "Age", "OpenInterval", "Sex"), colnames(df))])
  df <- select(df, !!vars, tidyselect::everything())

  return(df)
}

