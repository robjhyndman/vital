#' Read data directly from HFD and construct a `vital` object for use in other functions
#'
#' `read_hfd` reads single-year and single-age data from the Human Fertility Database (HFD
#' <https://www.humanfertility.org>) and constructs a `vital` object suitable
#' for use in other functions. This function uses [HMDHFDplus::readHFDweb()]
#' to download the required data. It is designed to handle age-specific fertility rates.
#' It may be extended to handle other types of data in the future.
#'
#' In order to read the data, users are required to create an account with the
#' HFD website (<https://www.humanfertility.org>), and obtain a valid username and password.
#'
#' @param country Directory abbreviation from the HMD. For instance, Norway = "NOR".
#' @param username HFD username (case-sensitive)
#' @param password HFD password (case-sensitive)
#' @param variables List of variables to download from the HFD. By default, the
#' age-specific fertility rate (asfrRR) is downloaded.
#'
#' @return `read_hfd` returns a `vital` object combining the downloaded data.
#'
#' @author Rob J Hyndman
#' @examples
#' \dontrun{
#' norway <- read_hfd(
#'   country = "NOR",
#'   username = "Nora.Weigh@mymail.com",
#'   password = "FF!5xeEFa6"
#' )
#' }
#' @export

read_hfd <- function(country, username, password, variables = "asfrRR") {
  data <- list()
  for(i in seq_along(variables)) {
    data[[i]] <- HMDHFDplus::readHFDweb(country, item = variables[i],
      username = username, password = password, fixup = TRUE)
  }
  names(data) <- variables
  hmd_to_vital(data)
}

#' Read data from files downloaded from HFD and construct a `vital` object for use in other functions
#'
#' `read_hfd_files` reads single-year and single-age data from files downloaded from the Human Mortality
#' Database (HFD <https://www.humanfertility.org>) and constructs a `vital` object suitable
#' for use in other functions. This function uses [HMDHFDplus::readHFD()]
#' to parse the files.
#'
#' @param files Vector of file names containing data downloaded from the HFD.
#' The file names are used to determine what they contain. If the file names are
#' as per the HFD, then the function will automatically determine the contents.
#' If it is unclear what a file contains, the columns will be named according to the filename.
#' If the data contains a mixture of age-specific and non-age-specific variables,
#' then the non-age-specific data will be repeated for each age. If you have HMD files
#' for many countries, all with the same names, then you should put them in separate
#' folders to avoid confusion, and to save changing all the filenames.
#'
#' @return `read_hfd_files` returns a `vital` object combining the downloaded data.
#'
#' @author Rob J Hyndman
#' @examples
#' \dontrun{
#' # File downloaded from the [Human Fertility Database](https://www.humanfertility.org)
#' fertility <- read_hfd_files("NORasfrRR.txt")
#' }
#' @keywords manip
#' @export
read_hfd_files <- function(files) {
  # Remove suffixes from file names
  variables <- gsub("\\.txt$", "", files)
  # Remove paths from file names
  variables <- gsub(".*/", "", variables)
  data <- list()
  for(i in seq_along(files)) {
    data[[i]] <- HMDHFDplus::readHFD(files[i], fixup = TRUE)
  }
  names(data) <- variables
  hmd_to_vital(data)
}

