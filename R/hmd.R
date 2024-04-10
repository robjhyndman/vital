#' Read data directly from HMD and construct a `vital` object for use in other functions
#'
#' `read_hmd` reads single-year and single-age data from the Human Mortality Database (HMD
#' <https://www.mortality.org>) and constructs a `vital` object suitable
#' for use in other functions. This function uses [HMDHFDplus::readHMDweb()]
#' to download the required data. It is designed to handle Deaths, Population,
#' Exposure, Death Rates and Births. By default, Deaths, Population, Exposure
#' and Death Rates are downloaded. It is better to handle Births separately as
#' they are not age-specific.
#'
#' In order to read the data, users are required to create an account with the
#' HMD website (<https://www.mortality.org>), and obtain a valid username and password.
#'
#' @param country Directory abbreviation from the HMD. For instance, Australia = "AUS".
#' @param username HMD username (case-sensitive)
#' @param password HMD password (case-sensitive)
#' @param variables List of variables to download from the HMD. If the data
#' contains a mixture of age-specific and non-age-specific variables, then
#' the non-age-specific data will be repeated for each age.
#'
#' @return `read_hmd` returns a `vital` object combining the downloaded data.
#'
#' @author Rob J Hyndman
#' @examples
#' \dontrun{
#' norway <- read_hmd(
#'   country = "NOR",
#'   username = "Nora.Weigh@mymail.com",
#'   password = "FF!5xeEFa6"
#' )
#' norway_births <- read_hmd(
#'   country = "NOR",
#'   username = "Nora.Weigh@mymail.com",
#'   password = "FF!5xeEFa6",
#'   variables = "Births"
#' )
#' }
#' @export

read_hmd <- function(country, username, password,
    variables = c("Deaths", "Exposures", "Population", "Mx")) {
  var1x1 <- variables %in% c("Deaths", "Exposures", "Mx")
  item <- variables
  item[var1x1] <- paste0(variables[var1x1],"_1x1")
  data <- list()
  for(i in seq_along(item)) {
    data[[i]] <- HMDHFDplus::readHMDweb(country, item = item[i],
      username = username, password = password, fixup = TRUE)
  }
  names(data) <- variables
  hmd_to_vital(data)
}

#' Read data from files downloaded from HMD and construct a `vital` object for use in other functions
#'
#' `read_hmd_files` reads single-year and single-age data from files downloaded from the Human Mortality
#' Database (HMD <https://www.mortality.org>) and constructs a `vital` object suitable
#' for use in other functions. This function uses [HMDHFDplus::readHMD()]
#' to parse the files.
#'
#' @param files Vector of file names containing data downloaded from the HMD.
#' The file names are used to determine what they contain. If the file names are
#' as per the HMD, then the function will automatically determine the contents.
#' If it is unclear what a file contains, the columns will be named according to the filename.
#' If the data contains a mixture of age-specific and non-age-specific variables,
#' then the non-age-specific data will be repeated for each age.
#'
#' @return `read_hmd_files` returns a `vital` object combining the downloaded data.
#'
#' @author Rob J Hyndman
#' @examples
#' \dontrun{
#' mortality <- read_hmd_files(
#'   c("Deaths_1x1.txt", "Exposures_1x1.txt", "Population.txt", "Mx_1x1.txt")
#' )
#' births <- read_hmd_files("Births.txt")
#' }
#' @keywords manip
#' @export
#'
read_hmd_files <- function(files) {
  # Remove suffixes from file names
  variables <- gsub("\\.txt$", "", files)
  variables <- gsub("_1x1", "", variables)
  # Remove paths from file names
  variables <- gsub(".*/", "", variables)
  data <- list()
  for(i in seq_along(files)) {
    data[[i]] <- HMDHFDplus::readHMD(files[i], fixup = TRUE)
  }
  names(data) <- variables
  hmd_to_vital(data)
}

# Convert hmd files into a vital object
hmd_to_vital <- function(object) {
  variables <- names(object)
  for(i in seq_along(object)) {
    if("Total2" %in% colnames(object[[i]])) {
      # Just grab 1 Jan population
      object[[i]] <- object[[i]] |>
        dplyr::select(Year:Total1) |>
        dplyr::rename_with(~ gsub("1$", "", .x), dplyr::ends_with("1")) |>
        tidyr::pivot_longer(Female:Total, names_to = "Sex", values_to = variables[i])
    } else {
      object[[i]] <- object[[i]] |>
        tidyr::pivot_longer(Female:Total, names_to = "Sex", values_to = variables[i])
    }
  }

  # Find which variables are present to be added as attributes
  deaths <- population <- NULL
  if("Deaths" %in% variables) {
    deaths <- "Deaths"
  }
  if("Exposures" %in% variables) {
    population <- "Exposures"
  } else if ("Population" %in% variables) {
    population <- "Population"
  }

  # Combine age-specific data and age-non-specific data into separate tsibbles
  data1 <- data2 <- NULL
  age_included <- unlist(lapply(object, function(x) {"Age" %in% colnames(x)}))
  if(any(age_included)) {
    data1 <- purrr::reduce(object[age_included], dplyr::left_join,
                           by = c("Year", "Age", "Sex", "OpenInterval")) |>
      tsibble::as_tsibble(index = Year, key = c(Age, Sex)) |>
      dplyr::arrange(Sex, Year, Age)
    if("Mx" %in% variables) {
      data1 <- data1 |>
        dplyr::rename(Mortality = Mx)
    }
    data1 <- data1 |>
      as_vital(.age = "Age", .sex = "Sex", .deaths = deaths, .population = population)
  }
  if(any(!age_included)) {
    data2 <- purrr::reduce(object[!age_included], dplyr::left_join,
                           by = c("Year", "Sex")) |>
      tsibble::as_tsibble(index = Year, key = c(Sex)) |>
      dplyr::arrange(Sex, Year) |>
      as_vital(.sex = "Sex", .deaths = deaths, .population = population)
  }
  if(!is.null(data1) & !is.null(data2)) {
    # Join age-specific and age-non-specific data by Year and Sex
    warning("Duplicating non-age-specific data for each age group")
    return(left_join(data1, data2, by = c("Year", "Sex")))
  } else if(!is.null(data1)) {
    return(data1)
  } else {
    return(data2)
  }
}

globalVariables(c("Female","Total","Total1","Sex","Mx"))
