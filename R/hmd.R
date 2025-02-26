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
#' @param country Country name or country code as specified by the HMD. For instance, Australian
#' data can be obtained using \code{country = "Australia"} or \code{country = "AUS"}.
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
#'   country = "Norway",
#'   username = "Nora.Weigh@mymail.com",
#'   password = "FF!5xeEFa6"
#' )
#' norway_births <- read_hmd(
#'   country = "Norway",
#'   username = "Nora.Weigh@mymail.com",
#'   password = "FF!5xeEFa6",
#'   variables = "Births"
#' )
#' }
#' @export

read_hmd <- function(
  country,
  username,
  password,
  variables = c("Deaths", "Exposures", "Population", "Mx")
) {
  # Get country code
  if (!(country %in% countries$hmd_code)) {
    if (country %in% countries$Country) {
      country <- countries$hmd_code[countries$Country == country]
    } else {
      stop("Unknown country")
    }
  }
  var1x1 <- variables %in% c("Deaths", "Exposures", "Mx")
  item <- variables
  item[var1x1] <- paste0(variables[var1x1], "_1x1")
  data <- list()
  for (i in seq_along(item)) {
    data[[i]] <- HMDHFDplus::readHMDweb(
      country,
      item = item[i],
      username = username,
      password = password,
      fixup = TRUE
    )
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
#' then the non-age-specific data will be repeated for each age. If you have HMD files
#' for many countries, all with the same names, then you should put them in separate
#' folders to avoid confusion, and to save changing all the filenames.
#'
#' @return `read_hmd_files` returns a `vital` object combining the downloaded data.
#'
#' @author Rob J Hyndman
#' @examples
#' \dontrun{
#' # Files downloaded from the [Human Mortality Database](https://mortality.org)
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
  for (i in seq_along(files)) {
    data[[i]] <- HMDHFDplus::readHMD(files[i], fixup = TRUE)
  }
  names(data) <- variables
  hmd_to_vital(data)
}

# Convert hmd files into a vital object
hmd_to_vital <- function(object) {
  variables <- names(object)
  sex_included <- any(grepl(
    "female",
    colnames(object[[1]]),
    ignore.case = TRUE
  ))
  if (sex_included) {
    sex <- "Sex"
  } else {
    sex <- NULL
  }
  for (i in seq_along(object)) {
    # Remove columns ending with "2"
    object[[i]] <- object[[i]] |>
      dplyr::select(-dplyr::ends_with("2")) |>
      dplyr::rename_with(~ gsub("1$", "", .x), dplyr::ends_with("1"))
    if (sex_included) {
      # Turn Sex into a variable
      object[[i]] <- object[[i]] |>
        tidyr::pivot_longer(
          Female:Total,
          names_to = sex,
          values_to = variables[i]
        )
    }
  }

  # Find which variables are present to be added as attributes
  deaths <- population <- births <- NULL
  if ("Deaths" %in% variables) {
    deaths <- "Deaths"
  }
  if ("Exposures" %in% variables) {
    population <- "Exposures"
  } else if ("Population" %in% variables) {
    population <- "Population"
  }
  if ("Births" %in% variables) {
    births <- "Births"
  }

  # Combine age-specific data and age-non-specific data into separate tsibbles
  data1 <- data2 <- NULL
  age_included <- unlist(lapply(object, function(x) {
    "Age" %in% colnames(x)
  }))
  if (any(age_included)) {
    data1 <- purrr::reduce(object[age_included], dplyr::left_join) |>
      suppressMessages() |>
      mutate(Age = as.integer(Age)) |>
      tsibble::as_tsibble(index = Year, key = all_of(c("Age", sex)))

    if ("Mx" %in% colnames(data1)) {
      data1 <- data1 |>
        dplyr::rename(Mortality = Mx)
    }
    data1 <- data1 |>
      as_vital(
        .age = "Age",
        .sex = sex,
        .deaths = deaths,
        .population = population,
        reorder = TRUE
      )
  }
  if (!all(age_included)) {
    data2 <- purrr::reduce(object[!age_included], dplyr::left_join) |>
      suppressMessages() |>
      tsibble::as_tsibble(index = Year, key = sex) |>
      as_vital(
        .sex = sex,
        .deaths = deaths,
        .population = population,
        .births = births,
        reorder = TRUE
      )
  }
  if (!is.null(data1) & !is.null(data2)) {
    # Join age-specific and age-non-specific data by Year and Sex
    warning("Duplicating non-age-specific data for each age group")
    return(left_join(data1, data2, by = c("Year", sex)))
  } else if (!is.null(data1)) {
    return(data1)
  } else {
    return(data2)
  }
}

globalVariables(c("Female", "Total", "Total1", "Sex", "Mx"))
