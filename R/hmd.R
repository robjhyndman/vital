#' Read data directly from HMD and construct a tsibble object for use in other functions
#'
#' \code{read_hmd} reads 1x1 data from the Human Mortality Database (HMD
#' \url{https://www.mortality.org}) and constructs a tsibble object suitable
#' for use in other functions. This function uses \code{\link[HMDHFDplus]{readHMDweb}}
#' to download the required data.
#'
#' In order to read the data, users are required to create an account with the
#' HMD website (\url{https://www.mortality.org}), and obtain a valid username and password.
#'
#' @param country Directory abbreviation from the HMD. For instance, Australia = "AUS".
#' @param username HMD username (case-sensitive)
#' @param password HMD password (case-sensitive)
#' @param variables List of variables to download from the HMD.
#'
#' @return \code{read_hmd} returns a \code{tsibble} object with the following variables:
#' \item{Year}{Numerical vector containing year of observation}
#' \item{Age}{Numerical vector containing age group}
#' \item{OpenInterval}{Logical vector indicating if the age interval is open}
#' \item{Sex}{Character vector taking values "Female", "Male", "Total"}
#' \item{Deaths}{Total deaths}
#' \item{Exposure}{Numerical vector of exposure-to-risk, equal to population size on 30 June}
#' \item{Population}{Numerical vector containing population size on 1 January}
#' \item{Mortality}{Numerical vector of mortality rate, equal to Deaths/Exposure}
#'
#' @author Rob J Hyndman
#' @examples
#' \dontrun{
#' norway <- read_hmd("NOR", "Nora.Nilsen@mymail.com", "FF!5xeEFa6")
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
    if(item[i] != "Population") {
      data[[i]] <- data[[i]] |>
        tidyr::pivot_longer(Female:Total, names_to = "Sex", values_to = variables[i])
    } else {
      # Just grab 1 Jan population
      data[[i]] <- data[[i]] |>
        dplyr::select(Year:Total1) |>
        dplyr::rename_with(~ gsub("1$", "", .x), dplyr::ends_with("1")) |>
        tidyr::pivot_longer(Female:Total, names_to = "Sex", values_to = variables[i])
    }
  }

  # Combine data into a single tsibble
  purrr::reduce(data, dplyr::left_join, by = c("Year", "Age", "Sex", "OpenInterval")) |>
    tsibble::as_tsibble(index = Year, key = c(Age, Sex)) |>
    dplyr::arrange(Sex, Year, Age) |>
    dplyr::rename(Mortality = Mx)
}

globalVariables(c("Female","Total","Total1","Sex","Mx"))
