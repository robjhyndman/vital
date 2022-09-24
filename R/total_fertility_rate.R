#' Compute total fertility rate from age-specific fertility rates
#'
#' Total fertility rate is the expected number of babies per woman in a life-time
#' given the fertility rate at each age of a woman's life.
#'
#' @param .data A tsibble including an age variable and a variable containing fertility rates.
#' @param age Variable in `.data` containing start year of age intervals. If omitted, the variable with name `Age` or `Age_group` will be used (not case sensitive).
#' @param fertility Variable in `.data` containing fertility rates. If omitted, the variable with name  `fx`, `Fertility` or `Rate` will be used (not case sensitive).
#'
#' @return A tsibble object with total fertility in column `tfr`.
#'
#' @examples
#' # Compute Australian total fertility rates over time
#' library(dplyr)
#' aus_fertility |>
#'   total_fertility_rate()
#' @export

total_fertility_rate <- function(.data, age, fertility) {
  # Index variable
  index <- tsibble::index_var(.data)
  # Keys including age
  keys <- tsibble::key_vars(.data)

  # Find age and fertility columns
  if (!missing(age)) {
    age <- {{ age }}
  } else {
    age <- find_key(.data, c("age", "age_group"))
  }
  if (!missing(fertility)) {
    fertility <- {{ fertility }}
  } else {
    fertility <- find_measure(.data, c("fx", "fertility", "rate"))
  }

  # Drop Age as a key and nest results
  keys_noage <- keys[keys != age]
  .data <- tidyr::nest(.data, lst_data = c(-index, -!!keys_noage))

  # Compute tfr for each sub-tibble
  .data$tfr <- purrr::map_dbl(.data[["lst_data"]], tfr, fertility = fertility)
  .data$lst_data <- NULL

  return(.data)
}

tfr <- function(dt, fertility) {
  return(sum(dt[[fertility]]))
}