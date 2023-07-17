#' Compute total fertility rate from age-specific fertility rates
#'
#' Total fertility rate is the expected number of babies per woman in a life-time
#' given the fertility rate at each age of a woman's life.
#'
#' @param .data A vital object including an age variable and a variable containing fertility rates.
#' @param fertility Variable in `.data` containing fertility rates. If omitted, the variable with name  `fx`, `Fertility` or `Rate` will be used (not case sensitive).
#'
#' @return A vital object with total fertility in column `tfr`.
#'
#' @examples
#' # Compute Australian total fertility rates over time
#' aus_fertility |>
#'   total_fertility_rate()
#' @author Rob J Hyndman
#' @export

total_fertility_rate <- function(.data,  fertility) {
  # Index variable
  index <- tsibble::index_var(.data)
  # Keys including age
  keys <- tsibble::key_vars(.data)
  # vital_names
  vital_names <- attributes(.data)
  col_names <- colnames(.data)

  # Find age and fertility columns
  age <- vital_names$agevar
  if(is.null(age)) {
    stop("No age variable identified")
  }
  col_names <- col_names[col_names != age]
  if (!missing(fertility)) {
    fertility <- {{ fertility }}
  } else {
    fertility <- find_measure(.data, c("fx", "fertility", "rate"))
  }

  # Drop Age as a key and nest results
  keys_noage <- keys[keys != age]
  .data <- tidyr::nest(.data, lst_data = c(-index, -!!keys_noage))
  .data[[age]] <- NULL

  # Compute tfr for each sub-tibble
  out <- purrr::map_dfr(.data[["lst_data"]], tfr, fertility = fertility)
  out[[index]] <- .data[[index]]
  out <- out[col_names]
  out |>
    as_tsibble(index = index, key = all_of(keys_noage)) |>
    as_vital(.sex=vital_names$sexvar, .births=vital_names$births,
             .population = vital_names$populationvar)
}

tfr <- function(dt, fertility) {
  dt |> dplyr::summarise(dplyr::across(is.numeric, sum))
}
