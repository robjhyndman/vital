#' Compute total fertility rate from age-specific fertility rates
#'
#' Total fertility rate is the expected number of babies per woman in a life-time
#' given the fertility rate at each age of a woman's life. Rates for age groups
#' wider than one year (e.g., 5-year age groups) are multiplied by the width of
#' each group, with the oldest group assumed to be as wide as the one before it.
#'
#' @param .data A vital object including an age variable and a variable containing fertility rates.
#' @param fertility Variable in `.data` containing fertility rates. If omitted, the variable with name  `fx`, `Fertility` or `Rate` will be used (not case sensitive).
#'
#' @return A vital object with total fertility in column `tfr`.
#'
#' @examples
#' # Compute Norwegian total fertility rates over time
#' norway_fertility |>
#'   total_fertility_rate()
#' @author Rob J Hyndman
#' @export

total_fertility_rate <- function(.data, fertility) {
  # Index variable
  index <- tsibble::index_var(.data)
  # vital_names
  vital_names <- vital_var_list(.data)

  # Find age and fertility columns
  age <- vital_names$age
  if (is.null(age)) {
    stop("No age variable identified")
  }
  fertility_quo <- enquo(fertility)
  if (!quo_is_missing(fertility_quo)) {
    fertility <- as_name(fertility_quo)
  } else {
    fertility <- find_measure(.data, c("fx", "fertility", "rate"))
  }
  if (is.na(fertility) || !(fertility %in% colnames(.data))) {
    stop("Fertility variable not found in data")
  }

  ages <- sort(unique(.data[[age]]))
  width <- if (length(ages) > 1L) diff(ages) else 1
  width <- c(width, width[length(width)])

  # Drop Age as a key and nest results
  keys_noage <- non_age_keys(.data)
  .data <- tidyr::nest(.data, lst_data = -all_of(c(index, keys_noage)))

  # Compute tfr for each sub-tibble, weighting rates by the width of each age
  # group, with the oldest group as wide as the one before it
  tfr <- map_dbl(.data[["lst_data"]], function(dt) {
    sum(dt[[fertility]] * width[match(dt[[age]], ages)], na.rm = TRUE)
  })
  out <- as_tibble(.data)[c(index, keys_noage)]
  out$tfr <- tfr
  out |>
    as_tsibble(index = index, key = all_of(keys_noage)) |>
    as_vital(
      .sex = vital_names$sex,
      .births = vital_names$births,
      .population = vital_names$population,
      reorder = TRUE
    )
}
