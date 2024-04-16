#' Undo a product/ratio transformation
#'
#' Make a new vital from products and ratios of a measured variable by a
#' key variable. The most common use case of this function is for computing mortality rates by
#' sex, from the sex ratios and geometric mean of the rates.
#' @details Note that when a measured variable takes value 0, the geometric mean
#' is set to 10^-6 to avoid infinite values in the ratio. Therefore, when the
#' transformation is undone, the results will not be identical to the original
#' in the case that the original data was 0.
#'
#' @param .data A vital object
#' @param .var A bare variable name of the measured variable to use.
#' @param key A bare variable name specifying the key variable to use. This key
#' variable must include the value `geometric_mean`.
#' @return A vital object
#' @references Hyndman, R.J., Booth, H., & Yasmeen, F. (2013). Coherent
#' mortality forecasting: the product-ratio method with functional time series
#' models. *Demography*, 50(1), 261-283.
#' @examples
#' # Make products and ratios
#' orig_data <- aus_mortality |>
#'   dplyr::filter(Year > 2015, Sex != "total")
#' pr <- orig_data |>
#'   make_pr(Mortality)
#' # Undo products and ratios
#' orig_data
#' pr
#' pr |> undo_pr(Mortality)
#' @export

undo_pr <- function(.data, .var, key = Sex) {
  if(!inherits(.data, "vital")) {
    stop(".data needs to be a vital object")
  }
  # Character strings for variable and key
  varname <- names(eval_select(enquo(.var), data = .data))
  key <- names(eval_select(enquo(key), data = .data))
  # Index variable
  index <- tsibble::index_var(.data)
  # Key variables
  keys <- tsibble::key_vars(.data)
  attr_data <- attributes(.data)
  age <- attr_data$agevar
  keys_noage <- keys[!(keys %in% c(age, "Age", "AgeGroup", "Age_Group"))]
  if(key %in% c(age, "Age", "AgeGroup", "Age_Group")) {
    stop("key cannot be an age variable")
  } else if(!(key %in% keys_noage)) {
    stop("key not found in data set")
  } else {
    # All keys other than the key argument
    keys_nokey <- keys[!keys %in% key]
  }
  # Find geometric means
  gm <- .data[.data[[key]] == "geometric_mean",] |>
    select(all_of(c(index, keys_noage, key, varname))) |>
    as_tibble()
  gm$.gm <- gm[[varname]]
  gm[[varname]] <- NULL
  gm[[key]] <- NULL
  # Multiply by ratios
  .data <- .data[.data[[key]] != "geometric_mean",] |>
    left_join(gm, by = c(index, keys_nokey))
  .data[[varname]] <- .data[[varname]]*.data$.gm
  .data$.gm <- NULL

  as_vital(.data, index = index, keys = keys,
           .age = age, .population = attr_data$populationvar,
           .sex = attr_data$sexvar, .deaths = attr_data$deathsvar,
           .births = attr_data$birthsvar, reorder = TRUE)
}
