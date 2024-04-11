#' Make a new vital containing products and ratios of a measured variable by a
#' key variable.
#'
#' The most common use case of this function is for mortality rates by sex.
#' That is, we want to compute the geometric mean of mortality rates, along
#' with the ratio of mortality to the geometric mean for each sex. The latter
#' are equal to the male/female and female/male ratios of mortality rates.
#'
#' @param .data A vital object
#' @param key A character string specifying the key variable
#' @return A vital object
#' @references Hyndman, R.J., Booth, H., & Yasmeen, F. (2013). Coherent
#' mortality forecasting: the product-ratio method with functional time series
#' models. *Demography*, 50(1), 261-283.
#' @examples
#' aus_mortality |>
#'   dplyr::filter(Year > 2015, Sex != "total") |>
#'   make_pr(Mortality)
#' @export

make_pr <- function(.data, .var, key = Sex) {
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

  # Compute geometric means
  # Avoid zeros by replacing them with 1e-5
  gm <- .data
  gm[[varname]] <- pmax(gm[[varname]], 1e-5)
  gm <- gm |>
    as_tibble() |>
    dplyr::group_by_at(dplyr::vars(c(index, keys_nokey))) |>
    summarise(.gm = exp(mean(log({{ .var }})))) |>
    ungroup()
  # Compute ratios of the variable to the geometric mean
  .data <- .data |>
    left_join(gm, by = c(index, keys_nokey))
  .data[[varname]] <- .data[[varname]]/.data$.gm
  .data$.gm <- NULL
  # Now add the geometric mean to the data set
  gm[[key]] <- "geometric_mean"
  gm[[varname]] <- gm$.gm
  gm$.gm <- NULL
  .data <- dplyr::bind_rows(.data, gm)

  as_vital(.data, index = index, keys = keys,
           .age = age, .population = attr_data$populationvar,
           .sex = attr_data$sexvar, .deaths = attr_data$deathsvar,
           .births = attr_data$birthsvar)
}
