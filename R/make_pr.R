#' Do a product/ratio transformation
#'
#' Make a new vital containing products and ratios of a measured variable by a
#' key variable. The most common use case of this function is for mortality rates by sex.
#' That is, we want to compute the geometric mean of age-specific mortality rates, along
#' with the ratio of mortality to the geometric mean for each sex. The latter
#' are equal to the male/female and female/male ratios of mortality rates.
#' @details When a measured variable takes value 0, it is set to 10^-5 to avoid
#' infinite values in the ratio.
#'
#' @param .data A vital object
#' @param .var A bare variable name of the measured variable to use.
#' @param key A bare variable name specifying the key variable to use.
#' @return A vital object
#' @references Hyndman, R.J., Booth, H., & Yasmeen, F. (2013). Coherent
#' mortality forecasting: the product-ratio method with functional time series
#' models. *Demography*, 50(1), 261-283.
#' @examples
#' pr <- norway_mortality |>
#'   dplyr::filter(Year > 2015, Sex != "Total") |>
#'   make_pr(Mortality)
#' pr |>
#'   dplyr::filter(Sex == "geometric_mean") |>
#'   autoplot(Mortality) +
#'   ggplot2::scale_y_log10()
#' @export

make_pr <- function(.data, .var, key = Sex) {
  make_centred(
    .data,
    enquo(.var),
    enquo(key),
    centre = function(x) exp(mean(log(pmax(x, 1e-5)))),
    deviation = function(x, centre) pmax(x, 1e-5) / centre,
    label = "geometric_mean"
  )
}

#' Do a sum/difference transformation
#'
#' Make a new vital containing means and differences of a measured variable by a
#' key variable. The most common use case of this function is for migration numbers by sex.
#' That is, we want to compute the age-specific mean migration, along
#' with the difference of migration to the mean for each sex. The latter
#' are equal to half the male/female and female/male differences of migration numbers.
#'
#' @param .data A vital object
#' @param .var A bare variable name of the measured variable to use.
#' @param key A bare variable name specifying the key variable to use.
#' @return A vital object
#' @references Hyndman, R.J., Booth, H., & Yasmeen, F. (2013). Coherent
#' mortality forecasting: the product-ratio method with functional time series
#' models. *Demography*, 50(1), 261-283.
#' @examples
#' mig <- net_migration(norway_mortality, norway_births) |>
#'   dplyr::filter(Sex != "Total")
#' sd <- mig |>
#'   make_sd(NetMigration)
#' sd |>
#'   autoplot(NetMigration)
#' @export

make_sd <- function(.data, .var, key = Sex) {
  make_centred(
    .data,
    enquo(.var),
    enquo(key),
    centre = mean,
    deviation = `-`,
    label = "mean"
  )
}

# Check inputs to make_pr(), make_sd(), undo_pr() and undo_sd(),
# and return the variable names needed
centred_vars <- function(.data, .var, key) {
  if (rlang::quo_is_missing(.var)) {
    stop("Missing .var. Please specify which variable to use.")
  }
  # Character strings for variable and key
  varname <- names(eval_select(.var, data = .data))
  key <- names(eval_select(key, data = .data))
  # Key variables
  keys <- tsibble::key_vars(.data)
  attr_data <- vital_var_list(.data)
  keys_noage <- non_age_keys(.data)
  if (key %in% setdiff(keys, keys_noage)) {
    stop("key cannot be an age variable")
  } else if (!(key %in% keys_noage)) {
    stop("key not found in data set")
  }
  list(
    varname = varname,
    key = key,
    index = tsibble::index_var(.data),
    keys = keys,
    keys_noage = keys_noage,
    # All keys other than the key argument
    keys_nokey = keys[!keys %in% key],
    attr_data = attr_data
  )
}

# Add a centre (labelled in the key) and replace the variable by its
# deviations from the centre
make_centred <- function(.data, .var, key, centre, deviation, label) {
  if (!inherits(.data, "vital")) {
    stop(".data needs to be a vital object")
  }
  v <- centred_vars(.data, .var, key)
  varname <- v$varname
  # Compute centres
  gm <- .data |>
    as_tibble() |>
    group_by(across(all_of(c(v$index, v$keys_nokey)))) |>
    summarise(.gm = centre(.data[[varname]])) |>
    ungroup()
  # Compute deviations of the variable from the centre
  .data <- .data |>
    left_join(gm, by = c(v$index, v$keys_nokey))
  .data[[varname]] <- deviation(.data[[varname]], .data$.gm)
  .data$.gm <- NULL
  # Now add the centre to the data set
  gm[[v$key]] <- label
  gm[[varname]] <- gm$.gm
  gm$.gm <- NULL
  .data <- dplyr::bind_rows(.data, gm)

  as_vital(
    .data,
    index = v$index,
    key = all_of(v$keys),
    .age = v$attr_data$age,
    .population = v$attr_data$population,
    .sex = v$attr_data$sex,
    .deaths = v$attr_data$deaths,
    .births = v$attr_data$births,
    reorder = TRUE
  )
}
