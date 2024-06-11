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
#' @param times When the variable is a distribution, the product must be computed
#' by simulation. This argument specifies the number of simulations to use.
#' @return A vital object
#' @references Hyndman, R.J., Booth, H., & Yasmeen, F. (2013). Coherent
#' mortality forecasting: the product-ratio method with functional time series
#' models. *Demography*, 50(1), 261-283.
#' @examples
#' # Make products and ratios
#' orig_data <- aus_mortality |>
#'   dplyr::filter(Year > 2015, Sex != "total", Code == "NSW")
#' pr <- orig_data |>
#'   make_pr(Mortality)
#' # Compare original data with product/ratio version
#' orig_data
#' pr
#' # Undo products and ratios
#' pr |> undo_pr(Mortality)
#' @export

undo_pr <- function(.data, .var, key = Sex, times = 2000) {
  if (!inherits(.data, "vital") & !inherits(.data, "fbl_vtl_ts")) {
    stop(".data needs to be a vital or fbl_vtl_ts object")
  }
  if (missing(.var)) {
    stop("Missing .var. Please specify which variable to use.")
  }
  # Are we working with a vital fable or regular fable?
  fable <- inherits(.data, "fbl_vtl_ts")
  # Character strings for variable and key
  varname <- names(eval_select(enquo(.var), data = .data))
  key <- names(eval_select(enquo(key), data = .data))
  # Index variable
  index <- tsibble::index_var(.data)
  # Key variables
  keys <- tsibble::key_vars(.data)
  vvar <- vital_var_list(.data)
  keys_noage <- keys[!(keys %in% c(vvar$age, "Age", "AgeGroup", "Age_Group"))]
  if (key %in% c(vvar$age, "Age", "AgeGroup", "Age_Group")) {
    stop("key cannot be an age variable")
  } else if (!(key %in% keys_noage)) {
    stop("key not found in data set")
  } else {
    # All keys other than the key argument
    keys_nokey <- keys[!keys %in% key]
  }
  # Find geometric means
  gm <- .data[.data[[key]] == "geometric_mean", ] |>
    select(all_of(c(index, keys_noage, key, varname))) |>
    as_tibble()
  gm$.gm <- gm[[varname]]
  gm[[varname]] <- NULL
  gm[[key]] <- NULL
  # Multiply by ratios
  .data <- .data[.data[[key]] != "geometric_mean", ] |>
    as_tibble() |>
    left_join(gm, by = c(index, keys_nokey))
  # Convert distributions to samples
  if (distributional::is_distribution(.data[[varname]])) {
    .data$.gm <- distributional::dist_sample(distributional::generate(.data$.gm, times))
    .data[[varname]] <- distributional::dist_sample(distributional::generate(.data[[varname]], times))
  }
  .data[[varname]] <- .data[[varname]] * .data$.gm
  if (distributional::is_distribution(.data[[varname]])) {
    .data$.mean <- mean(.data[[varname]])
  }
  .data$.gm <- NULL
  output <- as_vital(.data,
    index = index, key = keys,
    .age = vvar$age,
    .population = vvar$population,
    .sex = vvar$sex,
    .deaths = vvar$deaths,
    .births = vvar$births, reorder = TRUE
  )
  if (fable) {
    output <- build_vital_fable(output,
      response = varname, distribution = varname,
      vitals = vvar, reorder = TRUE
    )
  }
  return(output)
}

#' Undo a mean/difference transformation
#'
#' Make a new vital from means and differences of a measured variable by a
#' key variable. The most common use case of this function is for computing migration numbers by
#' sex, from the sex differences and mean of the numbers.
#'
#' @param .data A vital object
#' @param .var A bare variable name of the measured variable to use.
#' @param key A bare variable name specifying the key variable to use. This key
#' variable must include the value `geometric_mean`.
#' @param times When the variable is a distribution, the product must be computed
#' by simulation. This argument specifies the number of simulations to use.
#' @return A vital object
#' @references Hyndman, R.J., Booth, H., & Yasmeen, F. (2013). Coherent
#' mortality forecasting: the product-ratio method with functional time series
#' models. *Demography*, 50(1), 261-283.
#' @examples
#' # Make sums and differences
#' mig <- net_migration(norway_mortality, norway_births) |>
#'   dplyr::filter(Sex != "Total")
#' sd <- mig |>
#'   make_sd(NetMigration)
#' # Undo products and ratios
#' sd |> undo_sd(NetMigration)
#' @export

undo_sd <- function(.data, .var, key = Sex, times = 2000) {
  if (!inherits(.data, "vital") & !inherits(.data, "fbl_vtl_ts")) {
    stop(".data needs to be a vital or fbl_vtl_ts object")
  }
  if (missing(.var)) {
    stop("Missing .var. Please specify which variable to use.")
  }
  # Are we working with a vital fable or regular fable?
  fable <- inherits(.data, "fbl_vtl_ts")
  # Character strings for variable and key
  varname <- names(eval_select(enquo(.var), data = .data))
  key <- names(eval_select(enquo(key), data = .data))
  # Index variable
  index <- tsibble::index_var(.data)
  # Key variables
  keys <- tsibble::key_vars(.data)
  vvar <- vital_var_list(.data)
  keys_noage <- keys[!(keys %in% c(vvar$age, "Age", "AgeGroup", "Age_Group"))]
  if (key %in% c(vvar$age, "Age", "AgeGroup", "Age_Group")) {
    stop("key cannot be an age variable")
  } else if (!(key %in% keys_noage)) {
    stop("key not found in data set")
  } else {
    # All keys other than the key argument
    keys_nokey <- keys[!keys %in% key]
  }
  # Find geometric means
  gm <- .data[.data[[key]] == "mean", ] |>
    select(all_of(c(index, keys_noage, key, varname))) |>
    as_tibble()
  gm$.gm <- gm[[varname]]
  gm[[varname]] <- NULL
  gm[[key]] <- NULL
  # Add in means
  .data <- .data[.data[[key]] != "mean", ] |>
    as_tibble() |>
    left_join(gm, by = c(index, keys_nokey))
  # Convert distributions to samples
  if (distributional::is_distribution(.data[[varname]])) {
    .data$.gm <- distributional::dist_sample(distributional::generate(.data$.gm, times))
    .data[[varname]] <- distributional::dist_sample(distributional::generate(.data[[varname]], times))
  }
  .data[[varname]] <- .data[[varname]] + .data$.gm
  if (distributional::is_distribution(.data[[varname]])) {
    .data$.mean <- mean(.data[[varname]])
  }
  .data$.gm <- NULL
  output <- as_vital(.data,
    index = index, key = keys,
    .age = vvar$age,
    .population = vvar$population,
    .sex = vvar$sex,
    .deaths = vvar$deaths,
    .births = vvar$births, reorder = TRUE
  )
  if (fable) {
    output <- build_vital_fable(output,
      response = varname, distribution = varname,
      vitals = vvar, reorder = TRUE
    )
  }
  return(output)
}
