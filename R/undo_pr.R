#' Undo a product/ratio transformation
#'
#' Make a new vital from products and ratios of a measured variable by a
#' key variable. The most common use case of this function is for computing mortality rates by
#' sex, from the sex ratios and geometric mean of the rates.
#' @details Note that when a measured variable takes value 0, [make_pr()] sets
#' it to 10^-5 to avoid infinite values in the ratio. Therefore, when the
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
#' orig_data <- norway_mortality |>
#'   dplyr::filter(Year > 2015, Sex != "Total")
#' pr <- orig_data |>
#'   make_pr(Mortality)
#' # Compare original data with product/ratio version
#' orig_data
#' pr
#' # Undo products and ratios
#' pr |> undo_pr(Mortality)
#' @export
undo_pr <- function(.data, .var, key = Sex, times = 2000) {
  undo_centred(
    .data,
    enquo(.var),
    enquo(key),
    times = times,
    combine = `*`,
    label = "geometric_mean"
  )
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
#' variable must include the value `mean`.
#' @param times When the variable is a distribution, the sum must be computed
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
#' # Undo means and differences
#' sd |> undo_sd(NetMigration)
#' @export

undo_sd <- function(.data, .var, key = Sex, times = 2000) {
  undo_centred(
    .data,
    enquo(.var),
    enquo(key),
    times = times,
    combine = `+`,
    label = "mean"
  )
}

# Combine the deviations with the centre (labelled in the key)
undo_centred <- function(.data, .var, key, times, combine, label) {
  if (!inherits(.data, "vital") & !inherits(.data, "fbl_vtl_ts")) {
    stop(".data needs to be a vital or fbl_vtl_ts object")
  }
  # Are we working with a vital fable or regular fable?
  fable <- inherits(.data, "fbl_vtl_ts")
  v <- centred_vars(.data, .var, key)
  varname <- v$varname
  # Find centres
  gm <- .data[.data[[v$key]] == label, ] |>
    select(all_of(c(v$index, v$keys_noage, v$key, varname))) |>
    as_tibble()
  gm$.gm <- gm[[varname]]
  gm[[varname]] <- NULL
  gm[[v$key]] <- NULL
  # Combine with the centres
  .data <- .data[.data[[v$key]] != label, ] |>
    as_tibble() |>
    left_join(gm, by = c(v$index, v$keys_nokey))
  # Convert distributions to samples
  if (distributional::is_distribution(.data[[varname]])) {
    .data$.gm <- distributional::dist_sample(distributional::generate(
      .data$.gm,
      times
    ))
    .data[[varname]] <- distributional::dist_sample(distributional::generate(
      .data[[varname]],
      times
    ))
  }
  .data[[varname]] <- combine(.data[[varname]], .data$.gm)
  if (distributional::is_distribution(.data[[varname]])) {
    .data$.mean <- mean(.data[[varname]])
  }
  .data$.gm <- NULL
  vvar <- v$attr_data
  output <- as_vital(
    .data,
    index = v$index,
    key = all_of(v$keys),
    .age = vvar$age,
    .population = vvar$population,
    .sex = vvar$sex,
    .deaths = vvar$deaths,
    .births = vvar$births,
    reorder = TRUE
  )
  if (fable) {
    output <- build_vital_fable(
      output,
      response = varname,
      distribution = varname,
      vitals = vvar
    )
  }
  return(output)
}
