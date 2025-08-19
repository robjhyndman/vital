#' Do a product/ratio transformation
#'
#' Make a new vital containing products and ratios of a measured variable by a
#' key variable. The most common use case of this function is for mortality rates by sex.
#' That is, we want to compute the geometric mean of age-specific mortality rates, along
#' with the ratio of mortality to the geometric mean for each sex. The latter
#' are equal to the male/female and female/male ratios of mortality rates.
#' @details When a measured variable takes value 0, it is set to 10^-6 to avoid
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
  if (!inherits(.data, "vital")) {
    stop(".data needs to be a vital object")
  }
  if (missing(.var)) {
    stop("Missing .var. Please specify which variable to use.")
  }
  # Character strings for variable and key
  varname <- names(eval_select(enquo(.var), data = .data))
  key <- names(eval_select(enquo(key), data = .data))
  # Index variable
  index <- tsibble::index_var(.data)
  # Key variables
  keys <- tsibble::key_vars(.data)
  attr_data <- vital_var_list(.data)
  age <- attr_data$age
  keys_noage <- keys[!(keys %in% c(age, "Age", "AgeGroup", "Age_Group"))]
  if (key %in% c(age, "Age", "AgeGroup", "Age_Group")) {
    stop("key cannot be an age variable")
  } else if (!(key %in% keys_noage)) {
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
  .data[[varname]] <- .data[[varname]] / .data$.gm
  .data$.gm <- NULL
  # Now add the geometric mean to the data set
  gm[[key]] <- "geometric_mean"
  gm[[varname]] <- gm$.gm
  gm$.gm <- NULL
  .data <- dplyr::bind_rows(.data, gm)

  as_vital(
    .data,
    index = index,
    keys = keys,
    .age = age,
    .population = attr_data$population,
    .sex = attr_data$sex,
    .deaths = attr_data$deaths,
    .births = attr_data$births,
    reorder = TRUE
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
  if (!inherits(.data, "vital")) {
    stop(".data needs to be a vital object")
  }
  if (missing(.var)) {
    stop("Missing .var. Please specify which variable to use.")
  }
  # Character strings for variable and key
  varname <- names(eval_select(enquo(.var), data = .data))
  key <- names(eval_select(enquo(key), data = .data))
  # Index variable
  index <- tsibble::index_var(.data)
  # Key variables
  keys <- tsibble::key_vars(.data)
  attr_data <- vital_var_list(.data)
  age <- attr_data$age
  keys_noage <- keys[!(keys %in% c(age, "Age", "AgeGroup", "Age_Group"))]
  if (key %in% c(age, "Age", "AgeGroup", "Age_Group")) {
    stop("key cannot be an age variable")
  } else if (!(key %in% keys_noage)) {
    stop("key not found in data set")
  } else {
    # All keys other than the key argument
    keys_nokey <- keys[!keys %in% key]
  }

  # Compute means
  gm <- .data |>
    as_tibble() |>
    dplyr::group_by_at(dplyr::vars(c(index, keys_nokey))) |>
    summarise(.gm = mean({{ .var }})) |>
    ungroup()
  # Compute ratios of the variable to the geometric mean
  .data <- .data |>
    left_join(gm, by = c(index, keys_nokey))
  .data[[varname]] <- .data[[varname]] - .data$.gm
  .data$.gm <- NULL
  # Now add the mean to the data set
  gm[[key]] <- "mean"
  gm[[varname]] <- gm$.gm
  gm$.gm <- NULL
  .data <- dplyr::bind_rows(.data, gm)

  as_vital(
    .data,
    index = index,
    keys = keys,
    .age = age,
    .population = attr_data$population,
    .sex = attr_data$sex,
    .deaths = attr_data$deaths,
    .births = attr_data$births,
    reorder = TRUE
  )
}
