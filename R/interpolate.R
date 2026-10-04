#' Interpolate missing values using a vital model
#'
#' Uses a fitted vital model to interpolate missing values from a dataset.
#'
#' @param object A mable containing a single model column.
#' @param new_data A dataset with the same structure as the data used to fit the model.
#' @param ... Other arguments passed to interpolate methods.
#' @return A vital object with missing values interpolated.
#' @examples
#' nor_female <- norway_mortality |>
#'   dplyr::filter(Sex == "Female")
#' nor_female |>
#'   model(mean = FMEAN(Mortality)) |>
#'   interpolate(nor_female)
#' @rdname interpolate
#' @author Rob J Hyndman
#' @export
interpolate.mdl_vtl_df <- function(object, new_data, ...) {
  if (length(mable_vars(object)) > 1) {
    abort(
      "Interpolation can only be done using one model. \nPlease use select() to choose the model to interpolate with."
    )
  }
  keys <- key_vars(new_data)
  keys_noage <- non_age_keys(new_data)
  index <- index_var(new_data)
  object <- bind_new_data(object, new_data)
  object <- transmute(
    as_tibble(object),
    !!!syms(keys_noage),
    interpolated = map2(!!sym(mable_vars(object)), new_data, interpolate, ...)
  )
  unnest_tbl(object, "interpolated") |>
    as_tsibble(index = index, key = all_of(keys)) |>
    restore_vital(vital_var_list(new_data), reorder = TRUE)
}

#' @export
interpolate.mdl_vtl_ts <- function(object, new_data, ...) {
  object$model$stage <- "interpolate"
  object$model$add_data(new_data)
  specials <- tryCatch(
    parse_model_rhs(object$model),
    error = function(e) {
      abort(sprintf(
        "%s\nUnable to compute required variables from provided `new_data`.\nDoes your interpolation data include all variables required by the model?",
        e$message
      ))
    },
    interrupt = function(e) {
      stop("Terminated by user", call. = FALSE)
    }
  )
  object$model$remove_data()
  object$model$stage <- NULL
  resp <- map2(
    seq_along(object$response),
    object$response,
    function(i, resp) {
      expr(object$transformation[[!!i]](!!resp))
    }
  ) %>%
    set_names(map_chr(object$response, as_string))
  vvar <- vital_vars(new_data)
  # transmute() keeps the index and keys, including age
  new_data <- transmute(new_data, !!!resp)
  attr(new_data, "vital") <- vvar
  new_data <- interpolate(
    object[["fit"]],
    new_data = new_data,
    specials = specials,
    ...
  )
  new_data[names(resp)] <- map2(
    new_data[names(resp)],
    object$transformation,
    function(x, f) invert_transformation(f)(x)
  )
  new_data
}

# Replace missing values of the response with the fitted values of a model
interpolate_fitted <- function(object, new_data) {
  keyvar <- key_vars(new_data)
  vvar <- vital_var_list(new_data)
  agevar <- vvar$age
  timevar <- index_var(new_data)
  measures <- measured_vars(new_data)
  measures <- measures[!(measures %in% c(agevar, vvar$population))]
  measure <- measures[1]
  fits <- as_tibble(object$fitted)[c(agevar, timevar, ".fitted")]
  output <- as_tibble(new_data) |>
    dplyr::left_join(fits, by = c(agevar, timevar))
  missing <- is.na(output[[measure]])
  output[[measure]][missing] <- output$.fitted[missing]
  output$.fitted <- NULL
  vital(
    output,
    key = all_of(unique(c(keyvar, agevar))),
    index = timevar,
    .age = agevar
  )
}

#' @export
interpolate.FNAIVE <- function(object, new_data, specials, ...) {
  interpolate_fitted(object, new_data)
}

#' @export
interpolate.LC <- function(object, new_data, specials, ...) {
  interpolate_fitted(object, new_data)
}

#' @export
interpolate.FDM <- function(object, new_data, specials, ...) {
  interpolate_fitted(object, new_data)
}
