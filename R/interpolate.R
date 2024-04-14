
#' Interpolate missing values using a vital model
#'
#' Uses a fitted vital model to interpolate missing values from a dataset.
#'
#' @param object A mable containing a single model column.
#' @param new_data A dataset with the same structure as the data used to fit the model.
#' @param ... Other arguments passed to interpolate methods.
#' @return A vital object with missing values interpolated.
#' @examples
#' act_female <- aus_mortality |>
#'  dplyr::filter(Code == "ACTOT", Sex == "female")
#' act_female |>
#'  model(mean = FMEAN(Mortality)) |>
#'  interpolate(act_female)
#' @rdname interpolate
#' @author Rob J Hyndman
#' @export
interpolate.mdl_vtl_df <- function (object, new_data, ...) {
  if (length(mable_vars(object)) > 1) {
    abort("Interpolation can only be done using one model. \nPlease use select() to choose the model to interpolate with.")
  }
  keys <- key_vars(new_data)
  agevar <- attributes(new_data)$agevar
  keys_noage <- keys[keys != agevar]
  index <- index_var(new_data)
  object <- bind_new_data(object, new_data)
  object <- transmute(as_tibble(object), !!!syms(keys_noage),
                      interpolated = map2(!!sym(mable_vars(object)), new_data, interpolate, ...))
  unnest_tbl(object, "interpolated") |>
    as_tsibble(index = index, key = all_of(c(agevar, keys_noage))) |>
    as_vital(.age = agevar, reorder = TRUE)
}

#' @export
interpolate.mdl_vtl_ts <- function (object, new_data, ...)
{
  object$model$stage <- "interpolate"
  object$model$add_data(new_data)
  specials <- tryCatch(parse_model_rhs(object$model), error = function(e) {
    abort(sprintf("%s\nUnable to compute required variables from provided `new_data`.\nDoes your interpolation data include all variables required by the model?",
                  e$message))
  }, interrupt = function(e) {
    stop("Terminated by user", call. = FALSE)
  })
  object$model$remove_data()
  object$model$stage <- NULL
  resp <- map2(seq_along(object$response), object$response,
               function(i, resp) {
                 expr(object$transformation[[!!i]](!!resp))
               }) %>% set_names(map_chr(object$response, as_string))
  agevar <- attributes(new_data)$agevar
  age <- new_data[[agevar]]
  new_data <- transmute(new_data, !!!resp)
  new_data[[agevar]] <- age
  attr(new_data, "agevar") <- agevar
  new_data <- interpolate(object[["fit"]], new_data = new_data,
                          specials = specials, ...)
  new_data[names(resp)] <- map2(new_data[names(resp)], object$transformation,
                                function(x, f) invert_transformation(f)(x))
  new_data
}
