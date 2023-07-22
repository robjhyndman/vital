#' Extract age components from a model
#'
#' For a mable with a single model column, return the model components
#' that are indexed by age.
#'
#' @param object A vital mable object with a single model column.
#'
#' @return vital object containing the age components from the model.
#'
#' @examples
#' aus_mortality |>
#'   dplyr::filter(State == "Victoria", Sex == "female") |>
#'   model(lee_carter = LC(Mortality)) |>
#'   age_components()
#'
#' @export
age_components <- function(object, ...) {
  UseMethod("age_components")
}

#' Extract time components from a model
#'
#' For a mable with a single model column, return the model components
#' that are indexed by time.
#'
#' @param object A vital mable object with a single model column.
#'
#' @return tsibble object containing the time components from the model.
#'
#' @examples
#' aus_mortality |>
#'   dplyr::filter(State == "Victoria", Sex == "female") |>
#'   model(lee_carter = LC(Mortality)) |>
#'   time_components()
#'
#' @export
time_components <- function(object, ...) {
  UseMethod("time_components")
}


#' @export
time_components.mdl_vtl_df <- function(object, ...) {
  if (length(mable_vars(object)) > 1) {
    stop("Model plotting is only supported for one class of models. To produce a plot for a specific class of models, use `select()`")
  } else {
    model <- mable_vars(object)
    class(object) <- c(class(object[[model]][[1]]$fit), class(object))
    out <- time_components(object, ...)
    out[[model]] <- NULL
  }
  return(out)
}


#' @export
age_components.mdl_vtl_df <- function(object, ...) {
  if (length(mable_vars(object)) > 1) {
    stop("Model plotting is only supported for one class of models. To produce a plot for a specific class of models, use `select()`")
  } else {
    model <- mable_vars(object)
    class(object) <- c(class(object[[model]][[1]]$fit), class(object))
    out <- age_components(object, ...)
    out[[model]] <- NULL
  }
  return(out)
}
