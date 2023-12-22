#' Extract age components from a model
#'
#' For a mable with a single model column, return the model components
#' that are indexed by age.
#'
#' @param object A vital mable object with a single model column.
#' @param ... Not currently used.
#'
#' @return vital object containing the age components from the model.
#'
#' @examples
#' aus_mortality |>
#'   dplyr::filter(State == "Victoria", Sex == "female") |>
#'   model(lee_carter = LC(log(Mortality))) |>
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
#' @param ... Not currently used.
#'
#' @return tsibble object containing the time components from the model.
#'
#' @examples
#' aus_mortality |>
#'   dplyr::filter(State == "Victoria", Sex == "female") |>
#'   model(lee_carter = LC(log(Mortality))) |>
#'   time_components()
#'
#' @export
time_components <- function(object, ...) {
  UseMethod("time_components")
}


#' @export
time_components.mdl_vtl_df <- function(object, ...) {
  if (length(mable_vars(object)) > 1) {
    stop("Extracting components is only supported for individual models. Please use `select()` to choose one model column.")
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
    stop("Extracting components is only supported for individual models. Please use `select()` to choose one model column.")
  } else {
    model <- mable_vars(object)
    class(object) <- c(class(object[[model]][[1]]$fit), class(object))
    out <- age_components(object, ...)
    out[[model]] <- NULL
  }
  return(out)
}
