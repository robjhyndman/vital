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
#' norway_mortality |>
#'   dplyr::filter(Sex == "Female") |>
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
#' norway_mortality |>
#'   dplyr::filter(Sex == "Female") |>
#'   model(lee_carter = LC(log(Mortality))) |>
#'   time_components()
#' @export
time_components <- function(object, ...) {
  UseMethod("time_components")
}

#' Extract cohort components from a model
#'
#' For a mable with a single model column, return the model components
#' that are indexed by birth year of the cohort.
#'
#' @param object A vital mable object with a single model column.
#' @param ... Not currently used.
#'
#' @return tsibble object containing the cohort components from the model.
#'
#' @examples
#' norway_mortality |>
#'   dplyr::filter(Sex == "Male", Age > 50, Year > 1960) |>
#'   model(apc = APC(Mortality)) |>
#'   cohort_components()
#' @export
cohort_components <- function(object, ...) {
  UseMethod("cohort_components")
}


#' @export
time_components.mdl_vtl_df <- function(object, ...) {
  if (length(mable_vars(object)) > 1) {
    stop(
      "Extracting components is only supported for individual models. Please use `dplyr::select()` to choose one model column."
    )
  }
  model <- mable_vars(object)
  class(object) <- c(class(object[[model]][[1]]$fit), class(object)[-1])
  time_components(object, ...)
}

#' @export
age_components.mdl_vtl_df <- function(object, ...) {
  if (length(mable_vars(object)) > 1) {
    stop(
      "Extracting components is only supported for individual models. Please use `dplyr::select()` to choose one model column."
    )
  }
  model <- mable_vars(object)
  class(object) <- c(class(object[[model]][[1]]$fit), class(object))
  age_components(object, ...)
}

#' @export
cohort_components.mdl_vtl_df <- function(object, ...) {
  if (length(mable_vars(object)) > 1) {
    stop(
      "Extracting components is only supported for individual models. Please use `dplyr::select()` to choose one model column."
    )
  }
  model <- mable_vars(object)
  class(object) <- c(class(object[[model]][[1]]$fit), class(object)[-1])
  cohort_components(object, ...)
}
