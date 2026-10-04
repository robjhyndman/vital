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
  time_components(as_model_class(object, "Extracting components"), ...)
}

#' @export
age_components.mdl_vtl_df <- function(object, ...) {
  age_components(as_model_class(object, "Extracting components"), ...)
}

#' @export
cohort_components.mdl_vtl_df <- function(object, ...) {
  cohort_components(as_model_class(object, "Extracting components"), ...)
}

# Give a single-model mable the class of its fitted models, so methods for
# that model class are dispatched. what describes the operation for errors.
as_model_class <- function(object, what) {
  if (length(mable_vars(object)) > 1) {
    stop(
      what,
      " is only supported for individual models. Please use `dplyr::select()` to choose one model column."
    )
  }
  model <- mable_vars(object)
  class(object) <- c(class(object[[model]][[1]]$fit), class(object)[-1])
  object
}

# The first fitted model in a single-model mable
first_fit <- function(object) {
  object[[attributes(object)$model]][[1]]
}

# Apply extract to the model component of each fit in a single-model mable,
# and unnest the resulting tibbles alongside the keys
unnest_components <- function(object, extract) {
  modelname <- attributes(object)$model
  object <- as_tibble(object)
  object$out <- lapply(object[[modelname]], function(x) extract(x$fit$model))
  object[[modelname]] <- NULL
  tidyr::unnest(object, "out")
}

# As unnest_components(), but return a tsibble with the given index
unnest_time_components <- function(object, extract, index) {
  keys <- setdiff(colnames(as_tibble(object)), attributes(object)$model)
  unnest_components(object, extract) |>
    as_tsibble(index = index, key = all_of(keys))
}
