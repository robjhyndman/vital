# Functions to autoplot models

#' @export
autoplot.mdl_vtl_df <- function (object, ...) {
  if (length(mable_vars(object)) > 1) {
    stop("Model plotting is only supported for one class of models. To produce a plot for a specific class of models, use `select()`")
  } else {
    model <- mable_vars(object)
    output <- prepare_autoplot(object[[model]], ...)
    out <- attributes(object)$key |>
      select(-.rows) |>
      mutate(out = output) |>
      tidyr::unnest("out")
    class(out) <- c(class(object[[model]][[1]]$fit), class(out))
    agevar <- attributes(object[[model]][[1]]$fit$fitted)$agevar
    autoplot(out, age = agevar)
  }
  invisible(out)
}

#' @export
prepare_autoplot.lst_mdl <- function(object, ...) {
  # Combine results from list
  purrr:::map(object, prepare_autoplot)
}

#' @export
prepare_autoplot.mdl_vtl_ts <- function (object, ...) {
  tryCatch(prepare_autoplot(object[["fit"]], ...), error = function(e) {
    stop("\nautoplot() is not available for this model class.")
  })
}


#' @export
prepare_autoplot <- function(object, ...) {
  UseMethod("prepare_autoplot")
}
