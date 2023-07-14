# Functions to autoplot models

#' @export
autoplot.mdl_vtl_df <- function (object, ...) {
  if (length(mable_vars(object)) > 1) {
    stop("Model plotting is only supported for one class of models. To produce a plot for a specific class of models, use `select()`")
  } else {
    model <- mable_vars(object)
    output <- prepare_autoplot(object[[model]])
    out <- attributes(object)$key |>
      select(-.rows) |>
      mutate(out = output)
    class(out) <- c(class(object[[model]][[1]]$fit), class(out))
    agevar <- attributes(object[[model]][[1]]$fit$fitted)$agevar
    autoplot(out, age = agevar, ...)
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

# Plot a variable against age by key
age_plot <- function(object, .var, keys) {
  # Convert age to time and use fabletools::autoplot.tbl_ts
  names <- colnames(object)[!(colnames(object) %in% c(keys, deparse(substitute(.var))))]
  age <- names[grep("age", names, ignore.case=TRUE)]
  object_ts <- tsibble::as_tsibble(object, index=age, key = keys[keys != age])
  fabletools::autoplot(object_ts, {{ .var }}) + ggplot2::xlab(age)
}
