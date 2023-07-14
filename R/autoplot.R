#' Plot output from a vital model
#'
#' Produces a plot showing a model applied to a vital object. This can be applied
#' to one type of model only. So use select() to choose the model column to plot.
#' If there are multiple keys, separate models will be identified by colour.
#'
#' @param object A mable object obtained from a vital.
#' @param ... Further arguments ignored.
#' @author Rob J Hyndman
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' aus_mortality |>
#'  filter(State == "Victoria") |>
#'  model(ave = FMEAN(Mortality)) |>
#'  autoplot() + scale_y_log10()
#'
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
    p <- autoplot(out, age = agevar, ...)
  }
  p
}

#' @export
prepare_autoplot.lst_mdl <- function(object, ...) {
  # Combine results from list
  purrr::map(object, prepare_autoplot)
}

#' @export
prepare_autoplot.mdl_vtl_ts <- function (object, ...) {
  tryCatch(prepare_autoplot(object[["fit"]], ...), error = function(e) {
    stop("\nautoplot() is not available for this model class.")
  })
}

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

#' Rainbow plot of demographic data against age
#'
#' Produce rainbow plot (coloured by time index) of demographic variable against
#' against age.
#'
#' @param .data A vital, tsibble or fable object including an age variable and the variable you wish to plot.
#' @param .vars A bare expression containing the name of the variable you wish to plot.
#' @param age Variable in `.data` containing start year of age intervals.
#' If omitted, and `.data` is not a vital object, the variable with name `Age` or `Age_group`
#' will be used (not case sensitive).
#'
#' @author Rob J Hyndman
#' @references Hyndman, Rob J & Shang, Han Lin (2010) Rainbow plots, bagplots,
#' and boxplots for functional data. \emph{Journal of Computational and Graphical Statistics},
#' \bold{19}(1), 29-45.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' autoplot(aus_fertility, Fertility)
#' @export
autoplot.vital <- function(object, ...) {
  rainbow_plot(object, ...)
}

#' Plot forecasts from a vital model
#'
#' Produces a plot showing forecasts obtained from a model applied to a vital object.
#'
#' @param object A fable object obtained from a vital model.
#' @param ... Further arguments passed to rainbow_plot().
#' @author Rob J Hyndman
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' aus_mortality |>
#'  filter(State == "Victoria") |>
#'  model(ave = FMEAN(Mortality)) |>
#'  autoplot() + scale_y_log10()
#'
#' @author Rob J Hyndman
#' @export
autoplot.fbl_vtl_ts <- function(object, ...) {
  rainbow_plot(object, ...)
}
