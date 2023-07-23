#' Rainbow plot of demographic data against age
#'
#' Produce rainbow plot (coloured by time index) of demographic variable against
#' against age.
#'
#' @param object A vital including an age variable and the variable you wish to plot.
#' @param .vars The name of the variable you wish to plot.
#' @param ... Further arguments not used.
#'
#' @author Rob J Hyndman
#' @references Hyndman, Rob J & Shang, Han Lin (2010) Rainbow plots, bagplots,
#' and boxplots for functional data. \emph{Journal of Computational and Graphical Statistics},
#' \bold{19}(1), 29-45. \url{https://robjhyndman.com/publications/rainbow-fda/}
#'
#' @return A ggplot2 object.
#'
#' @examples
#' autoplot(aus_fertility, Fertility)
#' @export
autoplot.vital <- function(object, .vars = NULL, ...) {
  if(!quo_is_null(enquo(.vars))) {
    .vars <- as_name(enquo(.vars))
  }
  rainbow_plot(object, .vars = .vars, age = attributes(object)$agevar)
}

#' Plot forecasts from a vital model
#'
#' Produces a plot showing forecasts obtained from a model applied to a vital object.
#'
#' @param object A fable object obtained from a vital model.
#' @param ... Further arguments ignored.
#' @author Rob J Hyndman
#' @return A ggplot2 object.
#'
#' @examples
#' library(ggplot2)
#' aus_mortality |>
#'  dplyr::filter(State == "Victoria") |>
#'  model(ave = FMEAN(Mortality)) |>
#'  forecast(h = 10) |>
#'  autoplot() + scale_y_log10()
#'
#' @author Rob J Hyndman
#' @export
autoplot.fbl_vtl_ts <- function(object, ...) {
  # Find first variable to plot
  keys <- key_vars(object)
  index <- index_var(object)
  dist <- attributes(object)$dist
  to_plot <- colnames(object)
  to_plot <- to_plot[!(to_plot %in% c(keys, index, dist))]
  if(length(to_plot) > 1) {
    warning(paste("Multiple variables to plot. Choosing", to_plot[1]))
  }
  rainbow_plot(object, .vars = to_plot[1], age = attributes(object)$agevar)
}

#' Plot output from a vital model
#'
#' Produces a plot showing a model applied to a vital object. This can be applied
#' to one type of model only. So use select() to choose the model column to plot.
#' If there are multiple keys, separate models will be identified by colour.
#'
#' @param object A mable object obtained from a vital.
#' @param ... Further arguments ignored.
#' @author Rob J Hyndman
#' @return A ggplot2 object.
#'
#' @examples
#' library(ggplot2)
#' aus_mortality |>
#'  dplyr::filter(State == "Victoria") |>
#'  model(ave = FMEAN(Mortality)) |>
#'  autoplot() + scale_y_log10()
#'
#' @export
autoplot.mdl_vtl_df <- function (object, ...) {
  if (length(mable_vars(object)) > 1) {
    stop("Model plotting is only supported for one class of models. To produce a plot for a specific class of models, use `select()`")
  } else {
    model <- mable_vars(object)
    class(object) <- c(class(object[[model]][[1]]$fit), class(object))
    agevar <- attributes(object[[model]][[1]]$fit$fitted)$agevar
    p <- autoplot(object, age = agevar, ...)
  }
  p
}

# Plot a variable against age by key
age_plot <- function(object, .var, keys) {
  # Convert age to time and use fabletools::autoplot.tbl_ts
  names <- colnames(object)[!(colnames(object) %in% c(keys, .var))]
  age <- names[grep("age", names, ignore.case=TRUE)]
  object_ts <- tsibble::as_tsibble(object, index=sym(age), key = all_of(keys[keys != age]))
  fabletools::autoplot(object_ts, !!sym(.var)) + ggplot2::xlab(age)
}

# Plot a variable against time by key
time_plot <- function(object, .var, keys) {
  # Just use fabletools::autoplot.tbl_ts
  fabletools::autoplot(object, !!sym(.var)) +
    ggplot2::xlab(tsibble::index_var(object))
}
