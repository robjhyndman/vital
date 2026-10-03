#' Rainbow plot of demographic data against age
#'
#' Produce rainbow plot (coloured by time index) of demographic variable against
#' against age.
#'
#' @param object A vital including an age variable and the variable you wish to plot.
#' @param .vars The name of the variable you wish to plot.
#' @param age The name of the age variable. If not supplied, the function will attempt to find it.
#' @param ... Further arguments not used.
#'
#' @author Rob J Hyndman
#' @references Hyndman, Rob J & Shang, Han Lin (2010) Rainbow plots, bagplots,
#' and boxplots for functional data. *Journal of Computational and Graphical Statistics*,
#' 19(1), 29-45. <https://robjhyndman.com/publications/rainbow-fda/>
#'
#' @return A ggplot2 object.
#'
#' @examples
#' autoplot(norway_fertility, Fertility)
#' @export
autoplot.vital <- function(object, .vars = NULL, age = age_var(object), ...) {
  quo_vars <- enquo(.vars)

  # Index variable
  index <- tsibble::index_var(object)

  # A vital without age, so try a tsibble autoplot
  if (is.null(age)) {
    object <- as_tsibble(object)
    return(autoplot(object, .vars = {{ .vars }}, ...))
  }

  # Drop Age as a key and nest results
  kv <- tsibble::key_vars(object)
  kv <- kv[!(tolower(kv) %in% tolower(c(age, "Age", "AgeGroup", "Age_Group")))]
  nk <- length(kv)

  # Variable to plot
  if (quo_is_null(quo_vars)) {
    mv <- tsibble::measured_vars(object)
    pos <- which(vapply(object[mv], is.numeric, logical(1L)))
    if (is_empty(pos)) {
      abort(
        "Could not automatically identify an appropriate plot variable, please specify the variable to plot."
      )
    }
    inform(sprintf(
      "Plot variable not specified, automatically selected `.vars = %s`",
      mv[pos[1]]
    ))
    y <- sym(mv[pos[1]])
    .vars <- as_quosures(list(y), env = empty_env())
  } else if (possibly(compose(is_quosures, eval_tidy), FALSE)(.vars)) {
    .vars <- eval_tidy(.vars)
    response_names <- map_chr(.vars, quo_name)
    object <- tidyr::pivot_longer(
      mutate(object, !!!.vars),
      all_of(response_names),
      names_to = ".response",
      values_to = "value",
      names_transform = list(
        .response = function(x) factor(x, levels = response_names)
      )
    )
    y <- sym("value")
  } else {
    y <- quo_vars
    .vars <- list(y)
  }

  nyears <- length(unique(object[[index]]))
  aes_spec <- list(x = rlang::sym(age), y = y)
  if (nyears > 1) {
    aes_spec$color <- rlang::sym(index)
    aes_spec$group <- rlang::sym(index)
  }
  p <- object |>
    as_tsibble() |>
    ggplot2::ggplot(rlang::eval_tidy(rlang::expr(ggplot2::aes(!!!aes_spec)))) +
    ggplot2::geom_line() +
    ggplot2::xlab(age) +
    # ggplot2::scale_color_gradientn(colours = hcl.colors(palette = "Batlow", n=10))
    ggplot2::scale_color_gradientn(colours = rainbow(10))
  if (nk > 0) {
    p <- p + ggplot2::facet_wrap(kv)
  }
  return(p)
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
#' norway_mortality |>
#'   model(ave = FMEAN(Mortality)) |>
#'   forecast(h = 10) |>
#'   autoplot() + scale_y_log10()
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
  if (length(to_plot) > 1) {
    warning(paste("Multiple variables to plot. Choosing", to_plot[1]))
  }
  autoplot.vital(object, .vars = !!sym(to_plot[1]), ...)
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
#' norway_mortality |>
#'   model(ave = FMEAN(Mortality)) |>
#'   autoplot() + scale_y_log10()
#'
#' @export
autoplot.mdl_vtl_df <- function(object, ...) {
  if (length(mable_vars(object)) > 1) {
    stop(
      "Model plotting is only supported for one class of models. To produce a plot for a specific class of models, use `select()`"
    )
  } else {
    model <- mable_vars(object)
    class(object) <- c(class(object[[model]][[1]]$fit), class(object)[-1])
    agevar <- age_var(object[[model]][[1]]$fit$fitted)
    p <- autoplot(object, age = agevar, ...)
  }
  p
}

# Plot a variable against age by key
age_plot <- function(object, .var, keys) {
  names <- colnames(object)[!(colnames(object) %in% c(keys, .var))]
  age <- names[grep("age", names, ignore.case = TRUE)]
  kv_noage <- keys[keys != age]
  key_plot(object, sym(age), .var, kv_noage)
}

# Plot a variable against time by key
time_plot <- function(object, .var, keys) {
  key_plot(object, sym(tsibble::index_var(object)), .var, keys)
}

# Line plot of .var against x, with a coloured line for each combination of keys
key_plot <- function(object, x, .var, keys) {
  aes_spec <- list(x = x, y = sym(.var))
  if (length(keys) > 0) {
    col <- if (length(keys) == 1) {
      sym(keys)
    } else {
      expr(interaction(!!!syms(keys), sep = "/"))
    }
    aes_spec$colour <- col
    aes_spec$group <- col
  }
  p <- ggplot2::ggplot(object, ggplot2::aes(!!!aes_spec)) +
    ggplot2::geom_line()
  if (length(keys) > 1) {
    p <- p + ggplot2::labs(colour = paste(keys, collapse = "/"))
  }
  p
}
