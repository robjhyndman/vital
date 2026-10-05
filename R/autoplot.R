#' Rainbow plot of demographic data against age
#'
#' Produce rainbow plot (coloured by time index) of demographic variable against age.
#' If `object` has no age variable, the variable is plotted against time instead,
#' with a line for each combination of keys.
#'
#' @param object A vital including an age variable and the variable you wish to plot.
#' @param .vars The name of the variable you wish to plot.
#' @param age The name of the age variable. If not supplied, the function will attempt to find it.
#' @param ... Not used.
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
autoplot.vital <- function(object, .vars = NULL, age = NULL, ...) {
  quo_vars <- enquo(.vars)
  if (is.null(age)) {
    age <- age_var(object)
  }

  # Index variable
  index <- tsibble::index_var(object)
  interval <- format(tsibble::interval(object))

  # Keys identifying each series (all keys when there is no age variable,
  # e.g. age group labels in STMF data)
  kv <- if (is.null(age)) key_vars(object) else non_age_keys(object)
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
  }

  # With several variables, each is plotted in its own row of panels
  multiple <- ".response" %in% names(object)

  # Without an age variable, plot each series against time
  if (is.null(age)) {
    aes_spec <- list(x = rlang::sym(index), y = y)
    if (nk > 0) {
      aes_spec$colour <- if (nk == 1) {
        rlang::sym(kv)
      } else {
        rlang::expr(interaction(!!!rlang::syms(kv), sep = "/"))
      }
    }
    p <- ggplot2::ggplot(
      as_tibble(object),
      rlang::eval_tidy(rlang::expr(ggplot2::aes(!!!aes_spec)))
    ) +
      ggplot2::geom_line() +
      ggplot2::xlab(paste0(index, " [", interval, "]"))
    # Scales for tsibble time classes, which ggplot2 only finds when tsibble
    # is attached
    time_scale <- switch(
      class(object[[index]])[1],
      yearweek = tsibble::scale_x_yearweek,
      yearmonth = tsibble::scale_x_yearmonth,
      yearquarter = tsibble::scale_x_yearquarter,
      NULL
    )
    if (!is.null(time_scale)) {
      p <- p + time_scale()
    }
    if (nk > 1) {
      p <- p + ggplot2::labs(colour = paste(kv, collapse = "/"))
    }
    if (multiple) {
      p <- p +
        ggplot2::facet_grid(rows = ggplot2::vars(.response), scales = "free_y") +
        ggplot2::ylab(NULL)
    }
    return(p)
  }
  nyears <- length(unique(object[[index]]))
  aes_spec <- list(x = rlang::sym(age), y = y)
  if (nyears > 1) {
    aes_spec$color <- rlang::sym(index)
    aes_spec$group <- rlang::sym(index)
  }
  if (multiple) {
    aes_spec$group <- if (nyears > 1) {
      rlang::expr(interaction(!!rlang::sym(index), .response))
    } else {
      rlang::sym(".response")
    }
  }
  p <- object |>
    as_tsibble() |>
    ggplot2::ggplot(rlang::eval_tidy(rlang::expr(ggplot2::aes(!!!aes_spec)))) +
    ggplot2::geom_line() +
    ggplot2::xlab(age) +
    ggplot2::scale_color_gradientn(colours = rainbow(10))
  if (multiple) {
    p <- p +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.response),
        cols = ggplot2::vars(!!!rlang::syms(kv)),
        scales = "free_y"
      ) +
      ggplot2::ylab(NULL)
  } else if (nk > 0) {
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
  autoplot(as_model_class(object, "Model plotting"), ...)
}

# Plot a column of the model component of each fit in a single-model mable
# against age, with a coloured line for each combination of keys
model_component_plot <- function(object, age, .var) {
  if (is.null(age)) {
    age <- age_var(first_fit(object)$data)
  }
  keys <- setdiff(colnames(as_tibble(object)), attributes(object)$model)
  p <- key_plot(unnest_components(object, identity), sym(age), .var, keys)
  p + ggplot2::xlab(age)
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
