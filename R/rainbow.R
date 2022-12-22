#' Rainbow plot of demographic data against age
#'
#' Produce rainbow plot (coloured by time index) of demographic variable against
#' against age.
#'
#' @param .data A tsibble including an age variable and the variable you wish to plot.
#' @param .vars A bare expression containing the name of the variable you wish to plot.
#' @param age Variable in `.data` containing start year of age intervals. If omitted, the variable with name `Age` or `Age_group` will be used (not case sensitive).
#'
#' @author Rob Hyndman
#' @references Hyndman, Rob J & Shang, Han Lin (2010) Rainbow plots, bagplots,
#' and boxplots for functional data. \emph{Journal of Computational and Graphical Statistics},
#' \bold{19}(1), 29-45.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' rainbow_plot(aus_fertility, Fertility)
#' @export

rainbow_plot <- function(.data, .vars = NULL, age) {
  quo_vars <- enquo(.vars)
  kv <- tsibble::key_vars(.data)
  if (rlang::quo_is_null(quo_vars)) {
    mv <- tsibble::measured_vars(.data)
    pos <- which(vapply(.data[mv], is.numeric, logical(1L)))
    if (rlang::is_empty(pos)) {
      abort("Could not automatically identify an appropriate plot variable, please specify the variable to plot.")
    }
    rlang::inform(sprintf("Plot variable not specified, automatically selected `.vars = %s`",
                   mv[pos[1]]))
    y <- sym(mv[pos[1]])
    .vars <- rlang::as_quosures(list(y), env = rlang::empty_env())
  } else if (purrr::possibly(purrr::compose(rlang::is_quosures, rlang::eval_tidy), FALSE)(.vars)) {
    .vars <- rlang::eval_tidy(.vars)
    .data <- tidyr::gather(dplyr::mutate(.data, !!!.vars), ".response",
                     "value", !!!purrr::map(.vars, quo_name), factor_key = TRUE)
    y <- sym("value")
  }
  else {
    y <- quo_vars
    .vars <- list(y)
  }

  # Index variable
  index <- tsibble::index_var(.data)

  # Find age columns
  if (!missing(age)) {
    age <- {{ age }}
  } else {
    age <- find_key(.data, c("age", "age_group"))
  }
  # Drop Age as a key and nest results
  kv <- kv[kv != age]
  nk <- length(kv)

  aes_spec <- list(x = sym(age), y = y, color = sym(index), group = sym(index))
  p <- .data |>
    ggplot2::ggplot(rlang::eval_tidy(expr(ggplot2::aes(!!!aes_spec)))) +
    ggplot2::geom_line() +
    ggplot2::xlab(age) +
    ggplot2::scale_color_gradientn(colours = rainbow(10))
  if (nk > 1) {
    p <- p + ggplot2::facet_wrap(kv)
  }
  return(p)
}
