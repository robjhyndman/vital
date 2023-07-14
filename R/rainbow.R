

rainbow_plot <- function(.data, .vars = NULL, age) {
  quo_vars <- rlang::enquo(.vars)
  kv <- tsibble::key_vars(.data)
  if (rlang::quo_is_null(quo_vars)) {
    mv <- tsibble::measured_vars(.data)
    pos <- which(vapply(.data[mv], is.numeric, logical(1L)))
    if (rlang::is_empty(pos)) {
      rlang::abort("Could not automatically identify an appropriate plot variable, please specify the variable to plot.")
    }
    rlang::inform(sprintf("Plot variable not specified, automatically selected `.vars = %s`",
                   mv[pos[1]]))
    y <- rlang::sym(mv[pos[1]])
    .vars <- rlang::as_quosures(list(y), env = rlang::empty_env())
  } else if (purrr::possibly(purrr::compose(rlang::is_quosures, rlang::eval_tidy), FALSE)(.vars)) {
    .vars <- rlang::eval_tidy(.vars)
    .data <- tidyr::gather(dplyr::mutate(.data, !!!.vars), ".response",
                     "value", !!!purrr::map(.vars, rlang::quo_name), factor_key = TRUE)
    y <- rlang::sym("value")
  }
  else {
    y <- quo_vars
    .vars <- list(y)
  }

  # Index variable
  index <- tsibble::index_var(.data)

  # Age variable
  if(inherits(.data, "vital")) {
    age <- attributes(.data)$agevar
    if(is.null(age)) {
      stop("No age variable found")
    }
  } else if(inherits(.data, "tbl_ts")) {
    # Need to find the age variable
    age <- find_key(.data, c("age", "age_group"))
  } else {
    stop("Not sure how to handle this class")
  }

  # Drop Age as a key and nest results
  kv <- kv[kv != age]
  nk <- length(kv)

  aes_spec <- list(x = rlang::sym(age), y = y, color = rlang::sym(index), group = rlang::sym(index))
  p <- .data |>
    as_tsibble() |>
    ggplot2::ggplot(rlang::eval_tidy(rlang::expr(ggplot2::aes(!!!aes_spec)))) +
    ggplot2::geom_line() +
    ggplot2::xlab(age) +
    ggplot2::scale_color_gradientn(colours = rainbow(10))
  if (nk > 1) {
    p <- p + ggplot2::facet_wrap(kv)
  }
  return(p)
}
