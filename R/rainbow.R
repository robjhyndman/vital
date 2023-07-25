# Produce rainbow plot from .data
# .vars and age are character strings containing variable names

rainbow_plot <- function(.data, .vars, age) {
  # Index variable
  index <- tsibble::index_var(.data)

  # Age variable
  if(!is.null(age)) {
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
  }

  # Drop Age as a key and nest results
  kv <- tsibble::key_vars(.data)
  kv <- kv[kv != age]
  nk <- length(kv)

  # Variable to plot
  if(is.null(.vars)) {
    mv <- colnames(.data)
    mv <- mv[!(mv %in% c(kv, age, index))]
    pos <- which(vapply(.data[mv], is.numeric, logical(1L)))
    if (is_empty(pos)) {
      abort("Could not automatically identify an appropriate plot variable, please specify the variable to plot.")
    }
    inform(sprintf("Plot variable not specified, automatically selected `.vars = %s`",
                   mv[pos[1]]))
    .vars <- mv[pos[1]]
  }

  nyears <- length(unique(.data[[index]]))
  aes_spec <- list(x = rlang::sym(age), y = rlang::sym(.vars))
  if(nyears > 1) {
    aes_spec$color <- rlang::sym(index)
    aes_spec$group <- rlang::sym(index)
  }
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
