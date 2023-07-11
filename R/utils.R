# Based on utils.R from the tidyverse package

msg <- function(..., startup = FALSE) {
  if (startup) {
    if (!isTRUE(getOption("vital.quiet"))) {
      packageStartupMessage(text_col(...))
    }
  } else {
    message(text_col(...))
  }
}

text_col <- function(x) {
  # If RStudio not available, messages already printed in black
  if (!rstudioapi::isAvailable()) {
    return(x)
  }

  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }

  theme <- rstudioapi::getThemeInfo()

  if (isTRUE(theme$dark)) crayon::white(x) else crayon::black(x)
}

#' List all packages loaded by vital
#'
#' @param include_self Include vital in the list?
#' @return A character vector of package names.
#' @export
#' @examples
#' vital_packages()
vital_packages <- function(include_self = FALSE) {
  raw <- utils::packageDescription("vital")$Imports
  imports <- strsplit(raw, ",")[[1]]
  parsed <- gsub("^\\s+|\\s+$", "", imports)
  names <- vapply(strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1))

  if (include_self) {
    names <- c(names, "vital")
  }

  names
}

invert <- function(x) {
  if (length(x) == 0) {
    return()
  }
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}


style_grey <- function(level, ...) {
  crayon::style(
    paste0(...),
    crayon::make_style(grDevices::grey(level), grey = TRUE)
  )
}

nest_keys <- function(.data, nm = "data"){
  out <- unclass(key_data(.data))
  key <- tsibble::key_vars(.data)
  row_indices <- out[[length(out)]]
  out[[length(out)]] <- NULL
  col_nest <- -match(key, colnames(.data))
  if(is_empty(col_nest)){
    col_nest <- NULL
  }
  idx <- tsibble::index_var(.data)
  idx2 <- tsibble::index2_var(.data)
  ordered <- is_ordered(.data)
  regular <- is_regular(.data)
  out[[nm]] <- purrr::map(row_indices, function(x, i, j){
    out <- if(is.null(j)) x[i,] else x[i,j]
    tsibble::build_tsibble_meta(
      out,
      key_data = tibble::as_tibble(list(.rows = list(seq_along(i)))),
      index = idx, index2 = idx2, ordered = ordered,
      interval = if(length(i) > 1 && regular) tsibble::interval_pull(out[[idx]]) else tsibble::interval(.data)
    )
  }, x = tibble::as_tibble(.data), j = col_nest)
  tibble::as_tibble(out)
}

list_of_models <- function(x = list()){
  vctrs::new_vctr(x, class = "lst_mdl")
}

