# Make sure class is preserved

#' @export
arrange.mdl_vtl_df <- function(.data, ...) {
  out <- NextMethod()
  class(out) <- c("mdl_vtl_df", class(out))
  out
}

#' @export
select.mdl_vtl_df <- function(.data, ...) {
  out <- NextMethod()
  class(out) <- c("mdl_vtl_df", class(out))
  out
}

#' @export
transmute.mdl_vtl_df <- function(.data, ...) {
  out <- NextMethod()
  class(out) <- c("mdl_vtl_df", class(out))
  out
}

#' @exportS3Method dplyr::relocate
relocate.mdl_vtl_df <- function(.data, ...) {
  out <- NextMethod()
  class(out) <- c("mdl_vtl_df", class(out))
  out
}

#' @export
summarise.mdl_vtl_df <- function(.data, ..., .groups = NULL) {
  out <- NextMethod()
  class(out) <- c("mdl_vtl_df", class(out))
  out
}

#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.mdl_vtl_df <- function(data, i, ..., preserve = FALSE) {
  out <- NextMethod()
  class(out) <- c("mdl_vtl_df", class(out))
  out
}

#' @exportS3Method dplyr::dplyr_col_modify
dplyr_col_modify.mdl_vtl_df <- function(data, cols) {
  out <- NextMethod()
  class(out) <- c("mdl_vtl_df", class(out))
  out
}

#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.mdl_vtl_df <- function(data, template) {
  out <- NextMethod()
  class(out) <- c("mdl_vtl_df", class(out))
  out
}


#' @export
`[.mdl_vtl_df` <- function(x, i, j, drop = FALSE) {
  out <- NextMethod()
  class(out) <- c("mdl_vtl_df", class(out))
  out
}

# filtering and selecting of mdl_vtl_df objects

#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.mdl_vtl_df <- function(data, i, ..., preserve = FALSE) {
  out <- NextMethod()
  class(out) <- c("mdl_vtl_df", class(out))
  out
}

#' @exportS3Method dplyr::dplyr_col_modify
dplyr_col_modify.mdl_vtl_df <- function(data, cols) {
  out <- NextMethod()
  class(out) <- c("mdl_vtl_df", class(out))
  out
}

