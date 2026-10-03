# Make sure class is preserved

restore_mdl_vtl_df <- function(x) {
  # Results without model columns are no longer mables
  if (inherits(x, "mdl_df")) {
    class(x) <- unique(c("mdl_vtl_df", class(x)))
  }
  x
}

#' @export
arrange.mdl_vtl_df <- function(.data, ...) {
  restore_mdl_vtl_df(NextMethod())
}

#' @export
select.mdl_vtl_df <- function(.data, ...) {
  restore_mdl_vtl_df(NextMethod())
}

#' @export
transmute.mdl_vtl_df <- function(.data, ...) {
  restore_mdl_vtl_df(NextMethod())
}

#' @exportS3Method dplyr::relocate
relocate.mdl_vtl_df <- function(.data, ...) {
  restore_mdl_vtl_df(NextMethod())
}

#' @export
summarise.mdl_vtl_df <- function(.data, ..., .groups = NULL) {
  restore_mdl_vtl_df(NextMethod())
}

#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.mdl_vtl_df <- function(data, i, ..., preserve = FALSE) {
  restore_mdl_vtl_df(NextMethod())
}

#' @exportS3Method dplyr::dplyr_col_modify
dplyr_col_modify.mdl_vtl_df <- function(data, cols) {
  restore_mdl_vtl_df(NextMethod())
}

#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.mdl_vtl_df <- function(data, template) {
  restore_mdl_vtl_df(NextMethod())
}


#' @export
`[.mdl_vtl_df` <- function(x, i, j, drop = FALSE) {
  restore_mdl_vtl_df(NextMethod())
}
