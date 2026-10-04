# Verbs for vital fable objects.
# These grab the vital attributes and then use the fable method (or whatever is next method)
# before adding back the missing attributes

restore_vital_fable <- function(x, attr_data, vvar) {
  build_vital_fable(
    x,
    response = attr_data$response,
    distribution = attr_data$dist,
    vitals = vvar
  )
}

#' @export
arrange.fbl_vtl_ts <- function(.data, ...) {
  attr_data <- attributes(.data)
  vvar <- vital_var_list(.data)
  restore_vital_fable(NextMethod(), attr_data, vvar)
}

#' @export
select.fbl_vtl_ts <- function(.data, ...) {
  attr_data <- attributes(.data)
  loc <- eval_select(expr(c(...)), .data)
  vvar <- rename_vital_vars(vital_var_list(.data), .data, loc)
  restore_vital_fable(NextMethod(), attr_data, vvar) |>
    regroup_selected(.data, ...)
}

#' @export
rename.fbl_vtl_ts <- function(.data, ...) {
  attr_data <- attributes(.data)
  loc <- tidyselect::eval_rename(expr(c(...)), .data)
  vvar <- rename_vital_vars(vital_var_list(.data), .data, loc)
  # Follow a renamed distribution column
  old <- names(.data)[loc]
  if (attr_data$dist %in% old) {
    attr_data$dist <- names(loc)[match(attr_data$dist, old)]
  }
  restore_vital_fable(NextMethod(), attr_data, vvar)
}

#' @export
transmute.fbl_vtl_ts <- function(.data, ...) {
  attr_data <- attributes(.data)
  vvar <- vital_var_list(.data)
  restore_vital_fable(NextMethod(), attr_data, vvar)
}

#' @exportS3Method dplyr::relocate
relocate.fbl_vtl_ts <- function(.data, ...) {
  attr_data <- attributes(.data)
  vvar <- vital_var_list(.data)
  restore_vital_fable(NextMethod(), attr_data, vvar)
}

#' @export
summarise.fbl_vtl_ts <- function(.data, ..., .groups = NULL) {
  attr_data <- attributes(.data)
  vvar <- vital_var_list(.data)
  restore_vital_fable(NextMethod(), attr_data, vvar)
}

#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.fbl_vtl_ts <- function(data, i, ..., preserve = FALSE) {
  attr_data <- attributes(data)
  vvar <- vital_var_list(data)
  restore_vital_fable(NextMethod(), attr_data, vvar)
}

#' @exportS3Method dplyr::dplyr_col_modify
dplyr_col_modify.fbl_vtl_ts <- function(data, cols) {
  attr_data <- attributes(data)
  vvar <- vital_var_list(data)
  restore_vital_fable(NextMethod(), attr_data, vvar)
}

#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.fbl_vtl_ts <- function(data, template) {
  # data is often a bare data frame, so take the attributes from template
  attr_data <- attributes(template)
  vvar <- vital_var_list(template)
  restore_vital_fable(NextMethod(), attr_data, vvar)
}

#' @export
group_by.fbl_vtl_ts <- function(
  .data,
  ...,
  .add = FALSE,
  .drop = group_by_drop_default(.data)
) {
  attr_data <- attributes(.data)
  vvar <- vital_var_list(.data)
  restore_vital_fable(NextMethod(), attr_data, vvar)
}

#' @export
ungroup.grouped_fbl_vtl <- function(x, ...) {
  attr_data <- attributes(x)
  vvar <- vital_var_list(x)
  restore_vital_fable(NextMethod(), attr_data, vvar)
}

#' @export
arrange.grouped_fbl_vtl <- arrange.fbl_vtl_ts

#' @export
select.grouped_fbl_vtl <- select.fbl_vtl_ts

#' @export
rename.grouped_fbl_vtl <- rename.fbl_vtl_ts

#' @export
transmute.grouped_fbl_vtl <- transmute.fbl_vtl_ts

#' @export
summarise.grouped_fbl_vtl <- summarise.fbl_vtl_ts

#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.grouped_fbl_vtl <- dplyr_row_slice.fbl_vtl_ts

#' @exportS3Method dplyr::dplyr_col_modify
dplyr_col_modify.grouped_fbl_vtl <- dplyr_col_modify.fbl_vtl_ts

#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.grouped_fbl_vtl <- dplyr_reconstruct.fbl_vtl_ts

#' @export
`[.fbl_vtl_ts` <- function(x, i, j, drop = FALSE) {
  attr_data <- attributes(x)
  vvar <- vital_var_list(x)
  res <- NextMethod()
  if (inherits(res, "tbl_ts")) {
    restore_vital_fable(res, attr_data, vvar)
  } else {
    res
  }
}

#' @export
`[.grouped_fbl_vtl` <- `[.fbl_vtl_ts`
