# These just grab the vital attributes, then use the tsibble method,
# before adding back the missing attributes

restore_vital <- function(x, vvar, ...) {
  as_vital(
    x,
    ...,
    .age = vvar$age,
    .sex = vvar$sex,
    .deaths = vvar$deaths,
    .births = vvar$births,
    .population = vvar$population
  )
}

#' @export
arrange.vital <- function(.data, ...) {
  vvar <- vital_var_list(.data)
  restore_vital(NextMethod(), vvar)
}

#' @export
select.vital <- function(.data, ...) {
  loc <- eval_select(expr(c(...)), .data)
  vvar <- rename_vital_vars(vital_var_list(.data), .data, loc)
  restore_vital(NextMethod(), vvar) |>
    regroup_selected(.data, ...)
}

# Follow renamed columns in the vital variables
# loc is a named vector of column positions from eval_select() or eval_rename()
rename_vital_vars <- function(vvar, .data, loc) {
  old <- names(.data)[loc]
  lapply(vvar, function(v) {
    if (v %in% old) names(loc)[match(v, old)] else v
  })
}

# tsibble's select() drops any grouping, so restore it,
# following grouping variables that the selection renamed
regroup_selected <- function(res, .data, ...) {
  grps <- dplyr::group_vars(.data)
  if (length(grps) == 0) {
    return(res)
  }
  loc <- eval_select(expr(c(...)), .data)
  old <- names(.data)[loc]
  renamed <- grps %in% old
  grps[renamed] <- names(loc)[match(grps[renamed], old)]
  group_by(res, !!!syms(grps))
}

#' @export
transmute.vital <- function(.data, ...) {
  vvar <- vital_var_list(.data)
  restore_vital(NextMethod(), vvar)
}

#' @exportS3Method dplyr::relocate
relocate.vital <- function(.data, ...) {
  vvar <- vital_var_list(.data)
  restore_vital(NextMethod(), vvar)
}

#' @export
summarise.vital <- function(.data, ..., .groups = NULL) {
  vvar <- vital_var_list(.data)
  restore_vital(NextMethod(), vvar)
}

#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.vital <- function(data, i, ..., preserve = FALSE) {
  vvar <- vital_var_list(data)
  restore_vital(NextMethod(), vvar)
}

#' @exportS3Method dplyr::dplyr_col_modify
dplyr_col_modify.vital <- function(data, cols) {
  vvar <- vital_var_list(data)
  restore_vital(NextMethod(), vvar)
}

#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.vital <- function(data, template) {
  # data is often a bare data frame, so take the vital variables from template
  vvar <- vital_var_list(template)
  restore_vital(NextMethod(), vvar)
}

#' @export
group_by.vital <- function(
  .data,
  ...,
  .add = FALSE,
  .drop = group_by_drop_default(.data)
) {
  vvar <- vital_var_list(.data)
  restore_vital(NextMethod(), vvar)
}

#' @export
ungroup.grouped_vital <- function(x, ...) {
  vvar <- vital_var_list(x)
  restore_vital(NextMethod(), vvar)
}

#' @export
arrange.grouped_vital <- arrange.vital

#' @export
select.grouped_vital <- select.vital

#' @export
transmute.grouped_vital <- transmute.vital

#' @export
summarise.grouped_vital <- summarise.vital

#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.grouped_vital <- dplyr_row_slice.vital

#' @exportS3Method dplyr::dplyr_col_modify
dplyr_col_modify.grouped_vital <- dplyr_col_modify.vital

#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.grouped_vital <- dplyr_reconstruct.vital

#' @export
`[.vital` <- function(x, i, j, drop = FALSE) {
  vvar <- vital_var_list(x)
  res <- NextMethod()
  if (inherits(res, "tbl_ts")) {
    restore_vital(res, vvar)
  } else {
    res
  }
}

#' @export
`[.grouped_vital` <- `[.vital`

#' @export
rename.vital <- function(.data, ...) {
  loc <- tidyselect::eval_rename(expr(c(...)), .data)
  vvar <- rename_vital_vars(vital_var_list(.data), .data, loc)
  restore_vital(NextMethod(), vvar)
}

#' @exportS3Method tsibble::fill_gaps
fill_gaps.vital <- function(
  .data,
  ...,
  .full = FALSE,
  .start = NULL,
  .end = NULL
) {
  vvar <- vital_var_list(.data)
  restore_vital(NextMethod(), vvar)
}
