# Verbs for vital fable objects.
# These grab the vital attributes and then use the fable method (or whatever is next method)
# before adding back the missing attributes

#' @export
arrange.fbl_vtl_ts <- function(.data, ...) {
  attr_data <- attributes(.data)
  vvar <- vital_var_list(.data)
  build_vital_fable(
    NextMethod(),
    response = attr_data$response,
    distribution = attr_data$dist,
    vitals = vvar
  )
}

#' @export
select.fbl_vtl_ts <- function(.data, ...) {
  attr_data <- attributes(.data)
  loc <- eval_select(expr(c(...)), .data)
  vvar <- rename_vital_vars(vital_var_list(.data), .data, loc)
  build_vital_fable(
    NextMethod(),
    response = attr_data$response,
    distribution = attr_data$dist,
    vitals = vvar
  ) |>
    regroup_selected(.data, ...)
}

#' @export
transmute.fbl_vtl_ts <- function(.data, ...) {
  attr_data <- attributes(.data)
  vvar <- vital_var_list(.data)
  build_vital_fable(
    NextMethod(),
    response = attr_data$response,
    distribution = attr_data$dist,
    vitals = vvar
  )
}

#' @exportS3Method dplyr::relocate
relocate.fbl_vtl_ts <- function(.data, ...) {
  attr_data <- attributes(.data)
  vvar <- vital_var_list(.data)
  build_vital_fable(
    NextMethod(),
    response = attr_data$response,
    distribution = attr_data$dist,
    vitals = vvar
  )
}

#' @export
summarise.fbl_vtl_ts <- function(.data, ..., .groups = NULL) {
  attr_data <- attributes(.data)
  vvar <- vital_var_list(.data)
  build_vital_fable(
    NextMethod(),
    response = attr_data$response,
    distribution = attr_data$dist,
    vitals = vvar
  )
}

#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.fbl_vtl_ts <- function(data, i, ..., preserve = FALSE) {
  attr_data <- attributes(data)
  vvar <- vital_var_list(data)
  build_vital_fable(
    NextMethod(),
    response = attr_data$response,
    distribution = attr_data$dist,
    vitals = vvar
  )
}

#' @exportS3Method dplyr::dplyr_col_modify
dplyr_col_modify.fbl_vtl_ts <- function(data, cols) {
  attr_data <- attributes(data)
  vvar <- vital_var_list(data)
  build_vital_fable(
    NextMethod(),
    response = attr_data$response,
    distribution = attr_data$dist,
    vitals = vvar
  )
}

#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.fbl_vtl_ts <- function(data, template) {
  attr_data <- attributes(data)
  vvar <- vital_var_list(data)
  build_vital_fable(
    NextMethod(),
    response = attr_data$response,
    distribution = attr_data$dist,
    vitals = vvar
  )
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
  build_vital_fable(
    NextMethod(),
    response = attr_data$response,
    distribution = attr_data$dist,
    vitals = vvar
  )
}

#' @export
ungroup.grouped_fbl_vtl <- function(x, ...) {
  attr_data <- attributes(x)
  vvar <- vital_var_list(x)
  build_vital_fable(
    NextMethod(),
    response = attr_data$response,
    distribution = attr_data$dist,
    vitals = vvar
  )
}

#' @export
arrange.grouped_fbl_vtl <- arrange.fbl_vtl_ts

#' @export
select.grouped_fbl_vtl <- select.fbl_vtl_ts

#' @export
transmute.grouped_fbl_vtl <- transmute.fbl_vtl_ts

#' @export
summarise.grouped_fbl_vtl <- summarise.fbl_vtl_ts

#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.grouped_fbl_vtl <- dplyr_row_slice.fbl_vtl_ts

#' @exportS3Method dplyr::dplyr_col_modify
dplyr_col_modify.grouped_fbl_vtl <- dplyr_col_modify.fbl_vtl_ts

#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.grouped_fbl_vtl <- function(data, template) {
  attr_data <- attributes(data)
  vvar <- vital_var_list(data)
  build_vital_fable(
    NextMethod(),
    response = attr_data$response,
    distribution = attr_data$dist,
    vitals = vvar
  )
}

#' @export
`[.fbl_vtl_ts` <- function(x, i, j, drop = FALSE) {
  attr_data <- attributes(x)
  vvar <- vital_var_list(x)
  res <- NextMethod()
  if (inherits(res, "tbl_ts")) {
    build_vital_fable(
      res,
      response = attr_data$response,
      distribution = attr_data$dist,
      vitals = vvar
    )
  } else {
    res
  }
}

#' @export
`[.grouped_fbl_vtl` <- `[.fbl_vtl_ts`
