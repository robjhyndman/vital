# These just grab the vital attributes, then use the tsibble method,
# before adding back the missing attributes

#' @export
arrange.vital <- function(.data, ...) {
  vvar <- vital_var_list(.data)
  as_vital(NextMethod(),
    .age = vvar$age,
    .sex = vvar$sex,
    .deaths = vvar$deaths,
    .births = vvar$births,
    .population = vvar$population
  )
}

#' @export
select.vital <- function(.data, ...) {
  vvar <- vital_var_list(.data)
  as_vital(NextMethod(),
    .age = vvar$age,
    .sex = vvar$sex,
    .deaths = vvar$deaths,
    .births = vvar$births,
    .population = vvar$population
  )
}

#' @export
transmute.vital <- function(.data, ...) {
  vvar <- vital_var_list(.data)
  as_vital(NextMethod(),
    .age = vvar$age,
    .sex = vvar$sex,
    .deaths = vvar$deaths,
    .births = vvar$births,
    .population = vvar$population
  )
}

#' @exportS3Method dplyr::relocate
relocate.vital <- function(.data, ...) {
  vvar <- vital_var_list(.data)
  as_vital(NextMethod(),
    .age = vvar$age,
    .sex = vvar$sex,
    .deaths = vvar$deaths,
    .births = vvar$births,
    .population = vvar$population
  )
}

#' @export
summarise.vital <- function(.data, ..., .groups = NULL) {
  vvar <- vital_var_list(.data)
  as_vital(NextMethod(),
    .age = vvar$age,
    .sex = vvar$sex,
    .deaths = vvar$deaths,
    .births = vvar$births,
    .population = vvar$population
  )
}

#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.vital <- function(data, i, ..., preserve = FALSE) {
  vvar <- vital_var_list(data)
  as_vital(NextMethod(),
    .age = vvar$age,
    .sex = vvar$sex,
    .deaths = vvar$deaths,
    .births = vvar$births,
    .population = vvar$population
  )
}

#' @exportS3Method dplyr::dplyr_col_modify
dplyr_col_modify.vital <- function(data, cols) {
  vvar <- vital_var_list(data)
  as_vital(NextMethod(),
    .age = vvar$age,
    .sex = vvar$sex,
    .deaths = vvar$deaths,
    .births = vvar$births,
    .population = vvar$population
  )
}

#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.vital <- function(data, template) {
  vvar <- vital_var_list(data)
  as_vital(NextMethod(),
    .age = vvar$age,
    .sex = vvar$sex,
    .deaths = vvar$deaths,
    .births = vvar$births,
    .population = vvar$population
  )
}

#' @export
group_by.vital <- function(.data, ..., .add = FALSE,
                           .drop = group_by_drop_default(.data)) {
  vvar <- vital_var_list(.data)
  tmp <- as_vital(NextMethod(),
    .age = vvar$age,
    .sex = vvar$sex,
    .deaths = vvar$deaths,
    .births = vvar$births,
    .population = vvar$population
  )
  tmp_class <- class(tmp)
  grouped_classes <- grepl("grouped", tmp_class)
  class(tmp) <- c("grouped_vital", tmp_class[grouped_classes], tmp_class[!grouped_classes])
  return(tmp)
}

#' @export
ungroup.grouped_vital <- function(x, ...) {
  vvar <- vital_var_list(x)
  as_vital(NextMethod(),
    .age = vvar$age,
    .sex = vvar$sex,
    .deaths = vvar$deaths,
    .births = vvar$births,
    .population = vvar$population
  )
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
dplyr_reconstruct.grouped_vital <- function(data, template) {
  vvar <- vital_var_list(data)
  as_vital(NextMethod(),
    .age = vvar$age,
    .sex = vvar$sex,
    .deaths = vvar$deaths,
    .births = vvar$births,
    .population = vvar$population
  )
}

#' @export
`[.vital` <- function(x, i, j, drop = FALSE) {
  vvar <- vital_var_list(x)
  res <- NextMethod()
  if (inherits(res, "tbl_ts")) {
    as_vital(res,
      .age = vvar$age,
      .sex = vvar$sex,
      .deaths = vvar$deaths,
      .births = vvar$births,
      .population = vvar$population
    )
  } else {
    res
  }
}
