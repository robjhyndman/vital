# These just grab the vital attributes, then use the tsibble method,
# before adding back the missing attributes

#' @export
arrange.vital <- function(.data, ...) {
  attr_data <- attributes(.data)
  as_vital(NextMethod(), age = attr_data$agevar,
           sex = attr_data$sexvar,
           deaths = attr_data$deathsvar,
           births = attr_data$birthsvar,
           population = attr_data$populationvar)
}

#' @export
select.vital <- function(.data, ...) {
  attr_data <- attributes(.data)
  as_vital(NextMethod(), age = attr_data$agevar,
           sex = attr_data$sexvar,
           deaths = attr_data$deathsvar,
           births = attr_data$birthsvar,
           population = attr_data$populationvar)
}

#' @exportS3Method dplyr::transmute
transmute.vital <- function(.data, ...) {
  attr_data <- attributes(.data)
  as_vital(NextMethod(), age = attr_data$agevar,
           sex = attr_data$sexvar,
           deaths = attr_data$deathsvar,
           births = attr_data$birthsvar,
           population = attr_data$populationvar)
}

#' @exportS3Method dplyr::relocate
relocate.vital <- function(.data, ...) {
  attr_data <- attributes(.data)
  as_vital(NextMethod(), age = attr_data$agevar,
           sex = attr_data$sexvar,
           deaths = attr_data$deathsvar,
           births = attr_data$birthsvar,
           population = attr_data$populationvar)
}

#' @exportS3Method dplyr::summarise
summarise.vital <- function(.data, ..., .groups = NULL) {
  attr_data <- attributes(.data)
  as_vital(NextMethod(), age = attr_data$agevar,
           sex = attr_data$sexvar,
           deaths = attr_data$deathsvar,
           births = attr_data$birthsvar,
           population = attr_data$populationvar)
}

#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.vital <- function(data, i, ..., preserve = FALSE) {
  attr_data <- attributes(data)
  as_vital(NextMethod(), age = attr_data$agevar,
           sex = attr_data$sexvar,
           deaths = attr_data$deathsvar,
           births = attr_data$birthsvar,
           population = attr_data$populationvar)
}

#' @exportS3Method dplyr::dplyr_col_modify
dplyr_col_modify.vital <- function(data, cols) {
  attr_data <- attributes(data)
  as_vital(NextMethod(), age = attr_data$agevar,
           sex = attr_data$sexvar,
           deaths = attr_data$deathsvar,
           births = attr_data$birthsvar,
           population = attr_data$populationvar)
}

#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.vital <- function(data, template) {
  attr_data <- attributes(data)
  as_vital(NextMethod(), age = attr_data$agevar,
           sex = attr_data$sexvar,
           deaths = attr_data$deathsvar,
           births = attr_data$birthsvar,
           population = attr_data$populationvar)
}

#' @importFrom dplyr group_by_drop_default
#' @exportS3Method dplyr::group_by
group_by.vital <- function(.data, ..., .add = FALSE,
                           .drop = group_by_drop_default(.data)) {
  attr_data <- attributes(.data)
  tmp <- as_vital(NextMethod(), age = attr_data$agevar,
                  sex = attr_data$sexvar,
                  deaths = attr_data$deathsvar,
                  births = attr_data$birthsvar,
                  population = attr_data$populationvar)
  tmp_class <- class(tmp)
  grouped_classes <- grepl("grouped", tmp_class)
  class(tmp) <- c("grouped_vital", tmp_class[grouped_classes], tmp_class[!grouped_classes])
  return(tmp)
}

#' @exportS3Method dplyr::ungroup
ungroup.grouped_vital <- function(x, ...) {
  attr_data <- attributes(x)
  as_vital(NextMethod(), age = attr_data$agevar,
           sex = attr_data$sexvar,
           deaths = attr_data$deathsvar,
           births = attr_data$birthsvar,
           population = attr_data$populationvar)
}

#' @export
arrange.grouped_vital <- arrange.vital

#' @export
select.grouped_vital <- select.vital

#' @exportS3Method dplyr::transmute
transmute.grouped_vital <- transmute.vital

#' @exportS3Method dplyr::summarise
summarise.grouped_vital <- summarise.vital

#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.grouped_vital <- dplyr_row_slice.vital

#' @exportS3Method dplyr::dplyr_col_modify
dplyr_col_modify.grouped_vital <- dplyr_col_modify.vital

#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.grouped_vital <- function(data, template) {
  attr_data <- attributes(data)
  as_vital(NextMethod(), age = attr_data$agevar,
           sex = attr_data$sexvar,
           deaths = attr_data$deathsvar,
           births = attr_data$birthsvar,
           population = attr_data$populationvar)
}

#' @export
`[.vital` <- function(x, i, j, drop = FALSE) {
  attr_data <- attributes(x)
  res <- NextMethod()
  if(inherits(res, "tbl_ts")) {
    as_vital(res, age = attr_data$agevar,
             sex = attr_data$sexvar,
             deaths = attr_data$deathsvar,
             births = attr_data$birthsvar,
             population = attr_data$populationvar)
  } else {
    res
  }
}
