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
#' @export
transmute.vital <- function(.data, ...) {
  attr_data <- attributes(.data)
  as_vital(NextMethod(), age = attr_data$agevar,
           sex = attr_data$sexvar,
           deaths = attr_data$deathsvar,
           births = attr_data$birthsvar,
           population = attr_data$populationvar)
}

#' @exportS3Method dplyr::summarise
#' @export
summarise.vital <- function(.data, ..., .groups = NULL) {
  attr_data <- attributes(.data)
  as_vital(NextMethod(), age = attr_data$agevar,
           sex = attr_data$sexvar,
           deaths = attr_data$deathsvar,
           births = attr_data$birthsvar,
           population = attr_data$populationvar)
}

#' @importFrom dplyr group_by_drop_default
#' @exportS3Method dplyr::group_by
#' @export
group_by.vital <- function(.data, ..., .add = FALSE,
                            .drop = group_by_drop_default(.data)) {
  attr_data <- attributes(.data)
  as_vital(NextMethod(), age = attr_data$agevar,
           sex = attr_data$sexvar,
           deaths = attr_data$deathsvar,
           births = attr_data$birthsvar,
           population = attr_data$populationvar)
}

#' @exportS3Method dplyr::dplyr_row_slice
#' @export
dplyr_row_slice.vital <- function(data, i, ..., preserve = FALSE) {
  attr_data <- attributes(data)
  as_vital(NextMethod(), age = attr_data$agevar,
           sex = attr_data$sexvar,
           deaths = attr_data$deathsvar,
           births = attr_data$birthsvar,
           population = attr_data$populationvar)
}

#' @exportS3Method dplyr::dplyr_col_modify
#' @export
dplyr_col_modify.vital <- function(data, cols) {
  attr_data <- attributes(data)
  as_vital(NextMethod(), age = attr_data$agevar,
           sex = attr_data$sexvar,
           deaths = attr_data$deathsvar,
           births = attr_data$birthsvar,
           population = attr_data$populationvar)
}

#' @exportS3Method dplyr::dplyr_reconstruct
#' @export
dplyr_reconstruct.vital <- function(data, template) {
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
