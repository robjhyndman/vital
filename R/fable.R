# Internal function to make a vital fable object

build_vital_fable <- function(x, response, distribution,
                          .age = NULL, .sex = NULL, .deaths = NULL, .births = NULL, .population = NULL,
                          reorder = FALSE, ...) {
  # Add attributes to x to identify the various variables
  vnames <- colnames(x)
  if(!is.null(.age)) {
    if(!(.age %in% vnames)) { .age <- NULL }
  }
  if(!is.null(.sex)) {
    if(!(.sex %in% vnames)) { .sex <- NULL }
  }
  if(!is.null(.births)) {
    if(!(.births %in% vnames)) { .births <- NULL }
  }
  if(!is.null(.deaths)) {
    if(!(.deaths %in% vnames)) { .deaths <- NULL }
  }
  if(!is.null(.population)) {
    if(!(.population %in% vnames)) { .population <- NULL }
  }
  #
  final <- build_fable(x, response = response, distribution =  distribution) |>
    suppressWarnings()
  attr(final, "agevar") <- .age
  attr(final, "sexvar") <- .sex
  attr(final, "birthsvar") <- .births
  attr(final, "deathsvar") <- .deaths
  attr(final, "populationvar") <- .population
  class(final) <- c("fbl_vtl_ts", class(final))
  return(final)
}

#' @export
tbl_sum.fbl_vtl_ts <- function (x)
{
  out <- NextMethod()
  names(out)[1] <- "A vital fable"
  out
}
