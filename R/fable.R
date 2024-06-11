# Internal function to make a vital fable object

build_vital_fable <- function(x, response, distribution,
    vitals = NULL, reorder = FALSE) {
  final <- build_fable(x, response = response, distribution =  distribution) |>
    suppressWarnings()
  attr(final, "vital")  <- vitals[vitals %in% colnames(x)]
  class(final) <- c("fbl_vtl_ts", "fbl_ts", "vital", class(final)[-1])
  return(final)
}

#' @export
tbl_sum.fbl_vtl_ts <- function (x)
{
  out <- NextMethod()
  names(out)[1] <- "A vital fable"
  out
}
