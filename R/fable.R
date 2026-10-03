# Internal function to make a vital fable object

build_vital_fable <- function(
  x,
  response,
  distribution,
  vitals = NULL
) {
  # Without a distribution column (e.g. after summarise) it is no longer a fable
  if (!(distribution %in% colnames(x))) {
    return(x)
  }
  final <- as_fable(x, response = !!response, distribution = !!distribution) |>
    suppressWarnings()
  vitals <- unlist(vitals)
  attr(final, "vital") <- vitals[vitals %in% colnames(x)]
  # Keep grouping classes first so grouped methods dispatch before fable ones
  cls <- setdiff(class(final), c("grouped_fbl", "fbl_ts"))
  grouped <- grepl("^grouped", cls)
  class(final) <- c(
    if (any(grouped)) "grouped_fbl_vtl",
    cls[grouped],
    "fbl_vtl_ts",
    "fbl_ts",
    "vital",
    cls[!grouped]
  )
  return(final)
}

#' @export
tbl_sum.fbl_vtl_ts <- function(x) {
  out <- NextMethod()
  names(out)[1] <- "A vital fable"
  out
}
