#' Make a new vital containing products and ratios from a key variable
#'
#' @param .data A vital object
#' @param key A character string specifying the key variable
#' @return A vital object
#' @export

make_pr <- function(.data, .var, key = "Sex") {
  if(!inherits(.data, "vital")) {
    stop(".data needs to be a vital object")
  }
  colnames <- colnames(.data)
  attr_data <- attributes(.data)
  # Index variable
  index <- tsibble::index_var(.data)
  # Keys including age
  keys <- tsibble::key_vars(.data)
  age <- attr_data$agevar
  sex <- attr_data$sexvar
  keys_noage <- keys[!(keys %in% c(age, "Age", "AgeGroup", "Age_Group"))]

  if(!(key %in% keys_noage)) {
    stop("key not found in data set")
  }

  # Create a new key containing products and ratios
  varname <- names(eval_select(enquo(.var), data = .data))
  new_var <- .data[[varname]]
  new_key <- paste0(.data[[key]], "_ratio")


  return(.data)
}
