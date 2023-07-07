# Find column in data frame
# names contains vector of names to search for

find_key <- function(.data, names, return_error = TRUE) {
  keys <- tsibble::key_vars(.data)
  for (i in seq_along(names)) {
    col <- keys[tolower(keys) == names[i]]
    if (length(col) > 0) {
      return(col[1])
    }
  }
  if (return_error) {
    stop(paste("No key variable found with name in:", names))
  } else {
    return("None")
  }
}

find_measure <- function(.data, names) {
  find_measures(.data, names)[1]
}

find_measures <- function(.data, names) {
  measures <- tsibble::measured_vars(.data)
  col <- NULL
  for (i in seq_along(names)) {
    col <- c(col, measures[tolower(measures) == names[i]])
  }
  return(col)
}
