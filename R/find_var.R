# Find column in data frame
# names contains vector of names to search for

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
