new_humble <- function(.data, intervals = NULL, ..., class = NULL) {
  out <- tibble::new_tibble(.data, ..., class = c(class, "humble"))
  for (avar in intervals) {
    if (grepl("range", avar)) {
      text <- out[[avar]]
      lower <- regmatches(text, regexpr("[0-9]+", text))
      lower <- as.integer(lower)
      text_lower_removed <- sapply(
        regmatches(text, regexpr("[0-9]+", text), invert = TRUE),
        function(x) x[2]
      )
      # dummy number so below works for no upper bounds
      text_lower_removed <- paste0(text_lower_removed, "_99999")
      upper <- regmatches(text_lower_removed, regexpr("[0-9]+", text_lower_removed))
      upper <- as.integer(upper)
      upper[upper == 99999] <- Inf
      upper[lower == 0] <- 0
      out[[avar]] <- interval(lower = lower, upper = upper)
    }
  }
  out
}
