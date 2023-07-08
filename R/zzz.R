# Based on zzz.R from the tidyverse package

.onAttach <- function(...) {
  needed <- core[!is_attached(core)]
  if (length(needed) == 0) {
    return()
  }

  crayon::num_colors(TRUE)
  vital_attach()

  if (!"package:conflicted" %in% search()) {
    x <- vital_conflicts()
    msg(vital_conflict_message(x), startup = TRUE)
  }
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}
