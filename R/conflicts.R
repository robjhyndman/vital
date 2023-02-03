# Based on conflicts.R from the tidyverse package

#' Conflicts between tidylife packages and other packages
#'
#' This function lists all the conflicts between packages in the tidylife collection
#' and other packages that you have loaded.
#'
#' Some conflicts are deliberately ignored: \code{intersect}, \code{union},
#' \code{setequal}, and \code{setdiff} from dplyr; and \code{intersect},
#' \code{union}, \code{setdiff}, and \code{as.difftime} from lubridate.
#' These functions make the base equivalents generic, so shouldn't negatively affect any
#' existing code.
#'
#' @return A list object of class \code{tidylife_conflicts}.
#' @export
#' @examples
#' tidylife_conflicts()
tidylife_conflicts <- function() {
  envs <- grep("^package:", search(), value = TRUE)
  envs <- purrr::set_names(envs)
  objs <- invert(lapply(envs, ls_env))

  conflicts <- purrr::keep(objs, ~ length(.x) > 1)

  tidy_names <- paste0("package:", tidylife_packages())
  conflicts <- purrr::keep(conflicts, ~ any(.x %in% tidy_names))

  conflict_funs <- purrr::imap(conflicts, confirm_conflict)
  conflict_funs <- purrr::compact(conflict_funs)

  structure(conflict_funs, class = "tidylife_conflicts")
}

tidylife_conflict_message <- function(x) {
  if (length(x) == 0) {
    return("")
  }

  header <- cli::rule(
    left = crayon::bold("Conflicts"),
    right = "tidylife_conflicts"
  )

  pkgs <- x %>% purrr::map(~ gsub("^package:", "", .))
  others <- pkgs %>% purrr::map(`[`, -1)
  other_calls <- purrr::map2_chr(
    others, names(others),
    ~ paste0(crayon::blue(.x), "::", .y, "()", collapse = ", ")
  )

  winner <- pkgs %>% purrr::map_chr(1)
  funs <- format(paste0(crayon::blue(winner), "::", crayon::green(paste0(names(x), "()"))))
  bullets <- paste0(
    crayon::red(cli::symbol$cross), " ", funs,
    " masks ", other_calls,
    collapse = "\n"
  )

  paste0(header, "\n", bullets)
}

#' @export
print.tidylife_conflicts <- function(x, ..., startup = FALSE) {
  cli::cat_line(tidylife_conflict_message(x))
}

#' @importFrom magrittr %>%
confirm_conflict <- function(packages, name) {
  # Only look at functions
  objs <- packages %>%
    purrr::map(~ get(name, pos = .)) %>%
    purrr::keep(is.function)

  if (length(objs) <= 1) {
    return()
  }

  # Remove identical functions
  objs <- objs[!duplicated(objs)]
  packages <- packages[!duplicated(packages)]
  if (length(objs) == 1) {
    return()
  }

  packages
}

ls_env <- function(env) {
  x <- ls(pos = env)
  if (identical(env, "package:dplyr")) {
    x <- setdiff(x, c("intersect", "setdiff", "setequal", "union"))
  }
  if (identical(env, "package:lubridate")) {
    x <- setdiff(x, c("intersect", "setdiff", "union", "as.difftime"))
  }
  x
}
