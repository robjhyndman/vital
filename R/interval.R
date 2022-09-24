#' Scales for interval objects
#'
#' @name interval-scales
NULL

interval <- function(lower = double(), upper = double()) {
  lower <- vec_cast(lower, to = double())
  upper <- vec_cast(upper, to = double())
  new_interval(lower, upper)
}

new_interval <- function(lower = double(), upper = double()) {
  vec_assert(lower, ptype = double())
  vec_assert(upper, ptype = double())

  new_rcrd(list(lower = lower, upper = upper), class = "mortality_interval")
}

#' @export
format.mortality_interval <- function(x, ...) {
  lower <- vctrs::field(x, "lower")
  upper <- vctrs::field(x, "upper")
  out <- ifelse(upper == lower,
    lower,
    ifelse(upper == Inf,
      paste0(lower, "+"),
      paste0(lower, "-", upper)
    )
  )
  out
}


as.double.mortality_interval <- function(x, ..) {
  vctrs::field(x, "lower")
}

vec_math.mortality_interval <- function(.fn, .x, ...) {
  if (.fn %in% c("is.nan", "is.infinite")) {
    return(rep_len(FALSE, length(.x)))
  }
  if (.fn == "is.finite") {
    return(rep_len(TRUE, length(.x)))
  }

  out <- lapply(vctrs::field(.x, "lower"), get(.fn), ...)
  out
}

#' @export
vec_ptype_abbr.mortality_interval <- function(x, ...) "itvl"
#' @export
vec_ptype_full.mortality_interval <- function(x, ...) "interval"
#' @export
vec_ptype2.mortality_interval.mortality_interval <- function(x, y, ...) new_interval()
#' @export
vec_ptype2.mortality_interval.integer <- function(x, y, ...) new_interval()
#' @export
vec_ptype2.integer.mortality_interval <- function(x, y, ...) new_interval()

#' @export
vec_cast.mortality_interval.mortality_interval <- function(x, to, ...) x
#' @export
vec_cast.double.mortality_interval <- function(x, to, ...) vctrs::field(x, "lower")
#' @export
vec_cast.integer.mortality_interval <- function(x, to, ...) vctrs::field(x, "lower")
#' @export
vec_cast.mortality_interval.integer <- function(x, to, ...) interval(x, Inf)
#' @export
vec_cast.character.mortality_interval <- function(x, to, ...) format(x)
#' @export
vec_cast.factor.mortality_interval <- function(x, to, ...) {
  lvls <- x[!duplicated(format(x))]
  factor(format(x), levels = format(lvls)[order(field(lvl, "lower"))])
}

utils::globalVariables("lvl")
