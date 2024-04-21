#' Create a vital object
#'
#' A vital object is a type of tsibble that contains vital statistics such as
#' births, deaths, and population counts, and mortality and fertility rates.
#' It is a tsibble with a special class that allows for special methods to be used.
#' The object has an attribute that stores variables names needed for some functions,
#' including age, sex, births, deaths and population.
#' @param key Variable(s) that uniquely determine time indices. NULL for empty key,
#' and [c()] for multiple variables. It works with tidy selector
#' (e.g. [tidyselect::starts_with()])
#' @param ... A set of name-value pairs
#' @param index A variable to specify the time index variable.
#' @param .age Character string with name of age variable
#' @param .sex Character string with name of sex variable
#' @param .deaths Character string with name of deaths variable
#' @param .births Character string with name of births variable
#' @param .population Character string with name of population variable
#' @param regular	Regular time interval (`TRUE`) or irregular (`FALSE`). The interval
#' is determined by the greatest common divisor of index column, if `TRUE`.
#' @param .drop If `TRUE`, empty key groups are dropped.
#' @author Rob J Hyndman
#' @return A tsibble with class \code{vital}.
#' @examples
#' # create a vital with only age as a key
#' vital(
#'   year = rep(2010:2015, 100),
#'   age = rep(0:99, each = 6),
#'   mx = runif(600, 0, 1),
#'   index = year,
#'   key = age,
#'   .age = "age"
#' )
#' @seealso [tsibble::tsibble()]
#' @export
vital <- function(
    ..., key = NULL, index,
    .age = NULL, .sex = NULL, .deaths = NULL, .births = NULL, .population = NULL,
    regular = TRUE, .drop = TRUE) {
  tsibble(..., key = !!enquo(key), index = !!enquo(index), regular = regular, .drop = .drop) |>
    as_vital(
      .age = .age, .sex = .sex, .deaths = .deaths,
      .births = .births, .population = .population
    )
}

# This rebuilds a vital, usually just to reattach vital attributes
#' @export
as_vital.vital <- function(x, index, keys, ...) {
  as_tibble(x) |>
    as_tsibble(index= !!enquo(index), key = !!enquo(keys)) |>
    as_vital(...)
}

#' Coerce to a vital object
#'
#' A vital object is a type of tsibble that contains vital statistics such as
#' births, deaths, and population counts, and mortality and fertility rates.
#' It is a tsibble with a special class that allows for special methods to be used.
#' The object has an attribute that stores variables names needed for some
#' functions, including age, sex, births, deaths and population.
#'
#' @param x Object to be coerced to a vital format.
#' @param ... Other arguments passed on to [tsibble::as_tsibble()].
#'
#' @return A tsibble with class \code{vital}.
#' @author Rob J Hyndman
#' @rdname as_vital
#' @seealso [tsibble::tsibble()]
#'
#' @examplesIf requireNamespace("demography", quietly = TRUE)
#' # coerce demogdata object to vital
#' as_vital(demography::fr.mort)
#' @export
as_vital <- function(x, ...) {
  UseMethod("as_vital")
}

#' @param sex_groups Logical variable indicating if the groups denote sexes
#' @rdname as_vital
#' @export
as_vital.demogdata <- function(x, sex_groups = TRUE, ...) {
  rates_included <- ("rate" %in% names(x))
  pop_included <- ("pop" %in% names(x))
  # Avoid CRAN error check by declaring variables
  Year <- Age <- AgeGroup <- Exposure <- Group <- Rates <- Mortality <- Fertility <- NULL
  if (rates_included) {
    rates <- NULL
    for (i in seq_along(x$rate)) {
      tmp <- x$rate[[i]] |>
        tibble::as_tibble() |>
        mutate(
          AgeGroup = rownames(x$rate[[i]]),
          Age = x$age
        ) |>
        tidyr::gather(key = "Year", value = "Rates", -AgeGroup, -Age) |>
        mutate(
          Year = as.numeric(Year),
          Group = names(x$rate)[i]
        )
      rates <- rbind(rates, tmp)
    }
    # Assume Inf rates are due to 0/0
    rates <- rates |>
      mutate(Rates = if_else(Rates == Inf, NA_real_, Rates))
    if (x$type == "mortality") {
      rates <- rename(rates, Mortality = Rates)
    } else if (x$type == "fertility") {
      rates <- rename(rates, Fertility = Rates)
    } else if (x$type == "migration") {
      rates <- rename(rates, NetMigration = Rates)
    } else {
      stop("Unknown type")
    }
  }
  if (pop_included) {
    pop <- NULL
    for (i in seq_along(x$pop)) {
      tmp <- x$pop[[i]] |>
        as_tibble() |>
        mutate(
          AgeGroup = rownames(x$pop[[i]]),
          Age = x$age
        ) |>
        tidyr::gather(key = "Year", value = "Exposure", -AgeGroup, -Age) |>
        mutate(
          Year = as.numeric(Year),
          Group = names(x$pop)[i]
        )
      pop <- rbind(pop, tmp)
    }
  }
  if (rates_included & pop_included) {
    output <- dplyr::full_join(rates, pop, by = c("Group", "Year", "AgeGroup", "Age"))
    if ("Mortality" %in% colnames(output) & "Exposure" %in% colnames(output)) {
      output <- output |>
        mutate(
          Deaths = if_else(is.na(Mortality), 0, Exposure * Mortality),
          Mortality = if_else(is.na(Mortality) & Exposure > 0 & Deaths == 0, 0, Mortality)
        )
    } else if ("Fertility" %in% colnames(output) & "Exposure" %in% colnames(output)) {
      output <- output |>
        mutate(
          Births = if_else(is.na(Fertility), 0, Exposure * Fertility / 1000),
          Fertility = if_else(is.na(Fertility) & Exposure > 0 & Births == 0, 0, Fertility)
        )
    }
  }
  output <- output |>
    select(Year, AgeGroup, Age, Group, dplyr::everything()) |>
    mutate(
      Age = as.integer(Age),
      Year = as.integer(Year)
    ) |>
    as_tsibble(index = Year, key = c(AgeGroup, Age, Group), ...) |>
    arrange(Group, Year, Age)
  sexvar <- deathsvar <- birthsvar <- popvar <- NULL
  if (sex_groups) {
    output <- output |>
      rename(Sex = Group)
    sexvar <- "Sex"
  }
  if ("Deaths" %in% colnames(output)) {
    deathsvar <- "Deaths"
  }
  if ("Births" %in% colnames(output)) {
    birthsvar <- "Births"
  }
  if ("Exposure" %in% colnames(output)) {
    popvar <- "Exposure"
  } else if ("Population" %in% colnames(output)) {
    popvar <- "Population"
  }
  as_vital(output,
    .age = "Age", .sex = sexvar, .deaths = deathsvar,
    .births = birthsvar, .population = popvar
  )
}

#' @param .age Character string with name of age variable
#' @param .sex Character string with name of sex variable
#' @param .deaths Character string with name of deaths variable
#' @param .births Character string with name of births variable
#' @param .population Character string with name of population variable
#' @param reorder Logical indicating if the variables should be reordered.
#' @rdname as_vital
#' @export
as_vital.tbl_ts <- function(x,
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
  attr(x, "agevar") <- .age
  attr(x, "sexvar") <- .sex
  attr(x, "birthsvar") <- .births
  attr(x, "deathsvar") <- .deaths
  attr(x, "populationvar") <- .population
  # Add additional class
  class(x) <- c("vital", class(x))
  # Sort variables
  if (reorder) {
    agevar <- attributes(x)$agevar
    keys <- key_vars(x)
    agevars <- colnames(x)
    agevars <- agevars[grep("age", agevars, ignore.case=TRUE)]
    keys_noage <- keys[!(keys %in% c(agevar, agevars))]
    x <- select(x, all_of(c(index_var(x), attributes(x)$agevar)), everything()) |>
      arrange(across(all_of(c(index_var(x), keys_noage, agevar))))
  }
  return(x)
}

#' @param index A variable to specify the time index variable.
#' @param key Variable(s) that uniquely determine time indices. NULL for empty key,
#' and [c()] for multiple variables. It works with tidy selector
#' (e.g. [tidyselect::starts_with()]).
#' @param ... Other arguments passed to [tsibble::as_tsibble()]
#' @rdname as_vital
#' @examples
#' # create a vital with only age as a key
#' tibble::tibble(
#'   year = rep(2010:2015, 100),
#'   age = rep(0:99, each = 6),
#'   mx = runif(600, 0, 1)
#' ) |>
#'   as_vital(
#'     index = year,
#'     key = age,
#'     .age = "age"
#'   )
#' @export
as_vital.data.frame <- function(x, key = NULL, index,
                                .age = NULL, .sex = NULL, .deaths = NULL, .births = NULL, .population = NULL,
                                reorder = TRUE,
                                ...) {
  as_tsibble(x, key = !!enquo(key), index = !!enquo(index), ...) |>
    as_vital(
      .age = .age, .sex = .sex,
      .deaths = .deaths, .births = .births,
      .population = .population,
      reorder = reorder
    )
}


utils::globalVariables(c("Deaths", "Births"))

# Functions need for printing vital objects

#' @export
tbl_sum.vital <- function(x) {
  fnt_int <- format(tsibble::interval(x))
  dim_x <- dim(x)
  format_dim <- purrr::map_chr(dim_x, big_mark)
  dim_x <- paste(format_dim, collapse = " x ")
  first <- c(`A vital` = paste(dim_x, brackets(fnt_int)))
  if (is_empty(tsibble::key(x))) {
    first
  } else {
    n_keys <- big_mark(tsibble::n_keys(x))
    key_sum <- c(Key = paste(comma(tsibble::key_vars(x)), brackets(n_keys)))
    c(first, key_sum)
  }
}


#' @export
tbl_sum.grouped_vital <- function(x) {
  n_grps <- big_mark(length(dplyr::group_rows(x)))
  if (n_grps == 0) {
    n_grps <- "?"
  }
  grps <- dplyr::group_vars(x)
  idx2 <- rlang::quo_name(tsibble::index2(x))
  grp_var <- setdiff(grps, idx2)
  idx_suffix <- paste("@", idx2)
  res_grps <- NextMethod()
  res <- res_grps[utils::head(names(res_grps), -1L)] # rm "Groups"
  n_grps <- brackets(n_grps)
  if (is_empty(grp_var)) {
    c(res, "Groups" = paste(idx_suffix, n_grps))
  } else if (rlang::has_length(grps, length(grp_var))) {
    c(res, "Groups" = paste(comma(grp_var), n_grps))
  } else {
    c(res, "Groups" = paste(comma(grp_var), idx_suffix, n_grps))
  }
}

big_mark <- function(x, ...) {
  mark <- if (identical(getOption("OutDec"), ",")) {
    "."
  } else {
    ","
  }
  ret <- formatC(x, big.mark = mark, format = "d", ...)
  ret[is.na(x)] <- "??"
  ret
}

brackets <- function(x) {
  paste0("[", x, "]")
}

comma <- function(...) {
  paste(..., collapse = ", ")
}
