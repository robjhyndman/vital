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
  ...,
  key = NULL,
  index,
  .age = NULL,
  .sex = NULL,
  .deaths = NULL,
  .births = NULL,
  .population = NULL,
  regular = TRUE,
  .drop = TRUE
) {
  tsibble(
    ...,
    key = !!enquo(key),
    index = !!enquo(index),
    regular = regular,
    .drop = .drop
  ) |>
    as_vital(
      .age = .age,
      .sex = .sex,
      .deaths = .deaths,
      .births = .births,
      .population = .population
    )
}

# This rebuilds a vital when index or key are given,
# otherwise it just reattaches the vital attributes.
# Vital variables not given in ... are kept from x.
#' @export
as_vital.vital <- function(x, index, key, ...) {
  vvar <- vital_var_list(x)
  dots <- list2(...)
  vital_args <- list(
    .age = vvar$age,
    .sex = vvar$sex,
    .deaths = vvar$deaths,
    .births = vvar$births,
    .population = vvar$population
  )
  dots <- c(dots, vital_args[setdiff(names(vital_args), names(dots))])
  if (missing(index) && missing(key)) {
    return(exec(as_vital.tbl_ts, x, !!!dots))
  }
  index <- if (missing(index)) sym(index_var(x)) else enquo(index)
  key <- if (missing(key)) key_vars(x) else enquo(key)
  out <- as_tibble(x) |>
    as_tsibble(index = !!index, key = !!key)
  exec(as_vital, out, !!!dots)
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
#' @param ... Other arguments passed to methods. For data frames, these are
#' passed on to [tsibble::as_tsibble()].
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
        tsibble::as_tibble() |>
        mutate(
          AgeGroup = rownames(x$rate[[i]]),
          Age = x$age
        ) |>
        tidyr::pivot_longer(
          -c(AgeGroup, Age),
          names_to = "Year",
          values_to = "Rates"
        ) |>
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
        tidyr::pivot_longer(
          -c(AgeGroup, Age),
          names_to = "Year",
          values_to = "Exposure"
        ) |>
        mutate(
          Year = as.numeric(Year),
          Group = names(x$pop)[i]
        )
      pop <- rbind(pop, tmp)
    }
  }
  if (rates_included && pop_included) {
    output <- dplyr::full_join(
      rates,
      pop,
      by = c("Group", "Year", "AgeGroup", "Age")
    )
    if ("Mortality" %in% colnames(output) && "Exposure" %in% colnames(output)) {
      output <- output |>
        mutate(
          Deaths = if_else(is.na(Mortality), 0, Exposure * Mortality),
          Mortality = if_else(
            is.na(Mortality) & Exposure > 0 & Deaths == 0,
            0,
            Mortality
          )
        )
    } else if (
      "Fertility" %in% colnames(output) && "Exposure" %in% colnames(output)
    ) {
      output <- output |>
        mutate(
          Births = if_else(is.na(Fertility), 0, Exposure * Fertility / 1000),
          Fertility = if_else(
            is.na(Fertility) & Exposure > 0 & Births == 0,
            0,
            Fertility
          )
        )
    }
  } else if (rates_included) {
    output <- rates
  } else if (pop_included) {
    output <- pop
  } else {
    stop("No rates or population found in demogdata object")
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
  as_vital(
    output,
    .age = "Age",
    .sex = sexvar,
    .deaths = deathsvar,
    .births = birthsvar,
    .population = popvar
  )
}

#' @param .age Character string with name of age variable
#' @param .sex Character string with name of sex variable
#' @param .deaths Character string with name of deaths variable
#' @param .births Character string with name of births variable
#' @param .population Character string with name of population variable
#' @param reorder Logical indicating if the rows should be sorted by the index,
#' the keys other than age, and then age. The default is `TRUE` for data frames
#' and `FALSE` for tsibbles.
#' @rdname as_vital
#' @export
as_vital.tbl_ts <- function(
  x,
  .age = NULL,
  .sex = NULL,
  .deaths = NULL,
  .births = NULL,
  .population = NULL,
  reorder = FALSE,
  ...
) {
  # Add attributes to x to identify the various variables,
  # dropping any that are not in x
  vvar <- list(
    age = .age,
    sex = .sex,
    deaths = .deaths,
    births = .births,
    population = .population
  )
  vvar <- vvar[vapply(vvar, function(v) isTRUE(v %in% colnames(x)), logical(1L))]
  attr(x, "vital") <- unlist(vvar)
  # Add additional class, keeping grouping classes first
  cls <- setdiff(class(x), c("grouped_vital", "vital"))
  if (is_grouped_ts(x)) {
    grouped <- grepl("^grouped", cls)
    class(x) <- c("grouped_vital", cls[grouped], "vital", cls[!grouped])
  } else {
    class(x) <- c("vital", cls)
  }
  # Check class of variables
  for (v in setdiff(names(vvar), "sex")) {
    if (!is.numeric(x[[vvar[[v]]]])) {
      stop(toupper(substring(v, 1, 1)), substring(v, 2), " variable must be numeric")
    }
  }
  if (!is.null(vvar$sex) && !is.factor(x[[vvar$sex]]) && !is.character(x[[vvar$sex]])) {
    stop("Sex variable must be character or factor")
  }
  # Sort variables
  if (reorder) {
    agevar <- vvar$age
    keys_noage <- non_age_keys(x)
    x <- select(x, all_of(c(index_var(x), agevar)), everything()) |>
      arrange(across(all_of(c(index_var(x), keys_noage, agevar))))
  }
  return(x)
}

#' @param index A variable to specify the time index variable.
#' @param key Variable(s) that uniquely determine time indices. NULL for empty key,
#' and [c()] for multiple variables. It works with tidy selector
#' (e.g. [tidyselect::starts_with()]).
#' @rdname as_vital
#' @examples
#' # create a vital with only age as a key
#' data.frame(
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
as_vital.data.frame <- function(
  x,
  key = NULL,
  index,
  .age = NULL,
  .sex = NULL,
  .deaths = NULL,
  .births = NULL,
  .population = NULL,
  reorder = TRUE,
  ...
) {
  as_tsibble(x, key = !!enquo(key), index = !!enquo(index), ...) |>
    as_vital(
      .age = .age,
      .sex = .sex,
      .deaths = .deaths,
      .births = .births,
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
  keys <- tsibble::key_vars(x)
  n_keys <- tsibble::n_keys(x)
  if (is_empty(tsibble::key(x))) {
    first
  } else {
    age_key <- vital_var_list(x)$age
    if (!is.null(age_key)) {
      keys_noage <- keys[!(keys %in% age_key)]
      if (length(keys_noage) > 1) {
        keys <- paste0(age_key, " x (", comma(keys_noage), ")")
      } else if (length(keys_noage) == 1L) {
        keys <- paste0(age_key, " x ", comma(keys_noage))
      } else {
        keys <- age_key
      }
      nages <- length(unique(x[[age_key]]))
      # Number of series, which may not all have the same ages
      nkeys_noage <- NROW(vctrs::vec_unique(tsibble::key_data(x)[non_age_keys(x)]))
      n_keys <- paste(big_mark(nages), "x", big_mark(nkeys_noage))
    } else {
      keys <- comma(keys)
      n_keys <- big_mark(n_keys)
    }
    key_sum <- c(Key = paste(keys, brackets(n_keys)))
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
