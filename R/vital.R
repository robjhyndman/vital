#' Coerce to a vital object
#'
#' A vital object is a type of tsibble that contains vital statistics such as births, deaths, and population counts, and mortality and fertility rates. It is a tsibble with a special class attribute that allows for special methods to be used.
#'
#' @param x Objects to be coerced to a vital format.
#' @param ... Other arguments passed on to individual methods.
#'
#' @return A vital object.
#' @rdname as_vital
#' @seealso \code{\link[tsibble]{tsibble}()}
#'
#' @examples
#' # coerce demogdata object to vital
#' as_vital(demography::fr.mort)
#' @export
as_vital <- function(x, ...) {
  UseMethod("as_vital")
}

#' @param sex_groups Logical variable indicating of the groups denote sexes
#' @param validate `TRUE` suggests to verify that each key or each combination
#' of key variables leads to unique time indices (i.e. a valid tsibble). If you
#' are sure that it's a valid input, specify `FALSE` to skip the checks.
#' @rdname as_vital
#' @export
as_vital.demogdata <- function(x, sex_groups = TRUE, ..., validate = TRUE) {
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
      mutate(Rates = if_else(Rates==Inf, NA_real_, Rates))
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
        gather(key = "Year", value = "Exposure", -AgeGroup, -Age) |>
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
    as_tsibble(index = Year, key = c(AgeGroup, Age, Group), validate = validate) |>
    arrange(Group, Year, Age)
  attr(output, "agevar") <- "Age"
  if(sex_groups) {
    output <- output  |>
      rename(Sex = Group)
    attr(output, "sexvar") <- "Sex"
  }
  if("Deaths" %in% colnames(output)) {
    attr(output, "deathsvar") <- "Deaths"
  }
  if("Births" %in% colnames(output)) {
    attr(output, "birthsvar") <- "Births"
  }
  if("Exposure" %in% colnames(output)) {
    attr(output, "populationvar") <- "Exposure"
  } else if("Population" %in% colnames(output)) {
    attr(output, "populationvar") <- "Population"
  }
  # Add additional class
  class(output) <- c("vital", class(output))
  return(output)
}

#' @param age Character string specifying name of age variable (required)
#' @param sex Character string specifying name of sex variable (optional)
#' @param deaths Character string specifying name of deaths variable (optional)
#' @param births Character string specifying name of births variable (optional)
#' @param population Character string specifying name of population variable (optional)
#' @rdname as_vital
#' @export
as_vital.tbl_ts <- function(x,
  age, sex = NULL, deaths = NULL, births = NULL, population = NULL, ...) {
  # Add attributes to x to identify the various variables
  attr(x, "agevar") <- age
  if(!is.null(sex)) {
    attr(x, "sexvar") <- sex
  }
  if(!is.null(deaths)) {
    attr(x, "deathsvar") <- deaths
  }
  if(!is.null(births)) {
    attr(x, "birthsvar") <- births
  }
  if(!is.null(population)) {
    attr(x, "populationvar") <- population
  }
  # Add additional class
  class(x) <- c("vital", class(x))
  return(x)
}

utils::globalVariables(c("Deaths","Births"))

# Functions need for printing vital objects

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.vital <- function(x) {
  fnt_int <- format(tsibble::interval(x))
  dim_x <- dim(x)
  format_dim <- purrr::map_chr(dim_x, big_mark)
  dim_x <- paste(format_dim, collapse = " x ")
  first <- c(`A vital` = paste(dim_x, brackets(fnt_int)))
  if (is_empty(tsibble::key(x))) {
    first
  }
  else {
    n_keys <- big_mark(tsibble::n_keys(x))
    key_sum <- c(Key = paste(paste(tsibble::key_vars(x), collapse=", "), brackets(n_keys)))
    c(first, key_sum)
  }
}

big_mark <- function (x, ...) {
  mark <- if (identical(getOption("OutDec"), ","))
    "."
  else ","
  ret <- formatC(x, big.mark = mark, format = "d", ...)
  ret[is.na(x)] <- "??"
  ret
}

brackets <- function (x) {
  paste0("[", x, "]")
}
