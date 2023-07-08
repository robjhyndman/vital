#' Coerce to a life_tsibble object
#'
#' @param x Objects to be coerced to a life_tsibble (`life_tbl_ts`).
#' @param sex_groups Logical variable indicating of the groups denote sexes
#' @param validate `TRUE` suggests to verify that each key or each combination
#' of key variables leads to unique time indices (i.e. a valid tsibble). If you
#' are sure that it's a valid input, specify `FALSE` to skip the checks.
#' @param ... Other arguments passed on to individual methods.
#'
#' @return A life_tsibble object.
#' @rdname as_life_tsibble
#' @seealso \code{\link[tsibble]{tsibble}()}
#'
#' @examples
#' # coerce demogdata object to life_tsibble
#' as_life_tsibble(demography::fr.mort)
#' @export
as_life_tsibble <- function(x, ...) {
  UseMethod("as_life_tsibble")
}

#' @export
as_life_tsibble.demogdata <- function(x, sex_groups = TRUE, ..., validate = TRUE) {
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
  class(output) <- c("life_tbl_ts", class(output))
  return(output)
}

#' @param age Character string specifying name of age variable (required)
#' @param sex Character string specifying name of sex variable (optional)
#' @param deaths Character string specifying name of deaths variable (optional)
#' @param births Character string specifying name of births variable (optional)
#' @param population Character string specifying name of population variable (optional)
#' @rdname as_life_tsibble
#' @export
as_life_tsibble.tbl_ts <- function(x,
  age, sex = NULL, deaths = NULL, births = NULL, population = NULL) {
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
  class(x) <- c("life_tbl_ts", class(x))
  return(x)
}

utils::globalVariables(c("Deaths","Births"))
