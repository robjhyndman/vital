#' Coerce to a tsibble object
#'
#' @param x Objects to be coerced to a tsibble (`tbl_ts`).
#' @param validate `TRUE` suggests to verify that each key or each combination
#' of key variables leads to unique time indices (i.e. a valid tsibble). If you
#' are sure that it's a valid input, specify `FALSE` to skip the checks.
#' @param ... Other arguments passed on to individual methods.
#'
#' @return A tsibble object.
#' @rdname as-tsibble
#' @seealso [tsibble]
#'
#' @examples
#' # coerce demogdata object to tsibble ----
#' as_tsibble(demography::fr.mort)
#' @export
as_tsibble.demogdata <- function(x, ..., validate = TRUE) {
  rates_included <- ("rate" %in% names(x))
  pop_included <- ("pop" %in% names(x))
  # Avoid CRAN error check by declaring variables
  Year <- Age <- AgeGroup <- Exposure <- Group <- Rates <- Mortality <- Fertility <- NULL
  if (rates_included) {
    rates <- NULL
    for (i in seq_along(x$rate)) {
      tmp <- x$rate[[i]] |>
        tibble::as_tibble() |>
        dplyr::mutate(
          AgeGroup = rownames(x$rate[[i]]),
          Age = x$age
        ) |>
        tidyr::gather(key = "Year", value = "Rates", -AgeGroup, -Age) |>
        dplyr::mutate(
          Year = as.numeric(Year),
          Group = names(x$rate)[i]
        )
      rates <- rbind(rates, tmp)
    }
    if (x$type == "mortality") {
      rates <- dplyr::rename(rates, Mortality = Rates)
    } else if (x$type == "fertility") {
      rates <- dplyr::rename(rates, Fertility = Rates)
    } else if (x$type == "migration") {
      rates <- dplyr::rename(rates, NetMigration = Rates)
    } else {
      stop("Unknown type")
    }
  }
  if (pop_included) {
    pop <- NULL
    for (i in seq_along(x$pop)) {
      tmp <- x$pop[[i]] |>
        as_tibble() |>
        dplyr::mutate(
          AgeGroup = rownames(x$pop[[i]]),
          Age = x$age
        ) |>
        tidyr::gather(key = "Year", value = "Exposure", -AgeGroup, -Age) |>
        dplyr::mutate(
          Year = as.numeric(Year),
          Group = names(x$pop)[i]
        )
      pop <- rbind(pop, tmp)
    }
  }
  if (rates_included & pop_included) {
    output <- dplyr::full_join(rates, pop, by = c("Group", "Year", "AgeGroup", "Age"))
    if ("Mortality" %in% colnames(output) & "Exposure" %in% colnames(output)) {
      output <- output |> dplyr::mutate(Deaths = Exposure * Mortality)
    } else if ("Fertility" %in% colnames(output) & "Exposure" %in% colnames(output)) {
      output <- output |> dplyr::mutate(Births = Exposure * Fertility / 1000)
    }
  }
  output <- output |>
    dplyr::select(Year, AgeGroup, Age, Group, dplyr::everything()) |>
    dplyr::mutate(
      Age = as.integer(Age),
      Year = as.integer(Year)
    ) |>
    tsibble::as_tsibble(index = Year, key = c(AgeGroup, Age, Group), validate = validate) |>
    dplyr::arrange(Group, Year, Age)
  return(output)
}
