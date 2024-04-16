#' Calculate net migration from a vital object
#'
#' @param deaths A vital object containing at least a time index, age,
#' population at 1 January, and death rates.
#' @param births A vital object containing at least a time index and number of births per time period.
#' It is assumed that
#' the population variable is the same as in the deaths object, and that the same keys other than age
#' are present in both objects.
#' @return A vital object containing population, estimated deaths (not actual deaths) and net migration,
#' using the formula Net Migration = Population - lag(Population cohort) - Deaths + Births.
#' Births are returned as Population at Age -1, and deaths are estimated from the life table
#' @references
#' Hyndman and Booth (2008) Stochastic population forecasts using functional data
#' models for mortality, fertility and migration. *International Journal of Forecasting*, 24(3), 323-342.
#' @examples
#' \dontrun{
#' # Files downloaded from the [Human Mortality Database](https://mortality.org)
#' deaths <- read_hmd_files(c("Population.txt", "Mx_1x1.txt"))
#' births <- read_hmd_file("Births.txt")
#' mig <- net_migration(deaths, births)
#' }
#' @export
net_migration <- function(deaths, births) {
  # Check if not vital objects
  stopifnot(inherits(deaths, "vital"))
  stopifnot(inherits(births, "vital"))
  # Get keys and index variables
  death_keys <- key_vars(deaths)
  death_idx <- index_var(deaths)
  birth_keys <- key_vars(births)
  birth_idx <- index_var(births)

  # Grab age and population variables
  agevar <- attributes(deaths)$agevar
  popvar <- attributes(deaths)$populationvar

  # Check indexes are the same
  if(!identical(death_idx, birth_idx)) {
    stop("Index variables are different in deaths and births objects")
  }
  # Check keys are the same
  birth_keys <- unique(c(birth_keys, agevar))
  if(!identical(sort(death_keys), sort(birth_keys))) {
    stop("Keys are different in deaths and births objects")
  }

  # Convert births to population at age -1 so they are 0 on 1 January following year
  births <- births |>
    mutate(Age = -1) |>
    dplyr::filter(Year >= min(deaths$Year) & Year <= max(deaths$Year))
  if("Births" %in% colnames(births)) {
    births[[popvar]] <- births[["Births"]]
  } else if(popvar != colnames(births)) {
    stop("Births or Population variable not found in births object")
  }
  births <- births |>
    select(all_of(unique(c(birth_idx, birth_keys, agevar, popvar))))

  # Compute Lx and Tx
  prevtx <- nextlx <- lt <- life_table(deaths) |> select(Lx, Tx)
  # Next age Lx
  nextlx[[agevar]] <- nextlx[[agevar]] - 1
  nextlx$Lxplus1 <- nextlx$Lx
  nextlx$Tx <- nextlx$Lx <- NULL
  # Previous age Tx
  prevtx[[agevar]] <- prevtx[[agevar]] + 1
  prevtx$Txminus1 <- prevtx$Tx
  prevtx$Tx <- prevtx$Lx <- NULL

  attr_deaths <- attributes(deaths)
  deaths <- deaths |>
    dplyr::bind_rows(births) |>
    dplyr::left_join(lt) |>
    dplyr::left_join(nextlx) |>
    dplyr::left_join(prevtx) |>
    suppressMessages()

  deaths <- deaths |>
    mutate(
      Lx = if_else(Age == -1, 1, Lx),
      Lxplus1 = if_else(Age == max(Age), Tx, Lxplus1),
      Lx = if_else(Age == max(Age), Txminus1, Lx),
      Deaths = pmax(0, Population * (1 - Lxplus1/Lx)),
    ) |>
    dplyr::select(-Lx, -Lxplus1, -Tx, -Txminus1)

  nextpop <- deaths |> select(all_of(popvar))
  nextpop[[agevar]] <- nextpop[[agevar]] - 1
  nextpop[[death_idx]] <- nextpop[[death_idx]] - 1
  nextpop$nextpop <- nextpop[[popvar]]
  nextpop[[popvar]] <- NULL
  nextpop <- nextpop |>
    tsibble::group_by_key() |>
    dplyr::mutate(diff = tsibble::difference(nextpop)) |>
    dplyr::ungroup() |>
    dplyr::mutate(nextpop = if_else(Age == max(Age), diff, nextpop))
  nextpop$diff <- NULL

  mig <- deaths |> left_join(nextpop) |> suppressMessages()

  # Net migrants is difference between population and lagpop plus
  # average of deaths over this year and next
  mig$NetMigration <- mig$nextpop - mig[[popvar]] + mig[["Deaths"]]

  # Zap nextpop and nextdeaths
  mig$nextpop <- mig$nextdeaths <- NULL
  mig <- mig[!is.na(mig$NetMigration),]

  # Only return population, estimated (not actual) deaths, net migrants
  mig |>
    dplyr::select(dplyr::all_of(c(death_idx,death_keys,popvar, "Deaths", "NetMigration"))) |>
    as_vital(
      .index = death_idx,
      .keys = death_keys,
      .age = agevar,
      .deaths = "Deaths",
      .population = popvar,
      reorder = TRUE
  )
}

utils::globalVariables(c("Lxplus1","Population","Txminus1","population"))