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
#' net_migration(norway_mortality, norway_births)
#' \dontrun{
#' # Files downloaded from the [Human Mortality Database](https://mortality.org)
#' deaths <- read_hmd_files(c("Population.txt", "Mx_1x1.txt"))
#' births <- read_hmd_file("Births.txt")
#' mig <- net_migration(deaths, births)
#' }
#' @export
net_migration <- function(deaths, births) {
  # Stop if not vital objects
  stopifnot(inherits(deaths, "vital"))
  stopifnot(inherits(births, "vital"))
  # Get keys and index variables
  death_keys <- key_vars(deaths)
  death_idx <- index_var(deaths)
  birth_keys <- key_vars(births)
  birth_idx <- index_var(births)

  # Grab age and population variables
  dvvar <- vital_var_list(deaths)
  bvvar <- vital_var_list(births)
  deathsvar <- dvvar$deaths
  if(is.null(deathsvar)) {
    deathsvar <- "Deaths"
  }
  agevar <- dvvar$age
  popvar <- dvvar$population
  birthsvar <- bvvar$births
  bpopvar <- bvvar$population

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
  births[[agevar]] <- -1
  births <- births[births[[birth_idx]] >= min(deaths[[death_idx]]) &
                    births[[birth_idx]] <= max(deaths[[death_idx]]),]
  if(birthsvar %in% colnames(births)) {
    births[[popvar]] <- births[[birthsvar]]
  } else if(bpopvar != colnames(births)) {
    stop("Births or Population variable not found in births object")
  } else {
    births[[popvar]] <- births[[bpopvar]]
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

  deaths$Lx <- if_else(deaths[[agevar]] == -1, 1, deaths$Lx)
  deaths$Lxplus1 <- if_else(deaths[[agevar]] == max(deaths[[agevar]]),
                              deaths$Tx, deaths$Lxplus1)
  deaths$Lx <- if_else(deaths[[agevar]] == max(deaths[[agevar]]),
                      deaths$Txminus1, deaths$Lx)
  deaths[[deathsvar]] <- pmax(0, deaths[[popvar]] * (1 - deaths$Lxplus1/deaths$Lx))
  deaths$Lx <- deaths$Lxplus1 <- deaths$Tx <- deaths$Txminus1 <- NULL

  nextpop <- deaths |> select(all_of(popvar))
  nextpop[[agevar]] <- nextpop[[agevar]] - 1
  nextpop[[death_idx]] <- nextpop[[death_idx]] - 1
  nextpop$nextpop <- nextpop[[popvar]]
  nextpop[[popvar]] <- NULL
  nextpop <- nextpop |>
    tsibble::group_by_key() |>
    dplyr::mutate(diff = tsibble::difference(nextpop)) |>
    dplyr::ungroup()
  nextpop$nextpop <- if_else(nextpop[[agevar]] == max(nextpop[[agevar]]),
                             nextpop$diff, nextpop$nextpop)
  nextpop$diff <- NULL

  mig <- deaths |> left_join(nextpop) |> suppressMessages()

  # Net migrants is difference between population and lagpop plus
  # average of deaths over this year and next
  mig$NetMigration <- mig$nextpop - mig[[popvar]] + mig[[deathsvar]]

  # Zap nextpop and nextdeaths
  mig$nextpop <- mig$nextdeaths <- NULL
  mig <- mig[!is.na(mig$NetMigration),]

  # Only return population, estimated (not actual) deaths, net migrants
  mig |>
    dplyr::select(dplyr::all_of(c(death_idx,death_keys,popvar, deathsvar,
                                  "NetMigration"))) |>
    as_vital(
      index = death_idx,
      keys = death_keys,
      .age = agevar,
      .deaths = deathsvar,
      .population = popvar,
      reorder = TRUE
    )
}

utils::globalVariables(c("Population","population"))
