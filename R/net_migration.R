#' Calculate net migration from a vital object
#'
#' @param deaths A vital object containing at least a time index, age,
#' population at 1 January, and death rates.
#' @param births A vital object containing at least a time index and number of births per time period.
#' It is assumed that
#' the population variable is the same as in the deaths object, and that the same keys other than age
#' are present in both objects.
#' @return A vital object containing population, estimated deaths (not actual deaths) and net migration.
#' Net migration at age x in year t is for the cohort aged x at the end of year t (on 1 January
#' of year t+1), so it equals the population aged x on 1 January of year t+1, minus the cohort's
#' population on 1 January of year t (births during year t for age 0, and the two oldest ages
#' combined for the open age group), plus the cohort's deaths during year t. Deaths are
#' estimated from the survivorship ratios of the life table, as in `demography::netmigration()`.
#' @references
#' Hyndman and Booth (2008) Stochastic population forecasts using functional data
#' models for mortality, fertility and migration. *International Journal of Forecasting*, 24(3), 323-342.
#' @examples
#' net_migration(norway_mortality, norway_births)
#' \dontrun{
#' # Files downloaded from the [Human Mortality Database](https://mortality.org)
#' deaths <- read_hmd_files(c("Population.txt", "Mx_1x1.txt"))
#' births <- read_hmd_files("Births.txt")
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
  if (is.null(deathsvar)) {
    deathsvar <- "Deaths"
  }
  agevar <- dvvar$age
  popvar <- dvvar$population
  birthsvar <- bvvar$births
  bpopvar <- bvvar$population

  # Check indexes are the same
  if (!identical(death_idx, birth_idx)) {
    stop("Index variables are different in deaths and births objects")
  }
  # Check keys are the same
  birth_keys <- unique(c(birth_keys, agevar))
  if (!identical(sort(death_keys), sort(birth_keys))) {
    stop("Keys are different in deaths and births objects")
  }

  # Births during the year are the cohort aged 0 at the end of the year
  births[[agevar]] <- vctrs::vec_cast(0, deaths[[agevar]])
  births <- births[
    births[[birth_idx]] >= min(deaths[[death_idx]]) &
      births[[birth_idx]] <= max(deaths[[death_idx]]),
  ]
  if (!is.null(birthsvar) && birthsvar %in% colnames(births)) {
    births[[popvar]] <- births[[birthsvar]]
  } else if (!is.null(bpopvar) && bpopvar %in% colnames(births)) {
    births[[popvar]] <- births[[bpopvar]]
  } else {
    stop("Births or Population variable not found in births object")
  }
  births <- as_tibble(births)[c(death_idx, death_keys, popvar)]

  # Population at the start of the year of each cohort, by its age at the end
  # of the year, with the two oldest ages combined into the open age group
  pop <- as_tibble(deaths)[c(death_idx, death_keys, popvar)]
  start <- pop
  start[[agevar]] <- pmin(start[[agevar]] + 1L, max(start[[agevar]]))
  start <- dplyr::bind_rows(start, births) |>
    dplyr::summarise(
      start = sum(.data[[popvar]]),
      .by = all_of(c(death_idx, death_keys))
    )

  # Survivorship ratios rx (Lx/Lx-1, L0 for births, Tx/Tx-1 for the open age
  # group) give the deaths of each cohort during the year
  rx <- as_tibble(life_table(deaths))[c(death_idx, death_keys, "rx")]

  # Population at the end of the year
  nextpop <- pop
  nextpop[[death_idx]] <- nextpop[[death_idx]] - 1L
  names(nextpop)[names(nextpop) == popvar] <- "nextpop"

  by <- c(death_idx, death_keys)
  mig <- pop |>
    left_join(start, by = by) |>
    left_join(rx, by = by) |>
    left_join(nextpop, by = by)
  mig[[deathsvar]] <- pmax(0, mig$start * (1 - mig$rx))
  mig[[deathsvar]][is.na(mig[[deathsvar]])] <- 0
  mig$NetMigration <- mig$nextpop - mig$start + mig[[deathsvar]]
  mig <- mig[!is.na(mig$NetMigration), ]

  # Only return population, estimated (not actual) deaths, net migrants
  mig |>
    dplyr::select(dplyr::all_of(c(
      death_idx,
      death_keys,
      popvar,
      deathsvar,
      "NetMigration"
    ))) |>
    as_vital(
      index = death_idx,
      key = all_of(death_keys),
      .age = agevar,
      .deaths = deathsvar,
      .population = popvar,
      reorder = TRUE
    )
}
