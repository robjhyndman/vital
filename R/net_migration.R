#' Calculate net migration from a vital object
#'
#' @param deaths A vital object containing at least the columns Year, Population and Deaths
#' @param births A vital object containing at least the columns Year and Births
#' @return A vital object containing the same columns as deaths plus Net Migration computed
#' using the formula Net Migration = Population - lag(Population) - Deaths + Births
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
  # Grab age, deaths and population variables
  agevar <- attributes(deaths)$agevar
  deathsvar <- attributes(deaths)$deathsvar
  popvar <- attributes(births)$populationvar
  # Check indexes are the same
  if(!identical(death_idx, birth_idx)) {
    stop("Index variables are different in deaths and births objects")
  }
  # Check keys are the same
  birth_keys <- unique(c(birth_keys, agevar))
  if(!identical(sort(death_keys), sort(birth_keys))) {
    stop("Keys are different in deaths and births objects")
  }
  # Check populations and deaths included
  if(!all(c(popvar, deathsvar) %in% colnames(deaths))) {
    stop("Population and Deaths variables not found in deaths object")
  }
  if(!all(c(popvar) %in% colnames(births))) {
    stop("Population variable not found in births object")
  }

  # What was population last year?
  pop_last_year <- deaths |>
    select(popvar) |>
    # Add births with age at birth = -1, so they are 0 at the first occurrence of 1 January
    bind_rows(births |> mutate(Age = -1) |> select(all_of(unique(c(birth_idx, birth_keys, agevar, popvar)))))
  # Age population by one year
  pop_last_year[[agevar]] <- pop_last_year[[agevar]] + 1
  pop_last_year[[death_idx]] <- pop_last_year[[death_idx]] + 1
  pop_last_year$lagpop <- pop_last_year[[popvar]]
  pop_last_year[[popvar]] <- NULL

  # Join deaths with last year's population
  mig <- deaths |>
    left_join(pop_last_year) |>
    suppressMessages()
  mig$NetMigration <- mig[[popvar]] - mig$lagpop - mig[[deathsvar]]
  mig$lagpop <- NULL
  mig <- mig[!is.na(mig$NetMigration),]
  return(mig)
}
