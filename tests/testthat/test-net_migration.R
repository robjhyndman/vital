test_that("net_migration accepts births as a population variable", {
  pop <- norway_mortality |>
    dplyr::filter(Sex != "Total", Year > 2015)
  births <- norway_births |>
    dplyr::filter(Year > 2015)
  expected <- net_migration(pop, births)
  births_as_pop <- births |>
    dplyr::rename(Population = Births) |>
    as_vital(.population = "Population", .births = NULL)
  expect_equal(
    net_migration(pop, births_as_pop)$NetMigration,
    expected$NetMigration
  )
  expect_error(
    net_migration(pop, births |> as_vital(.births = NULL)),
    "Births or Population variable not found"
  )
})

test_that("net_migration indexes cohorts by age at the end of the year", {
  pop <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year > 2015) |>
    collapse_ages(max_age = 95)
  births <- norway_births |>
    dplyr::filter(Sex == "Female", Year > 2015)
  mig <- net_migration(pop, births)
  expect_equal(range(mig$Age), c(0L, 95L))
  P <- function(age, year) pop$Population[pop$Age == age & pop$Year == year]
  rx <- life_table(pop) |> dplyr::filter(Year == 2016)
  rx <- rx$rx[match(c(0, 50, 95), rx$Age)]
  nm <- function(age) mig$NetMigration[mig$Age == age & mig$Year == 2016]
  # Births become age 0
  B <- births$Births[births$Year == 2016]
  expect_equal(nm(0), P(0, 2017) - B * rx[1])
  # Interior ages
  expect_equal(nm(50), P(50, 2017) - P(49, 2016) * rx[2])
  # Open age group combines the two oldest ages
  expect_equal(nm(95), P(95, 2017) - (P(94, 2016) + P(95, 2016)) * rx[3])
})
