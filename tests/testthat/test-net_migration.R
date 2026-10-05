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

test_that("net_migration checks that deaths and births match", {
  pop <- norway_mortality |> dplyr::filter(Sex != "Total", Year > 2015)
  births <- norway_births |> dplyr::filter(Year > 2015)
  births_t <- births |>
    tibble::as_tibble() |>
    dplyr::rename(Time = Year) |>
    as_vital(index = Time, key = Sex, .sex = "Sex", .births = "Births")
  expect_error(net_migration(pop, births_t), "Index variables are different")
  births_k <- births |>
    dplyr::mutate(Region = "A") |>
    as_vital(key = c(Sex, Region))
  expect_error(net_migration(pop, births_k), "Keys are different")
  expect_error(net_migration(tibble::as_tibble(pop), births), "is not TRUE")
})

test_that("net_migration names estimated deaths Deaths when there is no deaths variable", {
  pop <- norway_mortality |>
    dplyr::filter(Sex != "Total", Year > 2015) |>
    dplyr::select(-Deaths) |>
    as_vital(.deaths = NULL)
  births <- norway_births |> dplyr::filter(Year > 2015)
  mig <- net_migration(pop, births)
  expect_identical(vital_vars(mig)[["deaths"]], "Deaths")
  expect_true(all(mig$Deaths >= 0))
})
