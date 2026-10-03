test_that("net_migration accepts births as a population variable", {
  pop <- norway_mortality |>
    dplyr::filter(Sex != "Total", Year > 2015)
  births <- norway_births |>
    dplyr::filter(Year > 2015)
  expected <- net_migration(pop, births)
  births_as_pop <- births |>
    dplyr::rename(Population = Births) |>
    as_vital(.sex = "Sex", .population = "Population")
  expect_equal(
    net_migration(pop, births_as_pop)$NetMigration,
    expected$NetMigration
  )
  expect_error(
    net_migration(pop, births |> as_vital(.sex = "Sex")),
    "Births or Population variable not found"
  )
})
