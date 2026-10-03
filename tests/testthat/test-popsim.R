test_that("generate_population undoes coherent migration models", {
  pop <- norway_mortality |>
    dplyr::filter(Sex != "Total", Year > 2015)
  mort <- pop |> model(m = FMEAN(Mortality))
  fert <- norway_fertility |>
    dplyr::filter(Year > 2015) |>
    model(m = FMEAN(Fertility))
  mig <- net_migration(pop, norway_births |> dplyr::filter(Year > 2015)) |>
    make_sd(NetMigration) |>
    model(m = FMEAN(NetMigration))
  called <- FALSE
  local_mocked_bindings(undo_sd = function(.data, ...) {
    called <<- TRUE
    .data
  })
  generate_population(
    pop,
    mortality_model = mort,
    fertility_model = fert,
    migration_model = mig,
    h = 2,
    n_reps = 3
  ) |>
    suppressWarnings()
  expect_true(called)
})
