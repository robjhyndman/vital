pop <- norway_mortality |>
  dplyr::filter(Sex != "Total", Year > 2015)
mort <- pop |> model(m = FMEAN(Mortality))
fert <- norway_fertility |>
  dplyr::filter(Year > 2015) |>
  model(m = FMEAN(Fertility))
mig <- net_migration(pop, norway_births |> dplyr::filter(Year > 2015)) |>
  make_sd(NetMigration) |>
  model(m = FMEAN(NetMigration))

test_that("generate_population undoes coherent migration models", {
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

test_that("generate_population accepts a female argument", {
  set.seed(1)
  out <- generate_population(
    pop,
    mortality_model = mort,
    fertility_model = fert,
    migration_model = mig,
    h = 2,
    n_reps = 3,
    female = "Female"
  ) |>
    suppressWarnings()
  expect_s3_class(out, "vital")
  expect_error(
    generate_population(pop, h = 2, n_reps = 3, female = "Women"),
    "female must be one of"
  )
})

test_that("generate_population works without mortality or migration models", {
  set.seed(1)
  no_deaths <- generate_population(
    pop,
    fertility_model = fert,
    migration_model = mig,
    h = 2,
    n_reps = 3
  ) |>
    suppressWarnings()
  expect_s3_class(no_deaths, "vital")
  no_migrants <- generate_population(
    pop,
    mortality_model = mort,
    fertility_model = fert,
    h = 2,
    n_reps = 3
  ) |>
    suppressWarnings()
  expect_s3_class(no_migrants, "vital")
})

test_that("generate_population works without a fertility model", {
  set.seed(1)
  no_births <- generate_population(
    pop,
    mortality_model = mort,
    migration_model = mig,
    h = 2,
    n_reps = 3
  ) |>
    suppressWarnings()
  expect_s3_class(no_births, "vital")
  # Only migrants can be aged 0
  first_year <- no_births |> dplyr::filter(Year == min(Year), Age == 0)
  expect_true(all(first_year$Population < 2000))
})
