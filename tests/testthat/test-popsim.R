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

test_that("generate_population uses the vital variable names", {
  pop2 <- pop |>
    dplyr::rename(year = Year, age = Age, sex = Sex, Exposure = Population)
  mort2 <- pop2 |> model(m = FMEAN(Mortality))
  fert2 <- norway_fertility |>
    dplyr::filter(Year > 2015) |>
    dplyr::rename(year = Year, age = Age) |>
    model(m = FMEAN(Fertility))
  births2 <- norway_births |>
    dplyr::filter(Year > 2015) |>
    dplyr::rename(year = Year, sex = Sex)
  mig2 <- net_migration(pop2, births2) |>
    make_sd(NetMigration, key = sex) |>
    model(m = FMEAN(NetMigration))
  set.seed(1)
  out <- generate_population(
    pop2,
    mortality_model = mort2,
    fertility_model = fert2,
    migration_model = mig2,
    h = 2,
    n_reps = 3
  ) |>
    suppressWarnings()
  expect_identical(tsibble::index_var(out), "year")
  expect_identical(
    vital_vars(out),
    c(age = "age", sex = "sex", population = "Exposure")
  )
})

test_that("generate_population requires one model per mable", {
  fert2 <- norway_fertility |>
    dplyr::filter(Year > 2015) |>
    model(m = FMEAN(Fertility), n = FNAIVE(Fertility))
  expect_error(
    generate_population(pop, fertility_model = fert2),
    "fertility_model must contain only one model"
  )
  mig2 <- mig |> dplyr::mutate(n = m)
  expect_error(
    generate_population(pop, migration_model = mig2),
    "migration_model must contain only one model"
  )
})

test_that("generate_population gives no missing populations", {
  set.seed(1)
  expect_no_warning(
    out <- generate_population(
      pop,
      mortality_model = mort,
      fertility_model = fert,
      migration_model = mig,
      h = 3,
      n_reps = 5
    )
  )
  expect_false(anyNA(out$Population))
})
