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
  real_undo_sd <- undo_sd
  local_mocked_bindings(undo_sd = function(...) {
    called <<- TRUE
    real_undo_sd(...)
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
  expect_false(anyNA(no_deaths$Population))
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
  expect_type(out$Year, "integer")
  expect_type(out$Age, "integer")
})

test_that("generate_population requires single-year ages", {
  gappy <- pop |> dplyr::filter(Age %% 5 == 0)
  expect_error(
    generate_population(gappy, h = 2, n_reps = 3),
    "consecutive single-year ages"
  )
})

test_that("single_year_rx() matches the survivorship ratios of life_table()", {
  x <- norway_mortality |>
    dplyr::filter(Year %in% c(1950, 2020), Age < 100)
  x$Mortality[c(5, 400)] <- NA
  x$Mortality[x$Age == 99 & x$Year == 1950] <- 0
  expect_warning(lt <- life_table(x), "interpolated")
  rx <- vital:::single_year_rx(
    x$Mortality,
    x$Age,
    paste(x$Year, x$Sex),
    x$Sex
  )
  key <- function(d) paste(d$Year, d$Sex, d$Age)
  expect_equal(rx, lt$rx[match(key(x), key(lt))])
})

test_that("generate_population simulates models whose data end before the starting population", {
  fert_early <- norway_fertility |>
    dplyr::filter(Year > 2010, Year <= 2019) |>
    model(m = FMEAN(Fertility))
  set.seed(1)
  out <- generate_population(
    pop,
    mortality_model = mort,
    fertility_model = fert_early,
    h = 2,
    n_reps = 3
  )
  expect_identical(sort(unique(out$Year)), max(pop$Year) + 1:2)
  expect_true(all(out$Population[out$Age == 0] > 0))
})

test_that("generate_population requires models trained up to the starting population", {
  expect_error(
    generate_population(
      pop |> dplyr::filter(Year <= 2020),
      mortality_model = mort,
      h = 2,
      n_reps = 3
    ),
    "up to the year of the starting population"
  )
})

test_that("generate_population adds net migrants by age at the end of the year", {
  pop100 <- pop |> collapse_ages(max_age = 100)
  # Net migration of exactly x at age x
  mig_age <- pop100 |>
    dplyr::mutate(NetMigration = as.numeric(Age)) |>
    model(m = FMEAN(NetMigration))
  out <- generate_population(pop100, migration_model = mig_age, h = 1, n_reps = 1)
  P0 <- pop100 |> dplyr::filter(Year == max(Year), Sex == "Female")
  P1 <- out |> dplyr::filter(Sex == "Female")
  P <- function(d, a) d$Population[d$Age == a]
  expect_equal(P(P1, 0), 0)
  expect_equal(P(P1, 50), P(P0, 49) + 50)
  expect_equal(P(P1, 100), P(P0, 99) + P(P0, 100) + 100)
})

test_that("generate_population survival matches the life table", {
  pop100 <- pop |> collapse_ages(max_age = 100)
  mort_naive <- pop100 |> model(m = FNAIVE(Mortality))
  set.seed(2)
  out <- generate_population(
    pop100,
    mortality_model = mort_naive,
    h = 1,
    n_reps = 200
  )
  P0 <- pop100 |> dplyr::filter(Year == max(Year), Sex == "Female")
  P1 <- out |>
    tibble::as_tibble() |>
    dplyr::filter(Sex == "Female") |>
    dplyr::summarise(Population = mean(Population), .by = Age)
  P <- function(d, a) d$Population[d$Age == a]
  lt <- life_table(P0)
  L <- function(a) lt$Lx[lt$Age == a]
  expect_equal(P(P1, 86) / P(P0, 85), L(86) / L(85), tolerance = 0.001)
  expect_equal(
    P(P1, 100),
    (P(P0, 99) + P(P0, 100)) * lt$rx[lt$Age == 100],
    tolerance = 0.02
  )
})

test_that("generate_population requires models with the ages and sexes of the starting population", {
  start <- pop |> dplyr::filter(Year == max(Year))
  mort_young <- pop |>
    dplyr::filter(Age < 100) |>
    model(m = FMEAN(Mortality))
  expect_error(
    generate_population(start, mortality_model = mort_young, h = 1, n_reps = 2),
    "same values of `Age`.*Missing: 100, 101"
  )
  mort_female <- pop |>
    dplyr::filter(Sex == "Female") |>
    model(m = FMEAN(Mortality))
  expect_error(
    generate_population(start, mortality_model = mort_female, h = 1, n_reps = 2),
    "same values of `Sex`.*Missing: Male"
  )
  expect_error(
    generate_population(
      start |> dplyr::filter(Age < 50),
      migration_model = mig,
      h = 1,
      n_reps = 2
    ),
    "migration model .* `Age`.*Extra: 50, 51"
  )
})

test_that("generate_population rejects missing simulated rates", {
  local_mocked_bindings(generate = function(...) {
    out <- fabletools::generate(...)
    out$.sim[out$Age == 30] <- NA
    out
  })
  expect_error(
    generate_population(
      pop |> dplyr::filter(Year == max(Year)),
      mortality_model = mort,
      h = 1,
      n_reps = 2
    ),
    "mortality model have missing values at ages 30"
  )
})

test_that("generate_population undoes product-ratio mortality models", {
  set.seed(1)
  mort_pr <- pop |>
    make_pr(Mortality) |>
    model(m = FMEAN(log(Mortality)))
  start <- pop |> dplyr::filter(Year == max(Year))
  out <- generate_population(start, mortality_model = mort_pr, h = 2, n_reps = 3)
  expect_false(anyNA(out$Population))
  expect_setequal(unique(out$Sex), c("Female", "Male"))
  # With no births or migration, the population only declines
  later <- out |>
    tibble::as_tibble() |>
    dplyr::summarise(total = sum(Population), .by = c(Year, .rep))
  expect_true(all(later$total[later$Year == 2025] <= later$total[later$Year == 2024]))
})

test_that("generate_population checks its arguments", {
  start <- pop |> dplyr::filter(Year == max(Year))
  expect_error(
    generate_population(tibble::as_tibble(start)),
    "starting_population must be a vital object"
  )
  for (arg in c("mortality_model", "fertility_model", "migration_model")) {
    args <- list(start, "not a mable")
    names(args) <- c("starting_population", arg)
    expect_error(do.call(generate_population, args), paste(arg, "must be a mable object"))
  }
  mort2 <- mort |> dplyr::mutate(n = m)
  expect_error(
    generate_population(start, mortality_model = mort2),
    "mortality_model must contain only one model"
  )
  expect_error(generate_population(start, h = 0), "h must be a positive")
  expect_error(generate_population(start, n_reps = "10"), "n_reps must be a positive")
  expect_error(
    generate_population(dplyr::filter(start, Sex == "Female")),
    "exactly 2 sexes"
  )
})

test_that("generate_population identifies the female sex", {
  start <- pop |>
    dplyr::filter(Year == max(Year)) |>
    dplyr::mutate(Sex = dplyr::if_else(Sex == "Female", "Women", "Men"))
  set.seed(1)
  out <- generate_population(start, h = 1, n_reps = 2)
  expect_setequal(unique(out$Sex), c("Women", "Men"))
  # Without a recognisable label, the first sex is used, with a warning
  unlabelled <- start |>
    dplyr::mutate(Sex = dplyr::if_else(Sex == "Women", "A", "B"))
  expect_warning(
    generate_population(unlabelled, h = 1, n_reps = 2),
    "Setting female to A$"
  )
})
