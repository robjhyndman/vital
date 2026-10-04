# Age group labels as keys, as in vitals created from demogdata objects

nor_ag <- norway_mortality |>
  dplyr::filter(Sex == "Female", Year > 2015, Age < 90) |>
  dplyr::mutate(AgeGroup = as.character(Age)) |>
  as_vital(index = Year, key = c(AgeGroup, Age, Sex))
nor <- norway_mortality |>
  dplyr::filter(Sex == "Female", Year > 2015, Age < 90)

test_that("life tables ignore age group keys", {
  expect_equal(life_table(nor_ag)$ex, life_table(nor)$ex)
  expect_true("AgeGroup" %in% colnames(life_table(nor_ag)))
})

test_that("total fertility rates keep keys other than age", {
  tfr <- total_fertility_rate(nor_ag, Mortality)
  expect_identical(NROW(tfr), 8L)
  expect_identical(key_vars(tfr), "Sex")
  expect_equal(tfr$tfr, total_fertility_rate(nor, Mortality)$tfr)
})

test_that("interpolating allows age group keys", {
  fit <- model(nor_ag, FMEAN(Mortality))
  missing <- nor_ag
  missing$Mortality[3] <- NA
  interpolated <- interpolate(fit, missing)
  expect_false(anyNA(interpolated$Mortality))
  expect_true("AgeGroup" %in% key_vars(interpolated))
})

test_that("forecasts with new_data match forecasts with h", {
  nor2 <- norway_mortality |>
    dplyr::filter(Sex != "Total", Year > 2015, Age < 90)
  fit <- model(nor2, FMEAN(Mortality), LC(log(Mortality), adjust = "none"))
  nd <- nor2 |>
    dplyr::filter(Year == max(Year)) |>
    dplyr::mutate(Year = Year + 1L) |>
    as_vital(index = Year, key = c(Age, Sex))
  fc <- forecast(fit, new_data = nd)
  fc_h <- forecast(fit, h = 1)
  expect_identical(NROW(fc), 360L)
  expect_equal(fc$.mean, fc_h$.mean)
})

test_that("forecasts keep age group keys", {
  fc <- nor_ag |>
    model(FMEAN(Mortality)) |>
    forecast(h = 2)
  expect_true("AgeGroup" %in% key_vars(fc))
  expect_identical(NROW(fc), 180L)
})

test_that("FNAIVE forecasts and simulations allow age group keys", {
  fc <- nor_ag |>
    model(FNAIVE(Mortality)) |>
    forecast(h = 2)
  fc0 <- nor |>
    model(FNAIVE(Mortality)) |>
    forecast(h = 2)
  expect_equal(fc$.mean, fc0$.mean)
  sim <- nor_ag |>
    model(FNAIVE(Mortality)) |>
    generate(h = 2, times = 2)
  expect_false(anyNA(sim$.sim))
})
