library(testthat)
library(vital)
library(dplyr)

nor <- norway_mortality |>
  filter(Sex == "Male", Age > 50, Year > 2000) |>
  collapse_ages()

mod <- nor |>
  model(
    gapc = GAPC(
      Mortality,
      link = "log",
      staticAgeFun = FALSE,
      periodAgeFun = c("1", function(x, ages) x - mean(ages))
    ),
    lc2 = LC2(Mortality),
    cbd = CBD(Mortality),
    rh = RH(Mortality),
    apc = APC(Mortality),
    m7 = M7(Mortality),
    plat = PLAT(Mortality)
  )

test_that("Fit GAPC models", {
  expect_s3_class(mod, "mdl_vtl_df")
  expect_equal(dim(mod), c(1L, 8L))
})

test_that("forecast GAPC models", {
  forecast <- mod |>
    forecast(h = 10, simulate = TRUE, times = 100)
  expect_equal(dim(forecast), c(3500L, 6L))
  expect_s3_class(forecast, "fbl_vtl_ts")
  expect_equal(sum(is.na(forecast$.mean)), 0)
})

test_that("generate from GAPC models", {
  generated <- mod |> generate(h = 10, times = 10)
  expect_equal(dim(generated), c(35000L, 6L))
  expect_s3_class(generated, "data.frame")
  expect_equal(sum(is.na(generated$.sim)), 0)
})

test_that("glance at GAPC models", {
  gl <- glance(mod)
  expect_s3_class(gl, "tbl_df")
  expect_equal(dim(gl), c(7L, 6L))
  expect_equal(
    colnames(gl),
    c("Sex", ".model", "loglik", "deviance", "nobs", "npar")
  )
  expect_true(all(gl$nobs == 1150L))
})

test_that("report a GAPC model", {
  expect_output(report(mod |> select(apc)))
})

test_that("time_components from GAPC model", {
  tc <- mod |> select(apc) |> time_components()
  expect_equal(colnames(tc), c("Year", "kt"))
  expect_equal(dim(tc), c(23L, 2L))
  expect_s3_class(tc, "tbl_ts")
})

test_that("age_components from GAPC model", {
  age_comp <- mod |> select(apc) |> age_components()
  expect_equal(dim(age_comp), c(50L, 4L))
  expect_equal(colnames(age_comp), c("Age", "ax", "b0x", "b1x"))
  expect_s3_class(age_comp, "tbl_df")
})

test_that("cohort_components from GAPC model", {
  cohort_comp <- mod |> select(apc) |> cohort_components()
  expect_equal(dim(cohort_comp), c(72L, 2L))
  expect_equal(colnames(cohort_comp), c("Birth_Year", "gc"))
  expect_s3_class(cohort_comp, "tbl_ts")
})

test_that("autoplot of a GAPC mable returns a plot, not infinite recursion", {
  fit <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year > 2000, Age < 90) |>
    model(apc = APC(Mortality))
  expect_s3_class(autoplot(fit), "patchwork")
})

test_that("GAPC forecasts work with any index and age names", {
  nor <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year > 2000, Age < 90)
  nor2 <- nor |> dplyr::rename(year = Year, age = Age)
  fc <- nor |>
    model(apc = APC(Mortality)) |>
    forecast(h = 2) |>
    suppressWarnings()
  fc2 <- nor2 |>
    model(apc = APC(Mortality)) |>
    forecast(h = 2) |>
    suppressWarnings()
  expect_equal(fc2$.mean, fc$.mean)
  set.seed(1)
  sim2 <- nor2 |> model(apc = APC(Mortality)) |> generate(h = 2, times = 2)
  expect_false(anyNA(sim2$.sim))
})

test_that("logit link works with missing values", {
  nor <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year > 2000, Age >= 50, Age < 90) |>
    dplyr::mutate(Deaths = dplyr::if_else(Age == 60 & Year == 2010, NA, Deaths))
  fit <- nor |> model(cbd = CBD(Mortality, link = "logit"))
  expect_s3_class(fit$cbd[[1]]$fit, "GAPC")
})

test_that("autoplot shows age, period and cohort components of GAPC models", {
  fits <- norway_mortality |>
    dplyr::filter(Sex != "Total", Age > 50, Age < 95, Year > 1990) |>
    model(
      LC2 = LC2(Mortality),
      CBD = CBD(Mortality),
      APC = APC(Mortality),
      RH = RH(Mortality),
      M7 = M7(Mortality),
      PLAT = PLAT(Mortality)
    ) |>
    suppressWarnings()
  # Number of panels (two rows), blank panels for constant age functions,
  # and whether the legend has its own panel (rather than to the right)
  expected <- list(
    LC2 = c(4, 0, 1),
    CBD = c(4, 0, 1),
    APC = c(6, 2, 1),
    RH = c(6, 1, 1),
    M7 = c(8, 1, 1),
    PLAT = c(8, 2, 1)
  )
  for (m in names(expected)) {
    p <- autoplot(dplyr::select(fits, Sex, dplyr::all_of(m)))
    panels <- c(p$patches$plots, list(p))
    expect_length(panels, expected[[m]][1])
    expect_identical(
      sum(vapply(panels, inherits, logical(1), "spacer")),
      as.integer(expected[[m]][2])
    )
    expect_identical(
      sum(vapply(panels, inherits, logical(1), "guide_area")),
      as.integer(expected[[m]][3])
    )
    grDevices::pdf(NULL)
    expect_no_error(print(p))
    grDevices::dev.off()
  }
})

test_that("GAPC forecasts one step ahead", {
  fit <- norway_mortality |>
    dplyr::filter(Sex == "Female", Age > 60, Age < 90, Year > 2000) |>
    model(LC2(Mortality))
  fc1 <- suppressWarnings(forecast(fit, h = 1))
  fc2 <- suppressWarnings(forecast(fit, h = 2))
  expect_identical(NROW(fc1), 29L)
  expect_equal(fc1$.mean, fc2$.mean[fc2$Year == min(fc2$Year)])
})

test_that("augment, fitted and residuals work for GAPC models", {
  aug <- augment(mod |> select(lc2, apc))
  expect_equal(NROW(aug), 2L * NROW(nor))
  expect_equal(aug$.resid, aug$.response - aug$.fitted)
  expect_false(anyNA(aug$.fitted))
  expect_equal(NROW(fitted(mod |> select(cbd))), NROW(nor))
  expect_equal(NROW(residuals(mod |> select(cbd))), NROW(nor))
})

test_that("GAPC models do not silently ignore bootstrap", {
  expect_error(generate(mod |> select(lc2), h = 2, bootstrap = TRUE), "not available")
  expect_error(forecast(mod |> select(lc2), h = 2, bootstrap = TRUE), "not available")
})

test_that("tidy() returns GAPC coefficients in long form", {
  td <- tidy(mod |> select(apc))
  expect_identical(sort(unique(td$term)), c("ax", "b0x", "b1x", "gc", "kt"))
  expect_equal(td$estimate[td$term == "gc"], cohort_components(mod |> select(apc))$gc)
})

test_that("APC and CBD models fit and forecast", {
  x <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year > 1990, Age >= 55, Age < 90)
  fit <- x |> model(apc = APC(Mortality), cbd = CBD(Mortality))
  expect_s3_class(fit, "mdl_vtl_df")
  fc <- forecast(fit, h = 2, simulate = TRUE, times = 20)
  expect_identical(NROW(fc), 2L * 2L * 35L)
  expect_true(all(fc$.mean > 0))
})

test_that("GAPC models zero-weight deaths with no exposure without a warning", {
  x <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year > 2000, Age >= 60)
  expect_true(any(x$Population == 0 & x$Deaths > 0))
  expect_no_warning(fit <- model(x, m = LC2(Mortality)))
  expect_s3_class(fit, "mdl_vtl_df")
})

test_that("GAPC models require annual data", {
  nor5 <- norway_mortality |>
    tibble::as_tibble() |>
    dplyr::filter(Year %% 5 == 0, Year > 1950, Sex == "Female", Age >= 50, Age < 90) |>
    as_vital(
      index = Year, key = c(Age, Sex),
      .age = "Age", .sex = "Sex", .deaths = "Deaths", .population = "Population"
    )
  expect_error(
    model(nor5, lc2 = LC2(Mortality), .safely = FALSE),
    "require annual data .* interval of 5Y"
  )
  expect_error(
    model(nor5, apc = APC(Mortality), .safely = FALSE),
    "require annual data"
  )
})

test_that("GAPC point forecasts give a message rather than warnings", {
  # Show the once-per-session message every time
  rlang::local_options(rlib_message_verbosity = "verbose")
  fit <- norway_mortality |>
    dplyr::filter(Sex != "Total", Year > 1990, Age >= 55, Age < 90) |>
    model(cbd = CBD(Mortality))
  expect_no_warning(
    expect_message(fc <- forecast(fit, h = 2), "point forecasts only")
  )
  expect_true(all(fc$.mean > 0))
})
