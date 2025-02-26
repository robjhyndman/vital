library(testthat)
library(vital)
library(dplyr)

aus <- aus_mortality %>%
  filter(State == "Victoria", Sex == "male", Age > 50)

test_that("Fit GAPC models", {
  mod <- aus |>
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
  expect_s3_class(mod, "mdl_vtl_df")
  expect_equal(dim(mod), c(1L, 10L))
})

test_that("forecast.GAPC works correctly", {
  forecast <- mod |>
    forecast(h = 10, simulate = TRUE, times = 100)
  expect_equal(dim(forecast), c(3500L, 8L))
  expect_s3_class(forecast, "fbl_vtl_ts")
  expect_equal(sum(is.na(forecast$.mean)), 0)
})

test_that("generate.GAPC works correctly", {
  generated <- mod |> generate(h = 10, times = 10)
  expect_equal(dim(generated), c(35000L, 8L))
  expect_s3_class(generated, "data.frame")
  expect_equal(sum(is.na(generated$.sim)), 0)
})

test_that("glance.GAPC works correctly", {
  gl <- glance(mod)
  expect_s3_class(gl, "tbl_df")
  expect_equal(dim(gl), c(7L, 8L))
  expect_equal(
    colnames(gl),
    c("Sex", "State", "Code", ".model", "loglik", "deviance", "nobs", "npar")
  )
  expect_true(all(gl$nobs == 5930L))
})

test_that("report.GAPC works correctly", {
  expect_output(report(mod |> select(apc)))
})

test_that("time_components.GAPC works correctly", {
  tc <- mod |> select(apc) |> time_components()
  expect_equal(colnames(tc), c("Year", "kt"))
  expect_equal(dim(tc), c(120L, 2L))
  expect_s3_class(tc, "tbl_ts")
})

test_that("age_components.GAPC works correctly", {
  age_comp <- mod |> select(apc) |> age_components()
  expect_equal(dim(age_comp), c(50L, 4L))
  expect_equal(colnames(age_comp), c("Age", "ax", "b0x", "b1x"))
  expect_s3_class(age_comp, "tbl_df")
})

test_that("cohort_components.GAPC works correctly", {
  cohort_comp <- mod |> select(apc) |> cohort_components()
  expect_equal(dim(cohort_comp), c(169L, 2L))
  expect_equal(colnames(cohort_comp), c("Birth_Year", "gc"))
  expect_s3_class(cohort_comp, "tbl_ts")
})
