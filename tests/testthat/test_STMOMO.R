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
