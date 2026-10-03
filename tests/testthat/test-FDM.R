# Check Functional data models

test_that("Functional data model", {
  if (requireNamespace("feasts", quietly = TRUE)) {
    library(feasts)
    hu <- norway_mortality |>
      filter(Year > 2000, Sex != "Total") |>
      model(hu = FDM(log(Mortality)))
    fc <- forecast(hu)
    expect_no_error(autoplot(fc))
    expect_identical(dim(hu), c(2L, 2L))
    expect_identical(NROW(tidy(hu)), 0L)
    expect_identical(
      colnames(glance(hu)),
      c("Sex", ".model", "nobs", "varprop")
    )
    expect_no_error(residuals(hu, type = "innov"))
    expect_no_error(residuals(hu, type = "response"))
    expect_no_error(fitted(hu))
    expect_identical(NROW(generate(hu, times = 2)), 888L)
    expect_identical(NROW(fc), 444L)
    expect_equal(
      fc |>
        dplyr::filter(Sex == "Female", Age == 0, Year == 2025) |>
        dplyr::pull(.mean),
      0.001985665,
      tolerance = 1e-5
    )
    expect_identical(
      forecast(hu, bootstrap = TRUE, times = 7L) |>
        head(1) |>
        dplyr::pull(Mortality) |>
        unlist() |>
        length(),
      7L
    )
    expect_identical(
      colnames(time_components(hu)),
      c("Sex", "Year", "mean", paste0("beta", 1:6))
    )
    expect_identical(
      colnames(age_components(hu)),
      c("Sex", "Age", "mean", paste0("phi", 1:6))
    )
  }
})

test_that("FDM checks the coherent time series model function", {
  expect_no_error(
    FDM(log(Mortality), coherent = TRUE, ts_model_fn = fable::ETS)
  )
  expect_error(
    FDM(log(Mortality), coherent = TRUE, coherent_ts_model_fn = fable::ETS),
    "coherent_ts_model_fn"
  )
})

test_that("fdpca compares the grid size with the number of ages", {
  set.seed(1)
  # More years than grid points, but few ages
  many_years <- matrix(rnorm(600 * 10), nrow = 600, ncol = 10)
  expect_no_error(fdpca(many_years, order = 2, ngrid = 500))
  many_ages <- matrix(rnorm(5 * 600), nrow = 5, ncol = 600)
  expect_error(fdpca(many_ages, order = 2, ngrid = 500), "Grid should be larger")
})

test_that("coherent FDM definitions pass the model by name to workers", {
  arfima <- FDM(log(Mortality), coherent = TRUE)
  expect_identical(arfima$extra$coherent_ts_model, "ARFIMA")
  expect_null(arfima$extra$coherent_ts_model_fn)
  arima <- FDM(
    log(Mortality),
    coherent = TRUE,
    coherent_ts_model_fn = fable::ARIMA
  )
  expect_identical(arima$extra$coherent_ts_model, "ARIMA")
  expect_error(
    FDM(log(Mortality), coherent = TRUE, coherent_ts_model_fn = fable::ETS),
    "must be fable::ARIMA or fable::ARFIMA"
  )
})
