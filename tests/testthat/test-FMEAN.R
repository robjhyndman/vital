# Check FMEAN models

test_that("Functional mean", {
  fm <- norway_mortality |>
    filter(Year > 2000, Sex != "Total") |>
    model(fm = FMEAN(Mortality))
  fc <- forecast(fm)
  expect_no_error(autoplot(fc))
  expect_identical(dim(fm), c(2L, 2L))
  expect_identical(dim(tidy(fm)), c(222L, 8L))
  expect_identical(
    colnames(glance(fm)),
    c("Sex", ".model", "sigma2")
  )
  expect_equal(mean(augment(fm)$.resid, na.rm = TRUE), 0)
  expect_no_error(residuals(fm, type = "innov"))
  expect_no_error(residuals(fm, type = "response"))
  expect_no_error(fitted(fm))
  expect_identical(NROW(generate(fm, times = 2)), 888L)
  expect_identical(NROW(fc), 444L)
  expect_equal(
    fc |>
      dplyr::filter(Sex == "Female", Age == 0, Year == 2024) |>
      dplyr::pull(.mean),
    0.002341783,
    tolerance = 1e-7
  )
  expect_identical(
    forecast(fm, bootstrap = TRUE, times = 7L) |>
      head(1) |>
      dplyr::pull(Mortality) |>
      unlist() |>
      length(),
    7L
  )
})
