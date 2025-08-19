# Check FMEAN models

test_that("Functional mean", {
  fm <- norway_mortality |>
    model(fm = FMEAN(Mortality))
  fc <- forecast(fm)
  expect_no_error(autoplot(fc))
  expect_identical(dim(fm), c(3L, 4L))
  expect_identical(dim(tidy(fm)), c(303L, 10L))
  expect_identical(
    colnames(glance(fm)),
    c("Sex", "State", "Code", ".model", "sigma2")
  )
  expect_equal(mean(augment(fm)$.resid, na.rm = TRUE), 0)
  expect_no_error(residuals(fm, type = "innov"))
  expect_no_error(residuals(fm, type = "response"))
  expect_no_error(fitted(fm))
  expect_identical(NROW(generate(fm, times = 2)), 1212L)
  expect_identical(NROW(fc), 606L)
  expect_equal(
    fc |>
      dplyr::filter(Sex == "female", Age == 0, Year == 2021) |>
      dplyr::pull(.mean),
    0.02701234,
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
