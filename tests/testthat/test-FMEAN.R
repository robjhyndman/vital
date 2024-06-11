# Check FMEAN models

test_that("Functional mean", {
  fm <- aus_mortality |>
    dplyr::filter(State == "Victoria") |>
    model(fm = FMEAN(Mortality))
  fc <- forecast(fm)
  expect_no_error(autoplot(fc))
  expect_equal(dim(fm), c(3L, 4L))
  expect_equal(dim(tidy(fm)), c(303L,10L))
  expect_equal(colnames(glance(fm)), c("Sex", "State", "Code", ".model", "sigma2"))
  expect_equal(mean(augment(fm)$.resid, na.rm=TRUE), 0)
  expect_no_error(residuals(fm, type = "innov"))
  expect_no_error(residuals(fm, type = "response"))
  expect_no_error(fitted(fm))
  expect_equal(NROW(generate(fm, times = 2)), 1212L)
  expect_equal(NROW(fc), 606)
  expect_equal(fc |>
    dplyr::filter(Sex == "female", Age == 0, Year == 2021) |>
      dplyr::pull(.mean), 0.02701234, tolerance = 1e-7)
  expect_equal(forecast(fm, bootstrap = TRUE, times = 7) |>
                 head(1) |>
                 dplyr::pull(Mortality) |>
                 unlist() |>
                 length(), 7)
})

