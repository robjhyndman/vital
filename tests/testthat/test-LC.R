# Check Lee-Carter models

test_that("Lee Carter", {
  lc <- aus_mortality |>
    dplyr::filter(State == "Victoria") |>
    model(lc = LC(Mortality))
  fc <- forecast(lc)
  expect_equal(dim(lc), c(3L, 4L))
  expect_equal(NROW(tidy(lc)), 0L)
  expect_equal(colnames(glance(lc)), c("Sex", "State", "Code", ".model", "sigma2"))
  expect_equal(mean(augment(lc)$.resid), 0.02064662, tolerance = 1e-7)
  expect_no_error(residuals(lc, type = "innov"))
  expect_no_error(residuals(lc, type = "response"))
  expect_no_error(fitted(lc))
  expect_equal(NROW(generate(lc, times = 2)), 1212L)
  expect_equal(NROW(fc), 606)
  expect_equal(fc |>
    dplyr::filter(Sex == "female", State == "Victoria", Age == 0, Year == 2004) |>
      dplyr::pull(.mean), 0.4554334, tolerance = 1e-7)
  expect_equal(forecast(lc, bootstrap = TRUE, times = 7) |>
                 head(1) |>
                 dplyr::pull(Mortality) |>
                 unlist() |>
                 length(), 7)
})

