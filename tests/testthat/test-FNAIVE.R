# Check FNAIVE models

test_that("Functional naive", {
  fnaive <- aus_mortality |>
    dplyr::filter(State == "Victoria") |>
    model(fnaive = FNAIVE(Mortality))
  fc <- forecast(fnaive)
  expect_no_error(autoplot(fc))
  expect_equal(dim(fnaive), c(3L, 4L))
  expect_equal(dim(tidy(fnaive)), c(0L, 5L))
  expect_equal(colnames(glance(fnaive)), c("Sex", "State", "Code", ".model", "sigma2"))
  expect_no_error(residuals(fnaive, type = "innov"))
  expect_no_error(residuals(fnaive, type = "response"))
  expect_no_error(fitted(fnaive))
  expect_equal(NROW(generate(fnaive, times = 2)), 1212L)
  expect_equal(NROW(fc), 606)
  expect_equal(fc |>
    dplyr::filter(Sex == "female", Age == 0, Year == 2021) |>
      dplyr::pull(.mean), 0.002523116, tolerance = 1e-7)
  expect_equal(forecast(fnaive, bootstrap = TRUE, times = 7) |>
                 head(1) |>
                 dplyr::pull(Mortality) |>
                 unlist() |>
                 length(), 7)
  expect_equal(generate(fnaive, times=3) |> dim(), c(1818L, 8L))
})

