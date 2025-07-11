# Check FNAIVE models

test_that("Functional naive", {
  fnaive <- aus_mortality |>
    dplyr::filter(State == "Victoria") |>
    model(fnaive = FNAIVE(Mortality))
  fc <- forecast(fnaive)
  expect_no_error(autoplot(fc))
  expect_identical(dim(fnaive), c(3L, 4L))
  expect_identical(dim(tidy(fnaive)), c(0L, 5L))
  expect_identical(
    colnames(glance(fnaive)),
    c("Sex", "State", "Code", ".model", "sigma2")
  )
  expect_no_error(residuals(fnaive, type = "innov"))
  expect_no_error(residuals(fnaive, type = "response"))
  expect_no_error(fitted(fnaive))
  expect_identical(NROW(generate(fnaive, times = 2)), 1212L)
  expect_identical(NROW(fc), 606L)
  expect_equal(
    fc |>
      dplyr::filter(Sex == "female", Age == 0, Year == 2021) |>
      dplyr::pull(.mean),
    0.002523116,
    tolerance = 1e-7
  )
  expect_identical(
    forecast(fnaive, bootstrap = TRUE, times = 7L) |>
      head(1) |>
      dplyr::pull(Mortality) |>
      unlist() |>
      length(),
    7L
  )
  expect_identical(generate(fnaive, times = 3) |> dim(), c(1818L, 8L))
})
