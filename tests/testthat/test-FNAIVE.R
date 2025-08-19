# Check FNAIVE models

test_that("Functional naive", {
  fnaive <- norway_mortality |>
    filter(Year > 2000, Sex != "Total") |>
    model(fnaive = FNAIVE(Mortality))
  fc <- forecast(fnaive)
  expect_no_error(autoplot(fc))
  expect_identical(dim(fnaive), c(2L, 2L))
  expect_identical(dim(tidy(fnaive)), c(0L, 3L))
  expect_identical(
    colnames(glance(fnaive)),
    c("Sex", ".model", "sigma2")
  )
  expect_no_error(residuals(fnaive, type = "innov"))
  expect_no_error(residuals(fnaive, type = "response"))
  expect_no_error(fitted(fnaive))
  expect_identical(NROW(generate(fnaive, times = 2)), 888L)
  expect_identical(NROW(fc), 444L)
  expect_equal(
    fc |>
      dplyr::filter(Sex == "Female", Age == 0, Year == 2024) |>
      dplyr::pull(.mean),
    0.001777,
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
  expect_identical(generate(fnaive, times = 3) |> dim(), c(1332L, 6L))
})
