# Check Lee-Carter models

test_that("Lee Carter", {
  lc <- aus_mortality |>
    dplyr::filter(State == "Victoria") |>
    model(lc = LC(Mortality))
  fc <- forecast(lc)
  expect_no_error(autoplot(fc))
  expect_equal(dim(lc), c(3L, 4L))
  expect_equal(NROW(tidy(lc)), 0L)
  expect_equal(colnames(glance(lc)), c("Sex", "State", "Code", ".model", "sigma2"))
  expect_equal(mean(augment(lc)$.resid), 0.00349808, tolerance = 1e-7)
  expect_no_error(residuals(lc, type = "innov"))
  expect_no_error(residuals(lc, type = "response"))
  expect_no_error(fitted(lc))
  expect_equal(NROW(generate(lc, times = 2)), 1212L)
  expect_equal(NROW(fc), 606)
  expect_equal(fc |>
    dplyr::filter(Sex == "female", State == "Victoria", Age == 0, Year == 2004) |>
      dplyr::pull(.mean), 0.002104314, tolerance = 1e-7)
  expect_equal(forecast(lc, bootstrap = TRUE, times = 7) |>
                 head(1) |>
                 dplyr::pull(Mortality) |>
                 unlist() |>
                 length(), 7)
  expect_equal(colnames(time_components(lc)), c("Sex", "State", "Code", "Year", "kt"))
  expect_equal(colnames(age_components(lc)), c("Sex", "State", "Code", "Age", "ax", "bx"))

  # Compare against demography
  library(demography)
  library(dplyr)
  lc1 <- demography::lca(fr.mort, series = 'female')
  lc2 <- as_vital(fr.mort) |>
    filter(Sex == "female") |>
    collapse_ages(max_age = 100) |>
    model(LC(Mortality))
  expect_true(sum(abs(lc1$kt- time_components(lc2)$kt)) < 0.0018)
  expect_true(sum(abs(lc1$ax - age_components(lc2)$ax)) < 1e-10)
  expect_true(sum(abs(lc1$bx - age_components(lc2)$bx)) < 1e-10)
  fc1 <- forecast(lc1)
  fc2 <- forecast(lc2)
  fc1$rate$female[1:5,1]
  fc2 |> filter(Year == 2007, Sex == "female", Age <= 4) |> pull(.mean)
})


