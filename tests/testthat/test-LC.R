# Check Lee-Carter models

test_that("Lee Carter", {
  lc <- aus_mortality |>
    dplyr::filter(State == "Victoria") |>
    model(
      fit = LC(log(Mortality)),
      actual = LC(log(Mortality), jump = "actual", adjust = "dxt")
    )
  fc <- forecast(lc)

  expect_no_error(autoplot(fc))
  expect_equal(dim(lc), c(3L, 4L))
  expect_equal(NROW(tidy(lc)), 0L)
  expect_equal(dim(glance(lc)), c(6L, 6L))
  expect_no_error(residuals(lc, type = "innov"))
  expect_no_error(residuals(lc, type = "response"))
  expect_no_error(fitted(lc))
  expect_equal(NROW(generate(lc, times = 2)), 2400L)
  expect_equal(NROW(fc), 1200)
  expect_equal(
    dplyr::filter(fc,
        Sex == "female", Age == 0, Year == 2021,
        .model == "actual"
      ) |>
      dplyr::pull(.mean), 0.002446, tolerance = 1e-5)
  expect_equal(forecast(lc, bootstrap = TRUE, times = 7) |>
                 head(1) |>
                 dplyr::pull(Mortality) |>
                 unlist() |>
                 length(), 7)
  expect_equal(colnames(time_components(lc |> select(fit))), c("Sex", "Code", "Year", "kt"))
  expect_error(age_components(lc))

  # Compare against demography
  if(requireNamespace("demography", quietly = TRUE)) {
    lc1 <- demography::lca(demography::fr.mort, series = 'female')
    # with jump = actual
    lc2 <- as_vital(demography::fr.mort) |>
      dplyr::filter(Sex == "female") |>
      collapse_ages(max_age = 100) |>
      model(LC(log(Mortality), jump="actual"))
    expect_true(sum(abs(lc1$kt- time_components(lc2)$kt)) < 0.0018)
    expect_true(sum(abs(lc1$ax - age_components(lc2)$ax)) < 1e-10)
    expect_true(sum(abs(lc1$bx - age_components(lc2)$bx)) < 1e-10)
    fc1 <- forecast(lc1, jump="actual", h=10)
    fc2 <- forecast(lc2, point_forecast = list(.median=median), h=10)
    expect_true(
      sum(abs(
        fc1$rate$female[,10] -
        fc2 |> dplyr::filter(Year == 2016, Sex == "female") |> dplyr::pull(.median)
      )) < 1e-7
    )
    # with jump = fit
    lc3 <- as_vital(demography::fr.mort) |>
      dplyr::filter(Sex == "female") |>
      collapse_ages(max_age = 100) |>
      model(LC(log(Mortality), jump="fit"))
    expect_identical(time_components(lc3), time_components(lc2))
    expect_identical(age_components(lc2), age_components(lc3))
    fc1 <- forecast(lc1, jump="fit", h=10)
    fc3 <- forecast(lc3, point_forecast = list(.median=median), h=10)
    expect_true(
      sum(abs(
        fc1$rate$female[,10] -
          fc3 |> dplyr::filter(Year == 2016, Sex == "female") |> dplyr::pull(.median)
      )) < 1e-7
    )
    # with adjust = dxt
    lc1 <- demography::lca(demography::fr.mort, series = "female", adjust = "dxt")
    lc2 <- as_vital(demography::fr.mort) |>
      dplyr::filter(Sex == "female") |>
      collapse_ages(max_age = 100) |>
      model(LC(log(Mortality), adjust = "dxt"))
    expect_identical(age_components(lc2), age_components(lc3))
    expect_false(identical(time_components(lc2), time_components(lc3)))
    expect_true(sum(abs(lc1$kt- time_components(lc2)$kt)) < 1e-10)
    # with adjust = e0
    lc1 <- demography::lca(demography::fr.mort, series = "female", adjust = "e0")
    lc2 <- as_vital(demography::fr.mort) |>
      dplyr::filter(Sex == "female") |>
      collapse_ages(max_age = 100) |>
      model(LC(log(Mortality), adjust = "e0"))
    expect_identical(age_components(lc2), age_components(lc3))
    expect_false(identical(time_components(lc2), time_components(lc3)))
    expect_true(sum(abs(lc1$kt- time_components(lc2)$kt)) < 0.61)
    # with adjust = none
    lc1 <- demography::lca(demography::fr.mort, series = "female", adjust = "none")
    lc2 <- as_vital(demography::fr.mort) |>
      dplyr::filter(Sex == "female") |>
      collapse_ages(max_age = 100) |>
      model(LC(log(Mortality), adjust = "none"))
    expect_identical(age_components(lc2), age_components(lc3))
    expect_false(identical(time_components(lc2), time_components(lc3)))
    expect_true(sum(abs(lc1$kt- time_components(lc2)$kt)) < 1e-10)
  }

  # Test LC on fertility
  expect_no_error(aus_fertility |>
    model(LC(log(Fertility))) |>
    autoplot())
})


