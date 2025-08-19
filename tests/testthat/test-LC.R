# Check Lee-Carter models

test_that("Lee Carter", {
  lc <- norway_mortality |>
    filter(Year > 2000, Sex != "Total") |>
    model(
      fit = LC(log(Mortality)),
      actual = LC(log(Mortality), jump = "actual", adjust = "dxt")
    ) |>
    suppressWarnings()
  fc <- forecast(lc)

  expect_no_error(autoplot(fc))
  expect_identical(dim(lc), c(2L, 3L))
  expect_identical(NROW(tidy(lc)), 0L)
  expect_identical(dim(glance(lc)), c(4L, 5L))
  expect_no_error(residuals(lc, type = "innov"))
  expect_no_error(residuals(lc, type = "response"))
  expect_no_error(fitted(lc))
  expect_identical(NROW(generate(lc, times = 2)), 1776L)
  expect_identical(NROW(fc), 888L)
  expect_equal(
    dplyr::filter(
      fc,
      Sex == "Female",
      Age == 0,
      Year == 2024,
      .model == "actual"
    ) |>
      dplyr::pull(.mean),
    0.00175119,
    tolerance = 1e-5
  )
  expect_identical(
    forecast(lc, bootstrap = TRUE, times = 7L) |>
      head(1) |>
      dplyr::pull(Mortality) |>
      unlist() |>
      length(),
    7L
  )
  expect_identical(
    colnames(time_components(lc |> select(fit))),
    c("Sex", "Year", "kt")
  )
  expect_error(age_components(lc))

  # Compare against demography
  if (requireNamespace("demography", quietly = TRUE)) {
    lc1 <- demography::lca(demography::fr.mort, series = "female")
    # with jump = actual
    lc2 <- as_vital(demography::fr.mort) |>
      dplyr::filter(Sex == "female") |>
      collapse_ages(max_age = 100) |>
      model(LC(log(Mortality), jump = "actual"))
    expect_lt(sum(abs(lc1$kt - time_components(lc2)$kt)), 0.0018)
    expect_lt(sum(abs(lc1$ax - age_components(lc2)$ax)), 1e-10)
    expect_lt(sum(abs(lc1$bx - age_components(lc2)$bx)), 1e-10)
    fc1 <- forecast(lc1, jump = "actual", h = 10)
    fc2 <- forecast(lc2, point_forecast = list(.median = median), h = 10)
    expect_lt(
      sum(abs(
        fc1$rate$female[, 10] -
          fc2 |>
            dplyr::filter(Year == 2016, Sex == "female") |>
            dplyr::pull(.median)
      )),
      1e-7
    )
    # with jump = fit
    lc3 <- as_vital(demography::fr.mort) |>
      dplyr::filter(Sex == "female") |>
      collapse_ages(max_age = 100) |>
      model(LC(log(Mortality), jump = "fit"))
    expect_equal(time_components(lc3), time_components(lc2))
    expect_equal(age_components(lc2), age_components(lc3))
    fc1 <- forecast(lc1, jump = "fit", h = 10)
    fc3 <- forecast(lc3, point_forecast = list(.median = median), h = 10)
    expect_lt(
      sum(abs(
        fc1$rate$female[, 10] -
          fc3 |>
            dplyr::filter(Year == 2016, Sex == "female") |>
            dplyr::pull(.median)
      )),
      1e-7
    )
    # with adjust = dxt
    lc1 <- demography::lca(
      demography::fr.mort,
      series = "female",
      adjust = "dxt"
    )
    lc2 <- as_vital(demography::fr.mort) |>
      dplyr::filter(Sex == "female") |>
      collapse_ages(max_age = 100) |>
      model(LC(log(Mortality), adjust = "dxt"))
    expect_equal(age_components(lc2), age_components(lc3))
    expect_false(identical(time_components(lc2), time_components(lc3)))
    expect_lt(sum(abs(lc1$kt - time_components(lc2)$kt)), 1e-10)
    # with adjust = e0
    lc1 <- demography::lca(
      demography::fr.mort,
      series = "female",
      adjust = "e0"
    )
    lc2 <- as_vital(demography::fr.mort) |>
      dplyr::filter(Sex == "female") |>
      collapse_ages(max_age = 100) |>
      model(LC(log(Mortality), adjust = "e0"))
    expect_equal(age_components(lc2), age_components(lc3))
    expect_false(identical(time_components(lc2), time_components(lc3)))
    expect_lt(sum(abs(lc1$kt - time_components(lc2)$kt)), 0.61)
    # with adjust = none
    lc1 <- demography::lca(
      demography::fr.mort,
      series = "female",
      adjust = "none"
    )
    lc2 <- as_vital(demography::fr.mort) |>
      dplyr::filter(Sex == "female") |>
      collapse_ages(max_age = 100) |>
      model(LC(log(Mortality), adjust = "none"))
    expect_equal(age_components(lc2), age_components(lc3))
    expect_false(identical(time_components(lc2), time_components(lc3)))
    expect_lt(sum(abs(lc1$kt - time_components(lc2)$kt)), 1e-10)
  }

  # Test LC on fertility
  expect_no_error(
    norway_fertility |>
      model(LC(log(Fertility))) |>
      autoplot()
  )
})
