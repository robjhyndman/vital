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
    0.00172968,
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

  # Zero rates should be treated as missing, not as log rates of 0
  zeros <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year > 1980, Age == 2)
  expect_true(any(zeros$Mortality == 0))
  lc_zero <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year > 1980) |>
    model(lc = LC(log(Mortality)))
  expect_equal(
    age_components(lc_zero) |> dplyr::filter(Age == 2) |> dplyr::pull(ax),
    mean(log(zeros$Mortality[zeros$Mortality > 0]))
  )

  # Simulations should be on the scale of the response
  sim <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year > 1980, Age < 90) |>
    model(lc = LC(log(Mortality))) |>
    generate(h = 2, times = 3)
  expect_lt(median(sim$.sim), 0.1)

  # Test LC on fertility
  expect_no_error(
    norway_fertility |>
      model(LC(log(Fertility))) |>
      autoplot()
  )
})

test_that("LC and GAPC models require a complete age by time grid", {
  gappy <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year > 2010, Age < 90) |>
    dplyr::filter(!(Age == 50 & Year == 2015))
  expect_warning(
    model(gappy, lc = LC(log(Mortality))),
    "every combination of Age and Year"
  )
  expect_warning(
    model(gappy, apc = APC(Mortality)),
    "every combination of Age and Year"
  )
  filled <- model(
    tsibble::fill_gaps(gappy),
    lc = LC(log(Mortality)),
    apc = APC(Mortality)
  )
  expect_s3_class(filled$lc[[1]]$fit, "LC")
  expect_s3_class(filled$apc[[1]]$fit, "GAPC")
})

test_that("LC simulations use the actual jump-off when requested", {
  nor <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year > 1990)
  fit <- nor |>
    model(
      actual = LC(log(Mortality), jump_choice = "actual"),
      fit = LC(log(Mortality), jump_choice = "fit")
    ) |>
    expect_warning("Rates are zero or missing in the final year")
  set.seed(1)
  sim_actual <- generate(dplyr::select(fit, actual), h = 2, times = 2)
  set.seed(1)
  sim_fit <- generate(dplyr::select(fit, fit), h = 2, times = 2)
  innov <- fit$actual[[1]]$fit$fitted |>
    dplyr::filter(Year == max(Year))
  jump <- innov$.innov[match(sim_actual$Age, innov$Age)]
  # Ages with zero final rates use the fitted jump-off
  jump[is.na(jump)] <- 0
  expect_equal(log(sim_actual$.sim) - log(sim_fit$.sim), jump)
})

test_that("LC and FDM plots work with several non-age keys", {
  two_keys <- norway_mortality |>
    dplyr::filter(Year > 2000, Sex != "Total") |>
    dplyr::mutate(Region = "Norway") |>
    as_vital(index = Year, key = c(Age, Sex, Region))
  fit <- two_keys |>
    model(lc = LC(log(Mortality)), fdm = FDM(log(Mortality), order = 2))
  draw <- function(p) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    print(p)
  }
  expect_no_error(draw(autoplot(dplyr::select(fit, lc))))
  expect_no_error(draw(autoplot(dplyr::select(fit, fdm))))
  expect_no_error(draw(autoplot(dplyr::filter(fit, Sex == "Female") |> dplyr::select(lc))))
})

test_that("LC only adjusts to deaths when deaths and population are available", {
  fert <- norway_fertility |> dplyr::filter(Year > 2000)
  fit <- fert |> model(lc = LC(log(Fertility)))
  expect_identical(fit$lc[[1]]$fit$model$adjust, "none")
  expect_true(all(is.na(unlist(glance(fit)[c("base_deviance", "total_deviance")]))))
  expect_error(
    fert |> model(lc = LC(log(Fertility), adjust = "dt"), .safely = FALSE),
    "requires deaths and population"
  )
  mort <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year > 2000)
  expect_identical(
    model(mort, lc = LC(log(Mortality)))$lc[[1]]$fit$model$adjust,
    "dt"
  )
})

test_that("LC does not adjust product-ratios to deaths by default", {
  pr <- norway_mortality |>
    dplyr::filter(Year > 2000, Age < 90, Sex != "Total") |>
    make_pr(Mortality)
  expect_no_warning(fit <- pr |> model(lc = LC(log(Mortality))))
  expect_identical(
    vapply(fit$lc, function(m) m$fit$model$adjust, character(1)),
    rep("none", 3)
  )
  # An explicit choice is respected
  explicit <- pr |>
    model(lc = LC(log(Mortality), adjust = "dt")) |>
    suppressWarnings()
  expect_identical(explicit$lc[[1]]$fit$model$adjust, "dt")
})

test_that("LC deviances omit ages with no population", {
  nf <- norway_mortality |> filter(Sex == "Female", Year > 1990)
  expect_identical(sum(nf$Population == 0) > 0, TRUE)
  gl <- glance(model(nf, LC(log(Mortality))))
  expect_true(is.finite(gl$base_deviance))
  expect_true(is.finite(gl$total_deviance))
  # Unchanged when all populations are positive
  gl2 <- glance(model(nf |> filter(Age < 100), LC(log(Mortality))))
  expect_equal(unname(gl2$base_deviance), 1.357878, tolerance = 1e-6)
})

test_that("LC with actual jump-off uses fitted rates for zero final rates", {
  nz <- norway_mortality |> filter(Sex == "Female", Year > 1990, Age < 100)
  nz$Mortality[nz$Year == max(nz$Year) & nz$Age == 5] <- 0
  expect_warning(
    fit <- model(nz, actual = LC(log(Mortality), jump_choice = "actual")),
    "final year for ages 5, 10, 13\\."
  )
  fit_fit <- model(nz, fit = LC(log(Mortality)))
  fc <- forecast(fit, h = 2)
  fc_fit <- forecast(fit_fit, h = 2)
  expect_false(anyNA(fc$.mean))
  expect_equal(fc$.mean[fc$Age == 5], fc_fit$.mean[fc_fit$Age == 5])
  expect_false(isTRUE(all.equal(fc$.mean[fc$Age == 6], fc_fit$.mean[fc_fit$Age == 6])))
  sim <- generate(fit, h = 2, times = 2)
  expect_false(anyNA(sim$.sim))
})
