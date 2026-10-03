# Check FMEAN models

test_that("Functional mean", {
  fm <- norway_mortality |>
    filter(Year > 2000, Sex != "Total") |>
    model(fm = FMEAN(Mortality))
  fc <- forecast(fm)
  expect_no_error(autoplot(fc))
  expect_identical(dim(fm), c(2L, 2L))
  expect_identical(dim(tidy(fm)), c(222L, 8L))
  # Standard errors use the number of years for each age
  tidy_age0 <- tidy(fm) |> filter(Sex == "Female", Age == 0)
  rates_age0 <- norway_mortality |>
    filter(Year > 2000, Sex == "Female", Age == 0) |>
    pull(Mortality)
  expect_equal(
    tidy_age0$std.error,
    sd(rates_age0) / sqrt(length(rates_age0))
  )
  expect_identical(
    colnames(glance(fm)),
    c("Sex", ".model", "sigma2")
  )
  expect_equal(mean(augment(fm)$.resid, na.rm = TRUE), 0)
  expect_no_error(residuals(fm, type = "innov"))
  expect_no_error(residuals(fm, type = "response"))
  expect_no_error(fitted(fm))
  expect_identical(NROW(generate(fm, times = 2)), 888L)
  expect_identical(NROW(fc), 444L)
  expect_equal(
    fc |>
      dplyr::filter(Sex == "Female", Age == 0, Year == 2024) |>
      dplyr::pull(.mean),
    0.002341783,
    tolerance = 1e-7
  )
  # Simulated innovations should use the sigma for each age
  set.seed(1)
  sim_sd <- generate(fm |> filter(Sex == "Female"), h = 3, times = 200) |>
    as_tibble() |>
    group_by(Age) |>
    summarise(sim_sd = sd(.sim))
  sigma <- fm$fm[[1]]$fit$model
  expect_gt(cor(sim_sd$sim_sd, sigma$sigma[match(sim_sd$Age, sigma$Age)]), 0.99)
  expect_identical(
    forecast(fm, bootstrap = TRUE, times = 7L) |>
      head(1) |>
      dplyr::pull(Mortality) |>
      unlist() |>
      length(),
    7L
  )
})

test_that("generate checks times against the replicates in new_data", {
  fm <- norway_mortality |>
    filter(Year > 2015, Sex == "Female") |>
    model(fm = FMEAN(Mortality))
  mdl <- fm$fm[[1]]
  new_data <- make_future_data(mdl$data, h = 1) |>
    dplyr::mutate(.rep = "1")
  expect_error(
    generate(mdl$fit, new_data = new_data, times = 2),
    "must equal the number of replicates"
  )
})
