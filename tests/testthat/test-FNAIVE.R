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

test_that("FNAIVE bootstrap simulations have no missing values", {
  set.seed(1)
  sim <- norway_mortality |>
    filter(Year > 2000, Sex == "Female") |>
    model(fnaive = FNAIVE(Mortality)) |>
    generate(h = 5, times = 10, bootstrap = TRUE)
  expect_false(anyNA(sim$.sim))
})

test_that("FNAIVE works with non-annual data", {
  set.seed(1)
  nor5 <- norway_mortality |>
    tibble::as_tibble() |>
    dplyr::filter(Year %% 5 == 0, Year > 1980, Sex == "Female") |>
    as_vital(index = Year, key = c(Age, Sex), .age = "Age", .sex = "Sex")
  fit <- nor5 |> model(fnaive = FNAIVE(Mortality))
  fc <- forecast(fit, h = 2)
  expect_identical(unique(fc$Year), c(2025, 2030))
  expect_false(anyNA(fc$.mean))
  last <- nor5 |> dplyr::filter(Year == max(Year))
  expect_equal(fc$.mean[fc$Year == 2025], last$Mortality)
  sim <- generate(fit, h = 2, times = 2)
  expect_false(anyNA(sim$.sim))
})

test_that("FNAIVE simulations are random walks from the last observation", {
  set.seed(1)
  fit <- norway_mortality |>
    filter(Year > 2000, Sex == "Female", Age == 60) |>
    model(fnaive = FNAIVE(Mortality))
  sim <- generate(fit, h = 2, times = 4000) |>
    tibble::as_tibble() |>
    dplyr::summarise(mean = mean(.sim), sd = sd(.sim), .by = Year)
  mdl <- fit$fnaive[[1]]$fit
  last <- mdl$fitted$Mortality[mdl$fitted$Year == max(mdl$fitted$Year)]
  expect_equal(sim$mean, rep(last, 2), tolerance = 0.02)
  expect_equal(sim$sd, mdl$model$sigma * sqrt(1:2), tolerance = 0.05)
})
