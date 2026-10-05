library(dplyr)

# Check FNAIVE models

test_that("Functional naive", {
  fnaive <- norway_mortality |>
    filter(Year > 2000, Sex != "Total") |>
    model(fnaive = FNAIVE(Mortality))
  fc <- forecast(fnaive)
  expect_no_error(ggplot2::ggplot_build(autoplot(fc)))
  expect_identical(dim(fnaive), c(2L, 2L))
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
  expect_identical(unique(fc$Year), c(2025L, 2030L))
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

test_that("FNAIVE treats log of zero rates as missing", {
  expect_warning(
    fit <- norway_mortality |>
      filter(Sex == "Female", Year > 1990) |>
      model(FNAIVE(log(Mortality))),
    "zero or missing in the final year"
  )
  expect_true(all(is.finite(fit[[2]][[1]]$fit$model$sigma)))
})

test_that("FNAIVE fills standard deviations that cannot be estimated", {
  males <- norway_mortality |> filter(Sex == "Male", Year > 2000)
  expect_warning(
    fit <- model(males, FNAIVE(log(Mortality))),
    "zero or missing in the final year"
  )
  expect_true(all(is.finite(fit[[2]][[1]]$fit$model$sigma)))
  for (b in c(FALSE, TRUE)) {
    expect_no_warning(sim <- generate(fit, h = 3, times = 2, bootstrap = b))
    expect_false(anyNA(sim$.sim))
  }
})

test_that("tidy() returns FNAIVE standard deviations by age", {
  fit <- norway_mortality |>
    filter(Sex == "Female", Year > 2000, Age < 90) |>
    model(FNAIVE(Mortality))
  td <- tidy(fit)
  expect_identical(unique(td$term), "sigma")
  expect_equal(td$estimate, age_components(fit)$sigma)
})

test_that("augment works when the response is a vital variable", {
  x <- norway_mortality |>
    dplyr::filter(Year > 2018, Sex == "Female")
  aug <- x |>
    model(fn = FNAIVE(Population)) |>
    augment() |>
    dplyr::arrange(Year, Age)
  expect_identical(NROW(aug), NROW(x))
  expect_equal(aug$.response, x$Population)
})

test_that("FNAIVE starts from the last finite value when the final value is zero", {
  x <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year > 2010, Age < 100)
  x$Mortality[x$Year == max(x$Year) & x$Age == 95] <- 0
  expect_warning(
    fit <- model(x, fn = FNAIVE(log(Mortality))),
    "final year for ages .*95"
  )
  fc <- forecast(fit, h = 1)
  expect_true(all(is.finite(fc$.mean)))
  prev <- x$Mortality[x$Year == max(x$Year) - 1 & x$Age == 95]
  expect_equal(median(fc$Mortality[fc$Age == 95]), prev)
  set.seed(1)
  expect_true(all(is.finite(generate(fit, h = 2, times = 2)$.sim)))
})

test_that("FNAIVE forecast horizons are counted in periods for non-annual data", {
  nor5 <- norway_mortality |>
    tibble::as_tibble() |>
    dplyr::filter(Year %% 5 == 0, Year > 1980, Sex == "Female", Age < 90) |>
    as_vital(index = Year, key = c(Age, Sex), .age = "Age", .sex = "Sex")
  fit <- nor5 |> model(fnaive = FNAIVE(log(Mortality)))
  # The index type is kept in the fitted values
  expect_type(fit$fnaive[[1]]$fit$fitted$Year, "integer")
  sigma <- fit$fnaive[[1]]$fit$model
  fc <- forecast(fit, h = 2) |>
    tibble::as_tibble() |>
    dplyr::filter(Age == 60)
  # Variances on the log scale grow by sigma^2 per 5-year step
  fc_var <- vapply(
    vctrs::vec_data(fc$Mortality),
    function(d) d$dist$sigma^2,
    numeric(1)
  )
  expect_equal(fc_var, sigma$sigma[sigma$Age == 60]^2 * c(1, 2))
})

test_that("FNAIVE interpolates starting values at ages with no finite values", {
  x <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year > 2010, Age >= 90, Age < 100)
  x$Mortality[x$Age == 95] <- 0
  expect_warning(
    fit <- model(x, fn = FNAIVE(log(Mortality))),
    "No finite values for ages 95"
  )
  fc <- forecast(fit, h = 1)
  expect_true(all(is.finite(fc$.mean)))
  last <- x |> dplyr::filter(Year == max(Year))
  expect_equal(
    median(fc$Mortality[fc$Age == 95]),
    sqrt(last$Mortality[last$Age == 94] * last$Mortality[last$Age == 96])
  )
  set.seed(1)
  expect_true(all(is.finite(generate(fit, h = 2, times = 2, bootstrap = TRUE)$.sim)))
})

test_that("FNAIVE bootstrap takes all ages of each step from one residual year", {
  set.seed(1)
  # Ages with positive rates in every year, so all residuals are finite
  d <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year > 2000, Age >= 40, Age < 80)
  fit <- d |> model(fn = FNAIVE(log(Mortality)))
  f <- fit$fn[[1]]$fit
  resid <- tibble::as_tibble(f$fitted) |>
    dplyr::filter(Year > min(Year)) |>
    dplyr::select(Year, Age, .innov) |>
    tidyr::pivot_wider(names_from = Year, values_from = .innov)
  last <- d |> dplyr::filter(Year == max(Year))
  sim <- generate(fit, h = 1, times = 3, bootstrap = TRUE) |>
    tibble::as_tibble() |>
    dplyr::arrange(Age)
  for (draw in split(sim, sim$.rep)) {
    innov <- log(draw$.sim) - log(last$Mortality[match(draw$Age, last$Age)])
    matches <- vapply(resid[-1], function(r) isTRUE(all.equal(r, innov)), logical(1))
    expect_true(any(matches))
  }
})
