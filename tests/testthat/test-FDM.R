# Check Functional data models

test_that("Functional data model", {
  if (requireNamespace("feasts", quietly = TRUE)) {
    library(feasts)
    hu <- norway_mortality |>
      filter(Year > 2000, Sex != "Total") |>
      model(hu = FDM(log(Mortality)))
    fc <- forecast(hu)
    expect_no_error(autoplot(fc))
    expect_identical(dim(hu), c(2L, 2L))
    expect_identical(NROW(tidy(hu)), 0L)
    expect_identical(
      colnames(glance(hu)),
      c("Sex", ".model", "nobs", "varprop")
    )
    expect_no_error(residuals(hu, type = "innov"))
    expect_no_error(residuals(hu, type = "response"))
    expect_no_error(fitted(hu))
    expect_identical(NROW(generate(hu, times = 2)), 888L)
    expect_identical(NROW(fc), 444L)
    expect_equal(
      fc |>
        dplyr::filter(Sex == "Female", Age == 0, Year == 2025) |>
        dplyr::pull(.mean),
      0.001695867,
      tolerance = 1e-5
    )
    expect_identical(
      forecast(hu, bootstrap = TRUE, times = 7L) |>
        head(1) |>
        dplyr::pull(Mortality) |>
        unlist() |>
        length(),
      7L
    )
    expect_identical(
      colnames(time_components(hu)),
      c("Sex", "Year", "mean", paste0("beta", 1:6))
    )
    expect_identical(
      colnames(age_components(hu)),
      c("Sex", "Age", "mean", paste0("phi", 1:6))
    )
  }
})

test_that("FDM checks the coherent time series model function", {
  expect_no_error(
    FDM(log(Mortality), coherent = TRUE, ts_model_fn = fable::ETS)
  )
  expect_error(
    FDM(log(Mortality), coherent = TRUE, coherent_ts_model_fn = fable::ETS),
    "coherent_ts_model_fn"
  )
})

test_that("fdpca compares the grid size with the number of ages", {
  set.seed(1)
  # More years than grid points, but few ages
  many_years <- matrix(rnorm(600 * 10), nrow = 600, ncol = 10)
  expect_no_error(fdpca(many_years, order = 2, ngrid = 500))
  many_ages <- matrix(rnorm(5 * 600), nrow = 5, ncol = 600)
  expect_error(fdpca(many_ages, order = 2, ngrid = 500), "Grid should be larger")
})

test_that("coherent FDM definitions pass the model by name to workers", {
  arfima <- FDM(log(Mortality), coherent = TRUE)
  expect_identical(arfima$extra$coherent_ts_model, "ARFIMA")
  expect_null(arfima$extra$coherent_ts_model_fn)
  arima <- FDM(
    log(Mortality),
    coherent = TRUE,
    coherent_ts_model_fn = fable::ARIMA
  )
  expect_identical(arima$extra$coherent_ts_model, "ARIMA")
  expect_error(
    FDM(log(Mortality), coherent = TRUE, coherent_ts_model_fn = fable::ETS),
    "must be fable::ARIMA or fable::ARFIMA"
  )
})

test_that("FDM requires at least one principal component", {
  expect_error(FDM(log(Mortality), order = 0), "positive integer")
  expect_error(FDM(log(Mortality), order = 1.5), "positive integer")
  expect_error(FDM(log(Mortality), order = c(1, 2)), "positive integer")
  fc <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year > 2000) |>
    model(fdm = FDM(log(Mortality), order = 1)) |>
    forecast(h = 1)
  expect_false(anyNA(fc$.mean))
})

test_that("fdpca uses the actual ages rather than equal spacing", {
  # Variation concentrated at age 0, plus variation across all ages
  make <- function(ages) {
    set.seed(2)
    t1 <- rnorm(40)
    t2 <- rnorm(40)
    outer(t1, 3 * exp(-ages)) + outer(t2, ages / 100)
  }
  single <- 0:100
  abridged <- c(0, 1, seq(5, 100, by = 5))
  phi_single <- fdpca(make(single), x = single, order = 1)$basis[, "phi1"]
  phi_abridged <- fdpca(make(abridged), x = abridged, order = 1)$basis[, "phi1"]
  expect_gt(abs(cor(phi_abridged, phi_single[abridged + 1])), 0.9999)
  # Single-year ages give the same results as equal spacing
  expect_equal(
    fdpca(make(single), x = single, order = 2),
    fdpca(make(single), order = 2)
  )
})

test_that("FDM works with abridged ages", {
  abridged <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year > 1990) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      Age = c(0, 1, seq(5, 100, by = 5))[
        findInterval(Age, c(0, 1, seq(5, 100, by = 5)))
      ]
    ) |>
    dplyr::summarise(
      Deaths = sum(Deaths),
      Population = sum(Population),
      .by = c(Year, Sex, Age)
    ) |>
    dplyr::mutate(Mortality = Deaths / Population) |>
    as_vital(index = Year, key = c(Age, Sex), .age = "Age", .sex = "Sex")
  fit <- abridged |> model(fdm = FDM(log(Mortality), order = 2))
  ages <- age_components(fit)
  expect_identical(ages$Age, c(0, 1, seq(5, 100, by = 5)))
  fits <- augment(fit)
  expect_lt(median(abs(log(fits$.fitted / fits$.response))), 0.1)
})

test_that("fdpca aligns years with missing values at the oldest ages", {
  # Data that are linear in age, so linear extrapolation is exact and
  # dropping the oldest ages in some years should not change the fit
  ages <- 0:100
  kt <- seq(-1, 1, length.out = 20)
  X <- outer(rep(1, 20), -9 + 0.08 * ages) + outer(kt, 0.01 * ages)
  gappy <- X
  gappy[c(3, 7, 12), 96:101] <- NA
  fit <- function(M) {
    pca <- fdpca(M, x = ages, order = 1)
    pca$coeff %*% t(pca$basis)
  }
  expect_equal(fit(gappy), fit(X))
})

test_that("spline_extrapolate continues linearly beyond the data", {
  x <- 0:10
  y <- 2 + 3 * x
  expect_equal(spline_extrapolate(x, y, c(-2, 5, 12)), 2 + 3 * c(-2, 5, 12))
})

test_that("autoplot shows no more components than were fitted", {
  fit <- norway_mortality |>
    filter(Sex == "Female", Year > 2000, Age < 90) |>
    model(FDM(log(Mortality), order = 1))
  p <- autoplot(fit)
  expect_s3_class(p, "patchwork")
  expect_length(p$patches$plots, 3L)
})

test_that("FDM reports an order that is too large for the data", {
  expect_warning(
    model(
      norway_mortality |> filter(Sex == "Female", Year > 2018, Age < 90),
      FDM(log(Mortality), order = 6)
    ),
    "order must be less than the number of time periods \\(5\\)"
  )
})
