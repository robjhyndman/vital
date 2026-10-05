library(dplyr)

# Check FMEAN models

test_that("Functional mean", {
  fm <- norway_mortality |>
    filter(Year > 2000, Sex != "Total") |>
    model(fm = FMEAN(Mortality))
  fc <- forecast(fm)
  expect_no_error(ggplot2::ggplot_build(autoplot(fc)))
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

test_that("mable methods keep the vital variables of the data", {
  nor <- norway_mortality |>
    dplyr::filter(Sex != "Total", Year > 2010)
  fit <- nor |> model(mean = FMEAN(Mortality))
  expect_identical(vital_var_list(forecast(fit, h = 2))$sex, "Sex")
  expect_identical(vital_var_list(generate(fit, h = 2))$sex, "Sex")
  expect_identical(vital_var_list(augment(fit))$sex, "Sex")
  expect_identical(vital_var_list(interpolate(fit, nor))$sex, "Sex")
})

test_that("FMEAN and FNAIVE plots find the age variable", {
  nor <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year > 2010) |>
    dplyr::rename(age = Age)
  fit <- nor |> model(mean = FMEAN(Mortality), naive = FNAIVE(Mortality))
  # Call the methods directly so that age is not supplied
  for (m in c("mean", "naive")) {
    mbl <- dplyr::select(fit, dplyr::all_of(m))
    class(mbl) <- c(class(mbl[[m]][[1]]$fit), class(mbl)[-1])
    p <- autoplot(mbl)
    expect_identical(rlang::as_label(p$mapping$x), "age")
  }
})

test_that("Formulas using vars() and invalid transformations are parsed", {
  nf <- norway_mortality |> filter(Sex == "Female", Year > 2010)
  expect_s3_class(
    model(nf, FMEAN(vars(Mortality, Deaths)), .safely = FALSE)[[2]][[1]],
    "mdl_vtl_ts"
  )
  expect_error(
    model(nf, FMEAN(Mortality^0), .safely = FALSE),
    "Cannot invert"
  )
})

test_that("FMEAN treats log of zero rates as missing", {
  nf <- norway_mortality |> filter(Sex == "Female", Year > 1990)
  fit <- model(nf, FMEAN(log(Mortality)))
  mod <- fit[[2]][[1]]$fit$model
  expect_true(all(is.finite(mod$mean)))
  expect_true(all(is.finite(mod$sigma)))
  nm <- nf
  nm$Mortality[5] <- NA
  expect_gt(interpolate(fit, nm)$Mortality[5], 0)
})

test_that("model() requires an age variable", {
  no_age <- norway_mortality |>
    filter(Sex == "Female", Age == 0) |>
    as_tibble() |>
    as_vital(index = Year, key = Sex, .sex = "Sex")
  expect_error(model(no_age, FMEAN(Mortality)), "No age variable found")
})

test_that("simulated forecasts are matched to rows of new_data", {
  nf <- norway_mortality |> filter(Sex == "Female", Year > 2000, Age < 20)
  fit <- model(nf, FMEAN(log(Mortality)))
  nd <- nf |>
    filter(Year == max(Year)) |>
    mutate(Year = Year + 1L) |>
    as_vital(index = Year, key = c(Age, Sex))
  set.seed(1)
  fc <- forecast(fit, new_data = nd, simulate = TRUE, times = 5000)
  fc0 <- forecast(fit, new_data = nd)
  expect_equal(median(fc$Mortality), median(fc0$Mortality), tolerance = 0.05)
})

test_that("forecasts and simulations keep the type of the index", {
  fit <- norway_mortality |>
    filter(Sex == "Female", Year > 2015) |>
    model(FMEAN(Mortality))
  expect_type(forecast(fit, h = 2)$Year, "integer")
  expect_type(generate(fit, h = 2)$Year, "integer")
})

test_that("missing standard deviations are interpolated across age", {
  expect_equal(fill_by_age(c(1, NA, 3, NA), 1:4), c(1, 2, 3, 3))
  expect_equal(fill_by_age(c(NA, 2, NA), 1:3), c(2, 2, 2))
  expect_identical(fill_by_age(c(NA_real_, NA_real_), 1:2), c(NA_real_, NA_real_))
})

test_that("FMEAN bootstrap handles ages with one or no residuals", {
  males <- norway_mortality |> filter(Sex == "Male", Year > 2000)
  fit <- model(males, FMEAN(log(Mortality)))
  expect_true(all(is.finite(fit[[2]][[1]]$fit$model$sigma)))
  set.seed(1)
  sim <- generate(fit, h = 2, times = 50, bootstrap = TRUE)
  expect_false(anyNA(sim$.sim))
  # Age 110 has a single residual (zero), which is resampled as itself
  resid110 <- fit[[2]][[1]]$fit$fitted |>
    as_tibble() |>
    filter(Age == 110, is.finite(.resid)) |>
    pull(.resid)
  mean110 <- fit[[2]][[1]]$fit$model$mean[fit[[2]][[1]]$fit$model$Age == 110]
  expect_equal(unique(log(sim$.sim[sim$Age == 110])), mean110 + resid110)
})

test_that("FMEAN bootstrap works at ages with no finite residuals", {
  d <- norway_mortality |> filter(Sex == "Male", Year > 2000, Age > 100)
  d$Mortality[d$Age == 110] <- 0
  expect_warning(
    fit <- model(d, FMEAN(log(Mortality))),
    "No finite values for ages 110"
  )
  sim <- generate(fit, h = 1, times = 3, bootstrap = TRUE)
  expect_true(all(is.finite(sim$.sim)))
  expect_true(all(is.finite(forecast(fit, h = 1)$.mean)))
})

test_that("forecast() gives a clear error when new_data is not a data frame", {
  nf <- norway_mortality |> filter(Sex == "Female", Year > 2015, Age < 90)
  fit <- model(nf, FMEAN(Mortality))
  nd <- nf |> filter(Year == max(Year)) |> mutate(Year = Year + 1L)
  expect_error(forecast(fit, new_data = list(a = nd, b = nd)), "requires a data frame\\.$")
  expect_error(forecast(fit, new_data = 3), "use `h = 3`")
})

test_that("FMEAN bootstrap innovations differ between future times", {
  set.seed(1)
  fit <- norway_mortality |>
    filter(Sex == "Female", Year > 1990) |>
    model(fm = FMEAN(log(Mortality)))
  sim <- generate(fit, h = 4, times = 2, bootstrap = TRUE) |>
    as_tibble() |>
    filter(Age == 50)
  # Each replicate has a different value in each year
  n_distinct_by_rep <- tapply(sim$.sim, sim$.rep, function(x) length(unique(x)))
  expect_true(all(n_distinct_by_rep == 4L))
})

test_that("model functions reject unused arguments", {
  expect_error(FMEAN(Mortality, typo = 1), "must be empty")
  expect_error(FNAIVE(Mortality, typo = 1), "must be empty")
  expect_error(LC(log(Mortality), adjsut = "e0"), "must be empty")
  expect_error(FDM(log(Mortality), typo = 1), "must be empty")
})
