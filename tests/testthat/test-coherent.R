test_that("Coherent calculatons", {
  # Product/ratio
  orig_data <- norway_mortality |>
    dplyr::filter(Year > 2015, Sex != "Total")
  pr <- orig_data |>
    make_pr(Mortality) |>
    undo_pr(Mortality)
  # Zero values are set to 1e-5 by make_pr()
  expect_equal(dplyr::mutate(orig_data, Mortality = pmax(Mortality, 1e-5)), pr)
  # Mean/difference
  mig <- net_migration(norway_mortality, norway_births) |>
    dplyr::filter(Sex != "Total")
  sd <- mig |>
    make_sd(NetMigration) |>
    undo_sd(NetMigration)
  expect_equal(mig, sd)
})

# Function to determine if ARIMA model is stationary
is_stationary <- function(x) {
  order <- x$fit[[1]]$fit$spec
  sum(order["d"] + order["D"]) == 0
}
# Extract time series models from an FDM model object
# and return TRUE if all are stationary
all_stationary <- function(object) {
  object$fit$ts_models |>
    purrr::map_lgl(is_stationary) |>
    all()
}

test_that("Coherent functional data model", {
  pr <- norway_mortality |>
    dplyr::filter(Sex != "Total", Year > 2000) |>
    make_pr(Mortality)
  pr1 <- pr |>
    model(hu = FDM(log(Mortality), coherent = TRUE))
  stationary <- purrr::map_lgl(pr1$hu, all_stationary)
  expect_identical(pr1$Sex[stationary], c("Female", "Male"))
  expect_identical(pr1$Sex[!stationary], "geometric_mean")
  pr2 <- pr |>
    model(hu = FDM(log(Mortality), coherent = FALSE))
  stationary <- purrr::map_lgl(pr2$hu, all_stationary)
  expect_true(!all(stationary))
})

test_that("coherent flag is set separately for each series", {
  nor <- norway_mortality |>
    dplyr::filter(Year > 2000, Age < 90, Sex != "Total") |>
    tibble::as_tibble()
  two <- dplyr::bind_rows(
    dplyr::mutate(nor, Country = "A"),
    dplyr::mutate(nor, Country = "B")
  ) |>
    as_vital(
      index = Year,
      key = c(Age, Sex, Country),
      .age = "Age",
      .sex = "Sex",
      .deaths = "Deaths",
      .population = "Population"
    ) |>
    make_pr(Mortality)
  fit <- two |>
    model(fdm = FDM(log(Mortality), coherent = TRUE)) |>
    suppressWarnings()
  flags <- vapply(fit$fdm, function(x) x$model$extra$coherent, logical(1))
  expect_identical(flags, fit$Sex != "geometric_mean")
  ts_models <- vapply(
    fit$fdm,
    function(x) model_sum(x$fit$ts_models[[1]]$fit[[1]]),
    character(1)
  )
  expect_true(all(grepl("^ARIMA", ts_models[fit$Sex == "geometric_mean"])))
})

test_that("make_pr() sets zero values to 1e-5 so ratios are positive", {
  orig <- norway_mortality |>
    dplyr::filter(Year > 2015, Sex != "Total")
  expect_true(any(orig$Mortality == 0, na.rm = TRUE))
  pr <- make_pr(orig, Mortality)
  ratios <- pr$Mortality[pr$Sex != "geometric_mean"]
  expect_true(all(ratios > 0, na.rm = TRUE))
  undone <- undo_pr(pr, Mortality)
  zero <- which(orig$Mortality == 0)
  expect_equal(undone$Mortality[zero], rep(1e-5, length(zero)))
})
