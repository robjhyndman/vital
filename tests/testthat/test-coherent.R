test_that("Coherent calculatons", {
  # Product/ratio
  orig_data <- aus_mortality |>
   dplyr::filter(Year > 2015, Sex != "total", Code == "NSW")
  pr <- orig_data |> make_pr(Mortality) |> undo_pr(Mortality)
  expect_equal(orig_data, pr)
  # Mean/difference
  mig <- net_migration(norway_mortality, norway_births) |>
     dplyr::filter(Sex != "Total")
  sd <- mig |> make_sd(NetMigration) |> undo_sd(NetMigration)
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
  pr <- aus_mortality |>
    dplyr::filter(State == "Victoria", Sex != "total", Year > 1950) |>
    make_pr(Mortality)
  pr1 <- pr |>
    model(hu = FDM(log(Mortality), coherent = TRUE))
  stationary <- purrr::map_lgl(pr1$hu, all_stationary)
  expect_identical(pr1$Sex[stationary], c("female","male"))
  expect_identical(pr1$Sex[!stationary], "geometric_mean")
  pr2 <- pr |>
    model(hu = FDM(log(Mortality), coherent = FALSE))
  stationary <- purrr::map_lgl(pr2$hu, all_stationary)
  expect_true(all(!stationary))
})




