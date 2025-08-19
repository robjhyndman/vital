test_that("smooth_mortality_law", {
  sm <- norway_mortality |>
    dplyr::filter(Year <= 1910, Sex == "Male") |>
    smooth_mortality_law(Mortality)
  expect_identical(dim(sm), c(1221L, 9L))
  expect_no_error(autoplot(sm, .smooth) + ggplot2::scale_y_log10())
})
