test_that("smooth_mortality_law", {
  sm <- aus_mortality |>
    dplyr::filter(Code == "NSW", Year <= 1910, Sex == "male") |>
    smooth_mortality_law(Mortality)
  expect_identical(dim(sm), c(1010L, 10L))
  expect_no_error(autoplot(sm, .smooth) + ggplot2::scale_y_log10())
})
