test_that("smooth_mortality_law", {
  sm <- norway_mortality |>
    dplyr::filter(Year <= 1910, Sex == "Male") |>
    smooth_mortality_law(Mortality)
  expect_identical(dim(sm), c(1221L, 9L))
  expect_no_error(autoplot(sm, .smooth) + ggplot2::scale_y_log10())
})

test_that("smooth_mortality_law uses deaths and population when available", {
  nor <- norway_mortality |>
    dplyr::filter(Year == 2000, Sex == "Female", Age >= 40, Age <= 95)
  sm <- smooth_mortality_law(nor, Mortality)
  fit <- MortalityLaws::MortalityLaw(
    x = nor$Age,
    Dx = nor$Deaths,
    Ex = nor$Population,
    law = "gompertz"
  )
  expect_equal(unname(sm$.smooth), unname(fit$fitted.values))
})
