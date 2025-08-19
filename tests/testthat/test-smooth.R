test_that("smoothing functions", {
  sm <- norway_fertility |>
    smooth_fertility(Fertility)
  expect_identical(
    colnames(sm),
    c("Year", "Age", "Fertility", "OpenInterval", ".smooth", ".smooth_se")
  )
  sm <- norway_mortality |>
    dplyr::filter(Year <= 1903, Sex == "Male") |>
    smooth_mortality(Mortality)
  expect_identical(dim(sm), c(444L, 9L))
  expect_no_error(autoplot(sm, .smooth) + ggplot2::scale_y_log10())
  sm <- norway_fertility |>
    smooth_spline(Fertility, k = -1)
  expect_no_error(autoplot(sm, .smooth))
  sm <- norway_fertility |>
    smooth_loess(Fertility, span = 0.3)
  expect_identical(NROW(sm), 2464L)

  # Check results are similar to demography
  if (requireNamespace("demography", quietly = TRUE)) {
    library(demography)
    sm1 <- smooth.demogdata(fr.mort |> extract.years(1945))
    expect_error(smooth_mortality(as_vital(fr.mort)))
    sm2 <- smooth_mortality(
      as_vital(fr.mort) |> dplyr::filter(Year == 1945, Sex == "male"),
      Mortality
    ) |>
      dplyr::select(.smooth)
    test1 <- extract.years(sm1, 1945)$rate$male
    test2 <- sm2 |>
      dplyr::filter(Year == 1945) |>
      dplyr::pull(.smooth)
    expect_lt(max(abs(c(test1) - test2), na.rm = TRUE), 0.01)
  }
})
