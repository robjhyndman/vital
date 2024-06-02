test_that("smoothing functions", {
  sm <- aus_fertility |>
    smooth_fertility(Fertility)
  expect_equal(colnames(sm), c("Year","Age","Fertility","Exposure","Births",".smooth",".smooth_se"))
  sm <- aus_mortality |>
    dplyr::filter(Code == "NSW", Year <= 1910, Sex == "male") |>
    smooth_mortality(Mortality)
  expect_equal(dim(sm), c(1010L, 10L))
  expect_no_error(autoplot(sm, .smooth) + ggplot2::scale_y_log10())
  sm <- aus_fertility |>
    smooth_spline(Fertility, k=-1)
  expect_no_error(autoplot(sm, .smooth))
  sm <- aus_fertility |>
    smooth_loess(Fertility, span=.3)
  expect_equal(NROW(sm), 3010L)

  # Check results are similar to demography
  if(requireNamespace("demography", quietly = TRUE)) {
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
    expect_true(sum(abs(c(test1)-test2), na.rm=TRUE) < 1e-4)
  }
})

