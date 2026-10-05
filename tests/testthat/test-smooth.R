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
  expect_no_error(ggplot2::ggplot_build(autoplot(sm, .smooth) + ggplot2::scale_y_log10()))
  sm <- norway_fertility |>
    smooth_spline(Fertility, k = -1)
  expect_no_error(ggplot2::ggplot_build(autoplot(sm, .smooth)))
  sm <- norway_fertility |>
    smooth_loess(Fertility, span = 0.3)
  expect_identical(NROW(sm), 2464L)

})

test_that("smooth_mortality is similar to demography::smooth.demogdata()", {
  skip_if_not_installed("demography")
  fr <- demography::fr.mort
  sm1 <- demography::smooth.demogdata(demography::extract.years(fr, 1945))
  expect_error(smooth_mortality(as_vital(fr)), ".var is missing")
  sm2 <- smooth_mortality(
    as_vital(fr) |> dplyr::filter(Year == 1945, Sex == "male"),
    Mortality
  )
  test1 <- demography::extract.years(sm1, 1945)$rate$male
  expect_lt(max(abs(c(test1) - sm2$.smooth), na.rm = TRUE), 0.01)
})

test_that("smoothing keeps integer ages when possible", {
  nf <- norway_mortality |> dplyr::filter(Sex == "Female", Year == 2000)
  expect_type(smooth_loess(nf, Mortality)$Age, "integer")
  expect_type(smooth_loess(nf, Mortality, age_spacing = 0.5)$Age, "double")
})

test_that("autoplot of several variables plots each separately", {
  x <- norway_mortality |>
    dplyr::filter(Year > 2015, Sex != "Total")
  built <- ggplot2::ggplot_build(autoplot(x, ggplot2::vars(Deaths, Population)))
  expect_identical(NROW(built$layout$layout), 4L)
  d <- built$data[[1]]
  expect_identical(unique(as.vector(table(paste(d$PANEL, d$group)))), 111L)
})

test_that("smooth_mortality works for data starting above age 50", {
  sm <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year == 2010, Age > 50, Age < 90) |>
    smooth_mortality(Mortality)
  expect_true(all(is.finite(sm$.smooth)))
  expect_true(all(diff(sm$.smooth[sm$Age >= 65]) >= 0))
})

test_that("smooth_loess weights rates by population over rate", {
  nor <- norway_mortality |>
    dplyr::filter(Year == 2000, Sex == "Female", Age >= 30, Age <= 90)
  sm <- smooth_loess(nor, Mortality)
  w <- nor$Population / nor$Mortality
  fit <- stats::loess(
    Mortality ~ Age,
    data = nor,
    span = 0.2,
    degree = 2,
    weights = w / sum(w),
    surface = "direct"
  )
  expect_equal(unname(sm$.smooth), unname(predict(fit, newdata = nor)))
})

test_that("smoothing checks its inputs", {
  nf <- norway_mortality |> dplyr::filter(Sex == "Female", Year == 2000)
  expect_error(smooth_loess(nf), ".var is missing")
  expect_error(
    smooth_loess(dplyr::mutate(nf, .smooth = 1), Mortality),
    "already contains a variable named '.smooth'"
  )
  expect_error(
    smooth_loess(dplyr::mutate(nf, .smooth_se = 1), Mortality),
    "already contains a variable named '.smooth_se'"
  )
  expect_error(smooth_loess(as_vital(nf, .age = NULL), Mortality), "No age variable found")
  expect_error(smooth.monotonic(1:10, 1:10, b = 5, k = 2), "Inappropriate value of k")
})

test_that("smooth_mortality works without population and below the monotonic age", {
  nf <- norway_mortality |> dplyr::filter(Sex == "Female", Year == 2000)
  no_pop <- smooth_mortality(as_vital(nf, .population = NULL), Mortality)
  expect_true(all(is.finite(no_pop$.smooth)))
  young <- smooth_mortality(dplyr::filter(nf, Age < 60), Mortality, b = 65)
  expect_true(all(is.finite(young$.smooth)))
  expect_identical(max(young$Age), 59L)
})

test_that("smooth_spline weights rates by population", {
  nf <- norway_mortality |> dplyr::filter(Sex == "Female", Year == 2000, Age >= 30, Age <= 90)
  sm <- smooth_spline(nf, Mortality)
  w <- nf$Population / max(nf$Population) * nf$Mortality^(-1)
  fit <- mgcv::gam(Mortality ~ s(Age, k = -1), weights = w / sum(w), data = nf)
  expect_equal(sm$.smooth, as.vector(stats::predict(fit, newdata = nf)))
})

test_that("smoothed values are plain numeric vectors", {
  nf <- norway_mortality |> dplyr::filter(Sex == "Female", Year == 2000, Age >= 30)
  for (sm in list(smooth_spline(nf, Mortality), smooth_loess(nf, Mortality))) {
    expect_null(attributes(sm$.smooth))
    expect_null(attributes(sm$.smooth_se))
  }
})
