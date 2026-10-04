test_that("interpolate fills missing values with fitted values", {
  nor <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year > 2000, Age < 90)
  gappy <- nor |>
    dplyr::mutate(
      Mortality = dplyr::if_else(Year == 2010 & Age %in% 50:52, NA, Mortality)
    )
  fit <- gappy |>
    model(
      mean = FMEAN(Mortality),
      naive = FNAIVE(Mortality),
      lc = LC(log(Mortality)),
      fdm = FDM(log(Mortality), order = 2)
    )
  gaps <- is.na(gappy$Mortality)
  for (m in c("mean", "naive", "lc", "fdm")) {
    out <- interpolate(dplyr::select(fit, Sex, dplyr::all_of(m)), gappy)
    expect_false(anyNA(out$Mortality))
    expect_equal(out$Mortality[!gaps], nor$Mortality[!gaps])
    fits <- augment(dplyr::select(fit, Sex, dplyr::all_of(m))) |>
      dplyr::filter(Year == 2010, Age %in% 50:52)
    expect_equal(out$Mortality[gaps], fits$.fitted)
  }
})

test_that("interpolate keeps other columns and works for GAPC models", {
  x <- norway_mortality |>
    dplyr::filter(Year > 2000, Sex == "Female", Age < 90) |>
    dplyr::mutate(
      Mortality = dplyr::if_else(
        Year == 2010 & Age %in% 50:52,
        NA_real_,
        Mortality
      )
    )
  for (fit in list(model(x, m = LC(log(Mortality))), model(x, m = APC(Mortality)))) {
    out <- interpolate(fit, x)
    expect_identical(colnames(out), colnames(x))
    expect_identical(out$Population, x$Population)
    expect_false(anyNA(out$Mortality))
    expect_equal(out$Mortality[!is.na(x$Mortality)], x$Mortality[!is.na(x$Mortality)])
  }
})
