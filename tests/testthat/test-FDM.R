# Check Functional data models

test_that("Functional data model", {
  if (requireNamespace("feasts", quietly = TRUE)) {
    library(feasts)
    hu <- norway_mortality |>
      filter(Year > 2000, Sex != "Total") |>
      model(hu = FDM(log(Mortality)))
    fc <- forecast(hu)
    expect_no_error(autoplot(fc))
    expect_identical(dim(hu), c(2L, 2L))
    expect_identical(NROW(tidy(hu)), 0L)
    expect_identical(
      colnames(glance(hu)),
      c("Sex", ".model", "nobs", "varprop")
    )
    expect_no_error(residuals(hu, type = "innov"))
    expect_no_error(residuals(hu, type = "response"))
    expect_no_error(fitted(hu))
    expect_identical(NROW(generate(hu, times = 2)), 888L)
    expect_identical(NROW(fc), 444L)
    expect_equal(
      fc |>
        dplyr::filter(Sex == "Female", Age == 0, Year == 2025) |>
        dplyr::pull(.mean),
      0.001985665,
      tolerance = 1e-5
    )
    expect_identical(
      forecast(hu, bootstrap = TRUE, times = 7L) |>
        head(1) |>
        dplyr::pull(Mortality) |>
        unlist() |>
        length(),
      7L
    )
    expect_identical(
      colnames(time_components(hu)),
      c("Sex", "Year", "mean", paste0("beta", 1:6))
    )
    expect_identical(
      colnames(age_components(hu)),
      c("Sex", "Age", "mean", paste0("phi", 1:6))
    )
  }
})
