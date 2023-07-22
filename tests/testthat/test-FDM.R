# Check Functional data models

test_that("Functional data model", {
  library(feasts)
  hu <- aus_mortality |>
    dplyr::filter(State == "Victoria") |>
    model(hu = FDM(Mortality))
  fc <- forecast(hu)
  expect_no_error(autoplot(fc))
  expect_equal(dim(hu), c(3L, 4L))
  expect_equal(NROW(tidy(hu)), 0L)
  expect_equal(colnames(glance(hu)), c("Sex", "State", "Code", ".model", "sigma2", "varprop"))
  expect_equal(mean(augment(hu)$.resid), -1.023669, tolerance = 1e-6)
  expect_no_error(residuals(hu, type = "innov"))
  expect_no_error(residuals(hu, type = "response"))
  expect_no_error(fitted(hu))
  expect_equal(NROW(generate(hu, times = 2)), 1212L)
  expect_equal(NROW(fc), 606)
  #expect_equal(fc |>
  #  dplyr::filter(Sex == "female", State == "Victoria", Age == 0, Year == 2004) |>
  #    dplyr::pull(.mean), 0.4554334, tolerance = 1e-7)
  expect_equal(forecast(hu, bootstrap = TRUE, times = 7) |>
                 head(1) |>
                 dplyr::pull(Mortality) |>
                 unlist() |>
                 length(), 7)
  expect_equal(colnames(time_components(hu)), c("Sex", "State", "Code", "Year", "mean", paste0("beta",1:6)))
  expect_equal(colnames(age_components(hu)), c("Sex", "State", "Code", "Age", "mean", paste0("phi",1:6)))

})
