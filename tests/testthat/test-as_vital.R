test_that("as_vital works for demogdata with only rates or only population", {
  skip_if_not_installed("demography")
  both <- as_vital(demography::fr.mort)
  rates_only <- demography::fr.mort
  rates_only$pop <- NULL
  rates_only <- as_vital(rates_only)
  expect_s3_class(rates_only, "vital")
  expect_equal(rates_only$Mortality, both$Mortality)
  expect_false("Exposure" %in% colnames(rates_only))
  pop_only <- demography::fr.mort
  pop_only$rate <- NULL
  pop_only <- as_vital(pop_only)
  expect_s3_class(pop_only, "vital")
  expect_equal(pop_only$Exposure, both$Exposure)
  expect_identical(vital_vars(pop_only)[["population"]], "Exposure")
})
