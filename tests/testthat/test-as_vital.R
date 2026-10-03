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

test_that("as_vital on a vital rebuilds keys and keeps vital variables", {
  nor <- norway_mortality |>
    dplyr::filter(Year > 2015, Sex != "Total")
  rebuilt <- as_vital(nor, index = Year, key = c(Age, Sex))
  expect_identical(tsibble::key_vars(rebuilt), c("Age", "Sex"))
  expect_identical(vital_vars(rebuilt), vital_vars(nor))
  # Vital variables can still be overridden
  no_sex <- nor |>
    dplyr::filter(Sex == "Female") |>
    as_vital(key = Age, .sex = NULL)
  expect_identical(tsibble::key_vars(no_sex), "Age")
  expect_null(vital_var_list(no_sex)$sex)
  expect_identical(vital_var_list(no_sex)$deaths, "Deaths")
})
