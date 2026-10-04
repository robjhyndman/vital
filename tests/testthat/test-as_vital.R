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

test_that("as_vital on a vital keeps vital variables that are not given", {
  vv <- vital_vars(norway_mortality)
  expect_identical(vital_vars(as_vital(norway_mortality)), vv)
  expect_identical(vital_vars(as_vital(norway_mortality, .age = "Age")), vv)
  expect_identical(
    vital_vars(as_vital(norway_mortality, .sex = NULL)),
    vv[names(vv) != "sex"]
  )
})

test_that("the print header counts series correctly", {
  x <- norway_mortality |>
    dplyr::filter(Age < 50 | Sex == "Female")
  expect_identical(unname(tibble::tbl_sum(x)["Key"]), "Age x Sex [111 x 3]")
  y <- read_stmf_files("AUSstmfout.csv")
  expect_identical(unname(tibble::tbl_sum(y)["Key"]), "Sex, Age_group [18]")
})
