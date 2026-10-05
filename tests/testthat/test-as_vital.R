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

test_that("vital() creates a vital object", {
  v <- vital(
    Year = rep(2000:2001, each = 3),
    Age = rep(0:2, 2),
    mx = 1:6 / 100,
    index = Year,
    key = Age,
    .age = "Age"
  )
  expect_s3_class(v, "vital")
  expect_identical(vital_vars(v), c(age = "Age"))
  expect_identical(tsibble::key_vars(v), "Age")
})

test_that("print headers describe vital fables, groups and several keys", {
  x <- norway_mortality |> dplyr::filter(Year > 2015, Sex != "Total")
  fc <- x |> model(FMEAN(Mortality)) |> forecast(h = 2)
  expect_identical(names(tibble::tbl_sum(fc))[1], "A vital fable")
  grouped <- dplyr::group_by(x, Sex)
  expect_identical(unname(tibble::tbl_sum(grouped)["Groups"]), "Sex [2]")
  two_keys <- x |>
    dplyr::mutate(Region = "A") |>
    as_vital(index = Year, key = c(Age, Sex, Region), .age = "Age", .sex = "Sex")
  expect_identical(
    unname(tibble::tbl_sum(two_keys)["Key"]),
    "Age x (Sex, Region) [111 x 2]"
  )
  expect_identical(
    unname(tibble::tbl_sum(dplyr::filter(x, Sex == "Female", Age == 0) |>
      as_vital(key = Age))["Key"]),
    "Age [1 x 1]"
  )
})

test_that("as_vital converts fertility demogdata objects", {
  skip_if_not_installed("demography")
  fert <- demography::aus.fert
  v <- as_vital(fert)
  expect_identical(
    vital_vars(v),
    c(age = "Age", sex = "Sex", births = "Births", population = "Exposure")
  )
  # Fertility rates are per 1000 women
  ok <- !is.na(v$Fertility)
  expect_equal(v$Births[ok], v$Exposure[ok] * v$Fertility[ok] / 1000)
  # Age group labels are kept as a key
  expect_equal(
    v$Fertility[v$Year == 1921 & v$AgeGroup == "30-34"],
    unname(fert$rate$female["30-34", "1921"])
  )
})

test_that("as_vital checks the types of vital variables", {
  x <- norway_mortality |> dplyr::filter(Year == 2000)
  expect_error(
    as_vital(dplyr::mutate(x, Deaths = as.character(Deaths))),
    "Deaths variable must be numeric"
  )
  expect_error(
    as_vital(dplyr::mutate(x, S = 1), .sex = "S"),
    "Sex variable must be character or factor"
  )
})
