library(dplyr)

# Small data set for testing purposes
nor_mortality <- norway_mortality |>
  filter(
    Year <= 1903,
    Age > 80
  )

test_that("classes", {
  expect_s3_class(nor_mortality |> filter(Sex == "Male"), "vital")
  expect_s3_class(nor_mortality |> select(Population), "vital")
  expect_s3_class(nor_mortality |> slice(10), "vital")
  expect_s3_class(
    nor_mortality |> arrange(Age, Sex, Year),
    "vital"
  )
  expect_s3_class(nor_mortality |> mutate(mx = Deaths / Population), "vital")
  expect_s3_class(nor_mortality |> head(10), "vital")
  expect_s3_class(nor_mortality |> tail(100), "vital")
  expect_s3_class(
    bind_rows(
      nor_mortality |> filter(Year == 1901),
      nor_mortality |> filter(Year == 1902)
    ),
    "vital"
  )
  expect_s3_class(
    bind_cols(
      nor_mortality |> select(-Population),
      Exposure = nor_mortality$Population
    ),
    "vital"
  )
  expect_s3_class(nor_mortality |> transmute(mx = Deaths / Population), "vital")
  expect_s3_class(nor_mortality |> relocate(Year, Population, Age), "vital")
  expect_s3_class(nor_mortality |> summarise(Deaths = mean(Deaths)), "vital")
  expect_s3_class(
    nor_mortality |> group_by(Sex) |> summarise(Deaths = mean(Deaths)),
    "vital"
  )
  expect_s3_class(nor_mortality |> group_by(Sex) |> ungroup(), "vital")
  expect_identical(
    nor_mortality |> group_by(Sex) |> select(Deaths) |> group_vars(),
    "Sex"
  )
  expect_identical(
    nor_mortality |> group_by(Sex) |> select(S = Sex, Deaths) |> group_vars(),
    "S"
  )
  expect_s3_class(
    left_join(
      nor_mortality |> select(Population),
      nor_mortality |> select(Deaths)
    ),
    "vital"
  )
})

test_that("fable classes", {
  fc <- nor_mortality |>
    filter(Sex == "Female") |>
    model(naive = FNAIVE(Mortality)) |>
    forecast(h = 2)
  grouped <- fc |> group_by(Age)
  expect_s3_class(grouped, c("grouped_fbl_vtl", "grouped_ts"))
  expect_s3_class(grouped, "fbl_vtl_ts")
  expect_s3_class(grouped |> filter(Year == 1904), "grouped_fbl_vtl")
  expect_s3_class(grouped |> mutate(z = .mean), "grouped_fbl_vtl")
  expect_s3_class(grouped |> arrange(Year), "grouped_fbl_vtl")
  expect_s3_class(grouped |> select(Mortality), "grouped_fbl_vtl")
  expect_identical(grouped |> select(Mortality) |> group_vars(), "Age")
  expect_s3_class(grouped |> ungroup(), "fbl_vtl_ts")
  expect_false(inherits(grouped |> ungroup(), "grouped_df"))
  expect_s3_class(grouped |> filter(Year == 1904) |> ungroup(), "fbl_vtl_ts")
  # Removing the distribution leaves a tsibble, as in fabletools
  expect_false(inherits(fc |> summarise(m = mean(.mean)), "fbl_ts"))
  expect_false(inherits(grouped |> summarise(m = mean(.mean)), "fbl_ts"))
})

test_that("group_by() with no variables keeps an ungrouped vital", {
  nor <- norway_mortality |> dplyr::filter(Year == 2000)
  ungrouped <- dplyr::group_by(nor)
  expect_identical(class(ungrouped), class(nor))
  expect_identical(vital_vars(ungrouped), vital_vars(nor))
  grouped <- dplyr::group_by(nor, Sex) |> dplyr::group_by(Age, .add = TRUE)
  expect_identical(sum(class(grouped) == "grouped_vital"), 1L)
  expect_identical(sum(class(grouped) == "vital"), 1L)
})

test_that("rename() and select() keep renamed vital variables", {
  nor <- norway_mortality |> dplyr::filter(Year == 2000)
  renamed <- dplyr::rename(nor, age = Age, D = Deaths)
  expect_identical(vital_vars(renamed)[["age"]], "age")
  expect_identical(vital_vars(renamed)[["deaths"]], "D")
  expect_equal(life_expectancy(renamed)$ex, life_expectancy(nor)$ex)
  selected <- dplyr::select(nor, Year, age = Age, Sex, Mortality)
  expect_identical(vital_vars(selected)[["age"]], "age")
  expect_identical(vital_vars(selected)[["sex"]], "Sex")
  grouped <- dplyr::group_by(nor, Sex) |> dplyr::rename(S = Sex)
  expect_identical(vital_vars(grouped)[["sex"]], "S")
})

test_that("grouped verbs keep grouped_vital first", {
  grouped <- norway_mortality |>
    dplyr::filter(Year == 2000) |>
    dplyr::group_by(Sex)
  results <- list(
    dplyr::rename(grouped, S = Sex),
    dplyr::mutate(grouped, z = 1),
    dplyr::relocate(grouped, Sex),
    dplyr::filter(grouped, Age < 5),
    dplyr::arrange(grouped, Age),
    dplyr::slice(grouped, 1:2),
    grouped[1:5, ]
  )
  for (res in results) {
    expect_identical(class(res)[1:2], c("grouped_vital", "grouped_ts"))
    expect_identical(sum(class(res) == "vital"), 1L)
  }
})

test_that("dplyr verbs on mables keep a single mdl_vtl_df class", {
  fit <- norway_mortality |>
    dplyr::filter(Year > 2015, Sex != "Total") |>
    model(m = FMEAN(Mortality))
  results <- list(
    dplyr::arrange(fit, Sex),
    dplyr::filter(fit, Sex == "Male"),
    dplyr::mutate(fit, n = m),
    dplyr::select(fit, Sex, m),
    fit[1, ]
  )
  for (res in results) {
    expect_identical(sum(class(res) == "mdl_vtl_df"), 1L)
  }
})

test_that("fill_gaps() keeps vital attributes", {
  nor <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year > 2010, Age < 90) |>
    dplyr::filter(!(Age == 50 & Year == 2015))
  filled <- tsibble::fill_gaps(nor)
  expect_s3_class(filled, "vital")
  expect_identical(vital_vars(filled), vital_vars(nor))
  expect_identical(NROW(filled), NROW(nor) + 1L)
})

test_that("joins keep vital attributes", {
  nor <- norway_mortality |> dplyr::filter(Sex == "Female", Year > 2015)
  joined <- left_join(nor, tibble::tibble(Year = 2020L, z = 1), by = "Year")
  expect_s3_class(joined, "vital")
  expect_identical(vital_vars(joined), vital_vars(nor))
  fc <- nor |>
    model(FMEAN(Mortality)) |>
    forecast(h = 2)
  fc_joined <- left_join(fc, tibble::tibble(Year = 2024, z = 1), by = "Year")
  expect_s3_class(fc_joined, "fbl_vtl_ts")
  expect_identical(vital_vars(fc_joined), vital_vars(fc))
})

test_that("dplyr verbs keep the vital variables of fables as a character vector", {
  fc <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year > 2015) |>
    model(FMEAN(Mortality)) |>
    forecast(h = 2)
  expect_identical(vital_vars(filter(fc, Age < 50)), vital_vars(fc))
  expect_type(vital_vars(filter(fc, Age < 50)), "character")
})

test_that("subsetting a mable without model columns gives a tibble", {
  fit <- norway_mortality |>
    dplyr::filter(Year > 2015, Sex != "Total") |>
    model(FMEAN(Mortality))
  expect_false(inherits(fit[, "Sex"], "mdl_vtl_df"))
  expect_s3_class(fit[1, ], "mdl_vtl_df")
})

test_that("rename keeps mables and fables vital", {
  x <- norway_mortality |>
    dplyr::filter(Year > 2015, Sex != "Total")
  fit <- x |>
    model(fn = FNAIVE(Mortality)) |>
    dplyr::rename(naive = fn)
  expect_s3_class(fit, "mdl_vtl_df")
  fc <- forecast(fit, h = 2)
  expect_s3_class(fc, "fbl_vtl_ts")
  expect_identical(unique(fc$.model), "naive")
  fc2 <- dplyr::rename(fc, mx = Mortality, age = Age)
  expect_s3_class(fc2, "fbl_vtl_ts")
  expect_identical(fabletools::distribution_var(fc2), "mx")
  expect_identical(vital_vars(fc2)[["age"]], "age")
  fc3 <- fc |>
    dplyr::group_by(Sex) |>
    dplyr::rename(age = Age)
  expect_s3_class(fc3, "grouped_fbl_vtl")
  expect_identical(dplyr::group_vars(fc3), "Sex")
})

test_that("forecast accepts new_data as a tsibble or with keys in any order", {
  x <- norway_mortality |>
    dplyr::filter(Year > 2015, Sex != "Total", Age < 5)
  fit <- x |> model(fn = FNAIVE(Mortality))
  nd <- tsibble::new_data(x, 2)
  expect_identical(NROW(forecast(fit, new_data = nd)), NROW(nd))
  expect_identical(NROW(generate(fit, new_data = nd, times = 2)), 2L * NROW(nd))
  nd_noage <- nd |>
    tibble::as_tibble() |>
    dplyr::rename(A = Age) |>
    tsibble::as_tsibble(index = Year, key = c(A, Sex))
  expect_error(forecast(fit, new_data = nd_noage), "must contain the age variable")
  x2 <- x |>
    dplyr::mutate(Region = "A") |>
    as_vital(index = Year, key = c(Age, Region, Sex), .age = "Age", .sex = "Sex")
  fit2 <- x2 |> model(fn = FNAIVE(Mortality))
  nd2 <- tsibble::new_data(x2, 2)
  expect_identical(NROW(forecast(fit2, new_data = nd2)), NROW(nd2))
})

test_that("summarise on a vital fable gives a vital", {
  fc <- norway_mortality |>
    dplyr::filter(Year > 2015, Sex != "Total") |>
    model(fn = FNAIVE(Mortality)) |>
    forecast(h = 2)
  s <- fc |>
    dplyr::group_by(Sex) |>
    dplyr::summarise(m = mean(.mean))
  expect_s3_class(s, "vital")
  expect_identical(vital_vars(s), c(sex = "Sex"))
})
