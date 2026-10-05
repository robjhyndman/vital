test_that("total_fertility_rate accepts bare and quoted variable names", {
  nor <- norway_fertility |>
    dplyr::filter(Year > 2010) |>
    dplyr::mutate(fx2 = Fertility * 2)
  default <- total_fertility_rate(nor)
  expect_equal(total_fertility_rate(nor, Fertility)$tfr, default$tfr)
  expect_equal(total_fertility_rate(nor, "Fertility")$tfr, default$tfr)
  expect_equal(total_fertility_rate(nor, fx2)$tfr, 2 * default$tfr)
})

test_that("total_fertility_rate() reports a missing fertility variable", {
  expect_error(
    total_fertility_rate(dplyr::rename(norway_fertility, F = Fertility)),
    "Fertility variable not found"
  )
})

test_that("total_fertility_rate weights rates by the width of age groups", {
  f <- norway_fertility |>
    dplyr::filter(Year == 2000, Age >= 15, Age < 50)
  f5 <- f |>
    tibble::as_tibble() |>
    dplyr::mutate(Age = 5 * (Age %/% 5)) |>
    dplyr::summarise(Fertility = mean(Fertility), .by = c(Year, Age)) |>
    as_vital(index = Year, key = Age, .age = "Age")
  expect_equal(
    total_fertility_rate(f5)$tfr,
    total_fertility_rate(f)$tfr
  )
})

test_that("total_fertility_rate() requires an age variable", {
  expect_error(
    total_fertility_rate(as_vital(norway_fertility, .age = NULL)),
    "No age variable identified"
  )
})
