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
