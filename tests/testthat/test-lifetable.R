test_that("ausfertility", {
  expect_error(aus_fertility |> life_table())
})

test_that("normortality", {
  lt <- norway_mortality |>
    dplyr::filter(Year == 1950, Sex == "Male") |>
    life_table()
  expect_lt(abs(lt$ex[10] - 60.05), 1e-3)
})
