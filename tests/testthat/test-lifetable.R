test_that("norfertility", {
  expect_error(norway_fertility |> life_table())
})

test_that("normortality", {
  lt <- norway_mortality |>
    dplyr::filter(Year == 1950, Sex == "Male") |>
    life_table()
  expect_lt(abs(lt$ex[10] - 63.97662), 1e-3)
})
