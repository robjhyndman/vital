test_that("ausfertility", {
  expect_error(aus_fertility |> life_table())
})

test_that("ausmortality", {
  lt <- aus_mortality |>
    dplyr::filter(Year == 1950, Sex == "male", Code == "WA") |>
    life_table()
  expect_true(abs(lt$ex[10] - 60.05) < 1e-3)
})

