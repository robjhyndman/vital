test_that("norfertility", {
  expect_error(norway_fertility |> life_table())
})

test_that("normortality", {
  lt <- norway_mortality |>
    dplyr::filter(Year == 1950, Sex == "Male") |>
    life_table()
  expect_lt(abs(lt$ex[10] - 63.97662), 1e-3)
})

test_that("life_expectancy uses the mortality argument", {
  nor <- norway_mortality |>
    dplyr::filter(Year == 2000) |>
    dplyr::mutate(mx2 = Mortality * 2)
  e_double <- life_expectancy(nor, mortality = mx2)
  expect_equal(
    e_double$ex,
    life_expectancy(nor |> dplyr::mutate(Mortality = mx2))$ex
  )
  expect_equal(
    life_expectancy(nor, mortality = "mx2")$ex,
    e_double$ex
  )
  expect_equal(
    life_table(nor, mortality = mx2)$ex,
    life_table(nor |> dplyr::mutate(Mortality = mx2))$ex
  )
})

test_that("life_expectancy works with any age variable name", {
  nor <- norway_mortality |>
    dplyr::filter(Year == 2000)
  nor_lower <- nor |>
    tibble::as_tibble() |>
    dplyr::rename(age = Age) |>
    as_vital(
      index = Year,
      key = c(age, Sex),
      .age = "age",
      .sex = "Sex",
      .deaths = "Deaths",
      .population = "Population"
    )
  expect_equal(
    life_expectancy(nor_lower, from_age = 65)$ex,
    life_expectancy(nor, from_age = 65)$ex
  )
})

test_that("life_table uses sex-specific a0 regardless of case", {
  nor <- norway_mortality |>
    dplyr::filter(Year == 1950, Sex != "Total")
  nor_lower <- nor |>
    dplyr::mutate(Sex = tolower(Sex))
  expect_equal(life_table(nor)$ax, life_table(nor_lower)$ax)
})
