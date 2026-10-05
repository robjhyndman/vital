test_that("life_table() requires mortality rates", {
  expect_error(norway_fertility |> life_table(), "Mortality variable not found")
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

# Aggregate single-year Norwegian data into the given age groups
group_ages <- function(data, groups) {
  data |>
    tibble::as_tibble() |>
    dplyr::mutate(Age = groups[findInterval(Age, groups)]) |>
    dplyr::summarise(
      Deaths = sum(Deaths),
      Population = sum(Population),
      .by = c(Year, Sex, Age)
    ) |>
    dplyr::mutate(Mortality = Deaths / Population) |>
    as_vital(
      index = Year,
      key = c(Age, Sex),
      .age = "Age",
      .sex = "Sex",
      .deaths = "Deaths",
      .population = "Population"
    )
}

test_that("life_table handles abridged age groups", {
  nor <- norway_mortality |>
    dplyr::filter(Year == 2000, Sex == "Female")
  abridged <- group_ages(nor, c(0, 1, seq(5, 100, by = 5)))
  lt <- life_table(abridged)
  expect_equal(lt$nx, c(1, 4, rep(5, 19), Inf))
  # Close to the single-year life expectancy
  expect_lt(abs(lt$ex[1] - life_table(nor)$ex[1]), 0.2)
})

test_that("life_table handles 5-year age groups above age 5", {
  nor <- norway_mortality |>
    dplyr::filter(Year == 2000, Sex == "Female", Age >= 20)
  lt <- life_table(group_ages(nor, seq(20, 100, by = 5)))
  expect_equal(lt$nx, c(rep(5, 16), Inf))
  expect_equal(lt$ax, c(rep(2.6, 16), Inf))
  expect_lt(abs(lt$ex[1] - life_table(nor)$ex[1]), 0.2)
})

test_that("life_table rejects 5-year age groups without an infant group", {
  nor <- norway_mortality |>
    dplyr::filter(Year == 2000, Sex == "Female")
  expect_error(life_table(group_ages(nor, seq(0, 100, by = 5))), "separate")
})

test_that("life_expectancy returns only the index, keys and ex", {
  e0 <- norway_mortality |>
    dplyr::filter(Year == 2000) |>
    life_expectancy()
  expect_identical(colnames(e0), c("Year", "Age", "Sex", "ex"))
})

test_that("life_table() interpolates missing rates from neighbouring ages", {
  x <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year == 2010)
  x$Mortality[x$Age == 49] <- NA
  expect_warning(lt <- life_table(x), "interpolated")
  m <- x$Mortality[x$Age %in% c(48, 50)]
  expect_equal(lt$mx[lt$Age == 49], sqrt(prod(m)))
  expect_false(anyNA(lt$ex))
})

test_that("life_table caps qx at 1 when mortality rates are very high", {
  lt <- norway_mortality |>
    dplyr::filter(Year == 1900, Sex == "Female") |>
    life_table()
  expect_true(all(lt$qx <= 1))
  expect_true(all(lt$dx >= 0))
  expect_true(all(diff(lt$lx) <= 0))
  expect_false(any(is.infinite(lt$ex)))
})

test_that("life_table works for short abridged tables", {
  mk <- function(ages) {
    vital(
      tibble::tibble(Year = 2000L, Age = ages, Mortality = 0.01 * seq_along(ages)),
      index = Year,
      key = Age,
      .age = "Age"
    )
  }
  lt3 <- life_table(mk(c(0, 1, 5)))
  expect_equal(lt3$rx[3], lt3$Tx[3] / lt3$Tx[1])
  lt4 <- life_table(mk(c(0, 1, 5, 10)))
  expect_equal(lt4$rx[3], lt4$Lx[3] / (lt4$Lx[1] + lt4$Lx[2]))
  expect_equal(lt4$rx[4], lt4$Tx[4] / lt4$Tx[3])
})

test_that("life_expectancy warns about ages not in the data", {
  nor <- norway_mortality |>
    dplyr::filter(Year == 2000, Sex == "Female")
  expect_warning(
    e <- life_expectancy(nor, from_age = c(65, 200)),
    "Ages not in the data are ignored: 200"
  )
  expect_identical(NROW(e), 1L)
})
