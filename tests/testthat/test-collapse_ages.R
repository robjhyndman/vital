library(dplyr)

# Check collapse_ages

test_that("collapse_ages matches demography::set.upperage()", {
  skip_if_not_installed("demography")
  fr <- demography::fr.mort
  up1 <- demography::set.upperage(fr, max.age = 100)$rate$female[101, ]
  up2 <- as_vital(fr) |>
    filter(Sex == "female") |>
    collapse_ages(max_age = 100) |>
    filter(Age == 100) |>
    pull(Mortality)
  expect_equal(unname(up1), up2)
  # Without population data, the rate at the upper age is kept
  expect_warning(
    as_vital(fr) |>
      select(Year:Mortality) |>
      collapse_ages(),
    "Cannot recompute rates for Mortality"
  )
})

test_that("collapse_ages keeps rates when only rates are available", {
  # The HMD Mx file has no deaths or population
  expect_warning(
    read_hmd_files("Mx_1x1.txt") |> collapse_ages(),
    "Cannot recompute rates for Mortality. Using upper age value"
  )
})

test_that("collapse_ages works with abridged age groups", {
  ab <- norway_mortality |>
    dplyr::filter(
      Year == 2000,
      Sex == "Female",
      Age %in% c(0, 1, seq(5, 110, by = 5))
    )
  x <- collapse_ages(ab, max_age = 85)
  expect_identical(max(x$Age), 85L)
  top <- x |> dplyr::filter(Age == 85)
  upper <- ab |> dplyr::filter(Age >= 85)
  expect_equal(top$Deaths, sum(upper$Deaths))
  expect_equal(top$Mortality, sum(upper$Deaths) / sum(upper$Population))
})

test_that("collapse_ages keeps the open interval at the top age", {
  hmd <- read_hmd_files(test_path("Mx_1x1.txt"))
  top <- max(hmd$Age)
  for (m in c(100, top)) {
    out <- collapse_ages(hmd, max_age = m) |> suppressWarnings()
    expect_true(all(out$OpenInterval[out$Age == m]))
    expect_false(any(out$OpenInterval[out$Age < m]))
  }
})

test_that("collapse_ages sums counts that change linearly with age", {
  nor <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year == 2000) |>
    dplyr::mutate(Count = 200 - Age)
  out <- collapse_ages(nor, max_age = 85)
  expect_identical(max(out$Age), 85L)
  expect_equal(
    out$Count[out$Age == 85],
    sum(nor$Count[nor$Age >= 85])
  )
})

test_that("collapse_ages() checks max_age and labels open groups once", {
  nf <- norway_mortality |>
    dplyr::filter(Sex == "Female", Year > 2015)
  expect_error(collapse_ages(nf, max_age = 120), "max_age must be one of")
  expect_error(
    collapse_ages(dplyr::filter(nf, Age %in% c(0, 5, 10)), max_age = 7),
    "max_age must be one of"
  )
  labelled <- nf |>
    dplyr::mutate(AG = dplyr::if_else(Age == 110, "110+", as.character(Age)))
  expect_identical(
    unique(collapse_ages(labelled, max_age = 110)$AG[nf$Age == 110]),
    "110+"
  )
  expect_identical(
    unique(collapse_ages(labelled, max_age = 100)$AG),
    c(as.character(0:99), "100+")
  )
})

test_that("collapse_ages() sums count columns even when they are constant over age", {
  x <- norway_mortality |>
    filter(Sex == "Female", Year == 2010) |>
    mutate(Deaths = 1, Extra = 2)
  out <- collapse_ages(x, max_age = 100)
  n_upper <- sum(x$Age >= 100)
  expect_equal(out$Deaths[out$Age == 100], n_upper)
  expect_equal(out$Extra[out$Age == 100], 2 * n_upper)
  expect_equal(out$Mortality[out$Age == 100], n_upper / sum(x$Population[x$Age >= 100]))
})

test_that("collapse_ages works when a group is missing an age", {
  x <- norway_mortality |>
    dplyr::filter(Year %in% 1999:2000, Sex == "Female")
  full <- collapse_ages(x, max_age = 100)
  gap <- collapse_ages(
    x |> dplyr::filter(!(Age == 50 & Year == 2000)),
    max_age = 100
  )
  expect_equal(
    gap |> dplyr::filter(Age == 100),
    full |> dplyr::filter(Age == 100)
  )
})

test_that("collapse_ages recomputes rates from population when counts are missing", {
  x <- norway_mortality |>
    filter(Sex == "Female", Year > 2015) |>
    select(-Deaths) |>
    as_vital(.deaths = NULL)
  out <- collapse_ages(x, max_age = 90)
  top <- x |> filter(Age >= 90, Year == 2016)
  expect_equal(
    out$Mortality[out$Age == 90 & out$Year == 2016],
    sum(top$Mortality * top$Population) / sum(top$Population)
  )
  # The temporary counts are not returned
  expect_identical(colnames(out), colnames(x))
  # Fertility rates use births computed in the same way
  f <- norway_fertility |>
    filter(Year == 2000) |>
    mutate(Population = 1000 + Age) |>
    as_vital(.population = "Population")
  out_f <- collapse_ages(f, max_age = 45)
  top_f <- f |> filter(Age >= 45)
  expect_equal(
    out_f$Fertility[out_f$Age == 45],
    sum(top_f$Fertility * top_f$Population) / sum(top_f$Population)
  )
})

test_that("collapse_ages requires a vital", {
  expect_error(collapse_ages(tibble::tibble(Age = 1:3)), "needs to be a vital object")
})
