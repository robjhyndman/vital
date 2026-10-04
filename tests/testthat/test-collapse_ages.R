library(dplyr)

# Check collapse_ages

test_that("collapse_ages", {
  # Compare against demography
  if (requireNamespace("demography", quietly = TRUE)) {
    library(demography)
    up1 <- set.upperage(fr.mort, max.age = 100)$rate$female[101, ]
    up2 <- as_vital(fr.mort) |>
      filter(Sex == "female") |>
      collapse_ages(max_age = 100) |>
      filter(Age == 100) |>
      pull(Mortality)
    expect_true(sum(abs(up1 - up2)) == 0)
    # Check that collapse ages works without population data
    expect_warning(
      as_vital(fr.mort) |>
        select(Year:Mortality) |>
        collapse_ages()
    )
    # Check that collapse ages handles OpenInterval column from HMD
    expect_warning(read_hmd_files("Mx_1x1.txt") |> collapse_ages())
  }
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
