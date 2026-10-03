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
