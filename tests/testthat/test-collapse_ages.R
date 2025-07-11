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
