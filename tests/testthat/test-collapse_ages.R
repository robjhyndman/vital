# Check collapse_ages

test_that("collapse_ages", {
  # Compare against demography
  library(demography)
  library(dplyr)
  up1 <- set.upperage(fr.mort, max.age = 100)$rate$female[101,]
  up2 <- as_vital(fr.mort) |>
    filter(Sex == "female") |>
    collapse_ages(max_age = 100) |>
    filter(Age == 100) |>
    pull(Mortality)
  expect_true(sum(abs(up1-up2)) == 0)
})
