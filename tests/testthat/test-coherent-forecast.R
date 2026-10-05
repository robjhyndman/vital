library(dplyr)

test_that("Coherent forecasts are close to independent forecasts", {
  nor <- norway_mortality |>
    filter(Sex != "Total", Year > 2000) |>
    collapse_ages() |>
    smooth_mortality(Mortality)
  # Regular forecasts
  fc1 <- nor |>
    model(fdm = FDM(log(.smooth))) |>
    forecast(h = 20)
  # Product ratio forecasts
  fc2 <- nor |>
    make_pr(.smooth) |>
    model(fdm = FDM(log(.smooth), coherent = TRUE)) |>
    forecast(h = 20) |>
    undo_pr(.smooth) |>
    as_tibble() |>
    mutate(prmean = .mean) |>
    select(Sex, .model, Year, Age, prmean)
  # Check they are similar order of magnitude
  both <- fc1 |>
    as_tibble() |>
    select(-.smooth) |>
    left_join(fc2, by = c("Sex", ".model", "Year", "Age"))
  expect_identical(NROW(both), NROW(fc1))
  expect_false(anyNA(both$prmean))
  expect_lt(mean(abs(both$.mean - both$prmean)), 0.0021)
})
