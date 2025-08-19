# Prepare data
nor <- norway_mortality |>
  filter(Sex != "Total", Year > 2000) |>
  collapse_ages() |>
  smooth_mortality(Mortality)

# Regular forecasts
fit1 <- nor |>
  model(fdm = FDM(log(.smooth)))
fc1 <- fit1 |>
  forecast(h = 20)

# Product ratio forecasts
fit2 <- nor |>
  make_pr(.smooth) |>
  model(fdm = FDM(log(.smooth), coherent = TRUE))
fc2 <- fit2 |>
  forecast(h = 20) |>
  undo_pr(.smooth) |>
  as_tibble() |>
  mutate(prmean = .mean) |>
  select(Sex, .model, Year, Age, prmean)

test_that("Coherent forecasts", {
  # Check they are similar order of magnitude
  fc1 <- fc1 |>
    as_tibble() |>
    select(-.smooth) |>
    left_join(fc2, by = c("Sex", ".model", "Year", "Age")) |>
    mutate(diff = abs(.mean - prmean))
  expect_lt(mean(fc1$diff), 0.0021)
})
