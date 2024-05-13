library(vital)
library(dplyr)
library(ggplot2)

# Regular forecasts
nor <- norway_mortality |>
  filter(Sex != "Total") |>
  collapse_ages()
fit1 <- nor |>
  model(fdm = FDM(log(Mortality)))
autoplot(fit1)
fc1 <- fit1 |>
  forecast(h=20)
fc1 |>
  autoplot() + scale_y_log10()

# Product ratio forecasts
pr <- nor |>
  make_pr(Mortality)
nor |> autoplot() + scale_y_log10()
pr |> autoplot(Mortality) + scale_y_log10()

fit2 <- pr |>
  model(fdm = FDM(log(Mortality), coherent = TRUE))
autoplot(fit2)
fc_pr <- fit2 |>
  forecast(h=20)
fc_pr |> autoplot() + scale_y_log10()
fc2 <- fc_pr |> undo_pr(Mortality)
fc2 |>
  autoplot() + scale_y_log10()
