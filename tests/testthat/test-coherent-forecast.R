library(vital)
library(dplyr)
library(ggplot2)

# Regular forecasts
nor <- norway_mortality |>
  filter(Sex != "Total")
fit1 <- nor |>
  model(fdm = FDM(log(Mortality)))
fc1 <- fit1 |>
  forecast(h=20)
fc1 |>
  autoplot() + scale_y_log10()

# Product ratio forecasts
pr <- nor |>
  make_pr(Mortality)
fit2 <- pr |>
  model(fdm = FDM(log(Mortality), coherent = TRUE))
fc_pr <- fit2 |>
  forecast(h=20)
fc_pr |> autoplot()
fc2 <- fc_pr |> undo_pr(Mortality)
fc2 |>
  autoplot() + scale_y_log10()
