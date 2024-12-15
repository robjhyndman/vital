library(dplyr)

# HMD Norwegian data as downloaded from mortality.org on 21 April 2024
# Only include data from 1900 onwards to keep size down
norway_mortality <- read_hmd_files(here::here("data-raw", c("Population.txt", "Mx_1x1.txt"))) |>
  mutate(
    Mortality = if_else(is.na(Mortality) & Population == 0, 0, Mortality)
  ) |>
  filter(Year >= 1900)
norway_births <- read_hmd_files(here::here("data-raw", "Births.txt")) |>
  filter(Year >= 1900)
norway_fertility <- read_hfd_files(here::here("data-raw", "NORasfrRR.txt")) |>
  rename(Fertility = ASFR) |>
  filter(Year >= 1900)

usethis::use_data(norway_mortality, overwrite = TRUE)
usethis::use_data(norway_births, overwrite = TRUE)
usethis::use_data(norway_fertility, overwrite = TRUE)
