# HMD Norwegian data as downloaded from mortality.org on 21 April 2024
norway_mortality <- read_hmd_files(here::here("data-raw",c("Population.txt", "Mx_1x1.txt")))
norway_births <- read_hmd_files(here::here("data-raw","Births.txt"))
norway_fertility <- read_hfd_files(here::here("data-raw","NORasfrRR.txt")) |>
  rename(Fertility = ASFR)

usethis::use_data(norway_mortality, overwrite = TRUE)
usethis::use_data(norway_births, overwrite = TRUE)
usethis::use_data(norway_fertility, overwrite = TRUE)

