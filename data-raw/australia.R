library(tidyverse)
library(tidylife)
aus_fertility <- as_tsibble(addb::aus.fertility) |>
  as_tibble() |>
  select(-AgeGroup, -Group) |>
  as_tsibble(index = Year, key = Age)
usethis::use_data(aus_fertility, overwrite = TRUE)

aus_mortality <- bind_rows(
  as_tsibble(addb::australia) |> mutate(State = "Australia", Code = "AUS") |> as_tibble(),
  as_tsibble(addb::nsw) |> mutate(State = "New South Wales", Code = "NSW") |> as_tibble(),
  as_tsibble(addb::vic) |> mutate(State = "Victoria", Code = "VIC") |> as_tibble(),
  as_tsibble(addb::qld) |> mutate(State = "Queensland", Code = "QLD") |> as_tibble(),
  as_tsibble(addb::sa) |> mutate(State = "South Australia", Code = "SA") |> as_tibble(),
  as_tsibble(addb::nt) |> mutate(State = "Northern Territory", Code = "NT") |> as_tibble(),
  as_tsibble(addb::actot) |> mutate(State = "Australian Capital Territory and Overseas Territories", Code = "ACTOT") |> as_tibble(),
  as_tsibble(addb::tas) |> mutate(State = "Tasmania", Code = "TAS") |> as_tibble(),
  as_tsibble(addb::wa) |> mutate(State = "Western Australia", Code = "WA") |> as_tibble()
) |>
  select(-AgeGroup) |>
  as_tsibble(index = Year, key = c(Age, Group, State, Code)) |>
  select(Year, Age, Group, State, Code, everything()) |>
  arrange(State, Group, Year, Age) |>
  rename(Sex = Group)
usethis::use_data(aus_mortality, overwrite = TRUE)
