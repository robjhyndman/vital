library(tidyverse)
library(tidylife)

aus_fertility <- as_life_tsibble(addb::aus.fertility) |>
  as_tibble() |>
  select(-AgeGroup, -Sex) |>
  as_tsibble(index = Year, key = Age) |>
  as_life_tsibble(births = "Births", age = "Age", population = "Exposure")
usethis::use_data(aus_fertility, overwrite = TRUE)

aus_mortality <- bind_rows(
  as_life_tsibble(addb::australia) |> mutate(State = "Australia", Code = "AUS") |> as_tibble(),
  as_life_tsibble(addb::nsw) |> mutate(State = "New South Wales", Code = "NSW") |> as_tibble(),
  as_life_tsibble(addb::vic) |> mutate(State = "Victoria", Code = "VIC") |> as_tibble(),
  as_life_tsibble(addb::qld) |> mutate(State = "Queensland", Code = "QLD") |> as_tibble(),
  as_life_tsibble(addb::sa) |> mutate(State = "South Australia", Code = "SA") |> as_tibble(),
  as_life_tsibble(addb::nt) |> mutate(State = "Northern Territory", Code = "NT") |> as_tibble(),
  as_life_tsibble(addb::actot) |> mutate(State = "Australian Capital Territory and Overseas Territories", Code = "ACTOT") |> as_tibble(),
  as_life_tsibble(addb::tas) |> mutate(State = "Tasmania", Code = "TAS") |> as_tibble(),
  as_life_tsibble(addb::wa) |> mutate(State = "Western Australia", Code = "WA") |> as_tibble()
) |>
  select(-AgeGroup) |>
  as_tsibble(index = Year, key = c(Age, Sex, State, Code)) |>
  select(Year, Age, Sex, State, Code, everything()) |>
  arrange(State, Sex, Year, Age) |>
  as_life_tsibble(age = "Age", sex = "Sex", deaths = "Deaths", population = "Exposure")
usethis::use_data(aus_mortality, overwrite = TRUE)
