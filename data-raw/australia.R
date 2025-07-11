library(tidyverse)
library(vital)
library(tsibble)

aus_fertility <- as_vital(addb::aus.fertility) |>
  as_tibble() |>
  select(-AgeGroup, -Sex) |>
  mutate(Fertility = Fertility / 1000) |>
  as_tsibble(index = Year, key = Age) |>
  as_vital(
    .births = "Births",
    .age = "Age",
    .population = "Exposure",
    reorder = TRUE
  )
usethis::use_data(aus_fertility, overwrite = TRUE)

aus_mortality <- bind_rows(
  as_vital(addb::nsw) |>
    mutate(State = "New South Wales", Code = "NSW") |>
    as_tibble(),
  as_vital(addb::vic) |>
    mutate(State = "Victoria", Code = "VIC") |>
    as_tibble(),
  as_vital(addb::qld) |>
    mutate(State = "Queensland", Code = "QLD") |>
    as_tibble(),
  as_vital(addb::sa) |>
    mutate(State = "South Australia", Code = "SA") |>
    as_tibble(),
  as_vital(addb::nt) |>
    mutate(State = "Northern Territory", Code = "NT") |>
    as_tibble(),
  as_vital(addb::actot) |>
    mutate(
      State = "Australian Capital Territory and Overseas Territories",
      Code = "ACTOT"
    ) |>
    as_tibble(),
  as_vital(addb::tas) |>
    mutate(State = "Tasmania", Code = "TAS") |>
    as_tibble(),
  as_vital(addb::wa) |>
    mutate(State = "Western Australia", Code = "WA") |>
    as_tibble()
) |>
  select(-AgeGroup) |>
  as_tsibble(index = Year, key = c("Age", "Sex", "State", "Code")) |>
  select(Year, Age, Sex, State, Code, everything()) |>
  arrange(State, Sex, Year, Age) |>
  as_vital(
    .age = "Age",
    .sex = "Sex",
    .deaths = "Deaths",
    .population = "Exposure"
  )

# Updated data

get_aus_mortality <- function(code) {
  deaths <- read.table(
    paste0(
      "https://aushd.org/assets/txtFiles/humanMortality/",
      code,
      "/Deaths_1x1.txt"
    ),
    skip = 2,
    header = TRUE
  ) |>
    as_tibble() |>
    tidyr::pivot_longer(Female:Total, names_to = "Sex", values_to = "Deaths") |>
    mutate(Code = code)
  read.table(
    paste0(
      "https://aushd.org/assets/txtFiles/humanMortality/",
      code,
      "/Exposures_1x1.txt"
    ),
    skip = 2,
    header = TRUE
  ) |>
    as_tibble() |>
    tidyr::pivot_longer(
      Female:Total,
      names_to = "Sex",
      values_to = "Exposure"
    ) |>
    left_join(deaths) |>
    mutate(
      Mortality = Deaths / Exposure,
      Age = stringr::str_remove(Age, "\\+"),
      Age = parse_integer(Age)
    ) |>
    select(Year, Age, Sex, Code, Mortality, Exposure, Deaths)
}

state_names <- aus_mortality |>
  as_tibble() |>
  select(State, Code) |>
  distinct() |>
  bind_rows(
    tribble(
      ~State,
      ~Code,
      "Australian Capital Territory",
      "ACT"
    )
  )

aus_mort <- bind_rows(
  get_aus_mortality("ACT"),
  get_aus_mortality("NSW"),
  get_aus_mortality("NT"),
  get_aus_mortality("QLD"),
  get_aus_mortality("SA"),
  get_aus_mortality("TAS"),
  get_aus_mortality("VIC"),
  get_aus_mortality("WA"),
) |>
  mutate(Sex = tolower(Sex)) |>
  left_join(state_names) |>
  select(Year, Age, Sex, State, Code, everything())

aus_mortality <- aus_mortality |>
  filter(
    Year < min(aus_mort$Year) | Code == "ACTOT",
    Code != "AUS"
  ) |>
  bind_rows(aus_mort) |>
  arrange(Sex, Year, Code, Age) |>
  mutate(Mortality = if_else(Deaths == 0 & Exposure == 0, 0, Mortality)) |>
  as_vital(
    index = Year,
    key = c(Age, Sex, Code, State),
    .age = "Age",
    .sex = "Sex",
    .deaths = "Deaths",
    .population = "Exposure",
    reorder = TRUE
  ) |>
  collapse_ages()

usethis::use_data(aus_mortality, overwrite = TRUE)
