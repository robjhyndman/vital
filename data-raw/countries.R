# Maintain a country look up table listing the countries on the various data bases.
# Needs to be updated from time to time as new countries are added.

library(rvest)
library(dplyr)

# HMD countries
hmd <- read_html("https://mortality.org/Data/DataAvailability") |>
  html_element(xpath = "/html/body/div/div/div[2]") |>
  html_table() |>
  select(Country = `Country and data series`, hmd_code = `HMD Country Code`) |>
  filter(hmd_code != "")
# Fix some names
hmd <- hmd |>
  mutate(
    Country = case_when(
      hmd_code == "FRATNP" ~ "France",
      hmd_code == "FRACNP" ~ "France (civilian)",
      hmd_code == "DEUTNP" ~ "Germany",
      hmd_code == "NZL_NP" ~ "New Zealand",
      hmd_code == "NZL_MA" ~ "New Zealand (Maori)",
      hmd_code == "NZL_NM" ~ "New Zealand (non Maori)",
      hmd_code == "GBR_NP" ~ "United Kingdom",
      hmd_code == "GBRTENW" ~ "England & Wales",
      hmd_code == "GBRCENW" ~ "England & Wales (civilian)",
      TRUE ~ Country
    )
  )

# STMF countries
stmf_html <- read_html("https://www.mortality.org/Data/STMF") |>
  html_element(xpath = "/html/body/div[1]/div/div/div/div/div[2]/table/tbody") |>
  html_elements("a")
stmf <- tibble(
  Country = html_text2(stmf_html),
  stmf_code = html_attr(stmf_html, "href") |>
    stringr::str_extract("[A-Z\\_]*stmfout.csv$") |>
    stringr::str_remove("stmfout.csv")
)

# KTDB countries
ktdb_html <- read_html("https://www.demogr.mpg.de/cgi-bin/databases/ktdb/datamap.plx") |>
  html_elements(xpath = "/html/body") |>
  html_elements("a")
ktdb <- tibble(
  Country = html_text2(ktdb_html)[8:42],
  ktdb_number = seq_along(Country)
)

# Combine data frames, completing missing entries due to name variations

countries <- hmd |>
  full_join(stmf, by = "Country") |>
  full_join(ktdb, by = "Country") |>
  # Add some alternative names
  mutate(
    hmd_code = case_when(
      Country == "Czech Republic" ~ hmd$hmd_code[hmd$Country == "Czechia"],
      Country == "England and Wales" ~ hmd$hmd_code[hmd$Country == "England & Wales"],
      Country == "Germany East" ~ hmd$hmd_code[hmd$Country == "East Germany"],
      Country == "Germany West" ~ hmd$hmd_code[hmd$Country == "West Germany"],
      Country == "Luxemburg" ~ hmd$hmd_code[hmd$Country == "Luxembourg"],
      Country == "S.Korea" ~ hmd$hmd_code[hmd$Country == "Republic of Korea"],
      Country == "USA" ~ hmd$hmd_code[hmd$Country == "U.S.A."],
      TRUE ~ hmd_code
    ),
    stmf_code = case_when(
      Country == "Czechia" ~ stmf$stmf_code[stmf$Country == "Czech Republic"],
      Country == "England & Wales" ~ stmf$stmf_code[stmf$Country == "England and Wales"],
      Country == "Luxemburg" ~ stmf$stmf_code[stmf$Country == "Luxembourg"],
      Country == "Republic of Korea" ~ stmf$stmf_code[stmf$Country == "S.Korea"],
      Country == "U.S.A." ~ stmf$stmf_code[stmf$Country == "USA"],
      TRUE ~ stmf_code
    ),
    ktdb_number = case_when(
      Country == "Czechia" ~ ktdb$ktdb_number[ktdb$Country == "Czech Republic"],
      Country == "England and Wales" ~ ktdb$ktdb_number[ktdb$Country == "England & Wales"],
      Country == "East Germany" ~ ktdb$ktdb_number[ktdb$Country == "Germany East"],
      Country == "West Germany" ~ ktdb$ktdb_number[ktdb$Country == "Germany West"],
      Country == "Luxembourg" ~ ktdb$ktdb_number[ktdb$Country == "Luxemburg"],
      Country == "U.S.A." ~ ktdb$ktdb_number[ktdb$Country == "USA"],
      TRUE ~ ktdb_number
    )
  ) |>
  arrange(hmd_code)

# Save for internal use only
usethis::use_data(countries, overwrite = TRUE, internal = TRUE)
