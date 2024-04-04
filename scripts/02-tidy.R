# package libraries
library(here)
library(tidyverse)
library(tidycensus)
library(janitor)

# options

var20 <- load_variables(2020, "pl", cache = TRUE)

race_var <- var20 %>%
  filter(concept == "RACE FOR THE POPULATION 18 YEARS AND OVER") %>%
  filter(str_detect(label, "American Indian and Alaska Native")) %>%
  select(name)

# united states
us2020 <- get_decennial(
  geography = "us",
  cache_table = TRUE,
  year = 2020,
  output = "wide",
  variables = c(
    "P3_001N",
    unique(race_var$name)
  )
  # variables = c(
  #   "Total" = "P1_001N",
  #   "P1_005N", # American Indian and Alaska Native alone
  #   "P1_007N", # Native Hawaiian and Other Pacific Islander alone
  #   "P1_012N", # White; American Indian and Alaska Native
  #   "P1_014N", # White; Native Hawaiian and Other Pacific Islander
  #   ""
  # )
) %>%
  clean_names()

us2020 %>%
  mutate(
    subtotal = sum(select(., 4:35))
  ) %>%
  mutate(
    percent_pop = 100 * (subtotal / p3_001n)
  ) %>%
  select(1, 2, subtotal, total = p3_001n, percent_pop)

# query Federal and state-recognized American Indian reservations and Hawaiian home lands
fed_state <- get_decennial(
  geography = "american indian area/alaska native area/hawaiian home land",
  cache_table = TRUE,
  year = 2020,
  output = "wide",
  variables = c(
    "P3_001N",
    unique(race_var$name)
  )
) %>%
  clean_names()

fed_state %>%
  mutate(
    subtotal = rowSums(x = select(., 4:35), na.rm = TRUE)
  ) %>%
  mutate(
    percent_pop = 100 * (subtotal / p3_001n)
  ) %>%
  select(1, 2, subtotal, total = p3_001n, percent_pop) %>%
  arrange(desc(subtotal))

# Only reservations and statistical entities
reservations <- get_decennial(
  geography = "american indian area/alaska native area (reservation or statistical entity only)",
  cache_table = TRUE,
  year = 2020,
  output = "wide",
  variables = c(
    "P3_001N",
    unique(race_var$name)
  )
) %>%
  clean_names()

reservations %>%
  mutate(
    subtotal = rowSums(x = select(., 4:35), na.rm = TRUE)
  ) %>%
  mutate(
    percent_pop = 100 * (subtotal / p3_001n)
  ) %>%
  select(1, 2, subtotal, total = p3_001n, percent_pop) %>%
  arrange(desc(subtotal))

# Only off-reservation trust lands and Hawaiian home lands
off_reservations <- get_decennial(
  geography = "american indian area (off-reservation trust land only)/hawaiian home land",
  cache_table = TRUE,
  year = 2020,
  output = "wide",
  variables = c(
    "P3_001N",
    unique(race_var$name)
  )
) %>%
  clean_names()

off_reservations %>%
  mutate(
    subtotal = rowSums(x = select(., 4:35), na.rm = TRUE)
  ) %>%
  mutate(
    percent_pop = 100 * (subtotal / p3_001n)
  ) %>%
  select(1, 2, subtotal, total = p3_001n, percent_pop) %>%
  arrange(desc(subtotal))

# Congressional district for the year-appropriate Congress
congress <- get_decennial(
  geography = "congressional district",
  cache_table = TRUE,
  year = 2020,
  output = "wide",
  variables = c(
    "P3_001N",
    unique(race_var$name)
  )
) %>%
  clean_names()

congress <- congress %>%
  mutate(
    subtotal = rowSums(x = select(., 4:35), na.rm = TRUE)
  ) %>%
  mutate(
    percent_pop = 100 * (subtotal / p3_001n)
  ) %>%
  select(1, 2, subtotal, total = p3_001n, percent_pop) %>%
  arrange(desc(percent_pop))

# read election data 
house_elections <- read_csv(
  file = "data-raw/1976-2022-house.csv"
) %>%
  clean_names()

native_house_elections <- house_elections %>%
  filter(year == 2020 | year == 2022) %>%
  mutate(state_fips = if_else(
    condition = state_fips < 10,
    true = str_c("0", state_fips),
    false = as.character(state_fips)
  )) %>%
  mutate(
    geoid = str_c(
      state_fips, 
      str_extract(district, "[:digit:]{2}$")
    )) %>%
  left_join(
    y = congress,
    by = "geoid"
  ) %>%
  mutate(
    percent_vote = 100 * (candidatevotes / totalvotes)
  ) %>%
  group_by(geoid, year) %>%
  slice_max(percent_vote) %>%
  ungroup() %>%
  arrange(desc(subtotal)) %>%
  select(
    year, 
    state,
    name,
    candidate,
    party,
    candidatevotes,
    totalvotes,
    percent_vote,
    subtotal,
    percent_pop
  ) %>%
  filter(percent_pop >= 5) %>%
  mutate(
    native = case_when(
      candidate == "MARKWAYNE MULLIN" ~ "Native Elected",
      candidate == "DEBRA A HAALAND" ~ "Native Elected",
      candidate == "TOM COLE" ~ "Native Elected",
      candidate == "YVETTE HERRELL" ~ "Native Elected",
      candidate == "JOSH BRECHEEN" ~ "Native Elected",
      candidate == "MARY SATTLER PELTOLA" ~ "Native Elected",
      candidate == "DAN BOREN" ~ "Native Elected",
      candidate == "DANIEL K. AKAKA" ~ "Native Elected",
      candidate == "FAITH EVANS" ~ "Native Elected",
      candidate == "BRICKWOOD GALUTERIA" ~ "Native Elected",
      candidate == "KAIALI'I KAHELE" ~ "Native Elected",
      candidate == "KAIALII \"KAI\" KAHELE" ~ "Native Elected",
      TRUE ~ "Not"
    )) 

native_house_elections %>%
  arrange(desc(percent_pop)) %>%
  print(n = 41)

native_house_elections %>%
  tabyl(native, year) %>%
  adorn_totals()

native_house_elections %>%
  tabyl(state, native) %>%
  adorn_totals()
