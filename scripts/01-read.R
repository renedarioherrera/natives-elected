# Introduction ####
# summarize for areas where native population is great:
# 1. population characteristics 
# 2. voter turn out 
# 3. percent native elected to office 

# setup #### 
# package libraries 
library(here)
library(tidyverse)
library(tidycensus)
library(janitor)
library(knitr)

# Population Demographics ####
## 2020  ####
# available variables to search
var20 <- load_variables(2020, "pl", cache = TRUE)

# for race population 18 years and over; with any American Indian or Native Hawaiian
race_var <- var20 %>%
  filter(concept == "RACE FOR THE POPULATION 18 YEARS AND OVER") %>%
  filter(str_detect(label, "American Indian | Native Hawaiian")) 

### USA ####
# query 2020 census data 
us2020 <- get_decennial(
  geography = "us",
  cache_table = TRUE,
  year = 2020,
  output = "wide",
  variables = c(
    "P3_001N",
    unique(race_var$name)
  )
) %>%
  clean_names()

# percent of American Indian or Native Hawaiian age 18 and over in USA
us2020 %>%
  mutate(
    total = p3_001n,
    subtotal = sum(select(., 4:49))
  ) %>%
  mutate(
    percent_pop = 100 * (subtotal / total)
  ) %>%
  select(1, 2, subtotal, total, percent_pop)

### Congress ####
con2020 <- get_decennial(
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

con2020 <- con2020 %>%
  mutate(
    total = p3_001n,
    subtotal = rowSums(x = select(., 4:49), na.rm = TRUE)
  ) %>%
  mutate(
    percent_pop = 100 * (subtotal / total)
  ) %>%
  select(1, 2, subtotal, total, percent_pop) %>%
  arrange(desc(percent_pop))

## 2010  ####
var10 <- load_variables(2010, "sf1", cache = TRUE)

race_var <- var10 %>%
  filter(concept == "RACE FOR THE POPULATION 18 YEARS AND OVER") %>%
  filter(str_detect(label, "American Indian | Native Hawaiian")) 

### USA ####
# query 2010 census data 
us2010 <- get_decennial(
  geography = "us",
  cache_table = TRUE,
  year = 2010,
  output = "wide",
  variables = c(
    "P010001",
    unique(race_var$name)
  )
) %>%
  clean_names()

# percent of American Indian or Native Hawaiian age 18 and over in USA
us2010 %>%
  mutate(
    total = p010001,
    subtotal = sum(select(., 4:49))
  ) %>%
  mutate(
    percent_pop = 100 * (subtotal / total)
  ) %>%
  select(1, 2, subtotal, total, percent_pop)

### Congress ####
con2010 <- get_decennial(
  geography = "congressional district",
  cache_table = TRUE,
  year = 2010,
  output = "wide",
  variables = c(
    "P010001",
    unique(race_var$name)
  )
) %>%
  clean_names()

con2010 <- con2010 %>%
  mutate(
    total = p010001,
    subtotal = rowSums(x = select(., 4:35), na.rm = TRUE)
  ) %>%
  mutate(
    percent_pop = 100 * (subtotal / total)
  ) %>%
  select(1, 2, subtotal, total, percent_pop) %>%
  arrange(desc(percent_pop))

## 2000 ####
var00 <- load_variables(2000, "sf1", cache = TRUE)

race_var <- var00 %>%
  filter(str_detect(concept, "RACE FOR THE POPULATION 18")) %>%
  filter(str_detect(label, "American Indian | Native Hawaiian")) 

### USA ####
# query 2000 census data 
us2000 <- get_decennial(
  geography = "us",
  cache_table = TRUE,
  year = 2000,
  output = "wide",
  variables = c(
    "P005001",
    unique(race_var$name)
  )
) %>%
  clean_names()

# percent of American Indian or Native Hawaiian age 18 and over in USA
us2000 %>%
  mutate(
    total = p005001,
    subtotal = sum(select(., 4:49))
  ) %>%
  mutate(
    percent_pop = 100 * (subtotal / total)
  ) %>%
  select(1, 2, subtotal, total, percent_pop)

### Congress ####
con2000 <- get_decennial(
  geography = "congressional district",
  cache_table = TRUE,
  year = 2000,
  output = "wide",
  variables = c(
    "P005001",
    unique(race_var$name)
  )
) %>%
  clean_names()

con2000 %>%
  mutate(
    total = p010001,
    subtotal = rowSums(x = select(., 4:35), na.rm = TRUE)
  ) %>%
  mutate(
    percent_pop = 100 * (subtotal / total)
  ) %>%
  select(1, 2, subtotal, total, percent_pop) %>%
  arrange(desc(percent_pop))

# read election data #### 
# source: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/IG0UN2
house_elections <- read_csv(
  file = "../us-census/data-raw/1976-2022-house.csv"
) %>%
  clean_names()

unique(house_elections$year)

house_elections <- house_elections %>%
  filter(year %in% seq(2010, 2022, 2)) %>%
  filter(stage == "GEN") %>%
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

fnct_house10 <- function(x){
  house_elections %>%
    filter(year == x) %>%
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
      y = con2010,
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
      native,
      party,
      candidatevotes,
      totalvotes,
      percent_vote,
      subtotal,
      percent_pop
    ) %>%
    filter(percent_pop >= 5) 
}

fnct_house20 <- function(x){
  house_elections %>%
    filter(year == x) %>%
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
      y = con2020,
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
      native,
      party,
      candidatevotes,
      totalvotes,
      percent_vote,
      subtotal,
      percent_pop
    ) %>%
    filter(percent_pop >= 5) 
}

map(
  .x = seq(2012, 2020, 2),
  .f = ~fnct_house10(.x) %>% distinct(candidate, .keep_all = TRUE) %>% select(candidate, native)
)


map(
  .x = seq(2022, 2030, 2),
  .f = ~fnct_house20(.x) %>% distinct(candidate, .keep_all = TRUE) %>% select(candidate, native)
)


map(
  .x = seq(2012, 2020, 2),
  .f = ~fnct_house10(.x)
) %>%
  bind_rows(map(
    .x = seq(2022, 2030, 2),
    .f = ~fnct_house20(.x)
  )) %>%
  tabyl(year, native) %>%
  adorn_totals(where = c("row", "col")) %>%
  kable(
    col.names = c("Year", "Native Elected", "Non-Native", "Total"),
    caption = "Recent election results for US congressional districts where the Native population 18-years and over is 5% or greater."
  )
  