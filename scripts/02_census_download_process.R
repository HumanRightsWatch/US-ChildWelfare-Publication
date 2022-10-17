#
# Authors:     BR
# Maintainers: BR
# Copyright:   2022
# =========================================
# 

library(pacman)
p_load(lubridate, readxl, readr, rcartocolor, extrafont, scales, tidycensus,
       CGPfunctions, tidyverse)
options(scipen=999)

here <- here::here

source("../plot_themes.R")


########### input and output files ##############
# input files:
inputfiles <- list(
   
) %>% map(here)

#output files. 
outputfiles <- list(
   state_family_poverty_percs = "processing/output/state_family_poverty_perc.csv",
   county_family_poverty_percs = "processing/output/county_family_poverty_perc.csv",
   county_family_median_income = "processing/output/county_family_median_income.csv",
   county_gini = "processing/output/county_gini.csv",
   county_race_pop = "processing/output/county_race.rds",
   state_race_pop = "processing/output/state_race.rds",
   US_race_pop = "processing/output/US_race.rds"
   
) %>% map(here)



#Computing % of families with children < 18 below poverty line
#get variables using race-specific tables, 
#combine races
total <- get_acs(
   geography = "state",
   table = "B17010",
   year = 2020
) %>%
   mutate(race = "Total")

latinx <- get_acs(
   geography = "state",
   table = "B17010I",
   year = 2020
) %>%
   mutate(race = "Latinx")

white <- get_acs(
   geography = "state",
   table = "B17010H",
   year = 2020
) %>%
   mutate(race = "White")

black <- get_acs(
   geography = "state",
   table = "B17010B",
   year = 2020
) %>%
   mutate(race = "Black")

nativeAm <- get_acs(
   geography = "state",
   table = "B17010C",
   year = 2020
) %>%
   mutate(race = "Native American")

states <- bind_rows(total, white, black, nativeAm, latinx)

#Select variables of interest from tables,
#compute percentages
states <- states %>% 
   mutate(poverty = case_when(
      endsWith(variable, "004") |
         endsWith(variable, "011") | 
         endsWith(variable, "017") ~ "Below",
      endsWith(variable, "024") |
         endsWith(variable, "031") | 
         endsWith(variable, "037") ~ "Above",
      TRUE ~ NA_character_)) %>%
   group_by(GEOID, NAME, race, poverty) %>%
   summarise(count = sum(estimate)) %>% 
   filter(!is.na(poverty)) %>% 
   pivot_wider(names_from = poverty, values_from = count) %>% 
   mutate(total = Above + Below,
          perc = Below/total)

write_csv(states, outputfiles$state_family_poverty_percs)

#counties perc of families with kids <18 below poverty line
#get variables using race-specific tables, 
#combine races
total <- get_acs(
   geography = "county",
   table = "B17010",
   year = 2020
) %>%
   mutate(race = "Total")

latinx <- get_acs(
   geography = "county",
   table = "B17010I",
   year = 2020
) %>%
   mutate(race = "Latinx")

white <- get_acs(
   geography = "county",
   table = "B17010H",
   year = 2020
) %>%
   mutate(race = "White")

black <- get_acs(
   geography = "county",
   table = "B17010B",
   year = 2020
) %>%
   mutate(race = "Black")

nativeAm <- get_acs(
   geography = "county",
   table = "B17010C",
   year = 2020
) %>%
   mutate(race = "Indigenous American")

counties <- bind_rows(total, white, black, nativeAm, latinx)


#Select variables of interest from tables,
#compute percentages
counties <- counties %>% 
   mutate(poverty = case_when(
      endsWith(variable, "004") |
         endsWith(variable, "011") | 
         endsWith(variable, "017") ~ "Below",
      endsWith(variable, "024") |
         endsWith(variable, "031") | 
         endsWith(variable, "037") ~ "Above",
      TRUE ~ NA_character_)) %>%
   group_by(GEOID, NAME, race, poverty) %>%
   summarise(count = sum(estimate)) %>% 
   filter(!is.na(poverty)) %>% 
   pivot_wider(names_from = poverty, values_from = count) %>% 
   mutate(total = Above + Below,
          perc = Below/total)

write_csv(counties, outputfiles$county_family_poverty_percs)

#median income, county, families with children < 18
race_vars <- c(
   Total = "B19113_001",
   White = "B19113H_001",
   Latinx = "B19113I_001",
   Black = "B19113B_001",
   `Indigenous American` = "B19113C_001"
)

median_family_income <- get_acs(
   geography = "county",
   variables = race_vars,
   year = 2020
) 

median_family_income <- median_family_income %>% 
   select(GEOID, race = variable, median_income = estimate)

write_csv(median_family_income, outputfiles$county_family_median_income)

#county < 18 pop by race
total <- get_acs(
   geography = "county",
   table = "B01001",
   year = 2020
) %>%
   mutate(race = "Total")

total <- total %>% 
   mutate(child = case_when(
   endsWith(variable, "003") |
      endsWith(variable, "004") | 
      endsWith(variable, "005") |
      endsWith(variable, "006") |
      endsWith(variable, "027") | 
      endsWith(variable, "028") |
      endsWith(variable, "029") |
      endsWith(variable, "030")   ~ "yes",
   TRUE ~ NA_character_)) %>% 
   filter(!is.na(child)) %>% 
   group_by(county_fips = GEOID, NAME, race) %>% 
   summarise(pop = sum(estimate))

black <- get_acs(
   geography = "county",
   table = "B01001B",
   year = 2020
) %>%
   mutate(race = "Black")

white <- get_acs(
   geography = "county",
   table = "B01001H",
   year = 2020
) %>%
   mutate(race = "White")

indig_am <- get_acs(
   geography = "county",
   table = "B01001C",
   year = 2020
) %>%
   mutate(race = "Indigenous American")

multiple_race <- get_acs(
   geography = "county",
   table = "B01001G",
   year = 2020
) %>%
   mutate(race = "Multiple Race")

asian <- get_acs(
   geography = "county",
   table = "B01001D",
   year = 2020
) %>%
   mutate(race = "Asian")

latinx <- get_acs(
   geography = "county",
   table = "B01001I",
   year = 2020
) %>%
   mutate(race = "Latinx")

county_together <- bind_rows(white, black, asian,
                             latinx, indig_am, multiple_race)

county_together <- county_together %>% 
   mutate(child = case_when(
      endsWith(variable, "003") |
         endsWith(variable, "004") | 
         endsWith(variable, "005") |
         endsWith(variable, "006") |
         endsWith(variable, "018") | 
         endsWith(variable, "019") |
         endsWith(variable, "020") |
         endsWith(variable, "021")   ~ "yes",
      TRUE ~ NA_character_)) %>% 
   filter(!is.na(child)) %>% 
   group_by(county_fips = GEOID, NAME, race) %>% 
   summarise(pop = sum(estimate))

county_together <- bind_rows(county_together, total) %>% 
   arrange(county_fips, race)

saveRDS(county_together, outputfiles$county_race_pop)

#states < 18 population
total <- get_acs(
   geography = "state",
   table = "B01001",
   year = 2020
) %>%
   mutate(race = "Total")

total <- total %>% 
   mutate(child = case_when(
      endsWith(variable, "003") |
         endsWith(variable, "004") | 
         endsWith(variable, "005") |
         endsWith(variable, "006") |
         endsWith(variable, "027") | 
         endsWith(variable, "028") |
         endsWith(variable, "029") |
         endsWith(variable, "030")   ~ "yes",
      TRUE ~ NA_character_)) %>% 
   filter(!is.na(child)) %>% 
   group_by(state_fips = GEOID, NAME, race) %>% 
   summarise(pop = sum(estimate))

black <- get_acs(
   geography = "state",
   table = "B01001B",
   year = 2020
) %>%
   mutate(race = "Black")

white <- get_acs(
   geography = "state",
   table = "B01001H",
   year = 2020
) %>%
   mutate(race = "White")

indig_am <- get_acs(
   geography = "state",
   table = "B01001C",
   year = 2020
) %>%
   mutate(race = "Indigenous American")

multiple_race <- get_acs(
   geography = "state",
   table = "B01001G",
   year = 2020
) %>%
   mutate(race = "Multiple Race")

asian <- get_acs(
   geography = "state",
   table = "B01001D",
   year = 2020
) %>%
   mutate(race = "Asian")

latinx <- get_acs(
   geography = "state",
   table = "B01001I",
   year = 2020
) %>%
   mutate(race = "Latinx")

state_together <- bind_rows(white, black, asian,
                            latinx, indig_am, multiple_race)

state_together <- state_together %>% 
   mutate(child = case_when(
      endsWith(variable, "003") |
         endsWith(variable, "004") | 
         endsWith(variable, "005") |
         endsWith(variable, "006") |
         endsWith(variable, "018") | 
         endsWith(variable, "019") |
         endsWith(variable, "020") |
         endsWith(variable, "021")   ~ "yes",
      TRUE ~ NA_character_)) %>% 
   filter(!is.na(child)) %>% 
   group_by(state_fips = GEOID, NAME, race) %>% 
   summarise(pop = sum(estimate))

state_together <- bind_rows(state_together, total)

saveRDS(state_together, outputfiles$state_race_pop)

#us < 18 population
total <- get_acs(
   geography = "US",
   table = "B01001",
   year = 2020
) %>%
   mutate(race = "Total")

total <- total %>% 
   mutate(child = case_when(
      endsWith(variable, "003") |
         endsWith(variable, "004") | 
         endsWith(variable, "005") |
         endsWith(variable, "006") |
         endsWith(variable, "027") | 
         endsWith(variable, "028") |
         endsWith(variable, "029") |
         endsWith(variable, "030")   ~ "yes",
      TRUE ~ NA_character_)) %>% 
   filter(!is.na(child)) %>% 
   group_by(NAME, race) %>% 
   summarise(pop = sum(estimate))


black <- get_acs(
   geography = "US",
   table = "B01001B",
   year = 2020
) %>%
   mutate(race = "Black")

white <- get_acs(
   geography = "US",
   table = "B01001H",
   year = 2020
) %>%
   mutate(race = "White")

indig_am <- get_acs(
   geography = "US",
   table = "B01001C",
   year = 2020
) %>%
   mutate(race = "Indigenous American")

multiple_race <- get_acs(
   geography = "US",
   table = "B01001G",
   year = 2020
) %>%
   mutate(race = "Multiple Race")

asian <- get_acs(
   geography = "US",
   table = "B01001D",
   year = 2020
) %>%
   mutate(race = "Asian")

latinx <- get_acs(
   geography = "US",
   table = "B01001I",
   year = 2020
) %>%
   mutate(race = "Latinx")

US_together <- bind_rows( white, black, asian,
                            latinx, indig_am, multiple_race)

US_together <- US_together %>% 
   mutate(child = case_when(
      endsWith(variable, "003") |
         endsWith(variable, "004") | 
         endsWith(variable, "005") |
         endsWith(variable, "006") |
         endsWith(variable, "018") | 
         endsWith(variable, "019") |
         endsWith(variable, "020") |
         endsWith(variable, "021")   ~ "yes",
      TRUE ~ NA_character_)) %>% 
   filter(!is.na(child)) %>% 
   group_by( NAME, race) %>% 
   summarise(pop = sum(estimate))

US_together <- bind_rows(US_together, total)

saveRDS(US_together, outputfiles$US_race_pop)

#county gini
county_gini <-  get_acs(
   geography = "county",
   table = "B19083",
   year = 2020
)
write_csv(county_gini, outputfiles$county_gini)
