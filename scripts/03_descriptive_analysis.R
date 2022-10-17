#
# Authors:     BR
# Maintainers: BR
# Copyright:   2021
# =========================================

library(pacman)
p_load(lubridate, readxl, readr, rcartocolor, extrafont, scales, tidycensus,
       CGPfunctions, tidyverse, qs)
options(scipen=999)

here <- here::here

source("../plot_themes.R")


########### input and output files ##############
# input files:
inputfiles <- list(
   state_census = "processing/output/state_race.rds",
   county_census = "processing/output/county_race.rds",
   US_census = "processing/output/US_race.rds",
   foster_care_15_20 = "import/output/foster_care_file2015_2019.csv",
   adoption_15_20 = "import/output/adoption_file2015_2019.csv",
   child19 = "processing/output/child_file19.qs",
   county_median_family_income = "processing/output/county_family_median_income.csv",
   county_family_poverty_perc = "processing/output/county_family_poverty_perc.csv",
   county_gini = "processing/output/county_gini.csv"

) %>% map(here)

#output files. 
outputfiles <- list(
   disposition_rate_race_national = "descriptives/output/disposition_rate_race_national.csv",
   disposition_rate_total_state = "descriptives/output/disposition_rate_total_state.csv",
   disposition_rate_race_state = "descriptives/output/disposition_rate_race_state.csv",
   maltreatment_type_state = "descriptives/output/maltreatment_type_state.csv",
   neglect_only_victim_race = "descriptives/output/neglect_only_victim_race.csv",
   parents_neglect = "descriptives/output/parents_neglect.csv",
   total_kids_race = "descriptives/output/total_kids_race.csv",
   total_percentages = "descriptives/output/total_percentages.csv",
   state_foster_rates = "descriptives/output/state_foster_rates.csv",
   total_time_to_removal = "descriptives/output/total_time_to_removal.csv",
   state_time_to_removal = "descriptives/output/state_time_to_removal.csv",
   ch2_table = "descriptives/output/chapter2_table.csv",
   county_table_correlations = "descriptives/output/county_table_correlations.csv",
   states_of_interest_specific_race_rates = "descriptives/output/states_of_interest_race_rates.csv",
   states_of_interest_reporters = "descriptives/output/states_of_interest_reporters.csv",
   removal_rate = "descriptives/output/removal_rates.csv",
   reporter_race_rates = "descriptives/output/reporter_race_rates.csv",
   race_tpr = "descriptives/output/race_tpr.csv",
   tpr_states = "descriptives/output/tpr_states.csv",
   waiting_adoption = "descriptives/output/waiting_adoption.csv",
   percent_substantiated_state = "descriptives/output/percent_substantiated_state.csv"

   ) %>% map(here)

#read in
df <- qread(inputfiles$child19)
state_census <- read_rds(inputfiles$state_census)
US_census <- read_rds(inputfiles$US_census)
county_census <- read_rds(inputfiles$county_census)
foster <- read_csv(inputfiles$foster_care_15_20) 
adoption <- read_csv(inputfiles$adoption_15_20)


#source citation
caption <- "Source: Human Rights Watch analysis of National Child Abuse and Neglect Data System (NCANDS) Child File and US Census Bureau Data."

#census data add in race code
US_census <- US_census %>% 
   mutate(RaceEthn = case_when(
      race == "Asian" ~ 4,
      race == "White" ~ 1,
      race == "Black" ~ 2,
      race == "Latinx" ~ 7,
      is.na(race) ~ 9,
      race == "Indigenous American" ~ 3,
      race == "Multiple Race" ~ 6
   ))

county_census <- county_census %>% 
   mutate(RaceEthn = case_when(
      race == "Asian" ~ 4,
      race == "White" ~ 1,
      race == "Black" ~ 2,
      race == "Latinx" ~ 7,
      is.na(race) ~ 9,
      race == "Indigenous American" ~ 3,
      race == "Multiple Race" ~ 6
   ))

state_census <- state_census %>% 
   mutate(RaceEthn = case_when(
      race == "Asian" ~ 4,
      race == "White" ~ 1,
      race == "Black" ~ 2,
      race == "Latinx" ~ 7,
      is.na(race) ~ 9,
      race == "Indigenous American" ~ 3,
      race == "Multiple Race" ~ 6
   ))

#All processing on df
#to check any totals against Maltreatment Report 2019, will need dataset to include puerto rico,
#but for our analysis, esp with race rates, we are removing puerto rico 
#because we don't have race disaggregated census data
df2 <- df #keep a dataframe with complete data
df <- df %>% 
   filter(StaTerr != "PR")

#2019 foster and adoption, no PR
foster19 <- foster %>% 
   filter(FY == 2019 & STATE != 72) %>%
   mutate(county_fips = FIPSCode)

adoption19 <- adoption %>% 
   filter(FY == 2019 & STATE != 72)

foster19 <- foster19 %>% 
   mutate(state_fips = as.character(
      str_pad(STATE, 2, pad = "0")))

#is TPR in FY 19
#TPR date in FY19
foster19 <- foster19 %>% 
   mutate(TPR_FY19 = ifelse(
      TPRDate > "2018-09-30" & TPRDate < "2019-10-01",
      1, 0))

#check against maltreament report 19
#table 6-2
df2 %>%
   summarise(count = n_distinct(child_report_ID))

df2 %>%
   summarise(count = n_distinct(RptID))

df2 %>%
   summarise(count = n_distinct(ChID))

#multiple reports
t <- df %>%
   group_by(victim_any_report, multiple_reports) %>% 
   summarise(count = n_distinct(ChID)) %>%
   mutate(perc = count/sum(count))

t <- df %>%
   group_by(multiple_reports) %>% 
   summarise(count = n_distinct(ChID)) %>%
   mutate(perc = count/sum(count))

#rates of children in multiple reports in US
t <- df %>%
   group_by(RaceEthn,  multiple_reports) %>%
   summarise(count = n_distinct(ChID)) %>%
   mutate(perc = count/sum(count))

#rate of referrals per 1000k pg 15  of maltreatment report
t <- df %>%
   summarise(count = n_distinct(RptID))

total_kids <- US_census$pop[US_census$race == "Total"]

t <- t %>%
   mutate(rate = (count/total_kids) * 1000) #getting correct rate

#now, get the child disposition rate.
t <- df %>%
   summarise(count = n_distinct(ChID))

t <- t %>%
   mutate(rate = (count/total_kids) * 1000)

#state disposition rate 
state <- state_census %>%
   filter(race == "Total") 

#add full state names to df
state_join <- state %>% 
   ungroup() %>% 
   select(state_fips, state = NAME)
df <- left_join(df, state_join)
rm(state_join)

t <- df %>%
   group_by(state_fips) %>%
   summarise(count = n_distinct(ChID))

t <- left_join(t, state) %>%
   mutate(rate = (count/pop) * 1000) %>%
   filter(!is.na(rate))
write_csv(t, outputfiles$disposition_rate_total_state)
rm(state)

#child disposition by race
t <- df %>%
   group_by(RaceEthn) %>%
   summarise(count = n_distinct(ChID))

US_race <- US_census %>%
   filter(race != "Total") %>%
   group_by(race, RaceEthn) %>%
   summarise(pop = sum(pop))

t <- left_join(t, US_race)

t <- t %>%
   mutate(rate = (count/pop) * 1000) %>%
   filter(!is.na(race)) %>%
   mutate(ratio_white = round(rate/rate[race == "White" ], 2))
write_csv(t, outputfiles$disposition_rate_race_national)

total_dispos <- t %>% 
   ungroup() %>% 
   mutate(`Percentage of Reports` = count/sum(count)) %>% 
   select(RaceEthn, `Total Reports (disposed)` = count, `Percentage of Reports`)

#black to white ratio by state
state_race <- state_census %>%
   filter(race != "Total") %>%
   group_by(state_fips, NAME, race, RaceEthn) %>%
   summarise(pop = sum(pop)) 

t <- df %>%
   group_by(state_fips, RaceEthn) %>%
   summarise(count = n_distinct(ChID))

t <- left_join(t, state_race)

t <- t %>%
   mutate(rate = (count/pop) * 1000) %>%
   filter(!is.na(race)) %>%
   mutate(ratio_white = round(rate/rate[race == "White" ], 2))

#for output top tens, exclude dc
t <- t %>%
   filter(NAME != "District of Columbia")

#race_specific rates
white <- t %>%
   filter(race == "White") %>%
   select(NAME, `White rate` = rate)

native_am <- t %>%
   ungroup() %>%
   filter(race == "Indigenous American") %>%
   slice_max(ratio_white, n = 10)  %>%
   left_join(white)

black <- t %>%
   ungroup() %>%
   filter(race == "Black") %>%
   slice_max(ratio_white, n = 10)  %>%
   left_join(white)

latinx <- t %>%
   ungroup() %>%
   filter(race == "Latinx") %>%
   slice_max(ratio_white, n = 10)  %>%
   left_join(white)

race_disposition_rate <- bind_rows(black, latinx, native_am)

write_csv(race_disposition_rate, outputfiles$disposition_rate_race_state)
rm(white, black, latnx, native_am)


#disposition findings
total_victims <- df %>%
   group_by(victim_any_report) %>%
   summarise(num_children = n_distinct(ChID)) 

sum(total_victims$num_children)

#percent of child-reports that found a victim
t <- df %>%
   group_by(RptVictim) %>%
   summarise(count = n_distinct(child_report_ID)) %>%
   mutate(perc = count/sum(count))

#percent of child-reports that were substantiated by state
#top and bottom 10 states
percent_substantiated_state <- df %>%
   group_by(state, RptVictim) %>%
   summarise(count = n_distinct(child_report_ID)) %>%
   mutate(perc = (count/sum(count)) * 100) %>%
   filter(RptVictim == 1) %>%
   ungroup() %>% 
   arrange(desc(perc)) 
t1 <- percent_substantiated_state %>% 
   slice_max(perc, n = 10)
t2 <- percent_substantiated_state %>% 
   slice_min(perc, n = 10)
percent_substantiated_state <- bind_rows(t1, t2) %>% 
   arrange(desc(perc))
write_csv(percent_substantiated_state, outputfiles$percent_substantiated_state)
rm(percent_substantiated_state, t1, t2)

#percent nationwide by race
t <- df %>%
   group_by(RaceEthn, RptVictim) %>%
   summarise(count = n_distinct(child_report_ID)) %>%
   mutate(perc = count/sum(count)) %>%
   filter(RptVictim == 1) %>%
   arrange(desc(perc))

#Rate per 1,000 children.  
t <- df %>%
   group_by(RaceEthn, victim_any_report) %>%
   summarise(count = n_distinct(ChID))

t <- left_join(t, US_race)

t <- t %>%
   mutate(rate = (count/pop) * 1000) %>%
   filter(!is.na(race)) %>%
   ungroup() %>%
   mutate(ratio_white = round(rate/rate[race == "White" ], 2))

total_victims <- t %>% 
   filter(victim_any_report == 1) %>% 
   mutate(`Percentage of victims` = count/sum(count)) %>% 
   select(RaceEthn,`Number of victims` = count, `Percentage of victims`)
             

#state level rate of substantiated cases
t <- df %>%
   group_by(state_fips, victim_any_report) %>%
   summarise(count = n_distinct(ChID))

state_tots <- state_race %>%
   group_by(state_fips, NAME) %>%
   summarise(tot_pop = sum(pop))

t <- left_join(t, state_tots)

t <- t %>%
   mutate(rate = (count/tot_pop) * 1000) %>%
   filter(victim_any_report == 1) %>%
   arrange(desc(rate))

#who is reporting?
reporters <- df %>%
   group_by(RptSrc) %>% 
   summarise(count = n_distinct(child_report_ID)) %>%
   mutate(perc = count/sum(count)) %>%
   arrange(desc(perc)) %>% 
   mutate(cumperc = cumsum(perc))

#race disparities in who reports?
#law enforcement
t <- df %>%
   group_by(RaceEthn, RptSrc) %>% 
   summarise(count = n_distinct(child_report_ID)) %>%
   mutate(perc = count/sum(count)) %>%
   filter(RptSrc == 4) %>% 
   arrange(desc(perc)) 

#education personnel
t <- df %>%
   group_by(RaceEthn, RptSrc) %>% 
   summarise(count = n_distinct(child_report_ID)) %>%
   mutate(perc = count/sum(count)) %>%
   filter(RptSrc == 5) %>% 
   arrange(desc(perc)) 

#Black children
t <- df %>%
   group_by(RaceEthn, RptSrc) %>% 
   summarise(count = n_distinct(child_report_ID)) %>%
   mutate(perc = count/sum(count)) %>%
   filter(RaceEthn == 2) %>% 
   arrange(desc(perc)) 

#state differences?
t <- df %>%
   group_by(state, RptSrc) %>% 
   summarise(count = n_distinct(child_report_ID)) %>%
   mutate(perc = count/sum(count)) %>%
   filter(RptSrc == 88) %>% 
   arrange(desc(perc)) 

mean(t$perc)
median(t$perc)

#filter out states with high amounts of unknown or other
#can change the report source to look at percs of specific
#sources at state level
t <- df %>%
   group_by(state, RptSrc) %>% 
   summarise(count = n_distinct(child_report_ID)) %>%
   mutate(perc = count/sum(count)) %>%
   filter(state != "NC" & state != "MD" &
             state != "IA") %>%
   filter(RptSrc == 1) %>% 
   arrange(desc(perc)) 

#to look at reporter in any state adjust filter.
t <- df %>%
   filter(state_of_interest == 1) %>% 
   group_by(state,RptSrc) %>% 
   summarise(count = n_distinct(child_report_ID)) %>%
   mutate(perc = count/sum(count)) %>%
   filter(RptSrc == 4 | RptSrc == 5 |
           RptSrc == 1 | RptSrc == 2 |
          RptSrc == 3 | RptSrc == 13 ) %>% 
   mutate(source = case_when(
      RptSrc == 1 ~ "Social Services Personnel",
      RptSrc == 2 ~ "Medical Personnel",
      RptSrc == 3 ~ "Mental Health Personnel",
      RptSrc == 4 ~ "Law Enforcement Personnel",
      RptSrc == 5 ~ "Education Personnel",
      RptSrc == 13 ~ "Anonymous Reporter"
   ))

other <- t %>% 
   group_by(state) %>% 
   summarise(total_perc = sum(perc)) %>% 
   mutate(source = "Other", perc = 1 - total_perc) %>% 
   select(-total_perc)
t <- bind_rows(t, other)  

t <- t %>% 
   select(state, source, perc) %>% 
   mutate(across(where(is.numeric), ~ round(.x, 3) * 100))


write_csv(t, outputfiles$states_of_interest_reporters)

#state specific race disparities in reporter?
t <- df %>%
   filter(state_of_interest == 1) %>% 
   group_by(state, RaceEthn, RptSrc) %>% 
   summarise(count = n_distinct(child_report_ID)) %>%
   mutate(perc = count/sum(count)) %>%
   filter(RptSrc == 4 | RptSrc == 5 |
             RptSrc == 1 | RptSrc == 2 |
             RptSrc == 3 | RptSrc == 13 ) %>% 
   group_by(state, RptSrc) %>% 
   mutate(ratio_white = round(perc/perc[RaceEthn == 1 ], 2))
t <- left_join(t, US_race)

#race rates by reporter
t <- df %>% 
   filter(FY19_report == 1) %>% 
   group_by(RaceEthn, RptSrc) %>% 
   summarise(count = n_distinct(child_report_ID))

t <- t %>% 
   mutate(source = case_when(
   RptSrc == 1 ~ "Social Services Personnel",
   RptSrc == 2 ~ "Medical Personnel",
   RptSrc == 3 ~ "Mental Health Personnel",
   RptSrc == 4 ~ "Law Enforcement Personnel",
   RptSrc == 5 ~ "Education Personnel",
   RptSrc == 13 ~ "Anonymous Reporter"
)) %>% 
   filter(RptSrc == 4 | RptSrc == 5) 

t <- left_join(t, US_census) %>% 
   mutate(rate = (count/pop) * 1000) 
t <- t %>% 
   filter(!is.na(NAME)) %>% 
   group_by(RptSrc) %>% 
   mutate(ratio_white = rate/rate[RaceEthn == 1 ])
write_csv(t, outputfiles$reporter_race_rates)


#maltreatment types
#percent of reports with number of alleged types
t <- df %>%
   group_by(number_maltreat) %>%
   summarise(count = n_distinct(child_report_ID)) %>%
   ungroup() %>% 
   mutate(perc = count/sum(count)) %>% 
   arrange(desc(perc)) 

#maltreatment types
t <- df %>%
   group_by(chmal1, chmal2, 
          chmal3, chmal4) %>% 
   summarise(count = n_distinct(child_report_ID)) %>% 
   mutate(perc = count/sum(count))
   
#is there a geographic thing happening with "no abuse"?
t <- df %>% 
   group_by(state, chmal1) %>% 
   summarise(count = n_distinct(child_report_ID)) %>% 
   mutate(perc = count/sum(count)) %>%
   filter(chmal1 == 6)

#reporter type on no abuse?
t <- df %>% 
   filter(chmal1 == 6) %>% 
   group_by(RptSrc) %>% 
   summarise(count = n_distinct(child_report_ID)) %>% 
   mutate(perc = count/sum(count)) 

#what about states using "other" or unknown?
#let's look at geographic differences in maltype
t2 <- df %>% 
   filter(chmal1 != 6) %>% 
   group_by(state, chmal1) %>% 
   summarise(count = n_distinct(child_report_ID)) %>% 
   mutate(perc = count/sum(count)) %>%
   filter(chmal1 == 8 | chmal1 == 9)

#going to exclude MA, HI and IA from maltreatment type
maltype <- df %>%
   filter(chmal1 != 6) %>% 
   filter(state != "Iowa" & state != "Hawaii" &
             state != "Missouri")

#types, including all maltreat columns, no abuse excluded
t <- maltype %>%
   select(child_report_ID, chmal1, chmal2, 
          chmal3, chmal4) %>%
   pivot_longer(-child_report_ID,
                values_to = "type") %>%
   filter(!is.na(type)) %>%
   group_by(type) %>%
   summarise(count = n_distinct(child_report_ID)) %>% 
   mutate(perc = count/sum(count)) %>% 
   arrange(desc(perc))

t2 <- maltype %>% 
   group_by(neglect_only) %>% 
   summarise(count = n_distinct(ChID)) %>%
   ungroup() %>% 
   mutate(perc = count/sum(count))


t <- df  %>% 
   group_by(neglect_only, RptVictim) %>% 
   summarise(count = n_distinct(ChID)) %>%
   ungroup() %>% 
   mutate(perc = count/sum(count))


#race and proportion of reports that are neglect 
t <- maltype %>% 
   group_by(RaceEthn, neglect_only) %>% 
   summarise(count = n_distinct(child_report_ID)) %>% 
   mutate(perc = count/sum(count)) %>%
   filter(neglect_only == 1)

#state and proportion of reports that are neglect 
t <- maltype %>% 
   group_by(state, neglect_only) %>% 
   summarise(count = n_distinct(child_report_ID)) %>% 
   mutate(perc = count/sum(count)) %>%
   filter(neglect_only == 1)

#what is up with some states?
t2 <- maltype %>% 
   filter(number_maltreat == 1)   %>% 
   group_by(state, chmal1) %>% 
   summarise(count = n_distinct(child_report_ID)) %>% 
   mutate(perc = count/sum(count)) 

t <- maltype %>% 
   filter(number_maltreat == 1)   %>% 
   group_by(state, chmal1, maltreatment1_type) %>% 
   summarise(count = n_distinct(child_report_ID)) %>% 
   ungroup() %>% 
   group_by(state) %>% 
   mutate(perc = (count/sum(count)*100)) %>%
   arrange(desc(perc))  %>% 
   filter(chmal1 == 1 | chmal1 == 2 | chmal1 == 4) %>% 
   select(-count, -chmal1) %>% 
   ungroup() %>%
   pivot_wider(names_from = maltreatment1_type,
               values_from = perc)

#top and bottom 10 states
t1 <- t %>% 
   slice_max(`Neglect or Deprivation of Necessities`, n = 10)
t2 <- t %>% 
   slice_min(`Neglect or Deprivation of Necessities`, n = 10)
t1 <- bind_rows(t1, t2) %>% 
   arrange(desc(`Neglect or Deprivation of Necessities`))

#output a grouped bar for top 3 maltreatment types
write_csv(t1, outputfiles$maltreatment_type_state)

#for neglect only cases, who are the main reporters?
t <- maltype %>% 
   filter(neglect_only == 1) %>% 
   group_by(RptSrc) %>% 
   summarise(count = n_distinct(child_report_ID)) %>%
   ungroup() %>% 
   mutate(neglect_only_perc = count/sum(count)) %>%
   select(-count)

t <- left_join(t, reporters) %>%
   mutate(diff = neglect_only_perc - perc)

#how about substantiation?
t <- maltype %>% 
   filter(neglect_only == 1) %>% 
   group_by(RptVictim) %>%
   summarise(count = n_distinct(child_report_ID)) %>%
   mutate(perc = count/sum(count)) %>%
   filter(RptVictim == 1) %>%
   arrange(desc(perc))

#by reporter?
t <- maltype %>% 
   filter(neglect_only == 1) %>% 
   group_by(RptSrc, RptVictim) %>%
   summarise(count = n_distinct(child_report_ID)) %>%
   mutate(perc = count/sum(count)) %>%
   filter(RptVictim == 1) %>%
   arrange(desc(perc))


#race rates?
t <- maltype %>% 
   filter(neglect_only == 1) %>% 
   group_by(RaceEthn) %>%
   summarise(count = n_distinct(ChID))

t <- left_join(t, US_race)

t <- t %>%
   mutate(rate = (count/pop) * 1000) %>%
   filter(!is.na(race)) %>%
   mutate(ratio_white = round(rate/rate[race == "White" ], 2))

#race victim neglect only cases
t <- maltype %>% 
   filter(neglect_only == 1 & RptVictim == 1) %>% 
   group_by(RaceEthn) %>%
   summarise(count = n_distinct(child_report_ID)) %>%
   mutate(perc = count/sum(count)) %>%
   arrange(desc(perc))

t <- left_join(t, US_race)

t <- t %>%
   mutate(rate = (count/pop) * 1000) %>%
   filter(!is.na(race)) %>%
   ungroup() %>% 
   mutate(ratio_white = round(rate/rate[race == "White" ], 2))
write_csv(t, outputfiles$neglect_only_victim_race)

#race and substantiated maltreatment cases
race_perc_substantiated <- maltype %>% 
   filter(neglect_only == 1) %>% 
   group_by(RaceEthn, RptVictim) %>%
   summarise(count = n_distinct(child_report_ID)) %>%
   mutate(perc = count/sum(count)) %>%
   filter(RptVictim == 1) %>%
   arrange(desc(perc))
rm(race_perc_substantiated)

race_victim <- df %>%
   group_by(RaceEthn, RptVictim) %>%
   summarise(count = n_distinct(child_report_ID)) %>%
   mutate(perc = count/sum(count)) %>%
   filter(RptVictim == 1) %>%
   arrange(desc(perc))

#race of children who have reports, found to be a victim, victim of neglect, population
#any neglect
t <- maltype %>% 
   filter(neglect_victim == 1 & RptVictim == 1) %>% 
   group_by(RaceEthn) %>%
   summarise(count = n_distinct(child_report_ID)) %>%
   ungroup () %>% 
   mutate(perc = count/sum(count)) %>%
   select(RaceEthn, 
          `Number of neglect victim reports` = count,
          `Percentage of neglect victim reports` = perc)

totals <- left_join(total_dispos, total_victims)
totals <- left_join(totals, t)
totals <- left_join(totals, US_race)

totals <- totals %>% 
   mutate(`Percentage of US Child Population` = round(pop/sum(pop), 2))


#add in percentage of foster care entries and 
#percentage of parental rights terminations
foster_entry_race <- foster19 %>% 
   filter(Entered == 1) %>% 
   group_by(RaceEthn) %>% 
   summarise(count = n_distinct(StFCID)) %>% 
   mutate(`Percentage of Foster Care Entries` = count/sum(count))

#proportion of parental rights terminations
pr_terminations <- foster19 %>% 
   filter(IsTPR == 1) %>% 
   group_by(RaceEthn) %>% 
   summarise(count = n_distinct(StFCID)) %>% 
   mutate(`Percentage of Parental Rights Terminations` = count/sum(count)) %>% 
   select(-count)

totals <- left_join(totals, foster_entry_race)
totals <- left_join(totals, pr_terminations)

totals <- left_join(totals, US_race)
totals <- totals %>% 
   select(race, `Percentage of US Child Population`, `Percentage of Reports`,
          `Percentage of victims`, `Percentage of neglect victim reports`,
          `Percentage of Foster Care Entries`,
          `Percentage of Parental Rights Terminations`) %>% 
   mutate(across(where(is.numeric), ~ round(.x, 3) * 100))

#write out
write_csv(totals, outputfiles$total_percentages)
rm(totals, total_dispos, total_victims)

#state specific table.  
states <- state_race %>% 
   filter(NAME == "California" |
          NAME == "New York" |
          NAME == "Oklahoma"|
          NAME == "West Virginia") %>% 
   filter(!is.na(race)) %>% 
   group_by(state = NAME, state_fips, race) %>% 
   summarise(count = pop) %>% 
   ungroup() %>% 
   group_by(state) %>% 
   mutate(`Percentage of State Child Population` = round(count/sum(count), 3)) 

#get race ethn
states <- left_join(states, US_race) %>% 
   select(-pop, -count)

#percent of investigations
t <- df %>%
   filter(state_of_interest == 1) %>% 
   group_by(state, RaceEthn) %>%
   summarise(count = n_distinct(ChID)) %>% 
   group_by(state) %>% 
   mutate(`Percentage of Investigations` = count/sum(count)) %>% 
   select(-count)

t1 <- foster19 %>% 
   filter(STATE == 06 | STATE == 36 |
             STATE == 40 | STATE == 54) %>% 
   filter(Entered == 1) %>% 
   group_by(STATE, RaceEthn) %>% 
   summarise(count = n_distinct(StFCID)) %>% 
   group_by(state_fips = STATE) %>% 
   mutate(`Percentage of Foster System Entries` = count/sum(count)) %>% 
   select(-count) %>% 
   mutate(state_fips = as.character(
      str_pad(state_fips, 2, pad = "0")))

t2 <- foster19 %>% 
   filter(STATE == 06 | STATE == 36 |
             STATE == 40 | STATE == 54) %>% 
   #filter(Entered == 1) %>% 
   group_by(STATE, RaceEthn) %>% 
   summarise(count = n_distinct(StFCID)) %>% 
   group_by(state_fips = STATE) %>% 
   mutate(`Percentage Served by the Foster System` = count/sum(count)) %>% 
   select(-count) %>% 
   mutate(state_fips = as.character(
      str_pad(state_fips, 2, pad = "0")))


states <- left_join(states, t, by = c("state", "RaceEthn"))
states <- left_join(states, t1, by = c("state_fips", "RaceEthn"))
states <- left_join(states, t2, by = c("state_fips", "RaceEthn"))

states <- states %>% 
   select(state, race, `Percentage of State Child Population`,
          `Percentage of Investigations`,
          `Percentage of Foster System Entries`,
          `Percentage Served by the Foster System`) %>% 
   # #pivot_longer(cols = 3:6, names_to = "category",
   #              values_to = "Percentage") %>% 
   mutate(across(where(is.numeric), ~ round(.x, 3) * 100))

write_csv(states, outputfiles$states_of_interest_specific_race_rates)

#who were perpetrators?  
#for all victims
t <- df %>% 
   filter(RptVictim == 1) %>% 
   group_by(parent_perp) %>%
   summarise(count = n_distinct(ChID)) %>% 
   mutate(perc = count/sum(count))

#for people with parent perpetrators and neglect only, how is race?
t <- df  %>% 
   filter(neglect_only == 1 & RptVictim == 1) %>% 
   group_by(parent_perp) %>%
   summarise(count = n_distinct(ChID)) %>% 
   mutate(perc = count/sum(count))
   
#neglect only parents
t <- df  %>% 
   filter(neglect_only == 1 & RptVictim == 1 & per1rel == 1) %>% 
   group_by(per1prnt) %>%
   summarise(count = n_distinct(ChID)) %>% 
   mutate(perc = count/sum(count))


t <- df %>% 
   filter(RptVictim == 1) %>% 
   group_by(mal1lev, mal2lev, mal3lev, mal4lev) %>%
   summarise(count = n_distinct(ChID)) %>% 
   mutate(perc = count/sum(count))

t <- neglect_only %>% 
   group_by(per1rel) %>%
   summarise(count = n_distinct(ChID)) %>% 
   mutate(perc = count/sum(count))

#parent perp
t <- maltype %>% 
   filter(neglect_victim == 1 & RptVictim == 1) %>% 
   group_by(parent_perp) %>%
   summarise(count = n_distinct(ChID)) %>%
   mutate(perc = count/sum(count)) %>%
   arrange(desc(perc))


#any neglect found. victims, parent perps and race
#how many reports?
maltype %>% 
   filter(neglect_victim == 1 & RptVictim == 1 & parent_perp == 1) %>% 
   summarise(count = n_distinct(child_report_ID))

#race rates of children
t <- maltype %>% 
   filter(neglect_victim == 1 & RptVictim == 1 & parent_perp == 1) %>% 
   group_by(RaceEthn) %>%
   summarise(count = n_distinct(ChID)) %>%
   mutate(perc = count/sum(count)) %>%
   arrange(desc(perc))
sum(t$count)

t <- left_join(t, US_race)

t <- t %>%
   mutate(rate = (count/pop) * 1000) %>%
   filter(!is.na(race)) %>%
   ungroup() %>% 
   mutate(ratio_white = round(rate/rate[race == "White" ], 2))
sum(t$count)

#number of parent perpetrators
t <- maltype %>% 
   filter(parent_neglect == 1) %>% 
   group_by(parent_type) %>% 
   summarise(count = n_distinct(child_report_ID)) %>%
   mutate(perc = count/sum(count))


#7% of kids have both a single and multiple parent report
#so we need to treat this as reports and not as individual children

#gender of parents and race of neglect victims
t1 <- maltype %>% 
   filter(parent_type == "Single parent") %>% 
   filter(parent_neglect == 1 & RptVictim == 1 & per1rel == 1 ) %>% 
   group_by(RaceEthn, parent_type, gender = per1sex) %>%
   summarise(count = n_distinct(child_report_ID))

t2 <- maltype %>% 
   filter(parent_type == "Single parent") %>% 
   filter(parent_neglect == 1 & RptVictim == 1 & per2rel == 1 ) %>% 
   group_by(RaceEthn, parent_type, gender = per2sex) %>%
   summarise(count = n_distinct(child_report_ID))

t3 <- maltype %>% 
   filter(parent_type == "Single parent") %>% 
   filter(parent_neglect == 1 & RptVictim == 1 & per3rel == 1 ) %>% 
   group_by(RaceEthn, parent_type, gender = per3sex) %>%
   summarise(count = n_distinct(child_report_ID))

t4 <- maltype %>% 
   filter(parent_type == "Multiple parents" | parent_type == "Other") %>% 
   filter(neglect_victim == 1 & RptVictim == 1 & 
             (per1rel == 1 | per2rel == 1 | per3rel == 1)) %>% 
   group_by(RaceEthn, parent_type) %>%
   summarise(count = n_distinct(child_report_ID)) %>%
   mutate(gender = NA)

t <- bind_rows(t1, t2, t3, t4)

t <- t %>% 
   mutate(parent_type2 = case_when(
      parent_type == "Single parent" & gender == 2 ~ "Single mother",
      parent_type == "Single parent" & gender == 1 ~ "Single father",
      parent_type == "Multiple parents" ~ "Multiple parents",
      TRUE ~ "Other"
   )) %>% 
   group_by(RaceEthn, parent_type2) %>% 
   summarise(num = sum(count)) 

t <- left_join(t, US_race)
sum(t$num) #matches total number

#first in sheer numbers and percent of cases
total_numbers <- t %>% 
   mutate(rate = (num/pop) * 1000) %>% 
   ungroup() %>% 
   filter(!is.na(race)) %>% 
   mutate(perc = num/sum(num) * 100,
          ratio_white = round(rate/rate[race == "White" ], 2)) %>% 
   arrange(desc(num)) %>% 
   mutate(cumperc = cumsum(perc)) 

total_output <- total_numbers %>% 
   arrange(desc(rate)) %>% 
   filter(parent_type2 != "Other") %>% 
   select(`Child race` = race, `Perpetrator` = parent_type2,
          `Rate of substantiated dispostions per 1,000 children` = rate)
write_csv(total_output, outputfiles$parents_neglect)

rm(total_output, t1, t2, t3, t4, t5, t6, gender_total, gender_total1, gender_total2)
rm(total_numbers)

#Dates
#report date to investigation date - replicate table 2-2 of Child Maltreatment report
#proportion of state records with report time
t <- df %>% 
   group_by(state, RptTm) %>%
   summarise(count = n_distinct(child_report_ID)) %>% 
   group_by(state) %>% 
   mutate(perc = count/sum(count))

#proportion of state records with investigation date
t <- df %>% 
   group_by(state, InvDate) %>%
   summarise(count = n_distinct(child_report_ID)) %>% 
   group_by(state) %>% 
   mutate(perc = count/sum(count))

#compute some average hours and see if anything matches with table
t <- df %>% 
   group_by(state) %>%
   summarise(mean_hours = mean(hours_between_rpt_invest, na.rm = T),
             median_hours = median(hours_between_rpt_invest, na.rm = T),
             mean_days = mean(days_between_rpt_invest, na.rm = T),
             median_days = median(days_between_rpt_invest, na.rm = T))

#good many are close.  Will have to identify bad states and pull them.
#suggest not worrying about this.

#time between report and disposition
t <- df %>% 
   group_by(state) %>%
   summarise(mean_days = mean(days_between_rpt_dispo, na.rm = T),
             median_days = median(days_between_rpt_dispo, na.rm = T))

total <- df %>% 
   summarise(mean_days = mean(days_between_rpt_dispo, na.rm = T),
             median_days = median(days_between_rpt_dispo, na.rm = T))

t <- df %>% 
   group_by(RptVictim) %>%
   summarise(mean_days = mean(days_between_rpt_dispo, na.rm = T),
             median_days = median(days_between_rpt_dispo, na.rm = T))

t <- df %>% 
   group_by(maltreatment1_type) %>%
   summarise(mean_days = mean(days_between_rpt_dispo, na.rm = T),
             median_days = median(days_between_rpt_dispo, na.rm = T))


t <- df %>% 
   group_by(RaceEthn) %>%
   summarise(mean_days = mean(days_between_rpt_dispo, na.rm = T),
             median_days = median(days_between_rpt_dispo, na.rm = T))

#services
#check states completion of this 
bad_states <- df %>% 
   filter(RptVictim == 1) %>% 
   group_by(state, postserv) %>% 
   summarise(count = n_distinct(child_report_ID)) %>% 
   mutate(perc = count/sum(count)) %>%
   filter(is.na(postserv) & perc > .4)

#some bad states on this
t <- df %>% 
   filter(RptVictim == 1) %>% 
   filter(!state %in% bad_states$state) %>% 
   group_by(postserv) %>% 
   summarise(count = n_distinct(child_report_ID)) %>% 
   mutate(perc = count/sum(count))

#remaining state rates
t <- df %>% 
   filter(RptVictim == 1) %>% 
   filter(!state %in% bad_states$state) %>% 
   group_by(state, state_of_interest, postserv) %>% 
   summarise(count = n_distinct(child_report_ID)) %>% 
   mutate(perc = count/sum(count))

#maltreatment and neglect
t <- df %>% 
   filter(RptVictim == 1 & neglect_victim == 1) %>% 
   filter(!state %in% bad_states$state) %>% 
   group_by(postserv) %>% 
   summarise(count = n_distinct(child_report_ID)) %>% 
   mutate(perc = count/sum(count))

t <- df %>% 
   filter(RptVictim == 1 & neglect_victim == 1) %>% 
   filter(!state %in% bad_states$state) %>% 
   group_by(state, postserv) %>% 
   summarise(count = n_distinct(child_report_ID)) %>% 
   mutate(perc = count/sum(count))

#single maltype
t <- df %>% 
   filter(RptVictim == 1 & neglect_victim == 1) %>% 
   filter(!state %in% bad_states$state) %>% 
   group_by(maltreatment1_type, postserv) %>% 
   summarise(count = n_distinct(child_report_ID)) %>% 
   mutate(perc = count/sum(count))

#race
t <- df %>% 
   filter(RptVictim == 1) %>% 
   filter(!state %in% bad_states$state) %>% 
   group_by(RaceEthn, postserv) %>% 
   summarise(count = n_distinct(child_report_ID)) %>% 
   mutate(perc = count/sum(count))

#Removal
bad_states <- c("New York", "Pennsylvania", "North Carolina")

#rates per state - match table 4-6 of child maltreatment report
#- have to remove people with removal
#dates before the report
total_victims <- df %>% 
   filter(!state %in% bad_states) %>% 
   group_by(state, RptVictim) %>% 
   summarise(count = n_distinct(child_report_ID)) %>% 
   mutate(perc = count/sum(count))

t1 <- df %>% 
   filter(!state %in% bad_states) %>% 
   filter(days_between_rpt_removal >= 0 | 
             is.na(days_between_rpt_removal)) %>% 
   group_by(state, RptVictim, fostercr) %>% 
   summarise(count = n_distinct(child_report_ID)) %>% 
   mutate(perc = count/sum(count) * 100) %>%
   filter(RptVictim == 1 & fostercr == 1) %>% 
   select(state, `Percent of Victims Who Were Placed in Foster Care` = perc)

t <- df %>% 
   filter(!state %in% bad_states) %>% 
   group_by(RptVictim, fostercr) %>% 
   summarise(count = n_distinct(child_report_ID)) %>% 
   mutate(perc = count/sum(count))

t <- df %>% 
   filter(!state %in% bad_states) %>% 
   group_by(fostercr, RptVictim) %>% 
   summarise(count = n_distinct(child_report_ID)) %>% 
   mutate(perc = count/sum(count))

t <- df %>% 
   filter(!state %in% bad_states) %>% 
   filter(days_between_rpt_removal >= 0 | 
             is.na(days_between_rpt_removal)) %>% 
   group_by(state, fostercr, RptVictim) %>% 
   summarise(count = n_distinct(child_report_ID)) %>% 
   mutate(perc = (count/sum(count)) * 100) %>% 
   filter(fostercr == 1 & RptVictim != 1) %>%
   select(state, `Percent of Foster Care Placements Who Were Not A Victim` = perc)

t <- left_join(t, t1) %>% 
   ungroup() %>% 
   select(state, `Percent of Victims Who Were Placed in Foster Care`,
          `Percent of Foster Care Placements Who Were Not A Victim`)
write_csv(t, outputfiles$state_foster_rates)

#time to removal
t1 <- df %>% 
   filter(!state %in% bad_states) %>% 
   filter(days_between_rpt_removal >= 0) %>% 
   group_by(fostercr) %>%
   summarise(`Total Children Removed` = n_distinct(child_report_ID),
      mean_days = mean(days_between_rpt_removal, na.rm = T),
             median_days = median(days_between_rpt_removal, na.rm = T))

t2 <- df %>% 
   filter(!state %in% bad_states) %>% 
   filter(days_between_rpt_removal >= 0) %>% 
   group_by(RptVictim, victim_any_report) %>%
   summarise(`Total Children Removed` = n_distinct(child_report_ID),
             mean_days = mean(days_between_rpt_removal, na.rm = T),
             median_days = median(days_between_rpt_removal, na.rm = T))
t <- bind_rows(t1, t2)
write_csv(t, outputfiles$total_time_to_removal)

#time to removal at state level

t1 <- df %>% 
   filter(!state %in% bad_states) %>% 
   filter(days_between_rpt_removal >= 0) %>% 
   group_by(state) %>%
   summarise(`Total Children Removed` = n_distinct(child_report_ID),
             mean_days = mean(days_between_rpt_removal, na.rm = T),
             median_days = median(days_between_rpt_removal, na.rm = T))

t2 <- df %>% 
   filter(!state %in% bad_states) %>% 
   filter(days_between_rpt_removal >= 0) %>% 
   group_by(state, RptVictim, victim_any_report) %>%
   summarise(`Total Children Removed` = n_distinct(child_report_ID),
             mean_days = mean(days_between_rpt_removal, na.rm = T),
             median_days = median(days_between_rpt_removal, na.rm = T))
t3 <- bind_rows(t1, t2)
t <- t3 %>% 
   filter(is.na(victim_any_report))
write_csv(t, outputfiles$state_time_to_removal)

test <- df %>% 
   filter(fostercr == 1 & StaTerr == "WY") %>% 
   select(RptDt, RmvDate, days_between_rpt_removal, child_report_ID)

#court action
bad_states <- c("Alabama", "Colorado", "Florida", "Illinois",
                "Maine", "Mississippi", "New York",
                "North Carolina", "Pennsylvania", "South Dakota",
                "Tennessee")
t <- df %>% 
   filter(!state %in% bad_states) %>% 
   filter(RptVictim == 1) %>% 
   group_by(RaceEthn, juvpet) %>% 
   summarise(count = n_distinct(child_report_ID)) %>% 
   mutate(perc = count/sum(count)) %>% 
   filter(juvpet == 1)


t <- df %>% 
   #filter(!state %in% bad_states) %>% 
   filter(RptVictim == 1) %>% 
   group_by(state, cochrep) %>% 
   summarise(count = n_distinct(child_report_ID)) %>% 
   mutate(perc = count/sum(count))



#Summary Table for Chapter 2
t1 <- df %>% 
    summarise(Count = 
                 n_distinct(ChID)) %>% 
   mutate(cat = "Number of children impacted by investigation")
#substantiated
t2 <- df %>% 
   filter(RptVictim == 1) %>% 
   summarise(Count = 
                n_distinct(ChID)) %>% 
   mutate(cat = "Number of children found to be maltreated/substantiated")

test <- df %>% 
   filter(neglect_only == 1 & RptVictim == 1) %>% 
   summarise(Count = 
                n_distinct(ChID)) %>% 
   mutate(cat = "Number of children found to be maltreated/substantiated with neglect-only allegation")

t3 <- foster19 %>% 
   summarise(Count = n_distinct(StFCID)) %>% 
   mutate(cat = "Number of children served by foster care system during FY")

t4 <- foster19 %>% 
   filter(Entered == 1) %>% 
   summarise(Count = n_distinct(StFCID)) %>% 
   mutate(cat = "Number of children who entered foster care system during FY")
  
t5 <- foster19 %>% 
   filter(IsTPR == 1) %>% 
   summarise(Count = n_distinct(StFCID)) %>% 
   mutate(cat = "Number of children whose parental rights are terminated during FY")

t6 <- adoption19 %>% 
   summarise(Count = n()) %>% 
   mutate(cat = "Number of children adopted during FY")

t7 <- foster19 %>% 
   group_by(DISREASN) %>%  
   summarise(Count = n_distinct(StFCID)) %>% 
   filter(DISREASN == 1 | DISREASN == 4) %>% 
   mutate(cat = ifelse(DISREASN == 4, 
                       "Number of children who were emancipated during FY",
                       "Number of children reunited with parent(s)/caretaker during FY")) %>% 
   select(-DISREASN)


#how many removed and returned to home in < 6 months
t8 <- foster19 %>% 
   filter(DISREASN == 1) %>% 
   filter(LatRemLOS < 181) %>%
   summarise(Count = n_distinct(StFCID)) %>% 
   mutate(cat = "Number of children returned home in < 6 months during FY")

table <- bind_rows(t1, t2, t3, t4, t5, t6, t7, t8) %>%
   select(cat, Count)
write_csv(table, outputfiles$ch2_table)
rm(t1, t2, t3, t4, t5, t6, t7, t8, table)

#number and percent with neglect only
test <- df %>% 
   group_by(`Neglect or Deprivation of Necessities`) %>% 
   summarise(Count = 
                n_distinct(ChID)) %>% 
   mutate(cat = "Number of children found to be maltreated/substantiated with neglect-only allegation")


#removal rates by race
removal_rate <- foster19 %>% 
   filter(Entered == 1) %>% 
   group_by(RaceEthn) %>% 
   summarise(count = n_distinct(StFCID)) 

removal_rate <- left_join(removal_rate, US_census)
removal_rate <- removal_rate %>% 
   mutate(`Removal rate` = count/pop * 1000) %>% 
   filter(!is.na(pop)) %>% 
   mutate(ratio_white = `Removal rate`/`Removal rate`[race == "White" ])

write_csv(removal_rate, outputfiles$removal_rate)



#correlations
#Rates of reports, maltreatment, placements and parental termination
#poverty rates county level
       
#counties
county1 <- df %>% 
   group_by(county_fips) %>% 
   summarise(count = n_distinct(ChID)) 

county_tots <- county_census %>% 
   group_by(county_fips) %>% 
   summarise(tot_pop = sum(pop))

county1 <- left_join(county1, county_tots) %>% 
   mutate(rate = (count/tot_pop) * 1000)
county1 <- county1 %>% 
   mutate(measure = "Investigations per 1,000 children",
          race = "Total")

#race specific, county investigation rates
county2 <- df %>% 
   group_by(county_fips,  RaceEthn) %>% 
   summarise(count = n_distinct(ChID))
county2 <- left_join(county2, county_census) %>% 
   mutate(rate = (count/pop) * 1000) %>% 
   mutate(measure = "Investigations per 1,000 children")

#county total, substantiated victim rate
county3 <- df %>%
   filter(victim_any_report == 1) %>%
   group_by(county_fips) %>% 
   summarise(count = n_distinct(ChID)) 
county3 <- left_join(county3, county_tots) %>% 
   mutate(rate = (count/tot_pop) * 1000)
county3 <- county3 %>% 
   mutate(measure = "Substantiated maltreatment victims per 1,000 children",
          race = "Total")

#victims, county, race specific
county4 <- df %>%
   filter(victim_any_report == 1) %>%
   group_by(county_fips,  RaceEthn) %>% 
   summarise(count = n_distinct(ChID)) 
county4 <- left_join(county4, county_census) %>% 
   mutate(rate = (count/pop) * 1000)
county4 <- county4 %>% 
   mutate(measure = "Substantiated maltreatment victims per 1,000 children")

#foster placements in FY19, total rate, county
#test if linked files have better data on county
#unique fips in foster services
foster_counties <- foster19 %>% 
   group_by(county_fips) %>% 
   summarise(count = n_distinct(StFCID)) %>% 
   filter(!endsWith(county_fips, "000") & !endsWith(county_fips,"999")) %>% 
   filter(county_fips != "00008")

#If people in foster are in care in a redacted county but were
#reported in a non-redacted county, we can use that county.
#No one who was reported in a county that is has <1000 cases in 
#foster set is included. No one in care in a county with less than
join <- df %>% 
   select(StFCID, county_fips_NCANDS = county_fips)

foster19 <- left_join(foster19, join)

foster19 <- foster19 %>% 
   mutate(county_fips_orig = county_fips,
          county_fips = ifelse(
             county_fips_NCANDS %in% foster_counties$county_fips &
                !county_fips %in% foster_counties$county_fips,
             county_fips_NCANDS, county_fips
          ))

#foster placements in FY19, total rate, county
county5 <- foster19 %>% 
   filter(Entered == 1) %>% 
   group_by(county_fips) %>% 
   summarise(count = n_distinct(StFCID)) 

county5 <- left_join(county5, county_tots) %>% 
   mutate(rate = (count/tot_pop) * 1000)
county5 <- county5 %>% 
   mutate(measure = "Children entering foster care per 1,000 children",
          race = "Total")

#foster placements in FY19, race specific rate, county
county6 <- foster19 %>% 
   filter(Entered == 1) %>% 
   group_by(county_fips, RaceEthn) %>% 
   summarise(count = n_distinct(StFCID)) 

county6 <- left_join(county6, county_census) %>% 
   mutate(rate = (count/pop) * 1000)
county6 <- county6 %>% 
   mutate(measure = "Children entering foster care per 1,000 children")

#Parental termination in FY19, total rate, county
county7 <- foster19 %>% 
   filter(IsTPR == 1) %>% 
   group_by(county_fips) %>% 
   summarise(count = n_distinct(StFCID))

county7 <- left_join(county7, county_tots) %>% 
   mutate(rate = (count/tot_pop) * 1000)
county7 <- county7 %>% 
   mutate(measure = "Parental rights terminated per 1,000 children",
          race = "Total")

#Parental termination in FY19, race specific rate, county
county8 <- foster19 %>% 
   filter(IsTPR == 1) %>% 
   group_by(county_fips, RaceEthn) %>% 
   summarise(count = n_distinct(StFCID)) 

county8 <- left_join(county8, county_census) %>% 
   mutate(rate = (count/pop) * 1000)
county8 <- county8 %>% 
   mutate(measure = "Parental rights terminated per 1,000 children")


county_table <- bind_rows(county1, county2, county3, county4,
                         county5, county6, county7, county8) %>% 
   select(county_fips, measure, race, count, rate)

rm(county1, county2, county3, county4,
 county5, county6, county7, county8)

#filter out non-counties
county_table <- county_table %>% 
   filter(!endsWith(county_fips, "000") & !endsWith(county_fips,"999")) %>% 
   filter(county_fips != "00008")

#bring in county economic data
county_poverty <- read_csv(inputfiles$county_family_poverty_perc)
county_income <- read_csv(inputfiles$county_median_family_income)
county_gini <- read_csv(inputfiles$county_gini) %>% 
   select(county_fips = GEOID, gini = estimate) %>% 
   mutate(race = "Total")

county_join <- full_join(county_poverty, county_income) %>% 
   select(county_fips = GEOID, race, median_income,
          percent_below_poverty = perc) 
county_join <- left_join(county_join, county_gini)
rm(county_poverty, county_income)

#join county
county_table <- left_join(county_table, county_join)

#join with county race pop
county_join <- county_census %>% 
   select(county_fips, race, pop)

county_table <- left_join(county_table, county_join)

#determine thresholds for county inclusion in correlation analysis
t <- county_table %>% 
   filter(measure == "Investigations per 1,000 children" & 
             race != "Total") 
quantile(t$count, probs = seq(.1, .9, by = .1))
quantile(t$pop, probs = seq(.1, .9, by = .1))

# #what about total child population in county?
# quantile(county_tots$tot_pop, probs = seq(.1, .9, by = .1))
# 
# county_keep <- county_tots %>% 
#    filter(tot_pop > 32000)
#so if we set the threshold that there needs to be 30 investigations
#per race in a county, that eliminates the bottom 30% of 
#county-race combos.  
rm(t)
county_race_keep <- county_table %>% 
   filter(race == "Total" | (measure == "Investigations per 1,000 children" & 
             race != "Total" & count > 30)) %>% 
   mutate(key = paste(county_fips, race))

county_table <- county_table %>% 
   mutate(key = paste(county_fips, race))


county_table2 <- semi_join(county_table, county_race_keep,
                           by = c("key"))

rm(county_race_keep)
county_table2 <- county_table2 %>% 
   filter(pop > 999)

county_table2 <- county_table2 %>% 
   filter(!is.na(median_income) & !is.na(percent_below_poverty))
write_csv(county_table2, outputfiles$county_table_correlations)

#grouped correlations
correlations <- county_table2 %>%
   group_by(measure, race) %>%
   summarize(cor = cor(rate, percent_below_poverty)) %>% 
   arrange(measure, cor)

correlations2 <- county_table2 %>%
   group_by(measure, race) %>%
   summarize(cor = cor(rate, median_income)) %>% 
   arrange(measure, cor)

correlations3 <- county_table2 %>%
   group_by(measure, race) %>%
   summarize(cor = cor(rate, gini)) %>% 
   arrange(measure, cor)

#for plots
investigations_total <- county_table2 %>% 
   filter(measure == "Investigations per 1,000 children" &
             race == "Total") %>% 
   mutate(percent_below_poverty = percent_below_poverty
          * 100)
write_csv(investigations_total, 
          "descriptives/output/investigations_total.csv")
n_distinct(investigations_total$county_fips)

investigations_race <- county_table2 %>% 
   filter(measure == "Investigations per 1,000 children" &
             race != "Total") %>% 
   mutate(percent_below_poverty = percent_below_poverty
          * 100)
investigations_white <- investigations_race %>% 
   filter(race == "White")
write_csv(investigations_white,
          "descriptives/output/investigations_race_white.csv")
n_distinct(investigations_race$county_fips)

investigations_NA <- investigations_race %>% 
   filter(race == "Indigenous American")
write_csv(investigations_NA,
          "descriptives/output/investigations_race_NA.csv")

investigations_latinx <- investigations_race %>% 
   filter(race == "Latinx")
write_csv(investigations_latinx,
          "descriptives/output/investigations_race_latinx.csv")

investigations_Black <- investigations_race %>% 
   filter(race == "Black")
write_csv(investigations_Black,
          "descriptives/output/investigations_race_Black.csv")
investigations_Black2 <- investigations_Black %>% 
   filter(percent_below_poverty > 1)
cor(investigations_Black2$rate, investigations_Black2$percent_below_poverty)

n_distinct(investigations_race$county_fips)

parental_termination <- county_table2 %>% 
   filter(measure == "Parental rights terminated per 1,000 children")


#resolve the child file so that it is only the last report for each
#child and report with a removal
to_link <- df %>% 
   group_by(StFCID) %>% 
   filter(RptDt == max(RptDt) & fostercr == 1) %>% 
   arrange(desc(RptDt)) %>% 
   slice_max(RptDt, n = 1, with_ties = F)

linked <- left_join(to_link, foster, by = c("StFCID"))


#length of time in foster care
test <- foster19 %>% 
   filter(Entered == 1) %>% 
   group_by(LatRemDt) %>% 
   summarise(count = n_distinct(StFCID))


#US median time
us_median_time <- foster19 %>% 
   filter(InAtEnd == 0 & 
             !is.na(LatRemLOS)) %>%
   summarise(median_days = median(LatRemLOS, na.rm = T)) %>% 
   mutate(St = "US Total")

us_percentage_removed_length <- foster19 %>% 
   filter(InAtEnd == 0 & 
             !is.na(LatRemLOS)) %>%
   group_by(LatRemLOS) %>% 
   summarise(count = n_distinct(StFCID)) %>% 
   mutate(cumcount = cumsum(count),
          perc = count/sum(count),
          cumperc = cumsum(perc))

time <- foster19 %>% 
   filter(St == "CA" | St == "NY" | 
              St == "OK" | St == "WV") %>% 
   filter(InAtEnd == 0) %>%
   group_by(St, LatRemLOS) %>% 
   summarise(count = n_distinct(StFCID)) %>% 
   mutate(perc = count/sum(count),
          cumperc = cumsum(perc))

#foster discharge reason by case goal
t1 <- foster19 %>% 
   filter(DISREASN != 99) %>% 
   group_by(DISREASN, CASEGOAL) %>% 
   summarise(count = n_distinct(StFCID)) %>% 
   mutate(perc = count/sum(count))

t2 <- foster19 %>% 
   filter(DISREASN != 99) %>% 
   group_by(CASEGOAL, DISREASN) %>% 
   summarise(count = n_distinct(StFCID)) %>% 
   mutate(perc = count/sum(count))

#parental rights termination
#of those in foster system in 2019, # with PRT
t <- foster19 %>% 
   filter(Exited == 1) %>% 
   group_by(IsTPR, IsWaiting) %>% 
   summarise(count = n_distinct(StFCID)) %>% 
   mutate(perc = count/sum(count))

t <- foster19 %>% 
   filter(IsTPR == 1) %>% 
   group_by(TPR_FY19, Exited, IsWaiting) %>% 
   summarise(count = n_distinct(StFCID)) 

t <- foster19 %>% 
   filter(IsTPR == 1 & TPR_FY19 == 1) %>% 
   group_by(Exited, IsWaiting) %>% 
   summarise(count = n_distinct(StFCID)) 
total_TPR_in_FY19 <- sum(t$count)

# of parents who had tpr in 19
t <- foster19 %>% 
   filter(IsTPR == 1 & TPR_FY19 == 1) %>% 
   group_by(StFCID, TPRMomDt, TPRDadDt) %>% 
   summarise(count = n()) 

#race rate for having TPR in Fy19
race_tpr <- foster19 %>% 
   filter(IsTPR == 1 & TPR_FY19 == 1) %>% 
   group_by(RaceEthn) %>% 
   summarise(count = n_distinct(StFCID)) %>% 
   left_join(US_race) %>% 
   mutate(rate = (count/pop) * 10000) %>%
   filter(!is.na(race)) %>%
   mutate(ratio_white = round(rate/rate[race == "White" ], 2))
write_csv(race_tpr, outputfiles$race_tpr)

#race rates in the states of interest
tpr_states <- foster19 %>% 
   filter(IsTPR == 1 & TPR_FY19 == 1) %>% 
   filter(St == "CA" | St == "NY" | 
             St == "OK" | St == "WV") %>% 
   group_by(state_fips, RaceEthn) %>% 
   summarise(count = n_distinct(StFCID)) %>% 
   left_join(state_race) %>% 
   mutate(rate = (count/pop) * 10000) %>%
   filter(!is.na(race)) %>%
   mutate(ratio_white = round(rate/rate[race == "White" ], 2))

#total rate of termination by state.
tpr_states <- foster19 %>% 
   filter(IsTPR == 1 & TPR_FY19 == 1) %>% 
   group_by(state_fips) %>% 
   summarise(count = n_distinct(StFCID)) %>% 
   left_join(state) %>% 
   mutate(rate = (count/pop) * 10000)
write_csv(tpr_states, outputfiles$tpr_states)


#Parental rights and adoption
adopt <- foster19 %>% 
   filter(IsTPR == 1 & TPR_FY19 == 1) %>% 
   group_by(RaceEthn, IsWaiting) %>% 
   summarise(count = n_distinct(StFCID)) %>% 
   pivot_wider(names_from = IsWaiting, values_from = count) %>% 
   mutate(total = `0` + `1`,
          perc = `1`/total) %>% 
   left_join(US_race)

total_adopt <- adopt %>% 
   ungroup() %>% 
   summarise(adopted = sum(`0`),
             waiting = sum(`1`),
             tot = sum(total)) %>% 
   mutate(perc = waiting/tot)

#proportion of children in foster care system sometime in 
#fy19 who had rights terminated in FY19
t1 <- foster19 %>% 
   group_by(RaceEthn) %>% 
   summarise(num_in_system = n_distinct(StFCID))  
t2 <- foster19 %>% 
   filter(TPR_FY19 == 1) %>% 
   group_by(RaceEthn) %>% 
   summarise(tpr19 = n_distinct(StFCID)) 

table <- left_join(t1, t2) %>% 
   mutate(proportion_tpr = tpr19/num_in_system) %>% 
   left_join(US_race)

#length of time to fiscal year end from TPR
foster19 <- foster19 %>% 
   mutate(days_since_tpr = 
             interval(TPRDate, "2019-10-01")/ 
             days(1))

waiting <- foster19 %>% 
   filter(IsTPR == 1 & IsWaiting == 1) %>% 
   group_by(RaceEthn) %>% 
   summarise(`Median days since TPR` = median(days_since_tpr),
             `Mean days since TPR` = mean(days_since_tpr)) %>% 
   left_join(US_race) %>% 
   ungroup() %>% 
   select(Race = race,
          `Median days since TPR`, 
          `Mean days since TPR` ) %>% 
   filter(!is.na(Race))
write_csv(waiting, outputfiles$waiting_adoption)

#for those waiting, with TPR, proportion  
#are in a pre-adoptive home
waiting <- foster19 %>% 
   filter(IsWaiting == 1 & IsTPR == 1 &
              !(CASEGOAL == 5 & AgeAtEnd > 16)) %>% 
   group_by(CURPLSET) %>% 
   summarise(count = n_distinct(StFCID)) %>% 
   mutate(perc = count/sum(count)) %>% 
   filter(CURPLSET == 1) 
rm(waiting)

#differences in setting between our focus states?
waiting <- foster19 %>% 
   filter(IsWaiting == 1 & IsTPR == 1 &
             !(CASEGOAL == 5 & AgeAtEnd > 16)) %>% 
   filter(St == "CA" | St == "NY" | 
             St == "OK" | St == "WV") %>% 
   group_by(St, CURPLSET) %>% 
   summarise(count = n_distinct(StFCID)) %>% 
   mutate(perc = count/sum(count))

#age out?
waiting <- foster19 %>% 
   filter(AgedOut == 1) %>% 
   group_by(IsTPR) %>% 
   summarise(count = n_distinct(StFCID)) %>% 
   mutate(perc = count/sum(count)) 


#Where are people?
waiting <- foster19 %>%
   filter(Exited == 0 & IsTPR == 1) %>%
   group_by(CURPLSET) %>%
   summarise(count = n_distinct(StFCID)) %>%
   mutate(perc = count/sum(count))


