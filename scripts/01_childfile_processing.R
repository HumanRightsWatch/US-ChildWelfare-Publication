#
# Authors:     BR
# Maintainers: BR
# Copyright:   2022
# =========================================
# 

library(pacman)
p_load(lubridate, readxl, readr, rcartocolor, extrafont, scales, tidycensus,
       CGPfunctions, tidyverse, here, qs)
options(scipen=999)

here <- here::here

source("../plot_themes.R")


########### input and output files ##############
# input files:
inputfiles <- list(
   foster_care_15_20 = "import/output/foster_care_file2015_2019.csv",
    child19 = "import/input/child_files/DS237 NCANDS Child File 2019v2/237 Data/Text Files/CF2019v2.tab"
   
) %>% map(here)

#output files. 
outputfiles <- list(
   child_file19 = "processing/output/child_file19.qs"

) %>% map(here)

#read in
df <- read_tsv(inputfiles$child19)

#variables created
#child-report ID
df <- df %>%
   mutate(child_report_ID = paste(RptID, "_", ChID))

#mark the states of interest, create state fips
df <- df %>%
   mutate(state_fips = substr(RptFIPS, 1, 2),
          state_of_interest = ifelse(
             state_fips == "06" |
                state_fips == "40" |
                state_fips ==  "54" |
                state_fips == "36", 1, 0))

#is report this in FY 2019
df <- df %>%
   mutate(FY19_report = ifelse(
      RptDt > "2018-09-30" & RptDt < "2019-10-01",
      1, 0))


#number of report IDs per child and binary multiple reports var.
t <- df %>%
   group_by(ChID) %>%
   summarise(num_reports_per_child = n_distinct(RptID)) %>%
   mutate(multiple_reports = ifelse(num_reports_per_child > 1,
                                    1, 0))

df <- left_join(df, t)

#if child is a victim on ANY report disposed of that year

t <- df %>%
   filter(RptVictim == 1) %>%
   select(ChID) %>%
   distinct()  %>% 
   mutate(victim_any_report = 1)

df <- left_join(df, t)

df <- df %>% 
   mutate(victim_any_report = 
             ifelse(is.na(victim_any_report), 0,
                    victim_any_report))

#maltreatment types
df <- df %>% 
   mutate(`Physical Abuse` = ifelse(
      chmal1 == 1 | chmal2 == 1 | chmal3 == 1 |
         chmal4 == 1, 1, 0),
      `Neglect or Deprivation of Necessities` = ifelse(
         chmal1 == 2 | chmal2 == 2 | chmal3 == 2 |
            chmal4 == 2, 1, 0),
      `Medical Neglect` = ifelse(
         chmal1 == 3 | chmal2 == 3 | chmal3 == 3 |
            chmal4 == 3, 1, 0),
      `Sexual Abuse` = ifelse(
         chmal1 == 4 | chmal2 == 4 | chmal3 == 4 |
            chmal4 == 4, 1, 0),
      `Psychological or Emotional Maltreatment` = ifelse(
         chmal1 == 5 | chmal2 == 5 | chmal3 == 5 |
            chmal4 == 5, 1, 0),
      `No Alleged Maltreatment` = ifelse(
         chmal1 == 6 | chmal2 == 6 | chmal3 == 6 |
            chmal4 == 6, 1, 0),
      `Sex Trafficking` = ifelse(
         chmal1 == 7 | chmal2 == 7 | chmal3 == 7 |
            chmal4 == 7, 1, 0),
      `Other` = ifelse(
         chmal1 == 8 | chmal2 == 8 | chmal3 == 8 |
            chmal4 == 8, 1, 0))

df <- df %>%
   mutate(neglect_only = ifelse(chmal1 == 2 &
                                   is.na(chmal2) &
                                   is.na(chmal3) &
                                   is.na(chmal4),
                                1, 0),
          one_maltreatment_allegation = ifelse(
             !is.na(chmal1) &
                                      is.na(chmal2) &
                                      is.na(chmal3) &
                                      is.na(chmal4),
                                   1, 0))

#number of maltreatment types alleged
df <- df %>% 
   mutate(number_maltreat = case_when(
      is.na(chmal2) ~ 1,
      !is.na(chmal4) ~ 4,
      is.na(chmal4) & !is.na(chmal3) ~ 3,
      is.na(chmal3) & !is.na(chmal2) ~ 2
   ))

#maltreatment codes
df <- df %>% 
   mutate(maltreatment1_type = case_when(
      chmal1 == 1 ~ "Physical Abuse",
      chmal1 == 2 ~ "Neglect or Deprivation of Necessities",
      chmal1 == 3 ~ "Medical Neglect",
      chmal1 == 4 ~ "Sexual Abuse",
      chmal1 == 5 ~ "Psychological or Emotional Maltreatment",
      chmal1 == 6 ~ "No Alleged Maltreatment",
      chmal1 == 7 ~ "Sex Trafficking",
      chmal1 == 8 ~ "Other",
      chmal1 == 9 ~ "Unknown or Missing"
   ))

#parent perpetrator
df <- df %>% 
   mutate(parent_perp = case_when(
          per1rel == 1 ~ 1,
           per2rel == 1 ~ 1,
           per3rel == 1 ~ 1,
           TRUE ~ 0))

#Any neglect found
df <- df %>% 
   mutate(neglect_victim = case_when(
         (chmal1 == 2 & mal1lev < 3) ~ 1,
         (chmal2 == 2 & mal2lev < 3) ~ 1,
         (chmal3 == 2 & mal3lev < 3) ~ 1,
         (chmal4 == 2 & mal4lev < 3) ~ 1,
         TRUE ~ 0))

#parent perp of neglect
df <- df %>% 
   mutate(parent_neglect = ifelse(
      (neglect_victim == 1 & RptVictim == 1 & parent_perp == 1), 1, 0),
      parent_type = case_when(
         (parent_neglect == 1 & per1rel == 1 & (per2rel > 1 | is.na(per2rel)) &
                            (per3rel > 1 | is.na(per3rel))) ~ "Single parent",
         (parent_neglect == 1 & (per1rel > 1 | is.na(per1rel)) & per2rel == 1 & (per3rel >1 | is.na(per3rel))) ~ "Single parent",
         (parent_neglect == 1 & (per1rel > 1 | is.na(per1rel)) & per3rel == 1 & (per2rel >1 | is.na(per2rel))) ~ "Single parent",
         (parent_neglect == 1 & ((per1rel == 1 & per2rel == 1) | 
             (per1rel == 1 & per3rel == 1) |
            (per2rel == 1 & per3rel == 1))) ~ "Multiple parents",
         TRUE ~ "Other")) 

#dates
#combine report datetime
df <- df %>% 
   mutate(report_date_time = ymd_hms(paste0(RptDt, RptTm)),
          investigation_date_time = ymd_hms(paste0(InvDate, InvStrTm))) 

df <- df %>% 
   mutate(hours_between_rpt_invest = interval(report_date_time, investigation_date_time)/ 
             hours(1),
          days_between_rpt_invest = interval(RptDt, InvDate)/ 
             days(1),
          days_between_rpt_dispo = interval(report_date_time, RpDispDt)/ 
             days(1),
          days_between_rpt_service = interval(report_date_time, ServDate)/ 
             days(1), 
          days_between_disp_service = interval(RpDispDt, ServDate)/ 
             days(1), 
          days_between_serv_removal = interval(ServDate, RmvDate)/ 
             days(1),
          days_between_rpt_removal = interval(RptDt, RmvDate)/ 
             days(1))

#duplicate fips for county level joining
df <- df %>% 
   mutate(county_fips = RptFIPS)

#write_out
qsave(df, outputfiles$child_file19)
   

