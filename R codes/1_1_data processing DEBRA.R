# DEBRA Alcohol
# Data preprocessing
# Carolin Kilian
# 07-11-2025

library(haven)
library(tidyverse)
library(data.table)

# Load data
rm(list = ls())
setwd("/Users/carolinkilian/Local data space/DEBRA/")
DATE <- 20260427

debra <- read_sav("DEBRA_up-toW04_2025_korrigiertMigration_degurba.sav")

# ------------------------------------------------------------------------------------------------------------------

# DEBRA

# definition of relevant covariates

data <- debra %>% 
  mutate(proxy = jahr*100 + welle) %>% arrange(proxy) %>% 
  mutate(surveywave = as.numeric(as.factor(proxy)),
         sex = factor(sex, levels = c(1,2,3), labels = c("men","women","other")),
         age_gr = factor(case_when(
           alter >= 18 & alter <= 24 ~ 1,
           alter >= 25 & alter <= 44 ~ 2,
           alter >= 45 & alter <= 64 ~ 3,
           alter >= 65 & alter <= 99 ~ 4,
           TRUE ~ NA),
           levels = c(1,2,3,4),
           labels = c("18-24","25-44","45-64","65-99")),
         education = factor(case_when(
           schule == 1 | schule == 2 ~ 1, 
           schule == 7 | berab_1 == 1 & schule < 5 ~ 2,
           schule == 5 | schule == 6 | berab_2 == 1 | berab_3 == 1 | berab_4 == 1 ~ 3,
           TRUE ~ NA),
           levels = c(1,2,3),
           labels = c("low", "med", "high")),
         migration = case_when(
           parentsmigration == 1 ~ 1,
           parentsmigration == 2 ~ 0,
           TRUE ~ NA),
         season = factor(case_when(
           welle == 1 | welle == 2 | welle == 12 ~ 1, # Jan, Feb, Dec 
           welle == 4 | welle == 5 | welle == 3 ~ 2, # Apr, May, Mar, 
           welle == 7 | welle == 8 | welle == 6 ~ 3, # Jul, Aug, Jun
           welle == 10 | welle == 11 | welle == 9 ~ 4, # Oct, Nov, Sep 
           TRUE ~ NA), 
           levels = c(1,2,3,4),
           labels = c("winter", "spring", "summer", "autumn")))

# definition of alcohol use

pdat <- data %>% 
  filter(surveywave >= 13 & surveywave <= 18 | surveywave >= 28 & surveywave <= 51) %>% 
  mutate(alc_any = case_when(
    auditc1 >= 2 & auditc1 < 9 ~ 1,
    auditc1 == 1 ~ 0,
    TRUE ~ NA),
    alc_daily = case_when(
      auditc1 == 5 ~ 1,
      auditc1 < 5 ~ 0,
      TRUE ~ NA),
    alc_hed = case_when(
      auditc3 == 5 ~ 1,
      auditc3 < 5 ~ 0,
      auditc1 == 1 ~ 0, 
      TRUE ~ NA),
    # impute cases in wave 28-31 where respondents were not shown auditc3 if auditc1==2, assume auditc3 = 0
    alc_hed = case_when(
      surveywave >= 28 & surveywave <= 31 & auditc1 == 2 ~ 0,
      TRUE ~ alc_hed))

# clean data (drop missing data or exclude if not relevant) 

cdat <- pdat %>% 
  # drop other sex, n = 23 / 62323
  filter(sex != "other") %>% 
  # drop age 14-17, n = 1475 / 62300 (2.4%)
  filter(alter >= 18) %>% 
  # drop if no education indicated, n = 665 / 60825 (1.1%)
  filter(!is.na(education)) # %>% 
# for now: disregard migration variable
# drop if no migration background indicated, n = 3262 / 60324 
# filter(parentsmigration != 9) %>% 
# drop if missing alcohol use, n = 884 / 57062 NOT NEEDED FOR INLA
# filter(!is.na(alc_daily) & !is.na(alc_hed))

# total n excluded: n = 2163 / 62323 (3.5%)

# selection of relevant variables

cdat_short <- cdat %>% 
  select(c(jahr, season, surveywave, gkz, sex, alter, age_gr, education, migration, alc_any, alc_daily, alc_hed, DEGURBA))

# ------------------------------------------------------------------------------------------------------------------

# save 

saveRDS(cdat_short, paste0("/Users/carolinkilian/Desktop/Research projects/DEBRA Alkohol/Data/", DATE,"_debra.rds"))
