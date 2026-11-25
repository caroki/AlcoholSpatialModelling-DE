# CannaStreet Alcohol
# Data preprocessing
# Carolin Kilian
# 07-11-2025

library(haven)
library(tidyverse)
library(data.table)

# Load data
rm(list = ls())
setwd("/Users/carolinkilian/Local data space/CannaStreet/data_cannastreet_20260318/SAV/")
DATE <- 20261120

cannastreet_t0 <- read_sav("full_GER_wave1_cleaned_20260318.sav")
cannastreet_t1 <- read_sav("full_GER_wave2_cleaned_20260318.sav")
cannastreet_t2 <- read_sav("full_GER_wave3_cleaned_20260318.sav")

# ------------------------------------------------------------------------------------------------------------------

# CannaStreet

# merge waves and exclude those interviewed twice
cannastreet_t1_cs <- cannastreet_t1 %>% filter(GS_CS == 1)
cannastreet_t2_cs <- cannastreet_t2 %>% filter(GS_CS == 1)
cannastreet <- cannastreet_t0 %>% bind_rows(cannastreet_t1_cs, cannastreet_t2_cs) #checked: warnings not relevant

# definition of relevant covariates

data <- cannastreet %>% 
  mutate(jahr = substr(START, 1, 4),
         sex = factor(ifelse(sex == 1, 2, ifelse(sex == 2, 1, ifelse(sex == 3, 3, NA))), 
                      levels = c(1,2,3), labels = c("men","women","other")),
         age_gr = factor(case_when(
           age_in_years >= 18 & age_in_years <= 24 ~ 1,
           age_in_years >= 25 & age_in_years <= 44 ~ 2,
           age_in_years >= 45 & age_in_years <= 64 ~ 3,
           age_in_years >= 65 & age_in_years <= 99 ~ 4,
           TRUE ~ NA),
           levels = c(1,2,3,4),
           labels = c("18-24","25-44","45-64","65-99")),
         education = factor(edu_group,
           levels = c(1,2,3),
           labels = c("low", "med", "high")),
         AGS8 = sprintf("%08s", AGS),
         gkz = substr(AGS8, 1, 5),
         survey = "cannastreet",
         sample = factor(ifelse(is.na(PANEL), 1, PANEL), 
                         levels = c(1, 2),
                         labels = c("Bilendi", "Norstat"))) %>% 
  rename("alter" = "age_in_years", "DEGURBA" = "DGURBA")

# definition of alcohol use

pdat <- data %>% 
  mutate(alc_any = case_when(
           alcohol_freq >= 2 ~ 1,
           alcohol_freq == 1 ~ 0,
           TRUE ~ NA),
         alc_daily = case_when(
           alcohol_freq == 5 ~ 1,
           alcohol_freq < 5 ~ 0,
           TRUE ~ NA))

# clean data (drop missing data or exclude if not relevant) 

cdat <- pdat %>% 
  # drop other sex, n = 81 / 28111
  filter(sex != "other") %>% 
  # drop if no education indicated, n = 41 / 28030 
  filter(!is.na(education)) %>% 
  filter(gkz != "00000")
         
      # total n excluded: n = 176 / 28111

# selection of relevant variables

cdat_short <- cdat %>% 
  dplyr::select(c(jahr, survey, sample, gkz, sex, alter, age_gr, education, alc_any, alc_daily, DEGURBA, weights_ohneWiederbefragt, weights_gesamt))

# ------------------------------------------------------------------------------------------------------------------

# save 

saveRDS(cdat_short, paste0("/Users/carolinkilian/Desktop/Research projects/Alcohol Spatial Modelling/Data/", DATE,"_cannastreet.rds"))
