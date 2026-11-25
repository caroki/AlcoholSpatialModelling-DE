# DEBRA Alcohol
# Data preprocessing
# Carolin Kilian
# 07-11-2025

library(haven)
library(readr)
library(tidyverse)
library(MASS)
library(ggplot2)
library(data.table)
library(sf)
library(openxlsx)

# Load data
rm(list = ls())
setwd("/Users/carolinkilian/Desktop/Research projects/Alcohol Spatial Modelling/Data/")
DATE <- 20261120

# Load surveys

debra <- readRDS("20261120_debra.rds")
cannastreet <- readRDS("20261120_cannastreet.rds")  

# Load area-level data 

#gisd <- read.csv("GISD_Bund_Kreis.csv") %>% mutate(gkz_5 = factor(sprintf("%05s", kreis_id)))
gisd <- read_tsv("GISD_Bund_2026update.tsv") %>% filter(region_type %like% "Kreis")
hosp_2024 <- read.csv("hospital discharge/23131-0003_de_flat_2024.csv", sep = ";", na.strings = "-")
hosp_2023 <- read.csv("hospital discharge/23131-0003_de_flat_2023.csv", sep = ";", na.strings = "-")
hosp_2022 <- read.csv("hospital discharge/23131-0003_de_flat_2022.csv", sep = ";", na.strings = "-")
hosp_2021 <- read.csv("hospital discharge/23131-0003_de_flat_2021.csv", sep = ";", na.strings = "-")
hosp_2020 <- read.csv("hospital discharge/23131-0003_de_flat_2020.csv", sep = ";", na.strings = "-")
hosp_2019 <- read.csv("hospital discharge/23131-0003_de_flat_2019.csv", sep = ";", na.strings = "-")
hosp_2018 <- read.csv("hospital discharge/23131-0003_de_flat_2018.csv", sep = ";", na.strings = "-")
rpop <- read.csv("12411-0013_de_flat.csv", sep = ";")
pop2019 <- read.csv("12411-02-03-4_flat.csv", sep = ";")
edu1824 <- read.csv("2000S-3057_de_flat_1824.csv", sep = ";", na.strings = "/")
edu2599 <- read.csv("2000S-3058_de_flat_2599.csv", sep = ";", na.strings = "/")
degurba_2021 <- st_read("DGURBA_RG_01M_2021_4258/DGURBA_RG_01M_2021_4258.shp") %>% filter(CNTR_CODE == "DE") 
de_sf <- st_read("NUTS_RG_20M_2024_3035.shp/NUTS_RG_20M_2024_3035.shp") %>% filter(CNTR_CODE == "DE")
nuts3code <- read.xlsx("04-kreise.xlsx", sheet = "Kreisfreie Städte u. Landkreise", startRow = 2) %>% dplyr::select(c("Amtlicher.Regionalschlüssel", "NUTS3")) %>% filter(!is.na(NUTS3))

# ------------------------------------------------------------------------------------------------------------------

# Combine surveys

data <- debra %>% 
  dplyr::select(-c(season, surveywave, migration)) %>% 
  mutate(survey = "debra",
         jahr = paste0("20",jahr)) %>% 
  bind_rows(cannastreet) %>% 
  mutate(year = as.numeric(jahr),
         survey = ifelse(survey == "cannastreet" & year == 2025, "cannastreet_mixed",
                         ifelse(survey == "cannastreet" & year != 2025, "cannastreet_bilendi", survey)))

# German Index of Social Deprivation

# impute GISD score for years 2024-2025 using linear prediction

ggplot(gisd) + geom_line(aes(x = year, y = gisd_score, color = region_id, group = region_id)) + theme_bw() + theme(legend.position = "")

sub <- gisd %>% dplyr::select(region_id) %>% sample_n(50) %>% pull()
gisd_sub <- gisd %>% filter(region_id %in% sub) %>% filter(year >= 2010)
ggplot(gisd_sub) + geom_line(aes(x = year, y = gisd_score, color = region_id, group = region_id)) + theme_bw() + theme(legend.position = "") 

model <- glmmPQL(gisd_score ~ year, random = ~ year|region_id, family = quasibinomial(link = "logit"), data = gisd[gisd$year >= 2010,])
summary(model)

gisd_pred <- gisd %>% filter(year == 2022 | year == 2023) %>% 
  mutate(year = year + 2,
         gisd_score = NA,
         gisd_5 = NA,
         gisd_10 = NA,
         gisd_k = NA, 
         predicted = 1)

gisd_pred$gisd_score <- predict(model, newdata = gisd_pred, type = "response")

gisd_imp <- gisd %>% mutate(predicted = 0) %>% rbind(., gisd_pred) %>%
  rename("gkz_5" = "region_id")

# merge

pdat_gisd <- data %>%
  mutate(gkz_5 = factor(substr(gkz, 1, 5))) %>%
  merge(., gisd_imp, by = c("gkz_5", "year"), all.x = T)

pdat_gisd %>% filter(is.na(gisd_score)) # n = 21, gkz = 16056 -> double-checked, refers to Eisenach Stadt 

pdat_gisd <- data %>%
  mutate(gkz_5 = factor(substr(gkz, 1, 5)),
         gkz_5 = factor(ifelse(gkz_5 == "16056", "16063", sprintf("%05s", gkz_5)))) %>%
  merge(., gisd_imp, by = c("gkz_5", "year"), all.x = T)

pdat_gisd %>% filter(is.na(gisd_score)) # n = 0

# ------------------------------------------------------------------------------------------------------------------

# alcohol-specific hospital discharges

pop <- rpop %>% dplyr::select(c(time, X1_variable_attribute_label, X2_variable_attribute_label, X3_variable_attribute_label, value)) %>% 
  rename_with(~ c("year", "state", "sex", "age", "pop"), everything()) %>% 
  filter(age != "Insgesamt") %>% mutate(age = as.numeric(gsub("[^0-9]", "", age))) %>% 
  filter(age >= 18) %>% 
  mutate(age_gr = factor(case_when(
    age >= 18 & age < 20 ~ "1820",
    age >= 20 & age < 25 ~ "2025",
    age >= 25 & age < 30 ~ "2530",
    age >= 30 & age < 35 ~ "3035",
    age >= 35 & age < 40 ~ "3540",
    age >= 40 & age < 45 ~ "4045",
    age >= 45 & age < 50 ~ "4550",
    age >= 50 & age < 55 ~ "5055",
    age >= 55 & age < 60 ~ "5560",
    age >= 60 & age < 65 ~ "6065",
    age >= 65 & age < 70 ~ "6570",
    age >= 70 & age < 75 ~ "7075",
    age >= 75 & age < 80 ~ "7580",
    age >= 80 & age < 85 ~ "8085",
    age >= 85 & age < 90 ~ "8590",
    age >= 90 ~ "90+",
    TRUE ~ NA)),
    year = substr(year, 1, 4)) %>% 
  group_by(year, state, sex, age_gr) %>% 
  summarise(pop = sum(pop)) %>% 
  mutate(state_id = case_when(
    state == "Schleswig-Holstein" ~ "01",
    state == "Hamburg" ~ "02",
    state == "Niedersachsen" ~ "03",
    state == "Bremen" ~ "04",
    state == "Nordrhein-Westfalen" ~ "05",
    state == "Hessen" ~ "06",
    state == "Rheinland-Pfalz" ~ "07",
    state == "Baden-Württemberg" ~ "08",
    state == "Bayern" ~ "09",
    state == "Saarland" ~ "10",
    state == "Berlin" ~ "11",
    state == "Brandenburg" ~ "12",
    state == "Mecklenburg-Vorpommern" ~ "13",
    state == "Sachsen" ~ "14",
    state == "Sachsen-Anhalt" ~ "15",
    state == "Thüringen" ~ "16",
    TRUE ~ NA))

hosp <- do.call(rbind, mget(ls(pattern = "hosp_"))) %>% dplyr::select(c(time, X4_variable_attribute_label, X5_variable_attribute_code, X2_variable_attribute_label, X3_variable_attribute_label, value)) %>% 
  rename_with(~ c("year", "state", "icd", "sex", "age", "hosp"), everything()) %>% 
  mutate(age_gr = gsub("[^0-9]", "", age),
         age_gr = ifelse(age_gr == "9095" | age_gr == "95", "90+", age_gr)) %>% 
  filter(age_gr != "" & age_gr != "1" & age_gr != "15" & age_gr != "1518" & age_gr != "510" & age_gr != "1015") %>% 
  filter(icd == "ICD10-F10" | icd == "ICD10-K70" | icd == "ICD10-T51")

ggplot(hosp) + geom_point(aes(x = year, y = hosp, group = icd, color = icd)) + facet_wrap(~ state) + theme_bw()

# exclude state == "Ausland" or "unbekannt", calculate age-standardised hosp rate (2018 standard population)
pop_standard <- pop %>% filter(year == "2018") %>% 
  rename(standard_pop = pop) %>% ungroup() %>% dplyr::select(-year)

hosprate <- hosp %>% filter(state != "Ausland" & state != "unbekannt") %>% 
  group_by(year, state, age_gr, sex) %>% 
  summarise(hosp = sum(hosp, na.rm = T)) %>% 
  merge(., pop, by = c("year", "state", "age_gr", "sex"), all.x = T) %>% 
  mutate(hosprate_age = hosp / pop) %>% 
  left_join(., pop_standard) %>%
  group_by(year, state, state_id, sex) %>% 
  summarise(hosprate = sum(hosprate_age * standard_pop) / sum(standard_pop) * 1000) %>% 
  dplyr::select(-sex)

ggplot(hosprate) + geom_line(aes(x = year, y = hosprate, group = state, color = state)) + theme_bw()
      
# merge
hosprate_25 <- hosprate %>% filter(year == 2024) %>% mutate(year = 2025)
hosprate_imp <- rbind(hosprate, hosprate_25)

pdat_gisd_hosp <- pdat_gisd %>%
  mutate(state_id = substr(gkz_5, 1, 2)) %>%
  left_join(., hosprate_imp)

pdat_gisd_hosp %>% filter(is.na(hosprate)) # n = 0

# ------------------------------------------------------------------------------------------------------------------

# PREDICTION FILE

# MERGE education data files

pop.edu1824 <- edu1824 %>% 
  mutate(age_gr = factor(1,
           levels = c(1,2,3,4),
           labels = c("18-24","25-44","45-64","65-99")),
         education = factor(case_when(
           X2_variable_attribute_code %like% "ISCED-1|2|3" ~ 1,
           X2_variable_attribute_code %like% "ISCED-4|5" ~ 2,
           X2_variable_attribute_code %like% "ISCED-6|7|8" ~ 3,
           TRUE ~ NA),
           levels = c(1,2,3),
           labels = c("low","med","high")),
         sex = factor(case_when(
           X4_variable_attribute_label == "Männlich" ~ 1,
           X4_variable_attribute_label == "Weiblich" ~ 2,
           TRUE ~ NA),
           levels = c(1,2), labels = c("men","women")),
         kreis_id = ifelse(nchar(X1_variable_attribute_code) != 5,
                           sprintf("%05s", X1_variable_attribute_code), X1_variable_attribute_code)) %>% 
  dplyr::select(c(kreis_id, sex, age_gr, education, value))

pop.edu <- edu2599 %>% 
  mutate(age = gsub("[^0-9]", "", X3_variable_attribute_label),
         age_gr = factor(case_when(
           age %like% "2529|3034|3539|4044" ~ 2, 
           age %like% "4549|5054|5559|6064" ~ 3, 
           age %like% "6569|7074|7579|8084|8589|90" ~ 4, 
           TRUE ~ NA),
           levels = c(1,2,3,4),
           labels = c("18-24","25-44","45-64","65-99")),
         education = factor(case_when(
           X2_variable_attribute_code %like% "ISCED-1|2|3" ~ 1,
           X2_variable_attribute_code %like% "ISCED-4|5" ~ 2,
           X2_variable_attribute_code %like% "ISCED-6|7|8" ~ 3,
           TRUE ~ NA),
           levels = c(1,2,3),
           labels = c("low","med","high")),
         sex = factor(case_when(
           X4_variable_attribute_label == "Männlich" ~ 1,
           X4_variable_attribute_label == "Weiblich" ~ 2,
           TRUE ~ NA),
           levels = c(1,2), labels = c("men","women")),
         kreis_id = ifelse(nchar(X1_variable_attribute_code) != 5,
                           sprintf("%05s", X1_variable_attribute_code), X1_variable_attribute_code)) %>% 
  dplyr::select(c(kreis_id, sex, age_gr, education, value)) %>% 
  rbind(pop.edu1824) %>% group_by(kreis_id, sex, age_gr, education) %>% 
  summarise(pop = sum(value, na.rm = T))

# just check total number of population
pop.edu %>% group_by(age_gr) %>% summarise(pop = sum(pop))


# MERGE area-level data

# for DEGURBA: select most relevant factor level by region based on 2021 DEGURBA
degurba <- degurba_2021 %>% 
  mutate(LAU_ID = sprintf("%08s", LAU_ID), 
         kreis_id = as.character(substr(LAU_ID, 1, 5))) %>% sf::st_drop_geometry() %>% 
  group_by(kreis_id) %>% summarise(DEGURBA = round(mean(DGURBA), 0))

pred.pop <- gisd %>% filter(year == 2021) %>% rename("gkz_5" = "region_id") %>%
  merge(., pop.edu, by.x = "gkz_5", by.y = "kreis_id", all.x = T) %>% 
  merge(., degurba, by.x = "gkz_5", by.y = "kreis_id", all.x = T) %>%  
  mutate(state_id = substr(gkz_5, 1, 2)) %>% 
  merge(., hosprate, by = c("state_id","year"), all.x = T)

# Replicate data to generate individual observations 
w <- min(pred.pop[pred.pop$pop > 0,]$pop)
pred.dat <- pred.pop %>% filter(pop > 0) %>% 
  mutate(factor = pop / w) %>% # have at least w (smallest number > 0) observations per group
  uncount(weights = round(pop / factor, 0), .id = "id")

# Prepare observed and prediction data for merging 
pdat_combined <- pdat_gisd_hosp %>% 
  group_by(gkz_5, sex, age_gr, education) %>% 
  mutate(id = 1:n(), source = "orig") 

temp <- pred.dat %>% mutate(source = "pred") %>% 
  dplyr::select(c(gkz_5, sex, age_gr, education, DEGURBA, gisd_score, hosprate, id, source)) 
names <- names(temp)

# Merge cleaned prediction data with observed data
pred.dat <- pdat_combined %>% ungroup() %>% 
  dplyr::select(all_of(names), year, survey, sample, alc_daily) %>% 
  bind_rows(., temp) %>% 
  # set default
  mutate(year = ifelse(is.na(year), 2022, year),
         survey = ifelse(is.na(survey), "debra", survey),
         sample = factor(ifelse(is.na(sample), 3, sample),
                         levels = c(3, 1, 2),
                         labels = c("Debra", "Bilendi", "Norstat")))

# check

table(pred.dat$source, pred.dat$year)
table(pred.dat$source, pred.dat$sample)

# Get alternative prediction data file based on DEBRA only (for sensitivity analysis)
#pdat_debra <- pdat_combined %>% filter(survey == "debra")

#temp <- pred.dat %>% mutate(source = "pred") %>% 
#  dplyr::select(c(gkz_5, sex, age_gr, education, DEGURBA, gisd_score, hosprate, id, source)) %>% 
#  anti_join(., pdat_debra, by = c("gkz_5", "sex", "age_gr", "education", "id")) 
#names <- names(temp)

# Merge cleaned prediction data with observed data
# pred.dat_debra <- pdat_debra %>% ungroup() %>% 
#   dplyr::select(all_of(names), year, survey, sample, alc_daily) %>% 
#   bind_rows(., temp) %>% 
#   # set default
#   mutate(year = ifelse(is.na(year), 2022, year))
# 
# # Get alternative prediction data file based on 2022-2025 data only (for sensitivity analysis)
# pdat_2022 <- pdat_combined %>% filter(year >= 2022)
# 
# temp <- pred.dat %>% mutate(source = "pred") %>% 
#   dplyr::select(c(gkz_5, sex, age_gr, education, DEGURBA, gisd_score, hosprate, id, source)) %>% 
#   anti_join(., pdat_2022, by = c("gkz_5", "sex", "age_gr", "education", "id")) 
# names <- names(temp)
# 
# # Merge cleaned prediction data with observed data
# pred.dat_2022 <- pdat_2022 %>% ungroup() %>% 
#   dplyr::select(all_of(names), year, survey, sample, alc_daily) %>% 
#   bind_rows(., temp) %>% 
#   # set default
#   mutate(year = ifelse(is.na(year), 2022, year),
#          survey = ifelse(is.na(survey), "debra", survey),
#          sample = factor(ifelse(is.na(sample), 3, sample),
#                          levels = c(3, 1, 2),
#                          labels = c("Debra", "Bilendi", "Norstat")))
# 

# ------------------------------------------------------------------------------------------------------------------

# save 

saveRDS(pdat_gisd_hosp, paste0(DATE,"_data_combined.rds"))
saveRDS(pred.dat, paste0(DATE,"_prediction.rds"))
 
# saveRDS(pred.dat_debra, paste0(DATE,"_prediction_debra.rds"))
# saveRDS(pred.dat_2022, paste0(DATE,"_prediction_2022f.rds"))
