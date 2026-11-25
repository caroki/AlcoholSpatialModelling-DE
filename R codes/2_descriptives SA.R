# DEBRA Alcohol
# Descriptives
# Carolin Kilian
# 07-11-2025

library(tidyverse)
library(openxlsx)
library(Hmisc)
library(ggplot2)
library(sf)
library(scales)
library(haven)
library(ggsflabel)


# Load data
rm(list = ls())
setwd("/Users/carolinkilian/Desktop/Research projects/Alcohol Spatial Modelling/Data/")
path_out <- "/Users/carolinkilian/Desktop/Research projects/Alcohol Spatial Modelling/Github/AlcoholSpatialModelling-DE/Output/"
DATE <- 20261120

data <- readRDS("20261120_data_combined.rds")
de_sf <- st_read("NUTS_RG_20M_2024_3035.shp/NUTS_RG_20M_2024_3035.shp") %>% filter(CNTR_CODE == "DE")
de_sf_states <- st_read("NUTS_RG_20M_2024_3035.shp/NUTS_RG_20M_2024_3035.shp") %>% filter(CNTR_CODE == "DE") %>% filter(LEVL_CODE == 1)
nuts3code <- read.xlsx("04-kreise.xlsx", sheet = "Kreisfreie Städte u. Landkreise", startRow = 2) %>% dplyr::select(c("Amtlicher.Regionalschlüssel", "NUTS3")) %>% filter(!is.na(NUTS3))

# ------------------------------------------------------------------------------------------------------------------

# Functions

format_prop <- function(var) {
  x <- round((var * 100), 1)
  out <- sprintf("%.1f", x)
  return(out)
}

format_2 <- function(var) {
  x <- round((var), 2)
  out <- sprintf("%.1f", x)
  return(out)
}

format_n <- function(var) {
  x <- round((var), 0)
  out <- format(x, big.mark = ",", nsmall = 0)
  return(out)
}

lci_prop <- function(var, weight) {
  prop <- wtd.mean(var, weight, na.rm = T)
  n <- length(na.omit(var))
  se <- 1.96 * sqrt(prop * (1 - prop) / n)
  lci <- prop - se
  return(lci)
}

uci_prop <- function(var, weight) {
  prop <- wtd.mean(var, weight, na.rm = T)
  n <- length(na.omit(var))
  se <- 1.96 * sqrt(prop * (1 - prop) / n)
  uci <- prop + se
  return(uci)  
}

# ------------------------------------------------------------------------------------------------------------------

# combine data from surveys

# STRATIFIED SAMPLE: YEAR and SURVEY
# note: this does not account for skewed sample distribution, as the spatial model. Still to be done.

dat_desc_survey_year <- data %>% 
  filter(!is.na(alc_daily)) %>% 
  mutate(sex_di = ifelse(sex == "men", 0, ifelse(sex == "women", 1, NA)),
         DEGURBA_rural = ifelse(as.numeric(DEGURBA) == 3, 1, ifelse(as.numeric(DEGURBA) != 3, 0, NA)), 
         DEGURBA_cities = ifelse(as.numeric(DEGURBA) == 1, 1, ifelse(as.numeric(DEGURBA) != 1, 0, NA)),
         education_low = ifelse(education == "low", 1, ifelse(education != "low", 0, NA)),
         education_high = ifelse(education == "high", 1, ifelse(education != "high", 0, NA)),
         # get supra-region variable
         mreg = case_when(
           state %like% "Bayern|Baden" ~ "South",
           state %like% "Nordrhein|Hessen|Rheinland|Saarland" ~ "West",
           state %like% "Bremen|Hamburg|Niedersachsen|Schleswig" ~ "North",
           state %like% "Thüringen|Sachsen|Sachsen-Anhalt|Brandenburg|Mecklenburg|Berlin" ~ "East",
           TRUE ~ NA),
         # get combined weight variable from different survey samples
         weight = case_when(
           survey %like% "debra" ~ Smoker_pp,
           survey %like% "cannastreet" & year == 2023 ~ weights_gesamt,
           survey %like% "cannastreet" & year > 2023 ~ weights_ohneWiederbefragt,
           TRUE ~ NA)) %>% 
  group_by(mreg, survey, year) %>% 
  summarise(nsample = n(),
            women = wtd.mean(sex_di, weight), lci_women = lci_prop(sex_di, weight), uci_women = uci_prop(sex_di, weight), 
            age = wtd.mean(alter, weight), age_sd = sqrt(wtd.var(alter, weight)),
            edu_low = wtd.mean(education_low, weight), lci_edu_low = lci_prop(education_low, weight), uci_edu_low = uci_prop(education_low, weight),
            edu_high = wtd.mean(education_high, weight), lci_edu_high = lci_prop(education_high, weight), uci_edu_high = uci_prop(education_high, weight),
            alcDaily = wtd.mean(alc_daily, weight, na.rm=T), lci_alcDaily = lci_prop(alc_daily, weight), uci_alcDaily = uci_prop(alc_daily, weight)) 


# get summary table

dat_out_survey_year <- dat_desc_survey_year %>% 
  mutate_at(vars(women, lci_women, uci_women,
                 edu_low, lci_edu_low, uci_edu_low,
                 edu_high, lci_edu_high, uci_edu_high,
                 alcDaily, lci_alcDaily, uci_alcDaily), format_prop) %>% 
  mutate_at(vars(age, age_sd), format_2) %>% 
  mutate(women = paste0(women, " (", lci_women, "-", uci_women, ")"),
         age = paste0(age, " (", age_sd, ")"),
         education_low = paste0(edu_low, " (", lci_edu_low, "-", uci_edu_low, ")"),
         education_high = paste0(edu_high, " (", lci_edu_high, "-", uci_edu_high, ")"),
         alcDaily = paste0(alcDaily, " (", lci_alcDaily, "-", uci_alcDaily, ")")) %>% 
  dplyr::select(c(mreg, year, nsample, women, age, education_low, education_high, alcDaily)) 

write.xlsx(dat_out_survey_year, paste0(path_out, Sys.Date(), "_Descriptives_YearSurvey.xlsx"))

# get figure (alcDaily only)

ggplot(dat_desc_survey_year, aes(x = year, y = alcDaily, group = survey)) + 
  geom_point(aes(shape = survey, color = survey), size = 2) + 
  geom_errorbar(aes(ymin = lci_alcDaily, ymax = uci_alcDaily, color = survey), width = 0.4) + 
  scale_colour_viridis_d(option = "inferno", begin = 0.2, end = 0.8,
                         labels = c("CannaStreet: Bilendi", "CannaStreet: Mixed", "DEBRA"), name = NULL) + 
  scale_shape_discrete(labels = c("CannaStreet: Bilendi", "CannaStreet: Mixed", "DEBRA"), name = NULL) + 
  scale_y_continuous(labels = scales::percent) + 
  facet_wrap(~ mreg, ncol = 4) + 
  xlab("") + ylab("Observed proportion in %") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 12, color = "black"), 
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.position = "bottom")
#ggsave(paste0(path_out, Sys.Date(), "_AlcDaily_observed_SurveyYear.svg"), width = 12, height = 8, dpi = 300, bg = "white")
