# DEBRA Alcohol
# Descriptives
# Carolin Kilian
# 07-11-2025

library(tidyverse)
library(openxlsx)
library(ggplot2)
library(sf)
library(scales)
library(haven)
library(ggsflabel)
library(biscale)
library(viridisLite)
library(colorspace)
library(patchwork)

# Load data
rm(list = ls())
setwd("/Users/carolinkilian/Desktop/Research projects/Alcohol Spatial Modelling/Data/")
path_out <- "/Users/carolinkilian/Desktop/Research projects/Alcohol Spatial Modelling/Github/AlcoholSpatialModelling-DE/Output/"
DATE <- 20261106

data <- readRDS("20260629_data_combined.rds")
de_sf <- st_read("NUTS_RG_20M_2024_3035.shp/NUTS_RG_20M_2024_3035.shp") %>% filter(CNTR_CODE == "DE")
de_sf_states <- st_read("NUTS_RG_20M_2024_3035.shp/NUTS_RG_20M_2024_3035.shp") %>% filter(CNTR_CODE == "DE") %>% filter(LEVL_CODE == 1)
nuts3code <- read.xlsx("04-kreise.xlsx", sheet = "Kreisfreie Städte u. Landkreise", startRow = 2) %>% select(c("Amtlicher.Regionalschlüssel", "NUTS3")) %>% filter(!is.na(NUTS3))

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

lci_prop <- function(var) {
  prop <- mean(var, na.rm = T)
  n <- length(na.omit(var))
  se <- 1.96 * sqrt(prop * (1 - prop) / n)
  lci <- prop - se
  return(lci)
}

uci_prop <- function(var) {
  prop <- mean(var, na.rm = T)
  n <- length(na.omit(var))
  se <- 1.96 * sqrt(prop * (1 - prop) / n)
  uci <- prop + se
  return(uci)  
}

# ------------------------------------------------------------------------------------------------------------------

# DESCRIBE SAMPLE

# Sample size 

dat_sum <- data %>% 
  filter(!is.na(alc_daily)) %>% 
  group_by(gkz_5) %>% 
  summarise(n = n(),
            DEGURBA = mean(DEGURBA),
            gisd_10 = mean(gisd_10),
            gisd = mean(gisd_score),
            hosprate = mean(hosprate),
            alc_daily = mean(alc_daily),
            alc_hed = mean(alc_hed)) 

dat_map <- de_sf %>% merge(., nuts3code, by.x = "NUTS_ID", by.y = "NUTS3", all = F) %>% 
  merge(., dat_sum, by.x = "Amtlicher.Regionalschlüssel", by.y = "gkz_5", all.x = T) 

# min / max sample size by Kreis
sum(dat_sum$n)
dat_sum %>% filter(n == min(n)) # n=18, 09776
dat_sum %>% filter(n == max(n)) # n=4919, 11000

dat_map %>% filter(is.na(n)) # no Kreis without data

# Visualise sample size by Kreis

ggplot(dat_map) +
  geom_sf(aes(fill = n), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(option = "rocket", direction = -1, trans = "log", na.value = "grey50", 
                       labels = scales::number_format(accuracy = 1), breaks = c(0,100,500,2500)) + 
  labs(fill = "Sample size") +
  theme_void() +
  theme(legend.title = element_text(size = 16),
        legend.text = element_text(size = 16))

ggsave(paste0(path_out, Sys.Date(), "_SampleSize.svg"), width = 12, height = 8, dpi = 300, bg = "white")

# Sample size by year 

dat_sum_year <- data %>% 
  group_by(year, gkz_5) %>% 
  summarise(n = n()) 

dat_map_year <- de_sf %>% merge(., nuts3code, by.x = "NUTS_ID", by.y = "NUTS3", all = F) %>% 
  merge(., dat_sum_year, by.x = "Amtlicher.Regionalschlüssel", by.y = "gkz_5", all.x = T) %>%
  filter(!is.na(n))

ggplot(dat_map_year) +
  geom_sf(aes(fill = n), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  facet_wrap(~year) + 
  scale_fill_viridis_c(trans = "log", na.value = "grey50", labels = scales::number_format(accuracy = 1)) + 
  labs(fill = "Sample size") +
  theme_void() +
  theme(legend.title = element_text(size = 16),
        legend.text = element_text(size = 16))

#ggsave(paste0(path_out, Sys.Date(), "_SampleSize_Combined_byYear.svg"), width = 12, height = 8, dpi = 300, bg = "white")


# empty map with labels

state_names_german <- c("Baden-Württemberg", "Bayern", "Berlin", "Brandenburg", "Bremen",
                        "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen",
                        "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen",
                        "Sachsen-Anhalt", "Schleswig-Holstein", "Thüringen")
state_names_english <- c("Baden-Wuerttemberg", "Bavaria", "Berlin", "Brandenburg", "Bremen",
                         "Hamburg", "Hesse", "Mecklenburg-Vorpommern", "Lower Saxony",
                         "North Rhine-Westphalia", "Rhineland-Palatinate", "Saarland", "Saxony",
                         "Saxony-Anhalt", "Schleswig-Holstein", "Thuringia")
german_to_english <- setNames(state_names_english, state_names_german)

de_sf_states$NAME_ENGLISH <- german_to_english[de_sf_states$NUTS_NAME]

ggplot(dat_map) +
  geom_sf(fill = NA, color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  geom_sf_label(data = de_sf_states[de_sf_states$NUTS_NAME != "Brandenburg",], aes(label = NAME_ENGLISH), color = "black", size = 3, fill = "white", alpha = 0.8) + 
  geom_sf_label_repel(data = de_sf_states[de_sf_states$NUTS_NAME == "Brandenburg",], aes(label = NAME_ENGLISH), color = "black", size = 3, fill = "white", nudge_y = -5) +
  theme_void() 
ggsave(paste0(path_out, Sys.Date(), "_DE_EmptyMap.svg"), width = 7, height = 5, dpi = 300, bg = "white")

# Individual and area-level characteristics

ggplot(data) + geom_histogram(aes(x = gisd_score)) + facet_wrap(~ state) # use median

dat_desc <- data %>% 
  filter(!is.na(alc_daily)) %>% 
  mutate(sex_di = ifelse(sex == "men", 0, ifelse(sex == "women", 1, NA)),
         DEGURBA_rural = ifelse(as.numeric(DEGURBA) == 3, 1, ifelse(as.numeric(DEGURBA) != 3, 0, NA)), 
         DEGURBA_cities = ifelse(as.numeric(DEGURBA) == 1, 1, ifelse(as.numeric(DEGURBA) != 1, 0, NA)),
         education_low = ifelse(education == "low", 1, ifelse(education != "low", 0, NA)),
         education_high = ifelse(education == "high", 1, ifelse(education != "high", 0, NA))) %>% 
  group_by(state) %>% 
  summarise(nsample = n(),
            women = mean(sex_di), n_women = sum(sex_di),
            age = mean(alter), age_sd = sd(alter),
            edu_low = mean(education_low), n_edu_low = sum(education_low),
            edu_high = mean(education_high), n_edu_high = sum(education_high),
            alcDaily = mean(alc_daily, na.rm=T), n_alcDaily = sum(alc_daily), 
            rural = mean(DEGURBA_rural), n_rural = sum(DEGURBA_rural), 
            cities = mean(DEGURBA_cities), n_cities = sum(DEGURBA_cities), 
            gisd = median(gisd_score), gisd_low = quantile(gisd_score, 0.25), gisd_upp = quantile(gisd_score, 0.75), 
            hosprate = mean(hosprate)) 

dat_out <- dat_desc %>% 
  mutate_at(vars(n_women, n_edu_low, n_edu_high, n_alcDaily, n_rural, n_cities), format_n) %>% 
  mutate_at(vars(women, edu_low, edu_high, alcDaily, rural, cities, 
                 gisd, gisd_low, gisd_upp), format_prop) %>% 
  mutate_at(vars(age, age_sd, hosprate), format_2) %>% 
  mutate(women = paste0(n_women, " (", women, ")"),
         age = paste0(age, " (", age_sd, ")"),
         education_low = paste0(n_edu_low, " (", edu_low, ")"),
         education_high = paste0(n_edu_high, " (", edu_high, ")"),
         alcDaily = paste0(n_alcDaily, " (", alcDaily, ")"),
         rural = paste0(n_rural, " (", rural, ")"),
         cities = paste0(n_cities, " (", cities, ")"),
         gisd = paste0(gisd, " (", gisd_low, "-", gisd_upp, ")")) %>% 
  select(c(state, nsample, women, age, education_low, education_high, alcDaily, rural, cities, gisd, hosprate))

write.xlsx(dat_out,paste0(path_out, Sys.Date(), "_DescriptivesByState.xlsx"))


# Get this data at NUTS3 level

dat_desc_nuts3 <- data %>% 
  filter(!is.na(alc_daily)) %>% # excluded: n = 629, new n = 87507
  mutate(sex_di = ifelse(sex == "men", 0, ifelse(sex == "women", 1, NA)),
         DEGURBA_rural = ifelse(as.numeric(DEGURBA) == 3, 1, ifelse(as.numeric(DEGURBA) != 3, 0, NA)), 
         DEGURBA_cities = ifelse(as.numeric(DEGURBA) == 1, 1, ifelse(as.numeric(DEGURBA) != 1, 0, NA)),
         education_low = ifelse(education == "low", 1, ifelse(education != "low", 0, NA)),
         education_high = ifelse(education == "high", 1, ifelse(education != "high", 0, NA))) %>% 
  group_by(gkz_5, region_name) %>% 
  summarise(nsample = n(),
            women = mean(sex_di), lci_women = lci_prop(sex_di), uci_women = uci_prop(sex_di), 
            age = mean(alter), age_sd = sd(alter),
            edu_low = mean(education_low), lci_edu_low = lci_prop(education_low), uci_edu_low = uci_prop(education_low),
            edu_high = mean(education_high), lci_edu_high = lci_prop(education_high), uci_edu_low = uci_prop(education_high),
            alcDaily = mean(alc_daily, na.rm=T), lci_alcDaily = lci_prop(alc_daily), uci_alcDaily = uci_prop(alc_daily), 
            rural = mean(DEGURBA_rural), lci_rural = lci_prop(DEGURBA_rural), uci_rural = uci_prop(DEGURBA_rural), 
            cities = mean(DEGURBA_cities), lci_cities = lci_prop(DEGURBA_cities), uci_cities = uci_prop(DEGURBA_cities), 
            gisd = median(gisd_score), gisd_low = quantile(gisd_score, 0.25), gisd_upp = quantile(gisd_score, 0.75), 
            hosprate = mean(hosprate)) 

write.xlsx(dat_desc_nuts3, paste0(path_out, Sys.Date(), "_Descriptives.xlsx"))


# Visualise GISD by Kreis

ggplot(dat_map) +
  geom_sf(aes(fill = gisd), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(na.value = "grey50") + 
  labs(fill = "German Index of Social Deprivation\nDEBRA and CannaStreet (Average)\n") +
  theme_void() +
  theme(legend.title = element_text(size = 16),
        legend.text = element_text(size = 16))
#ggsave(paste0(path_out, Sys.Date(), "_GISD_Combined.svg"), width = 12, height = 8, dpi = 300, bg = "white")


# Visualise alc hosp discharge rate by BL

ggplot(dat_map) +
  geom_sf(aes(fill = hosprate), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(na.value = "grey50") + 
  labs(fill = "Alcohol-specific hospital discharges\nper 1,000 population (Average)\n") +
  theme_void() +
  theme(legend.title = element_text(size = 16),
        legend.text = element_text(size = 16))

#ggsave(paste0(path_out, Sys.Date(), "_AlcHospDischarge_Combined.svg"), width = 12, height = 8, dpi = 300, bg = "white")


# Visualise DEGURBA 

ggplot(dat_map) +
  geom_sf(aes(fill = DEGURBA), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(na.value = "grey50", breaks = c(1,2,3), labels = c("cities", "suburbs and towns", "rural areas")) + 
  labs(fill = "DEGURBA\nDEBRA and CannaStreet (Average)\n") +
  theme_void() +
  theme(legend.title = element_text(size = 16),
        legend.text = element_text(size = 16))
#ggsave(paste0(path_out, Sys.Date(), "_DEGURBA_Combined.svg"), width = 12, height = 8, dpi = 300, bg = "white")

# Alcohol outcomes at NUTS3 

# exclude Kreise with n < 30 
select <- data %>% group_by(gkz_5) %>% 
  summarise(nsample = n()) %>% 
  filter(nsample >= 30) %>% pull(gkz_5)

sub <- data %>% filter(gkz_5 %in% select) %>% 
  group_by(gkz_5) %>% 
  summarise(nsample = n(),
            alcDaily = mean(alc_daily, na.rm = T),
            alcHED = mean(alc_hed, na.rm = T)) 

dat_map <- de_sf %>% merge(., nuts3code, by.x = "NUTS_ID", by.y = "NUTS3", all = F) %>% 
  merge(., sub, by.x = "Amtlicher.Regionalschlüssel", by.y = "gkz_5", all.x = T) 

# get desired minimum sample size for display purposes, assuming precision of 2 percentage points 
n_req <- ceiling((1.96^2 * mean(data$alc_daily, na.rm = T) * (1 - mean(data$alc_daily, na.rm = T))) / 0.02^2)

dat_map <- dat_map %>% 
  mutate(n_cat = factor(case_when(
    nsample >= n_req ~ 1,
    nsample < n_req & nsample >= 0.5*n_req ~ 2,
    nsample < 0.5*n_req & nsample >= 0.25*n_req ~ 3,
    nsample < 0.25*n_req ~ 4), levels = c(1, 2, 3, 4),
    labels = c(paste("n ≥", paste(n_req)), paste(paste(ceiling(0.5*n_req)), "-", paste(n_req)),
               paste(paste(ceiling(0.25*n_req)), "-", paste(ceiling(0.5*n_req-1))), paste("n <", paste(ceiling(0.25*n_req))))))


# Get bivariate color scale
n_vals <- c(1, 0.75, 0.5, 0.25)
names(n_vals) <- levels(dat_map$n_cat)

dat_map <- bi_class(dat_map, x = alcDaily, y = n_cat, style = "quantile", dim = 4)

p <- ggplot(dat_map) +
  geom_sf(aes(fill = bi_class), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  bi_scale_fill(pal = "DkBlue2", dim = 4) + #scale_fill_viridis_c(na.value = "grey50", labels = scales::percent) + 
  labs(fill = "Observed prevalence\nfrequent alcohol use") +
  theme_void() +
  theme(legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.position = "none")
#ggsave(paste0(path_out, Sys.Date(), "_AlcDaily_observed_Combined.svg"), width = 12, height = 8, dpi = 300, bg = "white")

legend <- bi_legend(pal = "DkBlue2", dim = 4, xlab = "Sample size", ylab = "Alcohol prevalence")

p + inset_element(legend, left = 0.9, bottom = 0.05, right = 1.35, top = 0.5)

