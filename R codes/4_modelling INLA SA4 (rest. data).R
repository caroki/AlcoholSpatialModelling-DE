# DEBRA Alcohol
# Sensitivity analysis 4 (excl. CannaStreet OR 2018-2021 data)
# Carolin Kilian
# 20-11-2026
# Tutorial: https://becarioprecario.bitbucket.io/inla-gitbook/ch-mixed.html, https://avramaral.github.io/INLA_tutorial/#inla 

library(tidyverse)
library(openxlsx)
library(sf)
library(sp)
library(spdep)
library(haven)
library(INLA)
library(SpatialEpi)
library(broom.mixed)
library(ggpubr)
library(writexl)

# Load data
rm(list = ls())
setwd("/Users/carolinkilian/Desktop/Research projects/Alcohol Spatial Modelling/Data/")
path_out <- "/Users/carolinkilian/Desktop/Research projects/Alcohol Spatial Modelling/Github/AlcoholSpatialModelling-DE/Output/"
DATE <- 20261120

de_sf <- st_read("NUTS_RG_20M_2024_3035.shp/NUTS_RG_20M_2024_3035.shp") %>% filter(CNTR_CODE == "DE") %>% filter(LEVL_CODE == 3)
de_sf_states <- st_read("NUTS_RG_20M_2024_3035.shp/NUTS_RG_20M_2024_3035.shp") %>% filter(CNTR_CODE == "DE") %>% filter(LEVL_CODE == 1)
nuts3code <- read.xlsx("04-kreise.xlsx", sheet = "Kreisfreie Städte u. Landkreise", startRow = 2) %>% dplyr::select(c("Amtlicher.Regionalschlüssel", "NUTS3")) %>% filter(!is.na(NUTS3))
prediction <- readRDS("20261120_prediction.rds")
data_validation <- read.xlsx("data_id_148_kreis16_10_j_1640995200_roh.xlsx", sheet = "Daten", rows = c(4:405)) %>% 
  mutate(audrate = as.numeric(gsub(",", ".", Wert))) %>% dplyr::select(c("Region", "Regions-ID", "audrate"))

# ------------------------------------------------------------------------------------------------------------------

# Process data

# get complete NUTS_NUM vector
de_sf <- de_sf %>% arrange(NUTS_ID) %>% 
  mutate(NUTS_NUM = as.numeric(as.factor(NUTS_ID))) %>% 
  merge(., nuts3code, by.x = "NUTS_ID", by.y = "NUTS3", all = F)

# merge de_sf to datafile
pdat <- de_sf %>% 
  merge(., prediction, by.x = "Amtlicher.Regionalschlüssel", by.y = "gkz_5", all.x = T) %>% 
  mutate(NUTS_ID = as.factor(NUTS_ID),
         sex = droplevels(sex),
         DEGURBA = as.factor(DEGURBA),
         year = as.factor(year),
         zgisd = scale(gisd_score)) 

# ------------------------------------------------------------------------------------------------------------------

# INLA Models, get neighbouring information
g <- inla.read.graph(filename = "20261120_nbmap.adj")

# ------------------------------------------------------------------------------------------------------------------

# Exclude CannaStreet, use DEBRA data only

pdat_debra <- pdat %>% filter(survey == "debra")

formula_bym <- alc_daily ~ 1 + sex + age_gr + sex*age_gr + education + sex*education + 
  DEGURBA + zgisd + education*zgisd + hosprate + year + 
  f(NUTS_NUM, model = "bym", graph = g)

model_bym_pred <- inla(formula_bym,
                       data = pdat_debra, family = "binomial", control.predictor = list(link = 1), 
                       control.compute = list(return.marginals.predictor=TRUE, cpo = T, dic = T, waic = T))

# Get fitted values

# visualise model fit
pdat_debra$fitted_bym_mean <- model_bym_pred$summary.fitted.values$mean
pdat_debra$fitted_bym_median <- model_bym_pred$summary.fitted.values$`0.5quant`
pdat_debra$fitted_bym_lci <- model_bym_pred$summary.fitted.values$`0.025quant`
pdat_debra$fitted_bym_uci <- model_bym_pred$summary.fitted.values$`0.975quant`

pdat_debra$fitted_diff <- pdat_debra$fitted_bym_mean - pdat_debra$fitted_bym_median
hist(pdat_debra$fitted_diff) #use mean

# get observed data
obs <- pdat_debra %>% filter(source == "orig") %>% group_by(NUTS_ID, NUTS_NUM) %>% 
  summarise(obs = mean(alc_daily, na.rm = T),
            n_obs = n()) %>% st_drop_geometry()

# get predicted data
map <- pdat_debra %>% filter(source == "pred") %>% group_by(NUTS_ID, NUTS_NUM) %>% 
  summarise(fitted = mean(fitted_bym_mean),
            fitted_lci = mean(fitted_bym_lci),
            fitted_uci = mean(fitted_bym_uci)) %>% left_join(., obs) %>% arrange(NUTS_NUM) 
map$randomIID <- model_bym_pred$summary.random$NUTS_NUM$mean[1:400]
map$randomICAR <- model_bym_pred$summary.random$NUTS_NUM$mean[401:800]

ggplot(map, aes(x=obs, y=fitted)) + geom_point(aes(size = n_obs, alpha = n_obs)) + geom_smooth() + 
  scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) + scale_alpha(range = c(0.3, 1)) +
  labs(x = "Observed prevalence of frequent alcohol use", y = "Predicted prevalence of frequent alcohol use") + theme_bw() + theme(legend.position = "none") 
ggsave(paste0(path_out, Sys.Date(), "_predAlcDaily_SAdebra_BYMmodel_fit.svg"), width = 8, height = 6, dpi = 300, bg = "white")

# fitted values
sa_A <- ggplot(map) + ggtitle("A") +
  geom_sf(aes(fill = fitted), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(na.value = "grey50", labels = scales::percent, limits = c(0, 0.38)) + 
  labs(fill = "") + theme_void() + theme(legend.position = "bottom", title = element_text(size = 16, face = "bold"))
#ggsave(paste0(path_out, Sys.Date(), "_predAlcDaily_SAdebra_BYMmodel.svg"), width = 6, height = 8, dpi = 300, bg = "white")

names <- de_sf %>% st_drop_geometry() %>% dplyr::select(c(NUTS_ID, NUTS_NAME)) %>% mutate(NUTS_ID_state = as.factor(substr(NUTS_ID, 1, 3)))
stnames <- de_sf_states %>% dplyr::select(c(NUTS_ID, NUTS_NAME)) %>% rename(NUTS_ID_state = NUTS_ID, NUTS_NAME_state = NUTS_NAME) %>% left_join(names) %>% st_drop_geometry()
pred.out.sa1 <- map %>% left_join(stnames) %>% st_drop_geometry() %>% ungroup() %>% 
  mutate_if(is.numeric, ~round(.*100,1)) %>% dplyr::select(c(NUTS_NAME_state, NUTS_NAME, fitted, fitted_lci, fitted_uci)) 


# EXTERNAL MODEL VALIDATION 
pdat_validation <- data_validation %>% 
  mutate(kreis_id = sprintf("%05s", `Regions-ID`),
         kreis_id = ifelse(Region == "Eisenach", 16063, kreis_id)) %>%
  merge(., nuts3code, by.x = "kreis_id", by.y = "Amtlicher.Regionalschlüssel", all.x = T) %>% 
  # combine data for Eisenach with Wartburgkreis
  group_by(NUTS3) %>% 
  summarise(audrate = mean(audrate))

# combine with predicted data
map_aud <- map %>% merge(., pdat_validation, by.x = "NUTS_ID", by.y = "NUTS3") 

# plot correlation
ggplot(map_aud, aes(x=fitted, y=audrate)) + geom_point() + geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::percent) +
  stat_cor(method = "pearson", label.x = 0.15) +
  labs(x = "Estimated prevalence of frequent alcohol use", y = "Crude incidence rate F10 per 10,000") + theme_bw()
ggsave(paste0(path_out, Sys.Date(), "_Validation_AUD_SAdebra_BYMmodel.svg"), width = 8, height = 6, dpi = 300, bg = "white")


# ------------------------------------------------------------------------------------------------------------------

# Exclude CannaStreet, use DEBRA data only

pdat_2022 <- pdat %>% filter(year %like% "2022|2023|2024|2025")

model_bym_pred <- inla(formula_bym,
                       data = pdat_2022, family = "binomial", control.predictor = list(link = 1), 
                       control.compute = list(return.marginals.predictor=TRUE, cpo = T, dic = T, waic = T))

# Get fitted values

# visualise model fit
pdat_2022$fitted_bym_mean <- model_bym_pred$summary.fitted.values$mean
pdat_2022$fitted_bym_median <- model_bym_pred$summary.fitted.values$`0.5quant`
pdat_2022$fitted_bym_lci <- model_bym_pred$summary.fitted.values$`0.025quant`
pdat_2022$fitted_bym_uci <- model_bym_pred$summary.fitted.values$`0.975quant`

pdat_2022$fitted_diff <- pdat_2022$fitted_bym_mean - pdat_2022$fitted_bym_median
hist(pdat_2022$fitted_diff) #use mean

# get observed data
obs <- pdat_2022 %>% filter(source == "orig") %>% group_by(NUTS_ID, NUTS_NUM) %>% 
  summarise(obs = mean(alc_daily, na.rm = T),
            n_obs = n()) %>% st_drop_geometry()

# get predicted data
map <- pdat_2022 %>% filter(source == "pred") %>% group_by(NUTS_ID, NUTS_NUM) %>% 
  summarise(fitted = mean(fitted_bym_mean),
            fitted_lci = mean(fitted_bym_lci),
            fitted_uci = mean(fitted_bym_uci)) %>% left_join(., obs) %>% arrange(NUTS_NUM) 
map$randomIID <- model_bym_pred$summary.random$NUTS_NUM$mean[1:400]
map$randomICAR <- model_bym_pred$summary.random$NUTS_NUM$mean[401:800]

ggplot(map, aes(x=obs, y=fitted)) + geom_point(aes(size = n_obs, alpha = n_obs)) + geom_smooth() + 
  scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) + scale_alpha(range = c(0.3, 1)) +
  labs(x = "Observed prevalence of frequent alcohol use", y = "Predicted prevalence of frequent alcohol use") + theme_bw() + theme(legend.position = "none") 
ggsave(paste0(path_out, Sys.Date(), "_predAlcDaily_SA2022-2025_BYMmodel_fit.svg"), width = 8, height = 6, dpi = 300, bg = "white")

# fitted values
sa_B <- ggplot(map) + ggtitle("B") +
  geom_sf(aes(fill = fitted), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(na.value = "grey50", labels = scales::percent, limits = c(0, 0.35)) + 
  labs(fill = "") + theme_void() + theme(legend.position = "bottom", title = element_text(size = 16, face = "bold"))
#ggsave(paste0(path_out, Sys.Date(), "_predAlcDaily_SA2022-2025_BYMmodel.svg"), width = 6, height = 8, dpi = 300, bg = "white")

names <- de_sf %>% st_drop_geometry() %>% dplyr::select(c(NUTS_ID, NUTS_NAME)) %>% mutate(NUTS_ID_state = as.factor(substr(NUTS_ID, 1, 3)))
stnames <- de_sf_states %>% dplyr::select(c(NUTS_ID, NUTS_NAME)) %>% rename(NUTS_ID_state = NUTS_ID, NUTS_NAME_state = NUTS_NAME) %>% left_join(names) %>% st_drop_geometry()
pred.out.sa2 <- map %>% left_join(stnames) %>% st_drop_geometry() %>% ungroup() %>% 
  mutate_if(is.numeric, ~round(.*100,1)) %>% dplyr::select(c(NUTS_NAME_state, NUTS_NAME, fitted, fitted_lci, fitted_uci)) 


# EXTERNAL MODEL VALIDATION 

# combine with predicted data
map_aud <- map %>% merge(., pdat_validation, by.x = "NUTS_ID", by.y = "NUTS3") 

# plot correlation
ggplot(map_aud, aes(x=fitted, y=audrate)) + geom_point() + geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::percent) +
  stat_cor(method = "pearson", label.x = 0.15) +
  labs(x = "Estimated prevalence of frequent alcohol use", y = "Crude incidence rate F10 per 10,000") + theme_bw()
ggsave(paste0(path_out, Sys.Date(), "_Validation_AUD_SA2022-2025_BYMmodel.svg"), width = 8, height = 6, dpi = 300, bg = "white")


# save full excel table
desc <- readRDS(paste0(path_out, "2026-06-29_Descriptives.rds"))
main <- readRDS(paste0(path_out, "2026-11-25_Pred_BYM.rds"))
sa1_excl_obs <- readRDS(paste0(path_out, "2026-11-25_Pred_SA1_excl_outliers.rds"))
sa2_excl_IID <- readRDS(paste0(path_out, "2026-11-25_Pred_SA2_excl_IID.rds"))

write_xlsx(list("Descriptives" = desc, 
                "Main model" = main,
                "SA1 excl obs" = sa1_excl_obs,
                "SA2 excl IID" = sa2_excl_IID,
                "SA3 DEBRA only" = pred.out.sa1,
                "SA4 2022-2025 " = pred.out.sa2), 
           paste0(path_out, Sys.Date(), "_Prediction_BYM.xlsx"))

# plot predicted prevalence across sensitivity analysis

ggarrange(sa_A, sa_B, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
ggsave(paste0(path_out, Sys.Date(), "_SA3_ExclData.svg"), width = 9, height = 6, dpi = 300, bg = "white")
