# DEBRA Alcohol
# Sensitivity analysis 2 (excl. extreme values)
# Carolin Kilian
# 06-01-2026
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
path_out <- "/Users/carolinkilian/Desktop/Research projects/Alcohol Spatial Modelling/Output/"
DATE <- 20260629

de_sf <- st_read("NUTS_RG_20M_2024_3035.shp/NUTS_RG_20M_2024_3035.shp") %>% filter(CNTR_CODE == "DE") %>% filter(LEVL_CODE == 3)
de_sf_states <- st_read("NUTS_RG_20M_2024_3035.shp/NUTS_RG_20M_2024_3035.shp") %>% filter(CNTR_CODE == "DE") %>% filter(LEVL_CODE == 1)
nuts3code <- read.xlsx("04-kreise.xlsx", sheet = "Kreisfreie Städte u. Landkreise", startRow = 2) %>% select(c("Amtlicher.Regionalschlüssel", "NUTS3")) %>% filter(!is.na(NUTS3))
prediction <- readRDS("20260629_prediction.rds")
data_validation <- read.xlsx("data_id_148_kreis16_10_j_1640995200_roh.xlsx", sheet = "Daten", rows = c(4:405)) %>% 
  mutate(audrate = as.numeric(gsub(",", ".", Wert))) %>% select(c("Region", "Regions-ID", "audrate"))
  
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

# INLA Models

# Identify neighboring regions based on contiguity (first order only)
de_sf <- de_sf %>% arrange(NUTS_ID)
nb <- poly2nb(de_sf, queen = T) # this is based on de_sf (rather than dat) given missing areas in dat

plot(st_geometry(de_sf))
plot.nb(nb, st_geometry(de_sf), add = T)

# Save for later use within INLA
nb2INLA(paste0(DATE, "_nbmap.adj"), nb)
g <- inla.read.graph(filename = "20260107_nbmap.adj")

# ------------------------------------------------------------------------------------------------------------------

# Exclude extreme values in the observed data

# Remove extreme values (prevalence of ≥0.3 or 0)
alc <- pdat %>% filter(source == "orig") %>% group_by(NUTS_ID) %>% 
  summarise(alc_daily = mean(alc_daily, na.rm = T))
ggplot(alc) + geom_histogram(aes(x = alc_daily), color = "black", fill = "grey80") + 
  scale_x_continuous(labels = scales::percent) + 
  labs(x = "Prevalence of frequent alcohol use (observed data)") + theme_bw()
ggsave(paste0(path_out, Sys.Date(), "_Distribution_AlcDaily_SA2.svg"), width = 6, height = 4, dpi = 300, bg = "white")

nuts3upp <- pdat %>% filter(source == "orig") %>% group_by(NUTS_ID) %>% 
  summarise(alc_daily = mean(alc_daily, na.rm = T)) %>% 
  filter(alc_daily > 0.3) %>% pull(NUTS_ID)
nuts3null <- pdat %>% filter(source == "orig") %>% group_by(NUTS_ID) %>% 
  summarise(alc_daily = mean(alc_daily, na.rm = T)) %>% 
  filter(alc_daily == 0) %>% pull(NUTS_ID)

# Remove all areas with extreme values
emp.dat <- pdat %>% filter(NUTS_ID %in% nuts3upp | NUTS_ID %in% nuts3null) %>% 
  mutate(alc_daily = NA)
  
pred.dat <- pdat %>% filter(!NUTS_ID %in% nuts3upp) %>% filter(!NUTS_ID %in% nuts3null) %>%
  rbind(emp.dat)

# Run INLA using combined data 

formula_bym <- alc_daily ~ 1 + sex + age_gr + sex*age_gr + education + sex*education + 
  DEGURBA + zgisd + education*zgisd + hosprate + sample + year + 
  f(NUTS_NUM, model = "bym", graph = g)

model_bym_pred <- inla(formula_bym,
                       data = pred.dat, family = "binomial", control.predictor = list(link = 1), 
                       control.compute = list(return.marginals.predictor=TRUE, cpo = T, dic = T, waic = T))

# Get fitted values

# visualise model fit
pred.dat$fitted_bym_mean <- model_bym_pred$summary.fitted.values$mean
pred.dat$fitted_bym_median <- model_bym_pred$summary.fitted.values$`0.5quant`
pred.dat$fitted_bym_lci <- model_bym_pred$summary.fitted.values$`0.025quant`
pred.dat$fitted_bym_uci <- model_bym_pred$summary.fitted.values$`0.975quant`

pred.dat$fitted_diff <- pred.dat$fitted_bym_mean - pred.dat$fitted_bym_median
hist(pred.dat$fitted_diff) #use mean

map <- pred.dat %>% group_by(NUTS_ID, NUTS_NUM) %>% 
  summarise(fitted = mean(fitted_bym_mean),
            fitted_lci = mean(fitted_bym_lci),
            fitted_uci = mean(fitted_bym_uci),
            obs = mean(alc_daily, na.rm = T)) %>% arrange(NUTS_NUM) 
map$randomIID <- model_bym_pred$summary.random$NUTS_NUM$mean[1:400]
map$randomICAR <- model_bym_pred$summary.random$NUTS_NUM$mean[401:800]

ggplot(map, aes(x=obs, y=fitted)) + geom_point() + geom_smooth() + 
  scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) + 
  labs(x = "Observed prevalence of frequent alcohol use", y = "Predicted prevalence of frequent alcohol use") + theme_bw() 
ggsave(paste0(path_out, Sys.Date(), "_predAlcDaily_SAnoExtreme_BYMmodel_fit.svg"), width = 8, height = 6, dpi = 300, bg = "white")

# fitted values
ggplot(map) +
  geom_sf(aes(fill = fitted), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(na.value = "grey50", labels = scales::percent, limits = c(0, 0.32)) + 
  labs(fill = "") + theme_void() + theme(legend.position = "bottom")
ggsave(paste0(path_out, Sys.Date(), "_predAlcDaily_SAnoExtreme_BYMmodel.svg"), width = 6, height = 8, dpi = 300, bg = "white")

names <- de_sf %>% st_drop_geometry() %>% select(c(NUTS_ID, NUTS_NAME)) %>% mutate(NUTS_ID_state = as.factor(substr(NUTS_ID, 1, 3)))
stnames <- de_sf_states %>% select(c(NUTS_ID, NUTS_NAME)) %>% rename(NUTS_ID_state = NUTS_ID, NUTS_NAME_state = NUTS_NAME) %>% left_join(names) %>% st_drop_geometry()
pred.out.sa1 <- map %>% left_join(stnames) %>% st_drop_geometry() %>% ungroup() %>% 
  mutate_if(is.numeric, ~round(.*100,1)) %>% select(c(NUTS_NAME_state, NUTS_NAME, fitted, fitted_lci, fitted_uci)) 


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
ggsave(paste0(path_out, Sys.Date(), "_Validation_AUD_SAnoExtreme_BYMmodel.svg"), width = 8, height = 6, dpi = 300, bg = "white")

# ------------------------------------------------------------------------------------------------------------------

# Exclude extreme values based on spatial error (iid)

# Run original model

model_bym <- inla(formula_bym,
                  data = pdat, family = "binomial", control.predictor = list(link = 1), 
                  control.compute = list(return.marginals.predictor=TRUE, cpo = T, dic = T, waic = T))

# Get spatial random effect 
map <- pdat %>% group_by(NUTS_ID, NUTS_NUM) %>% 
  summarise(obs = mean(alc_daily, na.rm = T)) %>% arrange(NUTS_NUM) 
map$randomIID <- model_bym$summary.random$NUTS_NUM$mean[1:400]

hist(map$randomIID)
nuts3upp <- map %>% filter(randomIID > 1.5) %>% pull(NUTS_ID)
nuts3low <- map %>% filter(randomIID < -1.5) %>% pull(NUTS_ID)

# Remove all areas with extreme values
emp.dat <- pdat %>% filter(NUTS_ID %in% nuts3upp | NUTS_ID %in% nuts3low) %>% 
  mutate(alc_daily = NA)

# Remove extreme values (based on IID spatial random effect) and merge cleaned prediction data
pred.dat <- pdat %>% filter(!NUTS_ID %in% nuts3upp) %>% filter(!NUTS_ID %in% nuts3low) %>% 
  rbind(emp.dat)

# Run model

model_bym_pred <- inla(formula_bym,
                       data = pred.dat, family = "binomial", control.predictor = list(link = 1), 
                       control.compute = list(return.marginals.predictor=TRUE, cpo = T, dic = T, waic = T))

# Get fitted values

# visualise model fit
pred.dat$fitted_bym_mean <- model_bym_pred$summary.fitted.values$mean
pred.dat$fitted_bym_median <- model_bym_pred$summary.fitted.values$`0.5quant`
pred.dat$fitted_bym_lci <- model_bym_pred$summary.fitted.values$`0.025quant`
pred.dat$fitted_bym_uci <- model_bym_pred$summary.fitted.values$`0.975quant`

pred.dat$fitted_diff <- pred.dat$fitted_bym_mean - pred.dat$fitted_bym_median
hist(pred.dat$fitted_diff) #use mean

map <- pred.dat %>% group_by(NUTS_ID, NUTS_NUM) %>% 
  summarise(fitted = mean(fitted_bym_mean),
            fitted_lci = mean(fitted_bym_lci),
            fitted_uci = mean(fitted_bym_uci),
            obs = mean(alc_daily, na.rm = T)) %>% arrange(NUTS_NUM) 
map$randomIID <- model_bym_pred$summary.random$NUTS_NUM$mean[1:400]
map$randomICAR <- model_bym_pred$summary.random$NUTS_NUM$mean[401:800]

ggplot(map, aes(x=obs, y=fitted)) + geom_point() + geom_smooth() + 
  scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) + 
  labs(x = "Observed prevalence of frequent alcohol use", y = "Predicted prevalence of frequent alcohol use") + theme_bw() 
ggsave(paste0(path_out, Sys.Date(), "_predAlcDaily_SAnoSpat_BYMmodel_fit.svg"), width = 8, height = 6, dpi = 300, bg = "white")

# fitted values
ggplot(map) +
  geom_sf(aes(fill = fitted), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(na.value = "grey50", labels = scales::percent, limits = c(0, 0.32)) + 
  labs(fill = "") + theme_void() + theme(legend.position = "bottom")
ggsave(paste0(path_out, Sys.Date(), "_predAlcDaily_SAnoSpat_BYMmodel.svg"), width = 6, height = 8, dpi = 300, bg = "white")

names <- de_sf %>% st_drop_geometry() %>% select(c(NUTS_ID, NUTS_NAME)) %>% mutate(NUTS_ID_state = as.factor(substr(NUTS_ID, 1, 3)))
stnames <- de_sf_states %>% select(c(NUTS_ID, NUTS_NAME)) %>% rename(NUTS_ID_state = NUTS_ID, NUTS_NAME_state = NUTS_NAME) %>% left_join(names) %>% st_drop_geometry()
pred.out.sa2 <- map %>% left_join(stnames) %>% st_drop_geometry() %>% ungroup() %>% 
  mutate_if(is.numeric, ~round(.*100,1)) %>% select(c(NUTS_NAME_state, NUTS_NAME, fitted, fitted_lci, fitted_uci)) 


# EXTERNAL MODEL VALIDATION 

# combine with predicted data
map_aud <- map %>% merge(., pdat_validation, by.x = "NUTS_ID", by.y = "NUTS3") 

# plot correlation
ggplot(map_aud, aes(x=fitted, y=audrate)) + geom_point() + geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::percent) +
  stat_cor(method = "pearson", label.x = 0.15) +
  labs(x = "Estimated prevalence of frequent alcohol use", y = "Crude incidence rate F10 per 10,000") + theme_bw()
ggsave(paste0(path_out, Sys.Date(), "_Validation_AUD_SAnoSpat_BYMmodel.svg"), width = 8, height = 6, dpi = 300, bg = "white")


# save full excel table
desc <- readRDS(paste0(path_out, "2026-06-29_Descriptives.rds"))
main <- readRDS(paste0(path_out, "2026-06-29_Pred_BYM.rds"))

write_xlsx(list("Descriptives" = desc, 
                "Main model" = main,
                "SA1 excl obs" = pred.out.sa1,
                "SA2 excl IID" = pred.out.sa2), 
           paste0(path_out, Sys.Date(), "_Prediction_BYM.xlsx"))
