# DEBRA Alcohol
# Sensitivity analysis 2: Federal state as additional spatial cluster
# Carolin Kilian
# 13-11-2026
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

# ------------------------------------------------------------------------------------------------------------------

# Functions

table_model_comparison <- function (models, ...) {
  
  n_models <- length(models)
  df <- as.data.frame(matrix(data = 0, nrow = n_models, ncol = 3))
  rownames(df) <- names(models)
  colnames(df) <- c("CPO", "DIC", "WAIC")
  
  for (i in 1:n_models) {
    tmp_CPO  <- sum(log(models[[i]]$cpo$cpo), na.rm = T) #* -1
    tmp_DIC  <- models[[i]]$dic$dic
    tmp_WAIC <- models[[i]]$waic$waic
    
    df[i, ] <- c(tmp_CPO, tmp_DIC, tmp_WAIC)
  }
  
  df
}

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
         zgisd = scale(gisd_score),
         state_num = as.numeric(str_sub(Amtlicher.Regionalschlüssel, 1, 2))) 

# ------------------------------------------------------------------------------------------------------------------

# INLA Models, get neighbouring information
g <- inla.read.graph(filename = "20261120_nbmap.adj")

# ------------------------------------------------------------------------------------------------------------------

# INLA Models: include state as additional cluster (IID component)

# 1) Simply multilevel model (no random effect)

formula_simple <- alc_daily ~ 1 + sex + age_gr + sex*age_gr + education + sex*education + 
  DEGURBA + zgisd + education*zgisd + hosprate + sample + year + f(state_num, model = "iid")

model_simple_StaIID <- inla(formula_simple,
                     data = pdat, family = "binomial", control.predictor = list(link = 1), 
                     control.compute = list(cpo = T, dic = T, waic = T))

summary(model_simple_StaIID)

# visualise model fit
pdat$fitted_simple_mean <- model_simple_StaIID$summary.fitted.values$mean
pdat$fitted_simple_median <- model_simple_StaIID$summary.fitted.values$`0.5quant`

pdat$fitted_diff <- pdat$fitted_simple_mean - pdat$fitted_simple_median
hist(pdat$fitted_diff) # use mean

# get observed data
obs <- pdat %>% filter(source == "orig") %>% group_by(NUTS_ID, NUTS_NUM) %>% 
  summarise(obs = mean(alc_daily, na.rm = T),
            n_obs = n()) %>% st_drop_geometry()

# get predicted data
map <- pdat %>% filter(source == "pred") %>% group_by(NUTS_ID, NUTS_NUM) %>% 
  summarise(fitted = mean(fitted_simple_mean)) %>% left_join(., obs)

fit_simple <- ggplot(map, aes(x=obs, y=fitted)) + geom_point() + geom_smooth() + 
  scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) + 
  labs(x = "observed", y = "fitted") + theme_bw() + ggtitle("Simple model")
#ggsave(paste0(path_out, Sys.Date(), "_Combined_simplemodel_fit.svg"), width = 12, height = 8, dpi = 300, bg = "white")

# fitted values
ggplot(map) +
  geom_sf(aes(fill = fitted), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(na.value = "grey50", labels = scales::percent) + 
  labs(fill = "Fitted posterior means\nsimple model") +
  theme_void() 

# 2) Independent random effects (IID) model 
formula_iid <- alc_daily ~ 1 + sex + age_gr + sex*age_gr + education + sex*education + 
  DEGURBA + zgisd + education*zgisd + hosprate + sample + year + 
  f(NUTS_NUM, model = "iid") + f(state_num, model = "iid")

model_iid_StaIID <- inla(formula_iid,
                  data = pdat, family = "binomial", control.predictor = list(link = 1), 
                  control.compute = list(cpo = T, dic = T, waic = T))

summary(model_iid_StaIID)

# visualise model fit
pdat$fitted_iid_mean <- model_iid_StaIID$summary.fitted.values$mean
pdat$fitted_iid_median <- model_iid_StaIID$summary.fitted.values$`0.5quant`

pdat$fitted_diff <- pdat$fitted_iid_mean - pdat$fitted_iid_median
hist(pdat$fitted_diff) #use mean

# get predicted data
map <- pdat %>% filter(source == "pred") %>% group_by(NUTS_ID, NUTS_NUM) %>% 
  summarise(fitted = mean(fitted_iid_mean)) %>% left_join(., obs) %>% arrange(NUTS_NUM) 
map$random <- model_iid_StaIID$summary.random$NUTS_NUM$mean

fit_iid <- ggplot(map, aes(x=obs, y=fitted)) + geom_point() + geom_smooth() + 
  scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) + 
  labs(x = "observed", y = "fitted") + theme_bw() + ggtitle("IID model")
#ggsave(paste0(path_out, Sys.Date(), "_Combined_IIDmodel_fit.svg"), width = 12, height = 8, dpi = 300, bg = "white")

# fitted values
ggplot(map) +
  geom_sf(aes(fill = fitted), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(na.value = "grey50", labels = scales::percent) + 
  labs(fill = "Fitted posterior means\nIID model") +
  theme_void() 

# model random effects
spatial_iid <- ggplot(map) +
  geom_sf(aes(fill = random), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradient2(low = "#abd9e9", mid = "white", high = "#d7191c", midpoint = 0, na.value = "grey50") + 
  labs(title = "IID model", fill = "") +
  theme_void() 
#ggsave(paste0(path_out, Sys.Date(), "_Combined_IIDmodel_random.svg"), width = 12, height = 8, dpi = 300, bg = "white")


# 3) Intrinsic conditional auto-regressive (icar) model

# Specify model
formula_icar <- alc_daily ~ 1 + sex + age_gr + sex*age_gr + education + sex*education + 
  DEGURBA + zgisd + education*zgisd + hosprate + sample + year + 
  f(NUTS_NUM, model = "besag", graph = g) + f(state_num, model = "iid")

model_icar_StaIID <- inla(formula_icar,
                   data = pdat, family = "binomial", control.predictor = list(link = 1), 
                   control.compute = list(cpo = T, dic = T, waic = T))

summary(model_icar_StaIID)

# visualise model fit
pdat$fitted_icar_mean <- model_icar_StaIID$summary.fitted.values$mean
pdat$fitted_icar_median <- model_icar_StaIID$summary.fitted.values$`0.5quant`

pdat$fitted_diff <- pdat$fitted_icar_mean - pdat$fitted_icar_median
hist(pdat$fitted_diff) #use mean

# get predicted data
map <- pdat %>% filter(source == "pred") %>% group_by(NUTS_ID, NUTS_NUM) %>% 
  summarise(fitted = mean(fitted_icar_mean)) %>% left_join(., obs) %>% arrange(NUTS_NUM) 
map$random <- model_icar_StaIID$summary.random$NUTS_NUM$mean

fit_icar <- ggplot(map, aes(x=obs, y=fitted)) + geom_point() + geom_smooth() +
  scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) + 
  labs(x = "observed", y = "fitted") + theme_bw() + ggtitle("ICAR model")
#ggsave(paste0(path_out, Sys.Date(), "_Combined_icarmodel_fit.svg"), width = 12, height = 8, dpi = 300, bg = "white")

# fitted values
ggplot(map) +
  geom_sf(aes(fill = fitted), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(na.value = "grey50", labels = scales::percent) + 
  labs(fill = "Fitted posterior means\nICAR model") +
  theme_void() 

# model random effects
spatial_icar <- ggplot(map) +
  geom_sf(aes(fill = random), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradient2(low = "#abd9e9", mid = "white", high = "#d7191c", midpoint = 0, na.value = "grey50") + 
  labs(title = "ICAR model", fill = "") +
  theme_void() 
#ggsave(paste0(path_out, Sys.Date(), "_Combined_icarmodel_random.svg"), width = 12, height = 8, dpi = 300, bg = "white")


# 4) BYM model

formula_bym <- alc_daily ~ 1 + sex + age_gr + sex*age_gr + education + sex*education + 
  DEGURBA + zgisd + education*zgisd + hosprate + sample + year + 
  f(NUTS_NUM, model = "bym", graph = g) + f(state_num, model = "iid")

model_bym_StaIID <- inla(formula_bym,
                  data = pdat, family = "binomial", control.predictor = list(link = 1), 
                  control.compute = list(return.marginals.predictor=TRUE, cpo = T, dic = T, waic = T))

summary(model_bym_StaIID)

# visualise model fit
pdat$fitted_bym_mean <- model_bym_StaIID$summary.fitted.values$mean
pdat$fitted_bym_median <- model_bym_StaIID$summary.fitted.values$`0.5quant`

pdat$fitted_diff <- pdat$fitted_bym_mean - pdat$fitted_bym_median
hist(pdat$fitted_diff) #use mean

# get predicted data
map <- pdat %>% filter(source == "pred") %>% group_by(NUTS_ID, NUTS_NUM) %>% 
  summarise(fitted = mean(fitted_bym_mean)) %>% left_join(., obs) %>% arrange(NUTS_NUM) 
map$randomIID <- model_bym_StaIID$summary.random$NUTS_NUM$mean[1:400]
map$randomicar <- model_bym_StaIID$summary.random$NUTS_NUM$mean[401:800]

fit_bym <- ggplot(map, aes(x=obs, y=fitted)) + geom_point() + geom_smooth() + 
  scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) + 
  labs(x = "observed", y = "fitted") + theme_bw() + ggtitle("BYM model")
#ggsave(paste0(path_out, Sys.Date(), "_Combined_BYMmodel_fit.svg"), width = 12, height = 8, dpi = 300, bg = "white")

# fitted values
ggplot(map) +
  geom_sf(aes(fill = fitted), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(na.value = "grey50", labels = scales::percent) + 
  labs(fill = "Fitted posterior means\nBYM model") +
  theme_void() 

# model random effects
spatial_bym_iid <- ggplot(map) +
  geom_sf(aes(fill = randomIID), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradient2(low = "#abd9e9", mid = "white", high = "#d7191c", midpoint = 0, na.value = "grey50") + 
  labs(title = "BYM model (IID component)", fill = "") +
  theme_void() 
#ggsave(paste0(path_out, Sys.Date(), "_Combined_BYMmodel_IIDrandom.svg"), width = 12, height = 8, dpi = 300, bg = "white")

spatial_bym_icar <- ggplot(map) +
  geom_sf(aes(fill = randomicar), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradient2(low = "#abd9e9", mid = "white", high = "#d7191c", midpoint = 0, na.value = "grey50") + 
  labs(title = "BYM model (ICAR component)", fill = "") +
  theme_void() 
#ggsave(paste0(path_out, Sys.Date(), "_Combined_BYMmodel_icarrandom.svg"), width = 12, height = 8, dpi = 300, bg = "white")


# INLA Models: include state as additional cluster (ICAR component)

# Get neighbours for states 
de_sf_states <- de_sf_states %>% arrange(NUTS_ID)
nb_states <- poly2nb(de_sf_states, queen = T) # this is based on de_sf (rather than dat) given missing areas in dat

plot(st_geometry(de_sf_states))
plot.nb(nb_states, st_geometry(de_sf_states), add = T)

# Save for later use within INLA
nb2INLA(paste0(DATE, "_nbmap_states.adj"), nb_states)
g_states <- inla.read.graph(filename = "20261120_nbmap_states.adj")

# 1) Simply multilevel model (no random effect)

formula_simple <- alc_daily ~ 1 + sex + age_gr + sex*age_gr + education + sex*education + 
  DEGURBA + zgisd + education*zgisd + hosprate + sample + year + f(state_num, model = "besag", graph = g_states)

model_simple_StaICAR <- inla(formula_simple,
                     data = pdat, family = "binomial", control.predictor = list(link = 1), 
                     control.compute = list(cpo = T, dic = T, waic = T))

summary(model_simple_StaICAR)

# visualise model fit
pdat$fitted_simple_mean <- model_simple_StaICAR$summary.fitted.values$mean
pdat$fitted_simple_median <- model_simple_StaICAR$summary.fitted.values$`0.5quant`

pdat$fitted_diff <- pdat$fitted_simple_mean - pdat$fitted_simple_median
hist(pdat$fitted_diff) # use mean

# get observed data
obs <- pdat %>% filter(source == "orig") %>% group_by(NUTS_ID, NUTS_NUM) %>% 
  summarise(obs = mean(alc_daily, na.rm = T),
            n_obs = n()) %>% st_drop_geometry()

# get predicted data
map <- pdat %>% filter(source == "pred") %>% group_by(NUTS_ID, NUTS_NUM) %>% 
  summarise(fitted = mean(fitted_simple_mean)) %>% left_join(., obs)

fit_simple <- ggplot(map, aes(x=obs, y=fitted)) + geom_point() + geom_smooth() + 
  scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) + 
  labs(x = "observed", y = "fitted") + theme_bw() + ggtitle("Simple model")
#ggsave(paste0(path_out, Sys.Date(), "_Combined_simplemodel_fit.svg"), width = 12, height = 8, dpi = 300, bg = "white")

# fitted values
ggplot(map) +
  geom_sf(aes(fill = fitted), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(na.value = "grey50", labels = scales::percent) + 
  labs(fill = "Fitted posterior means\nsimple model") +
  theme_void() 


# 2) Independent random effects (IID) model 
formula_iid <- alc_daily ~ 1 + sex + age_gr + sex*age_gr + education + sex*education + 
  DEGURBA + zgisd + education*zgisd + hosprate + sample + year + 
  f(NUTS_NUM, model = "iid") + f(state_num, model = "besag", graph = g_states)

model_iid_StaICAR <- inla(formula_iid,
                  data = pdat, family = "binomial", control.predictor = list(link = 1), 
                  control.compute = list(cpo = T, dic = T, waic = T))

summary(model_iid_StaICAR)

# visualise model fit
pdat$fitted_iid_mean <- model_iid_StaICAR$summary.fitted.values$mean
pdat$fitted_iid_median <- model_iid_StaICAR$summary.fitted.values$`0.5quant`

pdat$fitted_diff <- pdat$fitted_iid_mean - pdat$fitted_iid_median
hist(pdat$fitted_diff) #use mean

# get predicted data
map <- pdat %>% filter(source == "pred") %>% group_by(NUTS_ID, NUTS_NUM) %>% 
  summarise(fitted = mean(fitted_iid_mean)) %>% left_join(., obs) %>% arrange(NUTS_NUM) 
map$random <- model_iid_StaICAR$summary.random$NUTS_NUM$mean

fit_iid <- ggplot(map, aes(x=obs, y=fitted)) + geom_point() + geom_smooth() + 
  scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) + 
  labs(x = "observed", y = "fitted") + theme_bw() + ggtitle("IID model")
#ggsave(paste0(path_out, Sys.Date(), "_Combined_IIDmodel_fit.svg"), width = 12, height = 8, dpi = 300, bg = "white")

# fitted values
ggplot(map) +
  geom_sf(aes(fill = fitted), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(na.value = "grey50", labels = scales::percent) + 
  labs(fill = "Fitted posterior means\nIID model") +
  theme_void() 

# model random effects
spatial_iid <- ggplot(map) +
  geom_sf(aes(fill = random), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradient2(low = "#abd9e9", mid = "white", high = "#d7191c", midpoint = 0, na.value = "grey50") + 
  labs(title = "IID model", fill = "") +
  theme_void() 
#ggsave(paste0(path_out, Sys.Date(), "_Combined_IIDmodel_random.svg"), width = 12, height = 8, dpi = 300, bg = "white")

# 3) Intrinsic conditional auto-regressive (icar) model

# Specify model
formula_icar <- alc_daily ~ 1 + sex + age_gr + sex*age_gr + education + sex*education + 
  DEGURBA + zgisd + education*zgisd + hosprate + sample + year + 
  f(NUTS_NUM, model = "besag", graph = g) + f(state_num, model = "besag", graph = g_states)

model_icar_StaICAR <- inla(formula_icar,
                   data = pdat, family = "binomial", control.predictor = list(link = 1), 
                   control.compute = list(cpo = T, dic = T, waic = T))

summary(model_icar_StaICAR)

# visualise model fit
pdat$fitted_icar_mean <- model_icar_StaICAR$summary.fitted.values$mean
pdat$fitted_icar_median <- model_icar_StaICAR$summary.fitted.values$`0.5quant`

pdat$fitted_diff <- pdat$fitted_icar_mean - pdat$fitted_icar_median
hist(pdat$fitted_diff) #use mean

# get predicted data
map <- pdat %>% filter(source == "pred") %>% group_by(NUTS_ID, NUTS_NUM) %>% 
  summarise(fitted = mean(fitted_icar_mean)) %>% left_join(., obs) %>% arrange(NUTS_NUM) 
map$random <- model_icar_StaICAR$summary.random$NUTS_NUM$mean

fit_icar <- ggplot(map, aes(x=obs, y=fitted)) + geom_point() + geom_smooth() +
  scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) + 
  labs(x = "observed", y = "fitted") + theme_bw() + ggtitle("ICAR model")
#ggsave(paste0(path_out, Sys.Date(), "_Combined_icarmodel_fit.svg"), width = 12, height = 8, dpi = 300, bg = "white")

# fitted values
ggplot(map) +
  geom_sf(aes(fill = fitted), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(na.value = "grey50", labels = scales::percent) + 
  labs(fill = "Fitted posterior means\nICAR model") +
  theme_void() 

# model random effects
spatial_icar <- ggplot(map) +
  geom_sf(aes(fill = random), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradient2(low = "#abd9e9", mid = "white", high = "#d7191c", midpoint = 0, na.value = "grey50") + 
  labs(title = "ICAR model", fill = "") +
  theme_void() 
#ggsave(paste0(path_out, Sys.Date(), "_Combined_icarmodel_random.svg"), width = 12, height = 8, dpi = 300, bg = "white")


# 4) BYM model

formula_bym <- alc_daily ~ 1 + sex + age_gr + sex*age_gr + education + sex*education + 
  DEGURBA + zgisd + education*zgisd + hosprate + sample + year + 
  f(NUTS_NUM, model = "bym", graph = g) + f(state_num, model = "besag", graph = g_states)

model_bym_StaICAR <- inla(formula_bym,
                  data = pdat, family = "binomial", control.predictor = list(link = 1), 
                  control.compute = list(return.marginals.predictor=TRUE, cpo = T, dic = T, waic = T))

summary(model_bym_StaICAR)

# visualise model fit
pdat$fitted_bym_mean <- model_bym_StaICAR$summary.fitted.values$mean
pdat$fitted_bym_median <- model_bym_StaICAR$summary.fitted.values$`0.5quant`

pdat$fitted_diff <- pdat$fitted_bym_mean - pdat$fitted_bym_median
hist(pdat$fitted_diff) #use mean

# get predicted data
map <- pdat %>% filter(source == "pred") %>% group_by(NUTS_ID, NUTS_NUM) %>% 
  summarise(fitted = mean(fitted_bym_mean)) %>% left_join(., obs) %>% arrange(NUTS_NUM) 
map$randomIID <- model_bym_StaICAR$summary.random$NUTS_NUM$mean[1:400]
map$randomicar <- model_bym_StaICAR$summary.random$NUTS_NUM$mean[401:800]

fit_bym <- ggplot(map, aes(x=obs, y=fitted)) + geom_point() + geom_smooth() + 
  scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) + 
  labs(x = "observed", y = "fitted") + theme_bw() + ggtitle("BYM model")
#ggsave(paste0(path_out, Sys.Date(), "_Combined_BYMmodel_fit.svg"), width = 12, height = 8, dpi = 300, bg = "white")

# fitted values
ggplot(map) +
  geom_sf(aes(fill = fitted), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(na.value = "grey50", labels = scales::percent) + 
  labs(fill = "Fitted posterior means\nBYM model") +
  theme_void() 

# model random effects
spatial_bym_iid <- ggplot(map) +
  geom_sf(aes(fill = randomIID), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradient2(low = "#abd9e9", mid = "white", high = "#d7191c", midpoint = 0, na.value = "grey50") + 
  labs(title = "BYM model (IID component)", fill = "") +
  theme_void() 
#ggsave(paste0(path_out, Sys.Date(), "_Combined_BYMmodel_IIDrandom.svg"), width = 12, height = 8, dpi = 300, bg = "white")

spatial_bym_icar <- ggplot(map) +
  geom_sf(aes(fill = randomicar), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradient2(low = "#abd9e9", mid = "white", high = "#d7191c", midpoint = 0, na.value = "grey50") + 
  labs(title = "BYM model (ICAR component)", fill = "") +
  theme_void() 
#ggsave(paste0(path_out, Sys.Date(), "_Combined_BYMmodel_icarrandom.svg"), width = 12, height = 8, dpi = 300, bg = "white")


# Model comparison COMPARE MODELS BEFORE REDUCING MODEL, SHOULD ALL BE BASED ON formula_h
model_comp <- table_model_comparison(models = list(model_simple_StaIID, model_iid_StaIID, model_icar_StaIID, model_bym_StaIID,
                                                   model_simple_StaICAR, model_iid_StaICAR, model_icar_StaICAR, model_bym_StaICAR)) %>% # best fitting: model_bym 
              mutate(model = c("simple_stateIID", "iid_stateIID", "icar_stateIID", "bym_stateIID", "simple_stateICAR", "iid_stateICAR", "icar_stateICAR", "bym_stateICAR"))
write.xlsx(model_comp, paste0(path_out, Sys.Date(), "_ModelComparison_StateCluster.xlsx"))

