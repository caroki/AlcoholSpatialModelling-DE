# DEBRA Alcohol
# Modelling
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
         zgisd = scale(gisd_score)) 

# ------------------------------------------------------------------------------------------------------------------

# INLA Models

# 1) Simply multilevel model (no random effect)

formula_simple <- alc_daily ~ 1 + sex + age_gr + sex*age_gr + education + sex*education + 
  DEGURBA + zgisd + education*zgisd + hosprate + sample + year

model_simple <- inla(formula_simple,
                data = pdat, family = "binomial", control.predictor = list(link = 1), 
                control.compute = list(cpo = T, dic = T, waic = T))

summary(model_simple)

# visualise model fit
pdat$fitted_simple_mean <- model_simple$summary.fitted.values$mean
pdat$fitted_simple_median <- model_simple$summary.fitted.values$`0.5quant`

pdat$fitted_diff <- pdat$fitted_simple_mean - pdat$fitted_simple_median
hist(pdat$fitted_diff) # use mean

map <- pdat %>% group_by(NUTS_ID, NUTS_NUM) %>% 
  summarise(fitted = mean(fitted_simple_mean),
            obs = mean(alc_daily, na.rm = T)) %>% arrange(NUTS_NUM) 

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
  f(NUTS_NUM, model = "iid")
  
model_iid <- inla(formula_iid,
                data = pdat, family = "binomial", control.predictor = list(link = 1), 
                control.compute = list(cpo = T, dic = T, waic = T))

summary(model_iid)

# visualise model fit
pdat$fitted_iid_mean <- model_iid$summary.fitted.values$mean
pdat$fitted_iid_median <- model_iid$summary.fitted.values$`0.5quant`

pdat$fitted_diff <- pdat$fitted_iid_mean - pdat$fitted_iid_median
hist(pdat$fitted_diff) #use mean

map <- pdat %>% group_by(NUTS_ID, NUTS_NUM) %>% 
  summarise(fitted = mean(fitted_iid_mean),
            obs = mean(alc_daily, na.rm = T)) %>% arrange(NUTS_NUM) 
map$random <- model_iid$summary.random$NUTS_NUM$mean

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

# Identify neighboring regions based on contiguity (first order only)
de_sf <- de_sf %>% arrange(NUTS_ID)
nb <- poly2nb(de_sf, queen = T) # this is based on de_sf (rather than dat) given missing areas in dat

plot(st_geometry(de_sf))
plot.nb(nb, st_geometry(de_sf), add = T)

# Save for later use within INLA
nb2INLA(paste0(DATE, "_nbmap.adj"), nb)
g <- inla.read.graph(filename = "20260107_nbmap.adj")

# Specify model
formula_icar <- alc_daily ~ 1 + sex + age_gr + sex*age_gr + education + sex*education + 
  DEGURBA + zgisd + education*zgisd + hosprate + sample + year + 
  f(NUTS_NUM, model = "besag", graph = g)

model_icar <- inla(formula_icar,
                    data = pdat, family = "binomial", control.predictor = list(link = 1), 
                    control.compute = list(cpo = T, dic = T, waic = T))

summary(model_icar)

# visualise model fit
pdat$fitted_icar_mean <- model_icar$summary.fitted.values$mean
pdat$fitted_icar_median <- model_icar$summary.fitted.values$`0.5quant`

pdat$fitted_diff <- pdat$fitted_icar_mean - pdat$fitted_icar_median
hist(pdat$fitted_diff) #use mean

map <- pdat %>% group_by(NUTS_ID, NUTS_NUM) %>% 
  summarise(fitted = mean(fitted_icar_mean),
            obs = mean(alc_daily, na.rm = T)) %>% arrange(NUTS_NUM) 
map$random <- model_icar$summary.random$NUTS_NUM$mean

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
  f(NUTS_NUM, model = "bym", graph = g)

model_bym <- inla(formula_bym,
                  data = pdat, family = "binomial", control.predictor = list(link = 1), 
                  control.compute = list(return.marginals.predictor=TRUE, cpo = T, dic = T, waic = T))

summary(model_bym)

# visualise model fit
pdat$fitted_bym_mean <- model_bym$summary.fitted.values$mean
pdat$fitted_bym_median <- model_bym$summary.fitted.values$`0.5quant`
pdat$fitted_bym_lci <- model_bym$summary.fitted.values$`0.025quant`
pdat$fitted_bym_uci <- model_bym$summary.fitted.values$`0.975quant`
pdat$fitted_bym_median <- model_bym$summary.fitted.values$`0.5quant`


pdat$fitted_diff <- pdat$fitted_bym_mean - pdat$fitted_bym_median
hist(pdat$fitted_diff) #use mean

map <- pdat %>% group_by(NUTS_ID, NUTS_NUM) %>% 
  summarise(fitted = mean(fitted_bym_mean),
            fitted_lci = mean(fitted_bym_lci),
            fitted_uci = mean(fitted_bym_uci),
            obs = mean(alc_daily, na.rm = T)) %>% arrange(NUTS_NUM) 
map$randomIID <- model_bym$summary.random$NUTS_NUM$mean[1:400]
map$randomicar <- model_bym$summary.random$NUTS_NUM$mean[401:800]

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
model_comp <- table_model_comparison(models = list(model_simple, model_iid, model_icar, model_bym)) %>% # best fitting: model_bym 
  mutate(model = c("simple", "iid", "icar", "bym"))
write.xlsx(model_comp, paste0(path_out, Sys.Date(), "_ModelComparison.xlsx"))

# Plot CPO
cpo <- cbind(simple = model_simple$cpo$cpo, iid = model_iid$cpo$cpo, icar = model_icar$cpo$cpo, bym = model_bym$cpo$cpo) %>% 
  as.data.frame() %>% pivot_longer(cols = c("simple", "iid", "icar", "bym"), names_to = "model", values_to = "cpo") 

ggplot(cpo) + geom_histogram(aes(x = cpo)) + facet_wrap(~model, ncol = 2) + theme_bw()
ggsave(paste0(path_out, Sys.Date(), "_Combined_CPO.svg"), width = 6, height = 6, dpi = 300, bg = "white")

# Plot model fit
ggarrange(fit_simple, fit_iid, fit_icar, fit_bym,
          ncol = 2, nrow = 2, common.legend = TRUE)
ggsave(paste0(path_out, Sys.Date(), "_Combined_Model_FIT.svg"), width = 10, height = 10, dpi = 300, bg = "white")

# Plot model random effects
ggarrange(spatial_iid, spatial_icar, spatial_bym_iid, spatial_bym_icar,
          ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")
ggsave(paste0(path_out, Sys.Date(), "_Combined_Model_RANDOM.svg"), width = 10, height = 12, dpi = 300, bg = "white")


# Extract data of best-fitting model (model_bym)

fixed.effects <- data.frame(
  var = rownames(model_bym$summary.fixed),
  mean = model_bym$summary.fixed[,"mean"],
  median = model_bym$summary.fixed[,"0.5quant"],
  lower = model_bym$summary.fixed[,"0.025quant"],
  upper = model_bym$summary.fixed[,"0.975quant"]
)
random.effects.iid <- data.frame(
  var = rownames(model_bym$summary.random$NUTS_NUM),
  mean = model_bym$summary.random$NUTS_NUM[1:400,"mean"],
  median = model_bym$summary.random$NUTS_NUM[1:400,"0.5quant"],
  lower = model_bym$summary.random$NUTS_NUM[1:400,"0.025quant"],
  upper = model_bym$summary.random$NUTS_NUM[1:400,"0.975quant"]
)
random.effects.icar <- data.frame(
  var = rownames(model_bym$summary.random$NUTS_NUM),
  mean = model_bym$summary.random$NUTS_NUM[401:800,"mean"],
  median = model_bym$summary.random$NUTS_NUM[401:800,"0.5quant"],
  lower = model_bym$summary.random$NUTS_NUM[401:800,"0.025quant"],
  upper = model_bym$summary.random$NUTS_NUM[401:800,"0.975quant"]
)
  
write.xlsx(list(fixed_effects = fixed.effects,random_effects_iid = random.effects.iid,random_effects_icar = random.effects.icar), 
           paste0(path_out, Sys.Date(), "_BYMmodel_fitted.xlsx"))

# Plot fitted values

ggplot(map) +
  geom_sf(aes(fill = fitted), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(na.value = "grey50", labels = scales::percent, limits = c(0, 0.31)) + 
  labs(fill = "") + theme_void() + theme(legend.position = "bottom")
ggsave(paste0(path_out, Sys.Date(), "_predAlcDaily_Combined_BYMmodel.svg"), width = 6, height = 8, dpi = 300, bg = "white")

cutoff <- map %>% ungroup() %>% st_drop_geometry() %>% 
  summarise(low = quantile(fitted, 0.25),
            med = quantile(fitted, 0.5),
            high = quantile(fitted, 0.75))

map <- map %>% 
  mutate(AlcRank = factor(case_when(
    fitted <= cutoff$low ~ "low",
    fitted >= cutoff$high ~ "high", 
    fitted > cutoff$low & fitted < cutoff$high ~ "medium"),
    levels = c("low", "medium", "high"),
    labels = c("1st quartile", "2nd - 3rd quartile", "4th quartile")))

ggplot(map) +
  geom_sf(aes(fill = AlcRank), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_manual(values = c("#a6d96a", "#ffffbf", "#fdae61")) + 
  labs(fill = "") + theme_void() + theme(legend.position = "bottom")
ggsave(paste0(path_out, Sys.Date(), "_AlcRank_BYMmodel.svg"), width = 6, height = 8, dpi = 300, bg = "white")


# Plot model fit 

ggplot(map, aes(x=obs, y=fitted)) + geom_point() + geom_smooth() + 
  scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) + 
  labs(x = "Observed prevalence", y = "Predicted prevalence") + theme_bw() 
ggsave(paste0(path_out, Sys.Date(), "_Combined_BYMmodel_fit.svg"), width = 8, height = 6, dpi = 300, bg = "white")

# Save output

names <- de_sf %>% st_drop_geometry() %>% select(c(NUTS_ID, NUTS_NAME)) %>% mutate(NUTS_ID_state = as.factor(substr(NUTS_ID, 1, 3)))
stnames <- de_sf_states %>% select(c(NUTS_ID, NUTS_NAME)) %>% rename(NUTS_ID_state = NUTS_ID, NUTS_NAME_state = NUTS_NAME) %>% left_join(names) %>% st_drop_geometry()
pred.out <- map %>% left_join(stnames) %>% st_drop_geometry() %>% ungroup() %>% 
  mutate_if(is.numeric, ~round(.*100,1)) %>% select(c(NUTS_NAME_state, NUTS_NAME, fitted, fitted_lci, fitted_uci)) 
saveRDS(pred.out, paste0(path_out, Sys.Date(), "_Pred_BYM.rds"))

pred.out %>% summarise(mean = mean(fitted), median = quantile(fitted, 0.5))

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
  stat_cor(method = "pearson", label.x = 0.2) +
  labs(x = "Estimated prevalence of frequent alcohol use", y = "Crude incidence rate F10 per 10,000") + theme_bw()
ggsave(paste0(path_out, Sys.Date(), "_Validation_AUD_BYMmodel.svg"), width = 8, height = 6, dpi = 300, bg = "white")

