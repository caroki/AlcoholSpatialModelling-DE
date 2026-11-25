# DEBRA Alcohol
# Sensitivity analysis 1 (informed priors)
# Carolin Kilian
# 20-04-2026
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

# Fit model with uninformed priors
formula_orig <- alc_daily ~ 1 + sex + age_gr + sex*age_gr + education + sex*education + 
  DEGURBA + zgisd + education*zgisd + hosprate + sample + year + 
  f(NUTS_NUM, model = "bym", graph = g, 
    hyper = list(prec.unstruct = list(prior = "loggamma", param = c(inla.models()$latent$bym$hyper$theta1$param[1], inla.models()$latent$bym$hyper$theta1$param[2])), # IID component
                 prec.spatial = list(prior = "loggamma", param = c(inla.models()$latent$bym$hyper$theta2$param[1], inla.models()$latent$bym$hyper$theta2$param[2])))) # ICAR component

prior.fixed <- list(mean.intercept = inla.set.control.fixed.default()$mean.intercept, prec.intercept = inla.set.control.fixed.default()$prec.intercept,
                    mean = inla.set.control.fixed.default()$mean, prec = inla.set.control.fixed.default()$prec)

model_orig <- inla(formula_orig,
                   data = pdat, family = "binomial", control.predictor = list(link = 1), 
                   control.compute = list(return.marginals.predictor=TRUE, cpo = T, dic = T, waic = T),
                   control.fixed = prior.fixed)

summary(model_orig)

# SA1-A: Run model with informed priors (smaller precision)
formula_low <- alc_daily ~ 1 + sex + age_gr + sex*age_gr + education + sex*education + 
  DEGURBA + zgisd + education*zgisd + hosprate + sample + year + 
  f(NUTS_NUM, model = "bym", graph = g, 
    hyper = list(prec.unstruct = list(prior = "loggamma", param = c(inla.models()$latent$bym$hyper$theta1$param[1], inla.models()$latent$bym$hyper$theta1$param[2]*10)), # IID component
                 prec.spatial = list(prior = "loggamma", param = c(inla.models()$latent$bym$hyper$theta2$param[1], inla.models()$latent$bym$hyper$theta2$param[2]*10)))) # ICAR component

prior.fixed <- list(mean.intercept = inla.set.control.fixed.default()$mean.intercept, prec.intercept = inla.set.control.fixed.default()$prec.intercept,
                    mean = inla.set.control.fixed.default()$mean, prec = inla.set.control.fixed.default()$prec/10)

model_low <- inla(formula_low,
                 data = pdat, family = "binomial", control.predictor = list(link = 1), 
                 control.compute = list(return.marginals.predictor=TRUE, cpo = T, dic = T, waic = T),
                 control.fixed = prior.fixed)

summary(model_low)

# SA1-B: Run model with informed priors (larger precision)
formula_upp <- alc_daily ~ 1 + sex + age_gr + sex*age_gr + education + sex*education + 
  DEGURBA + zgisd + education*zgisd + hosprate + sample + year + 
  f(NUTS_NUM, model = "bym", graph = g, 
    hyper = list(prec.unstruct = list(prior = "loggamma", param = c(inla.models()$latent$bym$hyper$theta1$param[1], inla.models()$latent$bym$hyper$theta1$param[2]/10)), # IID component
                 prec.spatial = list(prior = "loggamma", param = c(inla.models()$latent$bym$hyper$theta2$param[1], inla.models()$latent$bym$hyper$theta2$param[2]/10)))) # ICAR component

prior.fixed <- list(mean.intercept = inla.set.control.fixed.default()$mean.intercept, prec.intercept = inla.set.control.fixed.default()$prec.intercept,
                    mean = inla.set.control.fixed.default()$mean, prec = inla.set.control.fixed.default()$prec*10)

model_upp <- inla(formula_upp,
                 data = pdat, family = "binomial", control.predictor = list(link = 1), 
                 control.compute = list(return.marginals.predictor=TRUE, cpo = T, dic = T, waic = T),
                 control.fixed = prior.fixed)

summary(model_upp)


# Compare models

models <- list(model_orig = model_orig, model_low = model_low, model_upp = model_upp)

# For fixed effects 

dfs <- list()
i <- 1
for (i in seq_along(models)) {
  summary <- models[[i]]$summary.fixed
  df <- data.frame(
    term = rownames(summary),
    mean = summary$mean,
    lower = summary$`0.025quant`,
    upper = summary$`0.975quant`,
    model = names(models)[i])
  dfs[[i]] <- df
}
comp_fixed <- do.call(rbind, dfs) %>% 
  pivot_wider(id_cols = term, names_from = model, values_from = c(mean, lower, upper)) %>% 
  dplyr::select(term, mean_model_orig, lower_model_orig, upper_model_orig, 
         mean_model_low, lower_model_low, upper_model_low, 
         mean_model_upp, lower_model_upp, upper_model_upp) %>%
  mutate(across(where(is.numeric), ~ sprintf("%.4f", .)))
  

# For random effects

dfs <- list()
for (i in seq_along(models)) {
  summary <- models[[i]]$summary.hyperpar
  df <- data.frame(
    term = rownames(summary),
    mean = summary$mean,
    lower = summary$`0.025quant`,
    upper = summary$`0.975quant`,
    model = names(models)[i])
  dfs[[i]] <- df
}
comp_hyper <- do.call(rbind, dfs) %>% 
  pivot_wider(id_cols = term, names_from = model, values_from = c(mean, lower, upper)) %>% 
  dplyr::select(term, mean_model_orig, lower_model_orig, upper_model_orig, 
         mean_model_low, lower_model_low, upper_model_low, 
         mean_model_upp, lower_model_upp, upper_model_upp) %>%
  mutate(across(where(is.numeric), ~ sprintf("%.4f", .)))


# Predict using alternative prior settings

# Predict prevalence

dfs <- list()
for (i in seq_along(models)) {
  summary <- models[[i]]$summary.fitted.values
  df <- data.frame(
    term = rownames(summary),
    mean = summary$mean,
    lower = summary$`0.025quant`,
    upper = summary$`0.975quant`,
    model = names(models)[i])
  dfs[[i]] <- df
}
comp_pred_sub <- do.call(rbind, dfs) %>% 
  pivot_wider(id_cols = term, names_from = model, values_from = c(mean, lower, upper)) %>%
  cbind(., pdat) %>% filter(source == "pred") %>%
  group_by(NUTS_ID, NUTS_NUM) %>% 
  summarise(across(.cols = starts_with("mean_"),
                   .fns = mean,
                   .names = "fitted_{.col}"),
            across(.cols = starts_with("lower_"),
                   .fns = mean,
                   .names = "fitted_{.col}"),
            across(.cols = starts_with("upper_"),
                   .fns = mean,
                   .names = "fitted_{.col}")) %>% 
  dplyr::select(NUTS_ID, NUTS_NUM,
         fitted_mean_model_orig, fitted_lower_model_orig, fitted_upper_model_orig, 
         fitted_mean_model_low, fitted_lower_model_low, fitted_upper_model_low, 
         fitted_mean_model_upp, fitted_lower_model_upp, fitted_upper_model_upp) %>%
  mutate(across(where(is.numeric), ~ sprintf("%.5f", .)))
  
  
# Predict spatial error

dfs <- list()
for (i in seq_along(models)) {
  summary <- models[[i]]$summary.random$NUTS_NUM
  df <- data.frame(
    term = rownames(summary)[1:400],
    iid = summary$mean[1:400],
    icar = summary$mean[401:800],
  model = names(models)[i])
  dfs[[i]] <- df
}
comp_pred <- do.call(rbind, dfs) %>% 
  pivot_wider(id_cols = term, names_from = model, values_from = c(iid, icar)) %>%
  cbind(., comp_pred_sub) %>% dplyr::select(-c(term, NUTS_NUM))


# Plot predicted prevalence across sensitivity analysis

map <- de_sf %>% left_join(comp_pred) 

orig <- ggplot(map) +
  geom_sf(aes(fill = as.numeric(fitted_mean_model_orig)), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(na.value = "grey50", labels = scales::percent, limits = c(0, 0.32)) + 
  ggtitle("Estimated prevalence of frequent alcohol use", "Vague priors") + labs(fill = "") + theme_void()  
low <- ggplot(map) +
  geom_sf(aes(fill = as.numeric(fitted_mean_model_low)), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(na.value = "grey50", labels = scales::percent, limits = c(0, 0.32)) + 
  ggtitle("", "Vague priors with smaller precision") + labs(fill = "") + theme_void()  
upp <- ggplot(map) +
  geom_sf(aes(fill = as.numeric(fitted_mean_model_upp)), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis_c(na.value = "grey50", labels = scales::percent, limits = c(0, 0.32)) + 
  ggtitle("", "Vague priors with larger precision") + labs(fill = "") + theme_void()  

ggarrange(orig, low, upp, ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom")
ggsave(paste0(path_out, Sys.Date(), "_SA1_PriorsPred.svg"), width = 12, height = 12, dpi = 300, bg = "white")


# Plot spatial error (IID) across sensitivity analysis

iid_orig <- ggplot(map) +
  geom_sf(aes(fill = as.numeric(iid_model_orig)), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradient2(low = "#abd9e9", mid = "white", high = "#d7191c", midpoint = 0, na.value = "grey50") + 
  ggtitle("IID", "Vague priors") + labs(fill = "") + theme_void()  
iid_low <- ggplot(map) +
  geom_sf(aes(fill = as.numeric(iid_model_low)), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradient2(low = "#abd9e9", mid = "white", high = "#d7191c", midpoint = 0, na.value = "grey50") + 
  ggtitle("", "Vague priors with smaller precision") + labs(fill = "") + theme_void()  
iid_upp <- ggplot(map) +
  geom_sf(aes(fill = as.numeric(iid_model_upp)), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradient2(low = "#abd9e9", mid = "white", high = "#d7191c", midpoint = 0, na.value = "grey50") + 
  ggtitle("", "Vague priors with larger precision") + labs(fill = "") + theme_void()  


# Plot spatial error (icar) across sensitivity analysis

icar_orig <- ggplot(map) +
  geom_sf(aes(fill = as.numeric(icar_model_orig)), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradient2(low = "#abd9e9", mid = "white", high = "#d7191c", midpoint = 0, na.value = "grey50") + 
  ggtitle("ICAR", "Vague priors") + labs(fill = "") + theme_void()  
icar_low <- ggplot(map) +
  geom_sf(aes(fill = as.numeric(icar_model_low)), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradient2(low = "#abd9e9", mid = "white", high = "#d7191c", midpoint = 0, na.value = "grey50") + 
  ggtitle("", "Vague priors with smaller precision") + labs(fill = "") + theme_void()  
icar_upp <- ggplot(map) +
  geom_sf(aes(fill = as.numeric(icar_model_upp)), color = "black", size = 0.1) + 
  geom_sf(data = de_sf_states, fill = NA, color = "black", size = 0.5) +
  scale_fill_gradient2(low = "#abd9e9", mid = "white", high = "#d7191c", midpoint = 0, na.value = "grey50") + 
  ggtitle("", "Vague priors with larger precision") + labs(fill = "") + theme_void()  

ggarrange(iid_orig, iid_low, iid_upp, icar_orig, icar_low, icar_upp,
          ncol = 3, nrow = 2, common.legend = TRUE, legend = "bottom") 
ggsave(paste0(path_out, Sys.Date(), "_SA1_PriorsRE.svg"), width = 12, height = 12, dpi = 300, bg = "white")


# Save output as table

write.xlsx(list(fixed_model_estimates = comp_fixed, hyper_model_estimates = comp_hyper, pred_prev = comp_pred), 
           paste0(path_out, Sys.Date(), "_SA1_InformedPriors.xlsx"))




