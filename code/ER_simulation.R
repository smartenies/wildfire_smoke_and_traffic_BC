#' =============================================================================
#' Project: ECHO Aim1
#' Date created: November 19, 2019
#' Author: Sheena Martenies
#' Contact: smarte4@illinois.edu
#' 
#' Description:
#' Conduct a simulation to see if exposure misclassification could influence 
#' exposure response coefficients
#' =============================================================================

library(stringr)
library(tidyverse)
library(lubridate)
library(readxl)

#' -----------------------------------------------------------------------------
#' Read in the dataset
#' -----------------------------------------------------------------------------

lur_data2 <- read_csv(here::here("data", "Paired_Longterm_BC.csv"))

#' -----------------------------------------------------------------------------
#' Summarize the differences by GIS category
#' -----------------------------------------------------------------------------

pct_diff_sum_dist <- lur_data2 %>% 
  group_by(distance_cat) %>% 
  summarize(mean_sd = paste0(round(mean(bc_pct_diff, na.rm = T), 2), " (",
                             round(sd(bc_pct_diff, na.rm = T), 2), ")"),
            min = round(min(bc_pct_diff, na.rm = T), 2),
            q25 = round(quantile(bc_pct_diff, probs = 0.25, na.rm = T), 2),
            med_iqr = paste0(round(median(bc_pct_diff, na.rm = T), 2), " (",
                             round(IQR(bc_pct_diff, na.rm = T), 2), ")"),
            q75 = round(quantile(bc_pct_diff, probs = 0.75, na.rm = T), 2),
            q95 = round(quantile(bc_pct_diff, probs = 0.95, na.rm = T), 2),
            max = round(max(bc_pct_diff, na.rm = T), 2),
            n = n()) %>% 
  as.data.frame()
pct_diff_sum_dist

pct_diff_sum_length <- lur_data2 %>% 
  group_by(length_cat) %>% 
  summarize(mean_sd = paste0(round(mean(bc_pct_diff, na.rm = T), 2), " (",
                             round(sd(bc_pct_diff, na.rm = T), 2), ")"),
            min = round(min(bc_pct_diff, na.rm = T), 2),
            q25 = round(quantile(bc_pct_diff, probs = 0.25, na.rm = T), 2),
            med_iqr = paste0(round(median(bc_pct_diff, na.rm = T), 2), " (",
                             round(IQR(bc_pct_diff, na.rm = T), 2), ")"),
            q75 = round(quantile(bc_pct_diff, probs = 0.75, na.rm = T), 2),
            q95 = round(quantile(bc_pct_diff, probs = 0.95, na.rm = T), 2),
            max = round(max(bc_pct_diff, na.rm = T), 2),
            n = n()) %>% 
  as.data.frame()
pct_diff_sum_length

pct_diff_sum_aadt <- lur_data2 %>% 
  group_by(aadt_cat) %>% 
  summarize(mean_sd = paste0(round(mean(bc_pct_diff, na.rm = T), 2), " (",
                             round(sd(bc_pct_diff, na.rm = T), 2), ")"),
            min = round(min(bc_pct_diff, na.rm = T), 2),
            q25 = round(quantile(bc_pct_diff, probs = 0.25, na.rm = T), 2),
            med_iqr = paste0(round(median(bc_pct_diff, na.rm = T), 2), " (",
                             round(IQR(bc_pct_diff, na.rm = T), 2), ")"),
            q75 = round(quantile(bc_pct_diff, probs = 0.75, na.rm = T), 2),
            q95 = round(quantile(bc_pct_diff, probs = 0.95, na.rm = T), 2),
            max = round(max(bc_pct_diff, na.rm = T), 2),
            n = n()) %>% 
  as.data.frame()
pct_diff_sum_aadt

#' -----------------------------------------------------------------------------
#' Simulating the exposure-response- linear regression
#' -----------------------------------------------------------------------------

set.seed(100)

nrow(lur_data2)
row_seq <- 1:nrow(lur_data2)

nsamps <- 1000 #' number of data sets
true_beta_list <- c(0, -5, -10, -20, -50) #' true regression coefficient
n_rows <- nrow(lur_data2)
beta_df <- data.frame()

for(j in 1:length(true_beta_list)) {
  
  temp_beta_df <- data.frame()
  true_beta <- true_beta_list[j]
  print(paste0("True beta = ", true_beta))
  
  for(i in 1:nsamps) {
    if(i %% 100 == 0) print(i)
    
    #' Select a subset of the points to use in the data set
    rows <- sample(row_seq, n_rows, replace = T)
    df_temp <- lur_data2[rows, ]
    bc0 <- df_temp$bc_wfs0
    bc1 <- df_temp$bc_wfs1
    
    #' Simulate Y for the data set using the wfs0 BC and a randomly selected error
    #' intercept is mean bw in the HS cohort
    int <- 3205 # bw in the HS cohort
    err <- rnorm(n_rows, 0, 1)
    y <- (true_beta * bc0) + err
    h.x <- true_beta * bc0
    
    #' "Correct" BC (no wildfires)
    fit_wfs0 <- lm(y ~ bc0)
    preds0 <- predict(fit_wfs0, newdata = data.frame(bc0),
                      level = 0.95, interval = "confidence")
    lwr0 <- preds0[,2] # lower bound
    upr0 <- preds0[,3] # upper bound 
    
    #' LM Summary
    temp_beta_df[i, "wfs0_beta"] <- summary(fit_wfs0)$coefficients[2,1]
    temp_beta_df[i, "wfs0_se"] <- summary(fit_wfs0)$coefficients[2,2]
    temp_beta_df[i, "wfs0_adj_r"] <- summary(fit_wfs0)$adj.r.squared
    
    #' bias and MSE
    temp_beta_df[i, "wfs0_bias"] <- summary(fit_wfs0)$coefficients[2,1] - true_beta
    temp_beta_df[i, "wfs0_mse"] <- mean((y - preds0[,1])^2)
    temp_beta_df[i, "wfs0_rmse"] <- sqrt(mean((y - preds0[,1])^2))
    
    #' Coverage metric
    #' percent of individual h(x_i)'s covered by the CI
    temp_beta_df[i, "coverage0"] <- (length(which(h.x > lwr0 & h.x < upr0))/n_rows)*100 
    
    #' "Incorrect" BC (with wildfires)
    fit_wfs1 <- lm(y ~ bc1)
    preds1 <- predict(fit_wfs1, newdata = data.frame(bc1),
                      level = 0.95, interval = "confidence")
    lwr1 <- preds1[,2] # lower bound
    upr1 <- preds1[,3] # upper bound 
    
    #' LM Summary
    temp_beta_df[i, "wfs1_beta"] <- summary(fit_wfs1)$coefficients[2,1]
    temp_beta_df[i, "wfs1_se"] <- summary(fit_wfs1)$coefficients[2,2]
    temp_beta_df[i, "wfs1_adj_r"] <- summary(fit_wfs1)$adj.r.squared
    
    #' bias and MSE
    temp_beta_df[i, "wfs1_bias"] <- summary(fit_wfs1)$coefficients[2,1] - true_beta
    temp_beta_df[i, "wfs1_mse"] <- mean((y - preds1[,1])^2)
    temp_beta_df[i, "wfs1_rmse"] <- sqrt(mean((y - preds1[,1])^2))
    
    #' Coverage metric
    #' percent of individual h(x_i)'s covered by the CI
    temp_beta_df[i, "coverage1"] <- (length(which(h.x > lwr1 & h.x < upr1))/n_rows)*100
  }
  
  temp_beta_df$true_beta <- true_beta
  beta_df <- bind_rows(beta_df, temp_beta_df)
  rm(temp_beta_df)
}

#' -----------------------------------------------------------------------------
#' Save simulation results
#' -----------------------------------------------------------------------------

write_csv(beta_df, here::here("results", "Beta_Simulation_Study_Linear.csv"))

#' -----------------------------------------------------------------------------
#' Compare distributions
#' -----------------------------------------------------------------------------

beta_df <- read_csv(here::here("results", "Beta_Simulation_Study_Linear.csv"))
true_beta_list <- c(0, -5, -10, -20, -50) 

beta_df_0 <- filter(beta_df, true_beta == true_beta_list[1])
summary(beta_df_0)

beta_df_5 <- filter(beta_df, true_beta == true_beta_list[2])
summary(beta_df_5)

beta_df_10 <- filter(beta_df, true_beta == true_beta_list[3])
summary(beta_df_10)

beta_df_20 <- filter(beta_df, true_beta == true_beta_list[4])
summary(beta_df_20)

beta_df_50 <- filter(beta_df, true_beta == true_beta_list[5])
summary(beta_df_50)

#' Paired Student's t-test for each "true" beta
mean(beta_df_0$wfs1_beta) - mean(beta_df_0$wfs0_beta) 
t.test(beta_df_0$wfs1_beta, beta_df_0$wfs0_beta, paired = T) #' beta = 0

mean(beta_df_5$wfs1_beta) - mean(beta_df_5$wfs0_beta) 
t.test(beta_df_5$wfs1_beta, beta_df_5$wfs0_beta, paired = T) #' beta = -5

mean(beta_df_10$wfs1_beta) - mean(beta_df_10$wfs0_beta)
t.test(beta_df_10$wfs1_beta, beta_df_10$wfs0_beta, paired = T) #' beta = -10

mean(beta_df_20$wfs1_beta) - mean(beta_df_20$wfs0_beta)
t.test(beta_df_20$wfs1_beta, beta_df_20$wfs0_beta, paired = T) #' beta = -20

mean(beta_df_50$wfs1_beta) - mean(beta_df_50$wfs0_beta)
t.test(beta_df_50$wfs1_beta, beta_df_50$wfs0_beta, paired = T) #' beta = -50

#' -----------------------------------------------------------------------------
#' Calculate mean percent bias, MSE, coverage
#' -----------------------------------------------------------------------------

beta_summary <- beta_df %>% 
  mutate(wfs0_pct_bias = ifelse(true_beta == 0, NA,
                                ((wfs0_bias)/true_beta) * 100),
         wfs1_pct_bias = ifelse(true_beta == 0, NA,
                                ((wfs1_bias)/true_beta) * 100)) %>% 
  group_by(true_beta) %>% 
  summarize(mean_bias0 = paste0(round(mean(wfs0_bias, na.rm=T), 1),
                                    " (",
                                    round(sd(wfs0_bias, na.rm=T), 1),
                                    ")"),
            mean_pct_bias0 = paste0(round(mean(wfs0_pct_bias, na.rm=T), 1),
                                    " (",
                                    round(sd(wfs0_pct_bias, na.rm=T), 1),
                                    ")"),
            rmse0 = paste0(round(mean(wfs0_rmse), 1),
                          " (",
                          round(sd(wfs0_rmse, na.rm=T), 1),
                          ")"),
            coverage0 = paste0(round(mean(coverage0), 1),
                               " (",
                               round(sd(coverage0, na.rm=T), 1),
                               ")"),
            mean_bias1 = paste0(round(mean(wfs1_bias, na.rm=T), 1),
                                " (",
                                round(sd(wfs1_bias, na.rm=T), 1),
                                ")"),
            mean_pct_bias1 = paste0(round(mean(wfs1_pct_bias, na.rm=T), 1),
                                    " (",
                                    round(sd(wfs1_pct_bias, na.rm=T), 1),
                                    ")"),
            rmse1 = paste0(round(mean(wfs1_rmse), 1),
                          " (",
                          round(sd(wfs1_rmse, na.rm=T), 1),
                          ")"),
            coverage1 = paste0(round(mean(coverage1), 1),
                               " (",
                               round(sd(coverage1, na.rm=T), 1),
                               ")")
            ) %>%
  arrange(desc(true_beta))
beta_summary

write_csv(beta_summary, here::here("results", "Simulation_Diagnostics_Linear.csv"))

