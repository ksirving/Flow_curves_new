## Delta H limits

getwd()
library(ggplot2)
library(dplyr)
library(tidyverse)

load("Code/functions/root_interpolation_function.Rdata")
## function to find value in curve

## full names for labels
labels <- read.csv("Data/ffm_names.csv")
labels <- labels[1:24, ]
labels <- labels %>% rename(hydro.endpoints = Flow.Metric.Code)
labels[25, 1] <- "Magnitude of largest annual storm"
labels[25, 2] <- "Q99"
labels[25, 3] <- "Peak Flow"
labels

# ASCI --------------------------------------------------------------------

## upload data
all_asci <- read.csv("output_data/01_h_asci_neg_pos_logR_metrics_figures_July2023.csv") ## change data here!!! #edited 8/29

## scale probability
all_asci <- all_asci %>%
  select(-X) %>%
  group_by(comb_code, Type) %>%
  mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
           (max(PredictedProbability)-min(PredictedProbability))) %>%
  mutate(comb_code_type = paste(comb_code, "_", Type, sep=""))

all_asci <- left_join(all_asci, labels, by ="hydro.endpoints")

head(all_asci)

# asci_metrics <- c("Q99", "SP_Dur", "DS_Dur_WS")
## added by rachel 8/29
mag_metrics <-c("d_ds_mag_50", "d_fa_mag", "d_peak_10", "d_peak_2", "d_peak_5", "d_sp_mag", 
                "d_wet_bfl_mag_10", "d_wet_bfl_mag_50", "delta_q99")


## subset to only important metrics
all_asci_sub <- subset(all_asci, hydro.endpoints %in% mag_metrics) # edited 8/29 by rachel 

unique(all_asci_sub$hydro.endpoints)


# CSCI --------------------------------------------------------------------

## upload data
all_csci <- read.csv("output_data/01_CSCI_neg_pos_logR_metrics_figures_July2023.csv") #edited 8/29

## scale probability
all_csci <- all_csci %>%
  select(-X) %>%
  group_by(comb_code, Type) %>%
  mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
           (max(PredictedProbability)-min(PredictedProbability))) %>%
  mutate(comb_code_type = paste(comb_code, "_", Type, sep=""))

all_csci <- left_join(all_csci, labels, by ="hydro.endpoints")

head(all_csci)

# csci_metrics <-c("Q99", "SP_Tim","DS_Dur_WS")
## added by rachel 8/29
# mag_metrics <-c("Q99", "DS_Mag_90", "DS_Mag_50", "SP_Mag", "Wet_BFL_Mag_50", "Wet_BFL_Mag_10", "FA_Mag", "DS_Dur_WS", "SP_Dur", "SP_Tim")
mag_metrics <-c("d_ds_mag_50", "d_fa_mag", "d_peak_10", "d_peak_2", "d_peak_5", "d_sp_mag", 
                "d_wet_bfl_mag_10", "d_wet_bfl_mag_50", "delta_q99")


## subset to only important metrics
all_csci_sub <- subset(all_csci, hydro.endpoints %in% mag_metrics) #edited by Rachel 8/29



# find roots of curve -----------------------------------------------------

## ASCI

## create df
df <- as.data.frame(matrix(ncol=10))
colnames(df) <- c("metric", "Threshold25", "Threshold50", "Threshold75", "n", "Type", "Biol", "Bio_endpoint", "Bio_threshold", "Hydro_endpoint")


## define metrics
metrics <- unique(all_asci_sub$comb_code_type)
metrics <- metrics[grep("0.86", metrics)]  # edited by Rachel 8/29
metrics

  ## loop through metrics
  for(i in 1: length(metrics)) {
    
    met <- metrics[i]

    hydroxx <- all_asci_sub %>%
      filter(comb_code_type == met)
    
    ## get curves values at different probabilities
    thresh50 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.7) ### 8/29 changed for new probability thresholds - used to be 0.5
    thresh50 <- ifelse(length(thresh50) == 0, NA, thresh50)

    thresh25 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.9) ### 8/29 changed for new probability thresholds - used to be 0.25
    thresh25 <- ifelse(length(thresh25) == 0, NA, thresh25)

    thresh75 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.99) ### 8/29 changed for new probability thresholds - used to be 0.75
    thresh75 <- ifelse(length(thresh75) == 0, NA, thresh75)

    ## add info to df
    df[i, 1] <- met
    df[i, 2] <- thresh25
    df[i, 3] <- thresh50
    df[i, 4] <- thresh75
    df[i, 5] <- length(hydroxx$PredictedProbabilityScaled)
    df[i ,6] <- hydroxx$Type[1]
    df[i ,7] <- "ASCI"
    df[i, 8] <- hydroxx$biol.endpoints[1]
    df[i, 9] <- hydroxx$thresholds[1]
    df[i, 10] <- hydroxx$hydro.endpoints[1]
    
    
  }
  

df
write.csv(df, "output_data/Manuscript/07_ASCI_delta_thresholds_scaled_updated.csv")


# CSCI --------------------------------------------------------------------

## create df
df <- as.data.frame(matrix(ncol=10))
colnames(df) <- c("metric", "Threshold25", "Threshold50", "Threshold75", "n", "Type", "Biol", "Bio_endpoint", "Bio_threshold", "Hydro_endpoint")


## define metrics
metrics <- unique(all_csci_sub$comb_code_type)
metrics <- metrics[grep("0.79", metrics)]  # edited by Rachel 8/29
metrics

## loop through metrics
for(i in 1: length(metrics)) {
  
  met <- metrics[i]
  
  hydroxx <- all_csci_sub %>%
    filter(comb_code_type == met)
  
  ## get curves values at different probabilities
  thresh50 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.7) ### 8/29 changed for new probability thresholds - used to be 0.5
  thresh50 <- ifelse(length(thresh50) == 0, NA, thresh50)
  
  thresh25 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.9) ### 8/29 changed for new probability thresholds - used to be 0.25
  thresh25 <- ifelse(length(thresh25) == 0, NA, thresh25)
  
  thresh75 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.99) ### 8/29 changed for new probability thresholds - used to be 0.75
  thresh75 <- ifelse(length(thresh75) == 0, NA, thresh75)
  
  ## add info to df
  df[i, 1] <- met
  df[i, 2] <- thresh25
  df[i, 3] <- thresh50
  df[i, 4] <- thresh75
  df[i, 5] <- length(hydroxx$PredictedProbabilityScaled)
  df[i ,6] <- hydroxx$Type[1]
  df[i ,7] <- "CSCI"
  df[i, 8] <- hydroxx$biol.endpoints[1]
  df[i, 9] <- hydroxx$thresholds[1]
  df[i, 10] <- hydroxx$hydro.endpoints[1]
  
  
}


df
write.csv(df, "output_data/Manuscript/07_CSCI_delta_thresholds_scaled_updated.csv")


# Combine data ------------------------------------------------------------

asci <- read.csv("output_data/Manuscript/07_ASCI_delta_thresholds_scaled_updated.csv")
csci <- read.csv("output_data/Manuscript/07_CSCI_delta_thresholds_scaled_updated.csv")

delta <- rbind(asci, csci)

write.csv(delta, "output_data/Manuscript/07_ALL_delta_thresholds_scaled_updated.csv")
