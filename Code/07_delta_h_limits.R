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
labels[25, 1] <- "Peak Flow Magnitude (Q99, cfs)"
labels[25, 2] <- "Q99"
labels[25, 3] <- "Peak Flow Magnitude"
labels

# ASCI --------------------------------------------------------------------

## upload data
all_asci <- read.csv("output_data/01_h_asci_neg_pos_logR_metrics_figures_July2023.csv") %>% ## change data here!!! #edited 8/29
  mutate(PredictedProbability = case_when(hydro.endpoints == "d_peak_10" & hydro > 0 ~ NA_real_, ### edited 9/12 to remove + peak values 
                                          hydro.endpoints == "d_peak_5" & hydro > 0 ~ NA_real_,
                                          hydro.endpoints == "d_peak_2" & hydro > 0 ~ NA_real_,
                                          TRUE ~ as.numeric(PredictedProbability)))


## FIX NAMES TO MATCH LABELS AND LIMITS - Rachel 9/6
all_asci <- all_asci %>% 
  mutate(hydro.endpoints = case_when(hydro.endpoints == "d_ds_mag_50" ~ "DS_Mag_50",                 #renamed all mag variables here and below
                                    hydro.endpoints == "d_fa_mag" ~ "FA_Mag",
                                    hydro.endpoints == "d_peak_10" ~ "Peak_10",
                                    hydro.endpoints == "d_peak_2" ~ "Peak_2",
                                    hydro.endpoints == "d_peak_5" ~ "Peak_5",
                                    hydro.endpoints == "d_sp_mag" ~ "SP_Mag",
                                    hydro.endpoints == "d_wet_bfl_mag_10" ~ "Wet_BFL_Mag_10",
                                    hydro.endpoints == "d_wet_bfl_mag_50" ~ "Wet_BFL_Mag_50", 
                                    hydro.endpoints == "delta_q99" ~ "Q99"))


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
mag_metrics <-c("DS_Mag_50", "FA_Mag", "Peak_10", "Peak_2", "Peak_5", "SP_Mag", 
                "Wet_BFL_Mag_10", "Wet_BFL_Mag_50", "Q99")


## subset to only important metrics
all_asci_sub <- subset(all_asci, hydro.endpoints %in% mag_metrics) # edited 8/29 by rachel 

unique(all_asci_sub$hydro.endpoints)


# CSCI --------------------------------------------------------------------

## upload data
all_csci <- read.csv("output_data/01_CSCI_neg_pos_logR_metrics_figures_July2023.csv") %>% #edited 8/29
  mutate(PredictedProbability = case_when(hydro.endpoints == "d_peak_10" & hydro > 0 ~ NA_real_, ### edited 9/12 to remove + peak values 
                                          hydro.endpoints == "d_peak_5" & hydro > 0 ~ NA_real_,
                                          hydro.endpoints == "d_peak_2" & hydro > 0 ~ NA_real_,
                                          TRUE ~ as.numeric(PredictedProbability)))

## FIX NAMES TO MATCH LABELS AND LIMITS - Rachel 9/6
all_csci <- all_csci %>% 
  mutate(hydro.endpoints = case_when(hydro.endpoints == "d_ds_mag_50" ~ "DS_Mag_50",                 #renamed all mag variables here and below
                                     hydro.endpoints == "d_fa_mag" ~ "FA_Mag",
                                     hydro.endpoints == "d_peak_10" ~ "Peak_10",
                                     hydro.endpoints == "d_peak_2" ~ "Peak_2",
                                     hydro.endpoints == "d_peak_5" ~ "Peak_5",
                                     hydro.endpoints == "d_sp_mag" ~ "SP_Mag",
                                     hydro.endpoints == "d_wet_bfl_mag_10" ~ "Wet_BFL_Mag_10",
                                     hydro.endpoints == "d_wet_bfl_mag_50" ~ "Wet_BFL_Mag_50", 
                                     hydro.endpoints == "delta_q99" ~ "Q99"))

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
mag_metrics <-c("DS_Mag_50", "FA_Mag", "Peak_10", "Peak_2", "Peak_5", "SP_Mag", 
                "Wet_BFL_Mag_10", "Wet_BFL_Mag_50", "Q99")


## subset to only important metrics
all_csci_sub <- subset(all_csci, hydro.endpoints %in% mag_metrics) #edited by Rachel 8/29



# find roots of curve -----------------------------------------------------

## ASCI

## create df
df <- as.data.frame(matrix(ncol=10))
colnames(df) <- c("metric", "Threshold70", "Threshold90", "Threshold99", "n", "Type", "Biol", "Bio_endpoint", "Bio_threshold", "Hydro_endpoint")
### edited threshold column names from 25, 50, 75 to 70, 90, 99 - Rachel 9/6

## define metrics
metrics <- unique(all_asci_sub$comb_code_type)
metrics <- metrics[grep("0.86", metrics)]  # edited by Rachel 8/29
metrics

# i = 1
  ## loop through metrics
  for(i in 1: length(metrics)) {
    
    met <- metrics[i]

    hydroxx <- all_asci_sub %>%
      filter(comb_code_type == met)
    
    ## get curves values at different probabilities
    thresh70 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.7) ### 8/29 changed for new probability thresholds - used to be 0.5
    thresh70 <- ifelse(length(thresh70) == 0, NA, thresh70)

    thresh90 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.9) ### 8/29 changed for new probability thresholds - used to be 0.25
    thresh90 <- ifelse(length(thresh90) == 0, NA, thresh90)

    thresh99 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.99) ### 8/29 changed for new probability thresholds - used to be 0.75
    thresh99 <- ifelse(length(thresh99) == 0, NA, thresh99)

    ## add info to df
    df[i, 1] <- met
    df[i, 2] <- thresh70
    df[i, 3] <- thresh90
    df[i, 4] <- thresh99
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
colnames(df) <- c("metric", "Threshold70", "Threshold90", "Threshold99", "n", "Type", "Biol", "Bio_endpoint", "Bio_threshold", "Hydro_endpoint")
### edited threshold column names from 25, 50, 75 to 70, 90, 99 - Rachel 9/6

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
  thresh70 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.7) ### 8/29 changed for new probability thresholds - used to be 0.5
  thresh70 <- ifelse(length(thresh70) == 0, NA, thresh70)
  
  thresh90 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.9) ### 8/29 changed for new probability thresholds - used to be 0.25
  thresh90 <- ifelse(length(thresh90) == 0, NA, thresh90)
  
  thresh99 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.99) ### 8/29 changed for new probability thresholds - used to be 0.75
  thresh99 <- ifelse(length(thresh99) == 0, NA, thresh99)
  
  ## add info to df
  df[i, 1] <- met
  df[i, 2] <- thresh70
  df[i, 3] <- thresh90
  df[i, 4] <- thresh99
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

