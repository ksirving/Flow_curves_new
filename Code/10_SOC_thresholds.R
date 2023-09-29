############################## SOC thresholds ##############################
library(tidyverse)
library(tidyr)

## load in root linear interpolation function
load("code/functions/root_interpolation_function.Rdata")

## regional curves delta H data
## upload and get delta h 


## full names for labels - flow metric name and corresponding hydrological endpoint & flow component label 
labels <- read.csv("Data/ffm_names.csv")
labels <- labels[1:24, ]
labels <- labels %>% rename(Hydro_endpoint = Flow.Metric.Code)
labels[25, 1] <- "Peak Flow Magnitude (Q99, cfs)"
labels[25, 2] <- "Q99"
labels[25, 3] <- "Peak Flow Magnitude"
labels


# CSCI --------------------------------------------------------------------
######################### Combine Old and New Datasets together - CSCI
# Rachel edited 9/26
# old 
all_csci_April2021 <- read.csv("output_data/01_csci_neg_pos_logR_metrics_figures_April2021.csv") %>% 
  #filtering just for the metrics we need from the old data
  filter(hydro.endpoints %in% c("DS_Dur_WS", "SP_Dur", "SP_Tim"))

# new 
all_csci_July2023 <- read.csv("output_data/01_csci_neg_pos_logR_metrics_figures_July2023.csv") %>% 
  # filtering for the metrics we actually use in the new data - includes ASCI and CSCI just in case 
  filter(hydro.endpoints %in% c("d_peak_2", "d_sp_mag", "d_wet_bfl_mag_50", "d_ds_mag_50", "d_peak_10", "d_fa_mag"))

#### combine both datasets together 
all_csci <- all_csci_April2021 %>% 
  bind_rows(all_csci_July2023)

### read in curve data based on glms & +/- delta H for each FFM - csci
all_csci <- all_csci %>% 
  mutate(PredictedProbability = case_when(hydro.endpoints == "d_peak_10" & hydro > 0 ~ NA_real_, ### edited 9/26 to remove + peak values 
                                          hydro.endpoints == "d_peak_5" & hydro > 0 ~ NA_real_,
                                          hydro.endpoints == "d_peak_2" & hydro > 0 ~ NA_real_,
                                          TRUE ~ as.numeric(PredictedProbability)))

## FIX NAMES TO MATCH LABELS AND LIMITS - Rachel 9/26
all_csci <- all_csci %>% 
  mutate(hydro.endpoints = case_when(hydro.endpoints == "d_ds_mag_50" ~ "DS_Mag_50",                 #renamed all mag variables here and below
                                     hydro.endpoints == "d_fa_mag" ~ "FA_Mag",
                                     hydro.endpoints == "d_peak_10" ~ "Peak_10",
                                     hydro.endpoints == "d_peak_2" ~ "Peak_2",
                                     hydro.endpoints == "d_peak_5" ~ "Peak_5",
                                     hydro.endpoints == "d_sp_mag" ~ "SP_Mag",
                                     hydro.endpoints == "d_wet_bfl_mag_10" ~ "Wet_BFL_Mag_10",
                                     hydro.endpoints == "d_wet_bfl_mag_50" ~ "Wet_BFL_Mag_50", 
                                     hydro.endpoints == "delta_q99" ~ "Q99", 
                                     T ~ as.character(hydro.endpoints))) %>% 
  # also fixing comb_code just in case (I left it alone in script 07 and 07a bc it didnt seem to be necessary)
  mutate(comb_code = paste(biol.endpoints, "_", hydro.endpoints, "_", thresholds, sep = ""))

# remove column x, scaling probability, adding column for combo code+threshold+type (- or +) 
all_csci <- all_csci %>%
  select(-X) %>%
  group_by(comb_code, Type) %>%
  mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
           (max(PredictedProbability)-min(PredictedProbability))) %>%
  mutate(comb_code_type = paste(comb_code, "_", Type, sep="")) 

# joining labels table to all_csci - by hydroendpoints 
all_csci <- left_join(all_csci, labels, by =c("hydro.endpoints" = "Hydro_endpoint"))

head(all_csci)

# csci_metrics <-c("Q99", "DS_Tim", "DS_Dur_WS") #edited - add all mag metrics
SOC_metrics <-c("DS_Dur_WS", "SP_Dur", "SP_Tim", "DS_Mag_50", "FA_Mag", "Peak_10", 
                "Peak_2", "SP_Mag", "Wet_BFL_Mag_50") ## edited by Rachel based on convo with Kris 9/26

## subset to only important metrics - in this case, only mag metrics, there are 7 mag metrics 
all_csci_sub <- subset(all_csci, hydro.endpoints %in% SOC_metrics) 
unique(all_csci_sub$hydro.endpoints)

####
write.csv(all_csci_sub, "output_data/Manuscript/10_SOC_all_csci_sub.csv") 

# ASCI --------------------------------------------------------------------
######################### Combine Old and New Datasets together - ASCI
# Rachel edited 9/26
# old 
all_asci_April2021 <- read.csv("output_data/01_h_asci_neg_pos_logR_metrics_figures_April2021.csv") %>% 
  #filtering just for the metrics we need from the old data
  filter(hydro.endpoints %in% c("DS_Dur_WS", "SP_Dur", "SP_Tim"))

# new 
all_asci_July2023 <- read.csv("output_data/01_h_asci_neg_pos_logR_metrics_figures_July2023.csv") %>% 
  # filtering for the metrics we actually use in the new data - includes ASCI and CSCI just in case 
  filter(hydro.endpoints %in% c("d_peak_2", "d_sp_mag", "d_wet_bfl_mag_50", "d_ds_mag_50", "d_peak_10", "d_fa_mag"))

#### combine both datasets together 
all_asci <- all_asci_April2021 %>% 
  bind_rows(all_asci_July2023)

### read in curve data based on glms & +/- delta H for each FFM - asci 
all_asci <- all_asci %>% 
  mutate(PredictedProbability = case_when(hydro.endpoints == "d_peak_10" & hydro > 0 ~ NA_real_, ### edited 9/26 to remove + peak values 
                                          hydro.endpoints == "d_peak_5" & hydro > 0 ~ NA_real_,
                                          hydro.endpoints == "d_peak_2" & hydro > 0 ~ NA_real_,
                                          TRUE ~ as.numeric(PredictedProbability)))
head(all_asci)

## FIX NAMES TO MATCH LABELS AND LIMITS - Rachel 9/26
all_asci <- all_asci %>% 
  mutate(hydro.endpoints = case_when(hydro.endpoints == "d_ds_mag_50" ~ "DS_Mag_50",                 #renamed all mag variables here and below
                                     hydro.endpoints == "d_fa_mag" ~ "FA_Mag",
                                     hydro.endpoints == "d_peak_10" ~ "Peak_10",
                                     hydro.endpoints == "d_peak_2" ~ "Peak_2",
                                     hydro.endpoints == "d_peak_5" ~ "Peak_5",
                                     hydro.endpoints == "d_sp_mag" ~ "SP_Mag",
                                     hydro.endpoints == "d_wet_bfl_mag_10" ~ "Wet_BFL_Mag_10",
                                     hydro.endpoints == "d_wet_bfl_mag_50" ~ "Wet_BFL_Mag_50", 
                                     hydro.endpoints == "delta_q99" ~ "Q99", 
                                     T ~ as.character(hydro.endpoints))) %>% 
  # also fixing comb_code just in case (I left it alone in script 07 and 07a bc it didnt seem to be necessary)
  mutate(comb_code = paste(biol.endpoints, "_", hydro.endpoints, "_", thresholds, sep = ""))


## scale probability
# remove column x, scaling probability, adding column for combo code+threshold+type (- or +) - asci
all_asci <- all_asci %>%
  dplyr::select(-X) %>%
  group_by(comb_code, Type) %>%
  mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
           (max(PredictedProbability)-min(PredictedProbability))) %>%
  mutate(comb_code_type = paste(comb_code, "_", Type, sep=""))

# joining labels table to all_csci - by hydroendpoints 
all_asci <- left_join(all_asci,  labels, by =c("hydro.endpoints" = "Hydro_endpoint"))


## subset to only important metrics - 7 mag metrics 
all_asci_sub <- subset(all_asci, hydro.endpoints %in% SOC_metrics) ## edited, changed to SOC_metrics
unique(all_asci_sub$hydro.endpoints)

####
write.csv(all_asci_sub, "output_data/Manuscript/10_SOC_all_asci_sub.csv") 

# find roots of curve -----------------------------------------------------

## ASCI

## create df w 10 cols
df <- as.data.frame(matrix(ncol=10))
# name columns 
colnames(df) <- c("metric", "Threshold70", "Threshold90", "Threshold99", "n", "Type", "Biol", "Bio_endpoint", "Bio_threshold", "Hydro_endpoint")


## define metrics - ASCI
metrics <- unique(all_asci_sub$comb_code_type) 
metrics <- metrics[grep("0.86", metrics)]
metrics

## loop through metrics

# i = 3
for(i in 1: length(metrics)) {
  
  met <- metrics[i] # pulls out mag metrics
  
  hydroxx <- all_asci_sub %>%
    filter(comb_code_type == met) #filters for that metric
  
  ## get curves values at different probabilities
  thresh70 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.7) ### changed for new probability thresholds - used to be 0.5
  thresh70 <- ifelse(length(thresh70) == 0, NA, thresh70)
  
  thresh90 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.9) ### changed for new probability thresholds - used to be 0.25
  thresh90 <- ifelse(length(thresh90) == 0, NA, thresh90)
  
  thresh99 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.99) ### changed for new probability thresholds - used to be 0.75
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

# full df w all 7 mag metrics
df 

# write out df to csv  
write.csv(df, "output_data/Manuscript/10_SOC_ASCI_delta_thresholds_scaled.csv")


# CSCI --------------------------------------------------------------------
# same notes as above 
## create df w 10 columns 
df <- as.data.frame(matrix(ncol=10))
# name columns
colnames(df) <- c("metric", "Threshold70", "Threshold90", "Threshold99", "n", "Type", "Biol", "Bio_endpoint", "Bio_threshold", "Hydro_endpoint")


## define metrics - CSCI
metrics <- unique(all_csci_sub$comb_code_type) 
metrics <- metrics[grep("0.79", metrics)]
metrics

## loop through metrics
for(i in 1: length(metrics)) {
  
  met <- metrics[i] # pulls out mag metrics
  
  hydroxx <- all_csci_sub %>%
    filter(comb_code_type == met) #filters for that metric
  
  ## get curves values at different probabilities
  thresh70 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.7) ### changed for new probability thresholds - used to be 0.5
  thresh70 <- ifelse(length(thresh70) == 0, NA, thresh70)
  
  thresh90 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.9) ### changed for new probability thresholds - used to be 0.25
  thresh90 <- ifelse(length(thresh90) == 0, NA, thresh90)
  
  thresh99 <- RootLinearInterpolant(hydroxx$hydro, hydroxx$PredictedProbabilityScaled, 0.99) ### changed for new probability thresholds - used to be 0.75
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

# full df w all 7 mag metrics
df
#write out df to csv 
write.csv(df, "output_data/Manuscript/10_SOC_CSCI_delta_thresholds_scaled.csv") 

# Combine data ------------------------------------------------------------

asci <- read.csv("output_data/Manuscript/10_SOC_ASCI_delta_thresholds_scaled.csv")
csci <- read.csv("output_data/Manuscript/10_SOC_CSCI_delta_thresholds_scaled.csv")

delta <- rbind(asci, csci)

write.csv(delta, "output_data/Manuscript/10_SOC_ALL_delta_thresholds_scaled.csv") 
