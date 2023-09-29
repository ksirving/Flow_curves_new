## packages

library(tidyverse)
library(tidyr)
library(tidylog)

# getwd()
# out.dir <- "figures/"

## full names for labels
labels <- read.csv("Data/ffm_names.csv")
labels <- labels[1:24, ]
labels <- labels %>% rename(Hydro_endpoint = Flow.Metric.Code)
labels[25, 1] <- "Peak Flow Magnitude (Q99, cfs)"
labels[25, 2] <- "Q99"
labels[25, 3] <- "Peak Flow Magnitude"
labels


## work flow
# get probability of current delta H points in curve, try 1 subbasin across years to test
# get probability of water cons delta h points in curve
# compare them somehow, absolute values or median?


### delta h limits

limits <- read.csv("output_data/Manuscript/10_SOC_ALL_delta_thresholds_scaled.csv")
limits

limits <- left_join(limits, labels, by = "Hydro_endpoint")


# RB9 data -----------------------------------------------------------
# SOC delta data - sent from Katie 
delta <- read.csv("ignore/SOC_deltaH_supp_final_12012021.csv")
# head(delta)
# dim(delta)

## remove duplicates
delta <- delta %>% distinct()

## get delta h at each subbasin


# format table to go into probability models below 
delta_long <- delta %>%  ### data is already in long format but kept the variable name for the future of the script 
  dplyr::select(-c(watercon_value_final, watercon_value_note, deltaH_watercon_ref_final)) %>%
  rename(comments = scenario, DeltaH_Raw = deltah_cur_ref_final, hydro.endpoints = flow_metric) %>%
  filter(hydro.endpoints %in% c("DS_Dur_WS", "SP_Dur", "SP_Tim", "Peak_2", "SP_Mag", 
              "Wet_BFL_Mag_50", "DS_Mag_50", "Peak_10", "FA_Mag")) %>% ## Rachel edited 9/27
  group_by(site, hydro.endpoints) %>% 
  summarise(DeltaH = median(na.omit(DeltaH_Raw))) %>% ##### taking the median
  ungroup() %>%
  mutate(Scenario = "Current") %>% 
  mutate(FlowMetric = tolower(hydro.endpoints)) 


# pivot_longer(d_ds_mag_50:d_wet_bfl_mag_50, names_to = "FlowMetric", values_to = "DeltaH")

head(delta_long)
unique(delta_long$FlowMetric)
# unique(all_asci$Hydro_endpoint)

# delta_long <- delta_long %>%
#   mutate(Scenario = "Current") %>%
# mutate(hydro.endpoints = case_when(FlowMetric == "ds_mag_50" ~ "DS_Mag_50",
#                                    FlowMetric == "ds_mag_90" ~ "DS_Mag_90",
#                                    FlowMetric == "fa_mag" ~ "FA_Mag",
#                                    FlowMetric == "sp_mag" ~ "SP_Mag",
#                                    FlowMetric == "wet_bfl_mag_10" ~ "Wet_BFL_Mag_10",
#                                    FlowMetric == "wet_bfl_mag_50" ~ "Wet_BFL_Mag_50",
#                                    FlowMetric == "q99" ~ "Q99")) %>%
# filter(!FlowMetric %in% c("peak_10", "peak_2", "peak_5"))

head(delta_long)
sum(is.na(delta_long))

RB9_metrics <- unique(delta_long$hydro.endpoints)
RB9_metrics


# test <- delta_long %>%
#   filter(comid == "20350539",hydro.endpoints == "DS_Mag_50" )

## save
write.csv(delta_long, "ignore/10a_SOC_delta_h_long.csv")


# ASCI Curve data --------------------------------------------------------------------

######################### Combine Old and New Datasets together - ASCI
# Rachel edited 9/27
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
all_asci <- all_asci %>%
  dplyr::select(-X) %>%
  group_by(comb_code, Type) %>%
  mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
           (max(PredictedProbability)-min(PredictedProbability))) %>%
  mutate(comb_code_type = paste(comb_code, "_", Type, sep="")) %>%
  rename(Hydro_endpoint = hydro.endpoints)

all_asci <- left_join(all_asci, labels, by = "Hydro_endpoint")


## subset to only important metrics
# all_asci_sub <- subset(all_asci, Hydro_endpoint %in% metrics)


# CSCI Curve data--------------------------------------------------------------------



######################### Combine Old and New Datasets together - CSCI
# Rachel edited 9/27
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


## scale probability
all_csci <- all_csci %>%
  dplyr::select(-X) %>%
  group_by(comb_code, Type) %>%
  mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
           (max(PredictedProbability)-min(PredictedProbability))) %>%
  mutate(comb_code_type = paste(comb_code, "_", Type, sep="")) %>%
  rename(Hydro_endpoint = hydro.endpoints)

all_csci <- left_join(all_csci, labels, by ="Hydro_endpoint")


## subset to only important metrics
# all_csci_sub <- subset(all_csci, Hydro_endpoint %in% metrics)

# delta H probability -----------------------------------------------------

## estimate probability of good score with all subbasins and years

# ASCI Probability --------------------------------------------------------

# biol.endpoints<-c("H_ASCI", "D_ASCI")#
# names(all_asci)
# ## hydro
# hydro.endpoints<- unique(all_asci$Hydro_endpoint)
# hydro.endpoints
## thresholds
# 
# thresholds <- c(0.86) ## hybrid and diatom are the same
asci_metrics <- c("Peak_2", "SP_Mag", "Wet_BFL_Mag_50", "DS_Mag_50",
                  "DS_Dur_WS", "SP_Dur") ## edited by Rachel 9/27
## add Q99 w new data ^

#making bio_h_summary
biol.endpoints<-c("H_ASCI","D_ASCI")#

## hydro
hydro.endpoints<- unique(all_asci$Hydro_endpoint)

## thresholds

thresholds <- c(0.75, 0.86, 0.94) ## hybrid and diatom are the same

## make grid with all models 
bio_h_summary<-  expand.grid(biol.endpoints=biol.endpoints,hydro.endpoints=hydro.endpoints, thresholds = thresholds,  stringsAsFactors = F)
bio_h_summary

## reduce glms and summary df to only rows needed
## find index to remove
ind1 <- which(bio_h_summary$biol.endpoints == "H_ASCI" & bio_h_summary$thresholds == 0.86
              & bio_h_summary$hydro.endpoints %in% asci_metrics) # where I selected for the specific metrics

ind1 ## use only these 

## remove from grid
bio_h_summary <- bio_h_summary[ind1,]

bio_h_summary <- bio_h_summary %>%
  mutate(comb_code = paste0( hydro.endpoints, "_", thresholds))

## upload GLMs and subset
load(file = "models/01a_ASCI_negative_GLM_all_delta_mets_April2021.RData")
neg.glm <- neg.glm[ind1]
# neg.glm
load(file = "models/01a_ASCI_positive_GLM_all_delta_mets_April2021.RData")
pos.glm <- pos.glm[ind1]

head(delta_long) ## new data to predict on
dim(delta_long)
## define metrics
metrics <- unique(bio_h_summary$hydro.endpoints)

metrics

## making empty df and writing out column names 
cols <- c("site", "hydro.endpoints", "hydro", "Scenario", "FlowMetric", 
          "PredictedProbability", "PredictedProbabilityScaled", "index")
finalASCI_df <- data.frame(matrix(nrow=1, ncol = length(cols)))
colnames(finalASCI_df) <- cols

# i=4
## loop through metrics
for(i in 1: length(metrics)) {
  
  met <- metrics[i]
  
  hydroxx <- delta_long %>%
    filter(hydro.endpoints == met)
  
  unique(hydroxx$FlowMetric)
  
  ## get models for pos and neg
  posMod <- pos.glm[i][[1]]
  negMod <- neg.glm[i][[1]]
  
  ## rename to match models, separate scenarios and delta positive and negative
  new_data_current_pos <- hydroxx %>%
    # pivot_wider(names_from = Scenario, values_from = DeltaH)
    rename(hydro = DeltaH) %>%
    filter(Scenario == "Current",
           !hydro < 0)
  
  new_data_current_neg <- hydroxx %>%
    # pivot_wider(names_from = Scenario, values_from = DeltaH)
    rename(hydro = DeltaH) %>%
    filter(Scenario == "Current",
           !hydro >= 0)
  
  
  ###### if else to deal with no negatives
  if(length(new_data_current_neg$hydro) < 1) {
    
    posModCurrent <- predict(posMod, new_data_current_pos, type = "response")
    
    new_data_current_pos <-  new_data_current_pos %>%
      mutate(PredictedProbability = posModCurrent) %>%
      mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
               (max(PredictedProbability)-min(PredictedProbability)))
    
    CurrentProbsASCI <- new_data_current_pos
    
  } else if(length(new_data_current_pos$hydro) < 1) {
    
    negModCurrent <- predict(negMod, new_data_current_neg, type = "response")
    
    new_data_current_neg <-  new_data_current_neg %>%
      mutate(PredictedProbability = negModCurrent) %>%
      mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
               (max(PredictedProbability)-min(PredictedProbability)))
    
    CurrentProbsASCI <- new_data_current_neg
    
  } else {
  
  ## predict current conditions
  posModCurrent <- predict(posMod, new_data_current_pos, type = "response")
  negModCurrent <- predict(negMod, new_data_current_neg, type = "response")
  
  
  ## add to dfs and scale
  ## current
  new_data_current_pos <-  new_data_current_pos %>%
    mutate(PredictedProbability = posModCurrent) %>%
    mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
             (max(PredictedProbability)-min(PredictedProbability)))# %>%
  # mutate(CurrentDeltaNorm = (hydro-min(hydro))/
  #          (max(hydro)-min(hydro))) 
  
  
  new_data_current_neg <-  new_data_current_neg %>%
    mutate(PredictedProbability = negModCurrent) %>%
    mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
             (max(PredictedProbability)-min(PredictedProbability)))# %>%
  # mutate(CurrentDeltaNorm = (hydro-min(hydro))/
  #          (max(hydro)-min(hydro)))  
  
  ## combine current
  CurrentProbsASCI <-  bind_rows(new_data_current_pos, new_data_current_neg)
  }
  
  CurrentProbsASCI <- CurrentProbsASCI %>%
    transmute(site, hydro.endpoints, hydro, Scenario, FlowMetric, 
              PredictedProbability, PredictedProbabilityScaled, index = "ASCI")
  
  finalASCI_df <- finalASCI_df  %>%
    bind_rows(CurrentProbsASCI)
  
}

finalASCI_df <- finalASCI_df[-1,]

## taking away + peak data - WILL HAVE TO EDIT METRIC NAME IF WE CHANGE METRICS
## Rachel edited 9/28
finalASCI_df <- finalASCI_df %>%
  mutate(PredictedProbability = case_when(FlowMetric == "peak_2" & hydro > 0 ~ NA_real_, ### edited 9/12 to remove + peak values
                                          TRUE ~ as.numeric(PredictedProbability))) %>%
  mutate(PredictedProbabilityScaled = case_when(FlowMetric == "peak_2" & hydro > 0 ~ NA_real_, ### edited 9/12 to remove + peak values
                                                TRUE ~ as.numeric(PredictedProbabilityScaled)))

## write out CSV
write.csv(finalASCI_df, file = "output_data/Manuscript/10a_SOC_ASCI_prob.csv")

# CSCI Probability --------------------------------------------------------


## hydro
hydro.endpoints<- unique(all_csci$Hydro_endpoint)
hydro.endpoints
## thresholds

thresholds <- c(0.63, 0.79, 0.92) 
biol.endpoints<-c("CSCI","OoverE","MMI")

csci_metrics <- c("DS_Mag_50", "Peak_10", "FA_Mag", "Wet_BFL_Mag_50", 
                  "DS_Dur_WS", "SP_Tim") #edited by Rachel 9/27

## make grid with all models 
bio_h_summary<-  expand.grid(biol.endpoints=biol.endpoints,hydro.endpoints=hydro.endpoints, thresholds = thresholds,  stringsAsFactors = F)
bio_h_summary

## reduce glms and summary df to only rows needed
## find index to remove
ind1 <- which(bio_h_summary$biol.endpoints == "CSCI" & bio_h_summary$thresholds == 0.79
              & bio_h_summary$hydro.endpoints %in% csci_metrics) 

ind1 ## use only these 

## remove from grid
bio_h_summary <- bio_h_summary[ind1,]

bio_h_summary <- bio_h_summary %>%
  mutate(comb_code = paste0(hydro.endpoints, "_", thresholds))

## upload GLMs and subset
load(file = "models/01_CSCI_negative_GLM_all_delta_mets_April2021.RData")
neg.glm <- neg.glm[ind1]
# neg.glm 
load(file = "models/01_CSCI_positive_GLM_all_delta_mets_April2021.RData")
pos.glm <- pos.glm[ind1]

head(delta_long) ## new data to predict on

length(pos.glm)
## define metrics
metrics <- unique(bio_h_summary$hydro.endpoints)

metrics


## making empty df and writing out column names 
cols <- c("site", "hydro.endpoints", "hydro", "Scenario", "FlowMetric", 
          "PredictedProbability", "PredictedProbabilityScaled", "index")
finalCSCI_df <- data.frame(matrix(nrow=1, ncol = length(cols)))
colnames(finalCSCI_df) <- cols

# i=2
## loop through metrics
for(i in 1: length(metrics)) {
  
  met <- metrics[i]
  
  hydroxx <- delta_long %>%
    filter(hydro.endpoints == met)
  
  unique(hydroxx$FlowMetric)
  
  ## get models for pos and neg
  posMod <- pos.glm[i][[1]]
  negMod <- neg.glm[i][[1]]
  
  ## rename to match models, separate scenarios and delta positive and negative
  new_data_current_pos <- hydroxx %>%
    # pivot_wider(names_from = Scenario, values_from = DeltaH)
    rename(hydro =  DeltaH) %>%
    filter(Scenario == "Current",
           !hydro < 0)
  
  new_data_current_neg <- hydroxx %>%
    # pivot_wider(names_from = Scenario, values_from = DeltaH)
    rename(hydro =  DeltaH) %>%
    filter(Scenario == "Current",
           !hydro >= 0)
  
  ###### if else to deal with no negatives
  if(length(new_data_current_neg$hydro) < 1) {
    
    posModCurrent <- predict(posMod, new_data_current_pos, type = "response")
    
    new_data_current_pos <-  new_data_current_pos %>%
      mutate(PredictedProbability = posModCurrent) %>%
      mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
               (max(PredictedProbability)-min(PredictedProbability)))
    
    CurrentProbsCSCI <- new_data_current_pos
    
  } else if(length(new_data_current_pos$hydro) < 1) {
    
    negModCurrent <- predict(negMod, new_data_current_neg, type = "response")
    
    new_data_current_neg <-  new_data_current_neg %>%
      mutate(PredictedProbability = negModCurrent) %>%
      mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
               (max(PredictedProbability)-min(PredictedProbability)))
    
    CurrentProbsCSCI <- new_data_current_neg
    
  } else {
    
    ## predict current conditions
    posModCurrent <- predict(posMod, new_data_current_pos, type = "response")
    negModCurrent <- predict(negMod, new_data_current_neg, type = "response")
    
    
    ## add to dfs and scale
    ## current
    new_data_current_pos <-  new_data_current_pos %>%
      mutate(PredictedProbability = posModCurrent) %>%
      mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
               (max(PredictedProbability)-min(PredictedProbability)))# %>%
    # mutate(CurrentDeltaNorm = (hydro-min(hydro))/
    #          (max(hydro)-min(hydro))) 
    
    
    new_data_current_neg <-  new_data_current_neg %>%
      mutate(PredictedProbability = negModCurrent) %>%
      mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
               (max(PredictedProbability)-min(PredictedProbability)))# %>%
    # mutate(CurrentDeltaNorm = (hydro-min(hydro))/
    #          (max(hydro)-min(hydro)))  
    
    ## combine current
    CurrentProbsCSCI <-  bind_rows(new_data_current_pos, new_data_current_neg)
  }
  ## combine current
  
  CurrentProbsCSCI <- CurrentProbsCSCI %>%
    transmute(site, hydro.endpoints, hydro, Scenario, FlowMetric, 
              PredictedProbability, PredictedProbabilityScaled, index = "CSCI")

  #final df 
  
  finalCSCI_df <- finalCSCI_df %>%
    bind_rows(CurrentProbsCSCI)
  
  
}

finalCSCI_df <- finalCSCI_df[-1,]

## taking away + peak data - WILL HAVE TO EDIT METRIC NAME IF WE CHANGE METRICS
## Rachel edited 9/28
finalCSCI_df <- finalCSCI_df %>%
  mutate(PredictedProbability = case_when(FlowMetric == "peak_10" & hydro > 0 ~ NA_real_, ### edited 9/12 to remove + peak values
                                          TRUE ~ as.numeric(PredictedProbability))) %>%
  mutate(PredictedProbabilityScaled = case_when(FlowMetric == "peak_10" & hydro > 0 ~ NA_real_, ### edited 9/12 to remove + peak values
                                                TRUE ~ as.numeric(PredictedProbabilityScaled)))

#### write to CSV
write.csv(finalCSCI_df, file = "output_data/Manuscript/10a_SOC_CSCI_prob.csv")

#################################################################
# Combine CSCi and ASCI into one df 
ASCI <- read.csv("output_data/Manuscript/10a_SOC_ASCI_prob.csv")
CSCI <- read.csv("output_data/Manuscript/10a_SOC_CSCI_prob.csv")

comb <- ASCI %>% 
  bind_rows(CSCI) %>% 
  dplyr::select(-c(X))

write.csv(comb, file = "output_data/10a_SOC_predicted_probability_ASCIandCSCI.csv", row.names = FALSE)

