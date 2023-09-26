### current conditions assessment

## use flow-ecology curves

## packages
library(tidyverse)
library(tidyr)
library(tidylog)

getwd()
out.dir <- "figures/"

## full names for labels - flow metric name and corresponding hydrological endpoint & flow component label
labels <- read.csv("input_data/ffm_names.csv")
labels <- labels[1:24, ]
labels <- labels %>% rename(Hydro_endpoint = Flow.Metric.Code)
labels[25, 1] <- "Peak Flow Magnitude (Q99, cfs)"
labels[25, 2] <- "Q99"
labels[25, 3] <- "Peak Flow Magnitude"
labels


## work flow:
## check relationship with RB9 metrics
## get limits
## apply alteration rules on RB9 data


### delta h limits
# the data made in last part of 01_format_data (the combined df of all mag metrics for csci and asci)
limits <- read.csv("output_data/Manuscript/07_ALL_delta_thresholds_scaled_updated.csv") # Rachel edited 9/6
limits
head(limits)

## clean up df, remove unnecessary columns, make longer
limits <- limits %>%
  dplyr::select(-X.1, -X, -n) %>%
  mutate(metric = paste0(Biol, "_", Hydro_endpoint, "_", Bio_threshold)) %>%  # combine these 3 variables into one and put in metric column  
  pivot_longer(Threshold70:Threshold99, names_to = "Threshold") %>% # edited from old threshold values  
  rename(DeltaH = value) # rename value to delta H (diff b/w reference flow and current flow)


## make wider with type - pos/neg
limits <- limits %>%
  pivot_wider(names_from = Type, values_from = DeltaH)    ## FIX 

# joining labels table to limits - by hydroendpoints 
limits <- left_join(limits, labels, by = "Hydro_endpoint")


# ASCI Curve data --------------------------------------------------------------------

### read in curve data based on glms & +/- delta H for each FFM - csci - same as data from 00_format_data
all_asci <- read.csv("output_data/01_h_asci_neg_pos_logR_metrics_figures_July2023.csv") %>% # rachel added new data 9/6
  mutate(PredictedProbability = case_when(hydro.endpoints == "d_peak_10" & hydro > 0 ~ NA_real_, ### edited 9/12 to remove + peak values 
                                          hydro.endpoints == "d_peak_5" & hydro > 0 ~ NA_real_,
                                          hydro.endpoints == "d_peak_2" & hydro > 0 ~ NA_real_,
                                          TRUE ~ as.numeric(PredictedProbability)))
head(all_asci)

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

## remove column x, scaling probability, adding column for combo code+threshold+type (- or +) - asci
all_asci <- all_asci %>%
  dplyr::select(-X) %>%
  group_by(comb_code, Type) %>%
  mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
           (max(PredictedProbability)-min(PredictedProbability))) %>%
  mutate(comb_code_type = paste(comb_code, "_", Type, sep="")) %>%
  rename(Hydro_endpoint = hydro.endpoints)

# joining labels table to limits - by hydroendpoints
all_asci <- left_join(all_asci, labels, by ="Hydro_endpoint")


## subset to only important metrics
# all_asci_sub <- subset(all_asci, Hydro_endpoint %in% metrics)


# CSCI Curve data--------------------------------------------------------------------

### read in curve data based on glms & +/- delta H for each FFM - csci - same as data from 00_format_data - csci
all_csci <- read.csv("output_data/01_csci_neg_pos_logR_metrics_figures_July2023.csv") %>% # rachel added new data 9/6
  mutate(PredictedProbability = case_when(hydro.endpoints == "d_peak_10" & hydro > 0 ~ NA_real_, ### edited 9/12 to remove + peak values 
                                          hydro.endpoints == "d_peak_5" & hydro > 0 ~ NA_real_,
                                          hydro.endpoints == "d_peak_2" & hydro > 0 ~ NA_real_,
                                          TRUE ~ as.numeric(PredictedProbability)))
head(all_csci)

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

## remove column x, scaling probability, adding column for combo code+threshold+type (- or +) - asci
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



# RB9 delta ---------------------------------------------------------------

# RB9 Data w comid, wy, and FFM - Rachel renamed to RB9 - NEW data goes here 
delta.RB9 <- read.csv("ignore/2022-11-29_predicted_abs_FFMq99_deltaFFMq99_SD_COMIDS_medianDelta_test2q99_test12FFM_allgages.csv")
head(delta.RB9)
dim(delta.RB9)

## remove duplicates
delta.RB9 <- delta.RB9 %>% distinct()

# # # No need to make longer it is already in long format 
# # make table longer
# delta_long <- delta.RB9 %>%
#   # dplyr::select(comid, region, year, flow_metric, deltah_cur_ref_final, deltaH_watercon_ref_final) %>%  
#   pivot_longer(d_ds_dur_ws:d_wet_tim, names_to = "FlowMetric", values_to = "DeltaH")
# 
# head(delta_long)


# pulling out needed metrics 
delta_long <- delta.RB9  %>%    ### this line edited but left variable name the same for rest of code 
  rename(DeltaH = delta_FFM_median_cfs, FlowMetric = metric) %>%
  filter(FlowMetric %in% c("ds_mag_50", "fa_mag", "peak_10", "peak_2", "peak_5", "sp_mag", "wet_bfl_mag_10", "wet_bfl_mag_50", "q99")) %>%  #edited, put all mag variables here 
  mutate(hydro.endpoint = case_when(FlowMetric == "ds_mag_50" ~ "DS_Mag_50",                 #renamed all mag variables here and below 
                                    FlowMetric == "fa_mag" ~ "FA_Mag",
                                    FlowMetric == "peak_10" ~ "Peak_10",
                                    FlowMetric == "peak_2" ~ "Peak_2",
                                    FlowMetric == "peak_5" ~ "Peak_5",
                                    FlowMetric == "sp_mag" ~ "SP_Mag",
                                    FlowMetric == "wet_bfl_mag_10" ~ "Wet_BFL_Mag_10",
                                    FlowMetric == "wet_bfl_mag_50" ~ "Wet_BFL_Mag_50", 
                                    FlowMetric == "q99" ~ "Q99"))  ####### Q99 variable - added from new data edited by Rachel 11/30

unique(delta_long$FlowMetric) ## 9 mag metrics
names(delta_long)

## get median delta H per comid & metric 
delta_med <- delta_long %>%
  group_by(comid, FlowMetric, hydro.endpoint) %>%
  summarise(MedDelta = median(DeltaH)) #edited by rachel from "DeltaH" to delta_FFM_median_cfs

head(delta_med)

write.csv(delta_med, "output_data/Manuscript/07a_delta_med.csv") #edited by rachel 

# Join limits with delta H RB9 --------------------------------------------

limits <- limits  %>%
  # dplyr::select(-X.1, -X) %>%
  rename(hydro.endpoint = Hydro_endpoint)

write.csv(limits, "output_data/Manuscript/07a_limits.csv")

## join limits with delta data
delta_df <- left_join(delta_long, limits, by = "hydro.endpoint")
head(delta_df)

## define alteration per subbasin, per year - within limits
delta_dfx <- delta_df %>%
  group_by(comid, comid_wy, wayr,  hydro.endpoint, Bio_endpoint, Bio_threshold, Threshold) %>%
  mutate(Alteration = ifelse(DeltaH <= Positive & DeltaH >= Negative, "Unaltered", "Altered"))  #edited by rachel from "DeltaH" to delta_FFM_median_cfs

write.csv(delta_dfx, "ignore/07a_alteration_by_year_comid_all_sites.csv") 

head(delta_dfx)

### removed threshold choosing bc we have decided thresholds