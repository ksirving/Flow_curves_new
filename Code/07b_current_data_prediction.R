################# Current Data Probability Predictions 
## relative change in probability of achieving a good bio score

## packages

library(tidyverse)
library(tidyr)
library(tidylog)
#for mapping
library(spDataLarge)
library(readxl)
library(sf)
library(ggsn)
library(ggmap)
library(ggspatial)
library(spData)      
library(geosphere)
library(rgeos)
library(sf)
library(raster)
library(maptools)
library(rgdal)

getwd()
# out.dir <- "figures/"

## full names for labels
labels <- read.csv("input_data/ffm_names.csv")
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

limits <- read.csv("output_data/Manuscript/07_ALL_delta_thresholds_scaled_updated.csv")
limits

limits <- left_join(limits, labels, by = "Hydro_endpoint")


# RB9 data -----------------------------------------------------------

delta <- read.csv("ignore/2022-11-29_predicted_abs_FFMq99_deltaFFMq99_SD_COMIDS_medianDelta_test2q99_test12FFM_allgages.csv")
head(delta)
dim(delta)

## remove duplicates
delta <- delta %>% distinct()

# delta_long <- delta %>%
#   # select(comid region, year, flow_metric, deltah_cur_ref_final, deltaH_watercon_ref_final) %>%
#   pivot_longer(d_ds_mag_50:d_wet_bfl_mag_50, names_to = "FlowMetric", values_to = "DeltaH")

delta_long <- delta %>%  ### data is already in long format but kept the variable name for the future of the script 
  rename(DeltaH = delta_FFM_median_cfs, FlowMetric = metric)
  # pivot_longer(d_ds_mag_50:d_wet_bfl_mag_50, names_to = "FlowMetric", values_to = "DeltaH")

head(delta_long)





## change metric names to match curve data
## remove peak metrics
unique(delta_long$FlowMetric)
# unique(all_asci$Hydro_endpoint)

delta_long <- delta_long %>%
  mutate(Scenario = "Current") %>%
  mutate(hydro.endpoints = case_when(FlowMetric == "ds_mag_50" ~ "DS_Mag_50",
                                     FlowMetric == "ds_mag_90" ~ "DS_Mag_90",
                                     FlowMetric == "fa_mag" ~ "FA_Mag",
                                     FlowMetric == "sp_mag" ~ "SP_Mag",
                                     FlowMetric == "wet_bfl_mag_10" ~ "Wet_BFL_Mag_10",
                                     FlowMetric == "wet_bfl_mag_50" ~ "Wet_BFL_Mag_50",
                                     FlowMetric == "q99" ~ "Q99",
                                     FlowMetric == "peak_2" ~ "Peak_2",
                                     FlowMetric == "peak_5" ~ "Peak_5",
                                     FlowMetric == "peak_10" ~ "Peak_10")) %>% 
  filter(hydro.endpoints != "DS_Mag_90")


head(delta_long)
sum(is.na(delta_long))

RB9_metrics <- unique(delta_long$hydro.endpoints)
RB9_metrics


# test <- delta_long %>%
#   filter(comid == "20350539",hydro.endpoints == "DS_Mag_50" )

## save
write.csv(delta_long, "ignore/07b_delta_h_long.csv")


# ASCI Curve data --------------------------------------------------------------------


all_asci <- read.csv("output_data/01_h_asci_neg_pos_logR_metrics_figures_July2023.csv") %>% 
  mutate(PredictedProbability = case_when(hydro.endpoints == "d_peak_10" & hydro > 0 ~ NA_real_, ### edited 9/12 to remove + peak values 
                                          hydro.endpoints == "d_peak_5" & hydro > 0 ~ NA_real_,
                                          hydro.endpoints == "d_peak_2" & hydro > 0 ~ NA_real_,
                                          TRUE ~ as.numeric(PredictedProbability)))
head(all_asci)

## FIX NAMES TO MATCH LABELS AND LIMITS - Rachel 9/7
all_asci <- all_asci %>% 
  mutate(hydro.endpoints = case_when(hydro.endpoints == "d_ds_mag_50" ~ "DS_Mag_50",                 #renamed all mag variables here and below
                                     hydro.endpoints == "d_fa_mag" ~ "FA_Mag",
                                     hydro.endpoints == "d_peak_10" ~ "Peak_10",
                                     hydro.endpoints == "d_peak_2" ~ "Peak_2",
                                     hydro.endpoints == "d_peak_5" ~ "Peak_5",
                                     hydro.endpoints == "d_sp_mag" ~ "SP_Mag",
                                     hydro.endpoints == "d_wet_bfl_mag_10" ~ "Wet_BFL_Mag_10",
                                     hydro.endpoints == "d_wet_bfl_mag_50" ~ "Wet_BFL_Mag_50", 
                                     hydro.endpoints == "delta_q99" ~ "Q99")) %>% 
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


all_csci <- read.csv("output_data/01_csci_neg_pos_logR_metrics_figures_July2023.csv") %>% 
  mutate(PredictedProbability = case_when(hydro.endpoints == "d_peak_10" & hydro > 0 ~ NA_real_, ### edited 9/12 to remove + peak values 
                                          hydro.endpoints == "d_peak_5" & hydro > 0 ~ NA_real_,
                                          hydro.endpoints == "d_peak_2" & hydro > 0 ~ NA_real_,
                                          TRUE ~ as.numeric(PredictedProbability)))
head(all_csci)

## FIX NAMES TO MATCH LABELS AND LIMITS - Rachel 9/7
all_csci <- all_csci %>% 
  mutate(hydro.endpoints = case_when(hydro.endpoints == "d_ds_mag_50" ~ "DS_Mag_50",                 #renamed all mag variables here and below
                                     hydro.endpoints == "d_fa_mag" ~ "FA_Mag",
                                     hydro.endpoints == "d_peak_10" ~ "Peak_10",
                                     hydro.endpoints == "d_peak_2" ~ "Peak_2",
                                     hydro.endpoints == "d_peak_5" ~ "Peak_5",
                                     hydro.endpoints == "d_sp_mag" ~ "SP_Mag",
                                     hydro.endpoints == "d_wet_bfl_mag_10" ~ "Wet_BFL_Mag_10",
                                     hydro.endpoints == "d_wet_bfl_mag_50" ~ "Wet_BFL_Mag_50", 
                                     hydro.endpoints == "delta_q99" ~ "Q99")) %>% 
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

biol.endpoints<-c("H_ASCI", "D_ASCI")#
# names(all_asci)
## hydro
hydro.endpoints<- unique(all_asci$Hydro_endpoint)
hydro.endpoints 
## thresholds

# thresholds <- c(0.86) ## hybrid and diatom are the same
asci_metrics <- c("Peak_2", "SP_Mag", "Wet_BFL_Mag_50", "DS_Mag_50") ## asci metrics based on those chosen in the previous scripts edited by Rachel 9/7
## added Q99 w new data ^

# making bio_h_summary
## bio 

biol.endpoints<-c("H_ASCI","D_ASCI")#

## hydro
# hydro.endpoints<- colnames(asci)[6:21]

## thresholds

thresholds <- c(0.75, 0.86, 0.94) ## hybrid and diatom are the same

## make grid with all models 
# bio_h_summary <-  expand.grid(biol.endpoints=biol.endpoints,hydro.endpoints=hydro.endpoints, thresholds = thresholds,  stringsAsFactors = F)
bio_h_summary <- read.csv("output_data/01_asci_hydro_endpoints_order_July2023.csv") %>% 
  mutate(hydro.endpoints = case_when(hydro.endpoints == "d_ds_mag_50" ~ "DS_Mag_50",                 #renamed all mag variables here and below
                                     hydro.endpoints == "d_fa_mag" ~ "FA_Mag",
                                     hydro.endpoints == "d_peak_10" ~ "Peak_10",
                                     hydro.endpoints == "d_peak_2" ~ "Peak_2",
                                     hydro.endpoints == "d_peak_5" ~ "Peak_5",
                                     hydro.endpoints == "d_sp_mag" ~ "SP_Mag",
                                     hydro.endpoints == "d_wet_bfl_mag_10" ~ "Wet_BFL_Mag_10",
                                     hydro.endpoints == "d_wet_bfl_mag_50" ~ "Wet_BFL_Mag_50", 
                                     hydro.endpoints == "delta_q99" ~ "Q99"))
bio_h_summary

## reduce glms and summary df to only rows needed
## find index to remove
# use "output_data/01_csci_hydro_endpoints_order_July2023.csv" to make table for mag metrics
ind1 <- which(bio_h_summary$biol.endpoints == "H_ASCI" & bio_h_summary$thresholds == 0.86
              & bio_h_summary$hydro.endpoints %in% asci_metrics)

ind1 ## use only these 

## remove from grid
bio_h_summary <- bio_h_summary[ind1,]

bio_h_summary <- bio_h_summary %>%
  mutate(comb_code = paste0( hydro.endpoints, "_", thresholds))

## upload GLMs and subset
load(file = "models/01a_ASCI_negative_GLM_all_delta_mets_July2023.RData")
neg.glm <- neg.glm[ind1]
# neg.glm
load(file = "models/01a_ASCI_positive_GLM_all_delta_mets_July2023.RData")
pos.glm <- pos.glm[ind1]

head(delta_long) ## new data to predict on
dim(delta_long)
## define metrics
metrics <- unique(bio_h_summary$hydro.endpoints)

metrics

## making empty df and writing out column names 
cols <- c("comid", "comid_wy", "wayr", "WYT", "abs_FFM_median_cfs", "hydro", "FlowMetric",
          "Scenario", "hydro.endpoints", "PredictedProbability", "PredictedProbabilityScaled", "index")
finalASCI_df_RB9 <- data.frame(matrix(nrow=1, ncol = length(cols)))
colnames(finalASCI_df_RB9) <- cols
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
    rename(hydro = DeltaH) %>%
    filter(Scenario == "Current",
           !hydro < 0)
  
  new_data_current_neg <- hydroxx %>%
    # pivot_wider(names_from = Scenario, values_from = DeltaH)
    rename(hydro = DeltaH) %>%
    filter(Scenario == "Current",
           !hydro >= 0)
  
  
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
  
  new_data_current_pos
  
  new_data_current_neg <-  new_data_current_neg %>%
    mutate(PredictedProbability = negModCurrent) %>%
    mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
             (max(PredictedProbability)-min(PredictedProbability)))# %>%
  # mutate(CurrentDeltaNorm = (hydro-min(hydro))/
  #          (max(hydro)-min(hydro)))  
  
  ## combine current
  CurrentProbsASCI <-  bind_rows(new_data_current_pos, new_data_current_neg)
  CurrentProbsASCI <- CurrentProbsASCI %>%
    transmute(comid, comid_wy, wayr, WYT, abs_FFM_median_cfs, hydro, FlowMetric, Scenario, hydro.endpoints, PredictedProbability, PredictedProbabilityScaled, 
              index = "ASCI")
  
  # combine
  finalASCI_df_RB9 <- finalASCI_df_RB9 %>%
    bind_rows(CurrentProbsASCI)
  
}

finalASCI_df_RB9 <- finalASCI_df_RB9[-1,]

## taking away + peak data - WILL HAVE TO EDIT METRIC NAME IF WE CHANGE METRICS
finalASCI_df_RB9 <- finalASCI_df_RB9 %>%
  mutate(PredictedProbability = case_when(FlowMetric == "peak_2" & hydro > 0 ~ NA_real_, ### edited 9/12 to remove + peak values
                                          TRUE ~ as.numeric(PredictedProbability))) %>%
  mutate(PredictedProbabilityScaled = case_when(FlowMetric == "peak_2" & hydro > 0 ~ NA_real_, ### edited 9/12 to remove + peak values
                                                TRUE ~ as.numeric(PredictedProbabilityScaled)))
#   

#### write to CSV
write.csv(finalASCI_df_RB9, file = "output_data/Manuscript/07b_ASCI_prob.csv")
# CSCI Probability --------------------------------------------------------


## hydro
hydro.endpoints<- unique(all_csci$Hydro_endpoint)
hydro.endpoints
## thresholds

thresholds <- c(0.63, 0.79, 0.92) 
biol.endpoints<-c("CSCI","OoverE","MMI")

csci_metrics <- c("DS_Mag_50", "Peak_10", "FA_Mag", "Wet_BFL_Mag_50") #edited by Rachel 9/7 - chosen for Katies portion of the deliverable

## make grid with all models 
# use "output_data/01_csci_hydro_endpoints_order_July2023.csv" to make table for mag metrics
# bio_h_summary <-  expand.grid(biol.endpoints=biol.endpoints,hydro.endpoints=hydro.endpoints, thresholds = thresholds,  stringsAsFactors = F)
bio_h_summary <- read.csv("output_data/01_csci_hydro_endpoints_order_July2023.csv") %>% 
  mutate(hydro.endpoints = case_when(hydro.endpoints == "d_ds_mag_50" ~ "DS_Mag_50",                 #renamed all mag variables here and below
                                     hydro.endpoints == "d_fa_mag" ~ "FA_Mag",
                                     hydro.endpoints == "d_peak_10" ~ "Peak_10",
                                     hydro.endpoints == "d_peak_2" ~ "Peak_2",
                                     hydro.endpoints == "d_peak_5" ~ "Peak_5",
                                     hydro.endpoints == "d_sp_mag" ~ "SP_Mag",
                                     hydro.endpoints == "d_wet_bfl_mag_10" ~ "Wet_BFL_Mag_10",
                                     hydro.endpoints == "d_wet_bfl_mag_50" ~ "Wet_BFL_Mag_50", 
                                     hydro.endpoints == "delta_q99" ~ "Q99"))
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
load(file = "models/01_CSCI_negative_GLM_all_delta_mets_July2023.RData")
neg.glm <- neg.glm[ind1]
# neg.glm 
load(file = "models/01_CSCI_positive_GLM_all_delta_mets_July2023.RData")
pos.glm <- pos.glm[ind1]

head(delta_long) ## new data to predict on

length(pos.glm)
## define metrics
metrics <- unique(bio_h_summary$hydro.endpoints)

metrics

## making empty df and writing out column names 
cols <- c("comid", "comid_wy", "wayr", "WYT", "abs_FFM_median_cfs", "hydro", "FlowMetric",
          "Scenario", "hydro.endpoints", "PredictedProbability", "PredictedProbabilityScaled", "index")
finalCSCI_df_RB9 <- data.frame(matrix(nrow=1, ncol = length(cols)))
colnames(finalCSCI_df_RB9) <- cols

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
    rename(hydro =  DeltaH) %>%
    filter(Scenario == "Current",
           !hydro < 0)
  
  new_data_current_neg <- hydroxx %>%
    # pivot_wider(names_from = Scenario, values_from = DeltaH)
    rename(hydro =  DeltaH) %>%
    filter(Scenario == "Current",
           !hydro >= 0)

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
  
  new_data_current_pos
  
  new_data_current_neg <-  new_data_current_neg %>%
    mutate(PredictedProbability = negModCurrent) %>%
    mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
             (max(PredictedProbability)-min(PredictedProbability)))# %>%
  # mutate(CurrentDeltaNorm = (hydro-min(hydro))/
  #          (max(hydro)-min(hydro)))  
  
  ## combine current
  CurrentProbsCSCI <-  bind_rows(new_data_current_pos, new_data_current_neg) #### plot predictedprobscaled
  CurrentProbsCSCI <- CurrentProbsCSCI %>%
    transmute(comid, comid_wy, wayr, WYT, abs_FFM_median_cfs, hydro, FlowMetric, Scenario, hydro.endpoints, PredictedProbability, PredictedProbabilityScaled, 
              index = "CSCI")
  
  # combine
  finalCSCI_df_RB9 <- finalCSCI_df_RB9 %>%
    bind_rows(CurrentProbsCSCI)
  
}

finalCSCI_df_RB9 <- finalCSCI_df_RB9[-1,]

## taking away + peak data - WILL HAVE TO EDIT METRIC NAME IF WE CHANGE METRICS
finalCSCI_df_RB9 <- finalCSCI_df_RB9 %>%
  mutate(PredictedProbability = case_when(FlowMetric == "peak_10" & hydro > 0 ~ NA_real_, ### edited 9/12 to remove + peak values
                                          TRUE ~ as.numeric(PredictedProbability))) %>%
  mutate(PredictedProbabilityScaled = case_when(FlowMetric == "peak_10" & hydro > 0 ~ NA_real_, ### edited 9/12 to remove + peak values
                                                TRUE ~ as.numeric(PredictedProbabilityScaled)))


#### write to CSV
write.csv(finalCSCI_df_RB9, file = "output_data/Manuscript/07b_CSCI_prob.csv")

##########################################################################################################################
##########################################################################################################################
# Cleaning data for maps --------------------------------------------------------
# Combine CSCi and ASCI into one df 
ASCI <- read.csv("output_data/Manuscript/07b_ASCI_prob.csv")
CSCI <- read.csv("output_data/Manuscript/07b_CSCI_prob.csv")

comb <- ASCI %>% 
  bind_rows(CSCI) %>%
  rename(COMID = comid) %>%
  dplyr::select(-c(X))

write.csv(comb, file = "output_data/Manuscript/07b_predicted_probability.csv", row.names = FALSE)

###########################################################################
# Maps --------------------------------------------------------------------
###########################################################################
# install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
## read in csv 
combined_metrics_index <- read.csv("output_data/Manuscript/07b_predicted_probability.csv")

# combined_metrics_index <- combined_metrics_index %>% 
#   dplyr::select(-c(X.1, X))
  
## projection
prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
### upload RB9 nhds

calinhd <- readOGR('/Users/racheld/SCCWRP/SD Hydro Vulnerability Assessment - General/Data/SpatialData/NHDplus_RB9.shp') %>%
  spTransform(prj) %>%
  st_as_sf %>%
  st_simplify(dTolerance = 0.5, preserveTopology = T)
# unique(calinhd$COMID)

## map

# fortified calinhd, joind with delta
# merge data set with comids to get spatial data
SynthNHD <- calinhd %>%
  filter(COMID %in% unique(combined_metrics_index$COMID)) %>%
  dplyr::select(COMID) %>% ## 46
  as('Spatial') %>% 
  st_as_sf(coords = c("long", "lat"), remove = FALSE)

# adding names of metrics for plots 
names <- limits %>%
  dplyr::select(Hydro_endpoint, Flow.Metric.Name) %>%
  rename(hydro.endpoints = Hydro_endpoint)

combined_metrics_index_name <- combined_metrics_index %>%
  left_join(names, by = "hydro.endpoints") %>%
  mutate(metric_index = paste0(index, "_", hydro.endpoints)) %>%
  distinct()

# subset to specific columns, rename columns
subset <- combined_metrics_index_name %>% 
  dplyr::select(COMID, hydro, hydro.endpoints, PredictedProbabilityScaled, index, metric_index, Flow.Metric.Name) %>% 
  # mutate(COMID = as.character(COMID)) %>% 
  data.frame() %>% 
  na.omit()

# update column names
# names(subset) <- c("COMID", "Biol","Alteration - Biology", "Threshold")

## Loop through CSCI and ASCI thresholds and metrics
met_ind <- unique(combined_metrics_index_name$metric_index)
met_ind

# names <- c("Magnitude of largest annual storm", "Dry-season median baseflow", "Magnitude of largest annual storm", "Wet-season low baseflow")
# rachel edited 9/7
names <- c("Dry-Season Baseflow Magnitude (cfs)", "Peak Flow Magnitude (2-year flood, cfs)", "Spring Recession Flow Magnitude (cfs)", "Wet-Season Median Magnitude (cfs)", 
           "Dry-Season Baseflow Magnitude (cfs)", "Fall Pulse Flow Magnitude (cfs)", "Peak Flow Magnitude (10-year flood, cfs)", "Wet-Season Median Magnitude (cfs)")

# z = "CSCI_Peak_10"
for(z in met_ind){
  #subset 
  subset.index <- subset[subset$metric_index == z,]
  subset.index
  
  iteration <- grep(z, met_ind)
  metric_z <- names[iteration]
    
    # merge with comids to get spatial data for map
    subset.join <- subset.index %>% 
      full_join(SynthNHD, by = c('COMID'))
    subset.join
    # set title and subtitle
    # title <- paste0(subset.join$index[z])
    title <- sapply(strsplit(z,"_"), `[`, 1)
    subtitle <- paste("Predicted Current Probability", metric_z, sep = "\n") 
    
    ## Plot
    # Set up base map 
    study <- ggplot(SynthNHD) + 
      # Rachel changed from color = lightgrey
      geom_sf(color = "lightgrey", fill= "white") +
      labs(title=title, subtitle = subtitle, x ="", y = "")  + 
      annotation_scale() +
      annotation_north_arrow(pad_y = unit(0.9, "cm"),  height = unit(.8, "cm"),
                             width = unit(.8, "cm")) +
      theme(panel.background = element_rect(fill = "white"),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_line(color = "white", size = 0.8),
            plot.title = element_text(size=20)) 
    #print map
    study
    
    # synthesis maps for bio index z
    
    #Rachel edited this to make an if else statement 9/13 to add the NA for peak metrics 
    #####################################################################################
    if(z %in% c("CSCI_Peak_10", "CSCI_Peak_5", "CSCI_Peak_2", "ASCI_Peak_10", "ASCI_Peak_5", "ASCI_Peak_2")) {
    
    syn.plot <- study + 
      geom_sf(data = subset.join, 
              aes(color= PredictedProbabilityScaled, 
                  geometry = geometry, 
                  linetype = !is.na(PredictedProbabilityScaled), 
                  linewidth = !is.na(PredictedProbabilityScaled))) +
      ## Rachel added "linetype = is.na(PredictedProbabilityScaled)" 9/12
      scale_color_distiller(palette = "Spectral", direction = 1) +
      labs(color = "Predicted Probability", linetype = "") +
      # rachel edited 9/12
      scale_linetype_manual(values = c(1,1), labels = c("Indeterminate", "")) +
      scale_linewidth_manual(values = c(0.009, 0.65)) + #0.009
      guides(
        linetype = guide_legend(override.aes = list(linetype = c(1,NA), color = "lightgrey"), order = 2),
        color = guide_colorbar(order = 1), 
        linewidth = "none"
      ) +
      theme(legend.key = element_blank()) 
    # print
    # print(syn.plot)
    
    } else{
      # synthesis map for bio index z
      syn.plot <- study + geom_sf(data = subset.join, aes(color= PredictedProbabilityScaled, geometry = geometry)) +
        scale_color_distiller(palette = "Spectral", direction = 1) +
        labs(color = "Predicted Probability")
    }
    # scale_color_viridis_c(option = "A") 
    
    print(syn.plot)
    
    # write plot
    out.filename <- paste0("output_data/Manuscript/Figures/Maps/07b_", z, "_predicted_prob_Current_updated_10162023.jpg") 
    ggsave(syn.plot, file = out.filename, dpi=300, height=4, width=6)
    
  # }
}


############ summary of predicted probabilities ############
## ASCI ##############################################################################################


# Peak_2 ------------------------------------------------------------------
## greater than or = to 0.7 or 70% of good score ------ changed this from 50%
ascipeak2_stats <- combined_metrics_index %>%
  filter(index == "ASCI" & PredictedProbabilityScaled >= 0.70 & hydro.endpoints == "Peak_2") %>%
  distinct()

# after NA's introduced 
#1329 comids
1329/2116 *100 ## 62.80718 %

## bad q99 < 0.7
ascipeak2_stats_bad <- combined_metrics_index %>%
  filter(index == "ASCI" & PredictedProbabilityScaled < 0.70 & hydro.endpoints == "Peak_2") %>%
  distinct()

# after NA's introduced 
#326 comids
326/2116 *100 ## 15.40643 %

#######################


# SP_Mag ------------------------------------------------------------------
## greater than or = to 0.7 or 70% of good score ------ changed this from 50%
ascisp_mag_stats <- combined_metrics_index %>%
  filter(index == "ASCI" & PredictedProbabilityScaled >= 0.70 & hydro.endpoints == "SP_Mag") %>%
  distinct()
##  comids 
2074/2116 *100 ## 98.01512 %

## bad q99 < 0.7
ascisp_mag_stats_bad <- combined_metrics_index %>%
  filter(index == "ASCI" & PredictedProbabilityScaled < 0.70 & hydro.endpoints == "SP_Mag") %>%
  distinct()
##  comids 
42/2116 *100  ## 1.984877 %

#######################


# Wet_BFL_Mag_50 ----------------------------------------------------------
## greater than or = to 0.7 or 70% of good score ------ changed this from 50%
asci_wetbfl_50_stats <- combined_metrics_index %>%
  filter(index == "ASCI" & PredictedProbabilityScaled >= 0.70 & hydro.endpoints == "Wet_BFL_Mag_50") %>%
  distinct()
##  comids 
1918/2116 *100 ## 90.64272 %

## bad q99 < 0.7
asci_wetbfl_50_stats_bad <- combined_metrics_index %>%
  filter(index == "ASCI" & PredictedProbabilityScaled < 0.70 & hydro.endpoints == "Wet_BFL_Mag_50") %>%
  distinct()
##  comids 
198/2116 *100  ## 9.357278 %

#######################


# DS_Mag_50 ---------------------------------------------------------------
## ## greater than or = to 0.7 or 70% of good score ------ changed this from 50%
asci_dm_50_stats <- combined_metrics_index %>%
  filter(index == "ASCI" & PredictedProbabilityScaled >= 0.70 & hydro.endpoints == "DS_Mag_50") %>%
  distinct()
##  comids 
2012/2116 *100 ## 95.08507 %

## bad q99 < 0.7
asci_dm_50_stats_bad <- combined_metrics_index %>%
  filter(index == "ASCI" & PredictedProbabilityScaled < 0.70 & hydro.endpoints == "DS_Mag_50") %>%
  distinct()
##  comids 
104/2116 *100  ## 4.914934 %

#####################################################################################################
#####################################################################################################
#####################################################################################################
## CSCI

# DS_Mag_50 ---------------------------------------------------------------
## ## greater than or = to 0.7 or 70% of good score ------ changed this from 50%
csci_dm_50_stats <- combined_metrics_index %>%
  filter(index == "CSCI" & PredictedProbabilityScaled >= 0.70 & hydro.endpoints == "DS_Mag_50") %>%
  distinct()
## 1755 comids 
1755/2116 *100  ## 82.93951 %

## bad q99 < 0.5
csci_dm_50_stats_bad <- combined_metrics_index %>%
  filter(index == "CSCI" & PredictedProbabilityScaled < 0.70 & hydro.endpoints == "DS_Mag_50") %>%
  distinct()
## 361 comids 
361/2116 * 100 ## 17.06049 %

# Peak_10 ---------------------------------------------------------------
## ## greater than or = to 0.7 or 70% of good score ------ changed this from 50%
csci_p10_stats <- combined_metrics_index %>%
  filter(index == "CSCI" & PredictedProbabilityScaled >= 0.70 & hydro.endpoints == "Peak_10") %>%
  distinct()
# after NA's introduced 
## 881 comids 
881/2116 *100  ## 41.63516 %

## bad q99 < 0.5
csci_p10_stats_bad <- combined_metrics_index %>%
  filter(index == "CSCI" & PredictedProbabilityScaled < 0.70 & hydro.endpoints == "Peak_10") %>%
  distinct()
# after NA's introduced 
## 126 comids 
126/2116 * 100 ## 5.954631 %

# FA_Mag ---------------------------------------------------------------
## ## greater than or = to 0.7 or 70% of good score ------ changed this from 50%
csci_FAMag_stats <- combined_metrics_index %>%
  filter(index == "CSCI" & PredictedProbabilityScaled >= 0.70 & hydro.endpoints == "FA_Mag") %>%
  distinct()
## 1640 comids 
1640/2116 *100  ## 77.50473 %

## bad q99 < 0.5
csci_FAMag_stats_bad <- combined_metrics_index %>%
  filter(index == "CSCI" & PredictedProbabilityScaled < 0.70 & hydro.endpoints == "FA_Mag") %>%
  distinct()
## 476 comids 
476/2116 * 100 ## 22.49527 %

# Wet_BFL_Mag_50 ---------------------------------------------------------------
## ## greater than or = to 0.7 or 70% of good score ------ changed this from 50%
csci_wetbfl_50_stats <- combined_metrics_index %>%
  filter(index == "CSCI" & PredictedProbabilityScaled >= 0.70 & hydro.endpoints == "Wet_BFL_Mag_50") %>%
  distinct()
##  comids 
1846/2116 *100  ## 87.24008 %

## bad q99 < 0.5
csci_wetbfl_50_stats_bad <- combined_metrics_index %>%
  filter(index == "CSCI" & PredictedProbabilityScaled < 0.70 & hydro.endpoints == "Wet_BFL_Mag_50") %>%
  distinct()
##  comids 
270/2116 * 100 ## 12.75992 %


####################################################################################################
# Deliverable submission ---------------------------------------------------------------------------
## clean file for deliverable (based on file created in section before maps are made)
## this is the clean CSV added to the deliverable readme 

#### FOR SHAPEFILE 
clean <- combined_metrics_index %>%
  dplyr::select(-c(comid_wy, wayr, WYT, FlowMetric, Scenario, abs_FFM_median_cfs, hydro, PredictedProbability)) %>%
  rename(FlowMetric = hydro.endpoints, Index = index) %>%
  mutate(IndexCode = paste(Index, FlowMetric, sep= "_")) %>% 
  # comment in our out depending on if needed for ArcGIS Files
  mutate(PredictedProbabilityScaled = if_else(is.na(PredictedProbabilityScaled), -9999, as.numeric(PredictedProbabilityScaled)))

#### create shape file of clean data set for deliverabl by joing shapefile from map code to the clean data
deliverable_shp_file <-  clean %>%
  full_join(SynthNHD, by = "COMID")

## write out the shapefile
sf::st_write(deliverable_shp_file, "output_data/Manuscript/Shapefiles/RB9_predicted_probability_combined_CSCI_ASCI.shp", 
         driver = "ESRI Shapefile")

# Final CSV ---------------------------------------------------------------
#### FOR CSV
clean <- combined_metrics_index %>%
  dplyr::select(-c(comid_wy, wayr, WYT, FlowMetric, Scenario, abs_FFM_median_cfs, hydro, PredictedProbability)) %>%
  rename(FlowMetric = hydro.endpoints, Index = index) %>%
  mutate(IndexCode = paste(Index, FlowMetric, sep= "_")) #%>% 
  # comment in our out depending on if needed for ArcGIS Files
  # mutate(PredictedProbabilityScaled = if_else(is.na(PredictedProbabilityScaled), -9999, as.numeric(PredictedProbabilityScaled)))

write_csv(clean, "output_data/Manuscript/RB9_predicted_probability_combined_CSCI_ASCI.csv")
