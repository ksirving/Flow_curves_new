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
out.dir <- "figures/"

## full names for labels
labels <- read.csv("input_data/ffm_names.csv")
labels <- labels[1:24, ]
labels <- labels %>% rename(Hydro_endpoint = Flow.Metric.Code)
labels[25, 1] <- "Magnitude of largest annual storm"
labels[25, 2] <- "Q99"
labels[25, 3] <- "Peak Flow"
labels


## work flow
# get probability of current delta H points in curve, try 1 subbasin across years to test
# get probability of water cons delta h points in curve
# compare them somehow, absolute values or median?


### delta h limits

limits <- read.csv("output_data/01_ALL_delta_thresholds_scaled.csv")
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
                                     FlowMetric == "q99" ~ "Q99")) %>%
                                     # FlowMetric == "peak_2" ~ "Peak_2", 
                                     # FlowMetric == "peak_5" ~ "Peak_5", 
                                     # FlowMetric == "peak_10" ~ "Peak_10")
  filter(!FlowMetric %in% c("peak_10", "peak_2", "peak_5"))


head(delta_long)
sum(is.na(delta_long))

RB9_metrics <- unique(delta_long$hydro.endpoints)
RB9_metrics


# test <- delta_long %>%
#   filter(comid == "20350539",hydro.endpoints == "DS_Mag_50" )

## save
write.csv(delta_long, "ignore/07a_delta_h_long.csv")


# ASCI Curve data --------------------------------------------------------------------


all_asci <- read.csv("input_data/01_h_asci_neg_pos_logR_metrics_figures_April2021.csv")
head(all_asci)

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


all_csci <- read.csv("input_data/01_csci_neg_pos_logR_metrics_figures_April2021.csv")
head(all_csci)

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
asci_metrics <- c("Q99", "DS_Mag_50") ## asci metrics based on those chosen in the previous scripts edited by Rachel 12/5 
## added Q99 w new data ^

# making bio_h_summary
## bio 

biol.endpoints<-c("H_ASCI","D_ASCI")#

## hydro
# hydro.endpoints<- colnames(asci)[6:21]

## thresholds

thresholds <- c(0.75, 0.86, 0.94) ## hybrid and diatom are the same

## make grid with all models 
bio_h_summary <-  expand.grid(biol.endpoints=biol.endpoints,hydro.endpoints=hydro.endpoints, thresholds = thresholds,  stringsAsFactors = F)
bio_h_summary

#######################################################################

## reduce glms and summary df to only rows needed
## find index to remove
ind1 <- which(bio_h_summary$biol.endpoints == "H_ASCI" & bio_h_summary$thresholds == 0.86
              & bio_h_summary$hydro.endpoints %in% asci_metrics)

ind1 ## use only these 

## remove from grid
bio_h_summary <- bio_h_summary[ind1,]

bio_h_summary <- bio_h_summary %>%
  mutate(comb_code = paste0( hydro.endpoints, "_", thresholds))

## upload GLMs and subset
load(file = "models/01a_ASCI_negative_GLM_all_delta_mets_April2021.RData")
neg.glm <- neg.glm[ind1]
neg.glm
load(file = "models/01a_ASCI_positive_GLM_all_delta_mets_April2021.RData")
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
# i=1
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
  ## add water year type
  # CurrentProbsASCI <- right_join(CurrentProbs, wyt_df, by = "wayr")  ##### don't need this line 
  
  ## calculate median per site per wyt
  # CurrentProbsMedASCI <-   CurrentProbsASCI %>%
  #   group_by(comid, hydro.endpoints) %>%  # removed: , wyt
  #   summarise(MedProb = median(PredictedProbabilityScaled), 
  #             MedDelta = median(hydro)) %>%
  #   mutate(Scenario = "Current")
  
  # ## combine historical and current
  # 
  # AllProbs <- bind_rows(HistProbsMed, CurrentProbsMed)
  # names(AllProbs)
  # 
  # AllDelta <- AllProbs %>%
  #   select(-MedProb) %>%
  #   # group_by(comid, IDs, hydro.endpoints)
  #   pivot_wider(names_from = "Scenario", values_from = "MedDelta") %>%
  #   mutate(RelChangeDelta = (Current-Historical)/Historical) %>%
  #   mutate(AbsChangeDelta = (Current-Historical)) 
  # 
  # AllProbs <- AllProbs %>%
  #   select(-MedDelta) %>%
  #   # mutate(PredictedProbabilityScaled = ifelse(PredictedProbabilityScaled == 0, 0.00001, PredictedProbabilityScaled)) %>%
  #   pivot_wider(names_from = "Scenario", values_from = "MedProb") %>%
  #   mutate(RelChange = (Current-Historical)/Historical) %>%
  #   mutate(AbsChange = (Current-Historical)) 
  
  
  # AllProbsMed <- AllProbs %>%
  #   group_by(comid, IDs, hydro.endpoints) %>%
  #   summarise(MedChange = median(RelChange))
  
  ## save
  # save(AllDelta, file = paste0("output_data/01_asci_rel_change_in_delta_", met, ".RData"))
  # save(AllProbs, file = paste0("output_data/01_asci_rel_change_in_prob_", met, ".RData"))
  # save(AllProbsMed, file = paste0("output_data/01_asci_median_rel_change_in_prob_", met, ".RData"))
  
  # save(CurrentProbsASCI, file = paste0("output_data/07a_asci_prob_", met, ".RData"))
  # # save(CurrentProbsMedASCI, file = paste0("output_data/07a_asci_median_prob_", met, ".RData"))
  # 
  # write_csv(CurrentProbsASCI, file = paste0("output_data/07a_asci_prob_", met, ".csv"))
  # write_csv(CurrentProbsMedASCI, file = paste0("output_data/07a_asci_median_prob_", met, ".csv"))
  
  finalASCI_df_RB9 <- finalASCI_df_RB9 %>%
    bind_rows(CurrentProbsASCI)
  
}

finalASCI_df_RB9 <- finalASCI_df_RB9[-1,]

#### write to CSV
write.csv(finalASCI_df_RB9, file = "output_data/07a_ASCI_prob.csv")
# CSCI Probability --------------------------------------------------------


## hydro
hydro.endpoints<- unique(all_csci$Hydro_endpoint)
hydro.endpoints
## thresholds

thresholds <- c(0.63, 0.79, 0.92) 
biol.endpoints<-c("CSCI","OoverE","MMI")

csci_metrics <- c("Q99", "Wet_BFL_Mag_10") #edited by Rachel 12/5 - chosen for Katies portion of the deliverable
## added Q99 w new data ^

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
neg.glm 
load(file = "models/01_CSCI_positive_GLM_all_delta_mets_April2021.RData")
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
  ## add water year type
  # CurrentProbsCSCI <- right_join(CurrentProbs, wyt_df, by = "wayr")
  
  
  ############ don't need anymore ######
  ## calculate median per site per wyt
  # CurrentProbsMedCSCI <-   CurrentProbsCSCI %>%
  #   group_by(comid, hydro.endpoints) %>%          ### removed wy
  #   summarise(MedProb = median(PredictedProbabilityScaled), 
  #             MedDelta = median(hydro)) %>%
  #   mutate(Scenario = "Current")
  
  ## combine historical and current
  
  # AllProbs <- bind_rows(HistProbsMed, CurrentProbsMed)
  # names(AllProbs)
  # 
  # AllDelta <- AllProbs %>%
  #   select(-MedProb) %>%
  #   # group_by(comid, IDs, hydro.endpoints)
  #   pivot_wider(names_from = "Scenario", values_from = "MedDelta") %>%
  #   mutate(RelChangeDelta = (Current-Historical)/Historical) %>%
  #   mutate(AbsChangeDelta = (Current-Historical)) 
  # 
  # AllProbs <- AllProbs %>%
  #   select(-MedDelta) %>%
  #   # mutate(PredictedProbabilityScaled = ifelse(PredictedProbabilityScaled == 0, 0.00001, PredictedProbabilityScaled)) %>%
  #   pivot_wider(names_from = "Scenario", values_from = "MedProb") %>%
  #   mutate(RelChange = (Current-Historical)/Historical) %>%
  #   mutate(AbsChange = (Current-Historical)) 
  # 
  
  # AllProbsMed <- AllProbs %>%
  #   group_by(comid, IDs, hydro.endpoints) %>%
  #   summarise(MedChange = median(RelChange))
  
  ## save
  # save(AllDelta, file = paste0("output_data/01_csci_rel_change_in_delta_", met, ".RData"))
  # save(AllProbs, file = paste0("output_data/01_csci_rel_change_in_prob_", met, ".RData"))
  # save(AllProbsMed, file = paste0("output_data/01_csci_median_rel_change_in_prob_", met, ".RData"))
  
  # save(CurrentProbsCSCI, file = paste0("output_data/07a_csci_prob_", met, ".RData"))
  # # save(CurrentProbsMedCSCI, file = paste0("output_data/07a_csci_median_prob_", met, ".RData"))
  # 
  # write_csv(CurrentProbsCSCI, file = paste0("output_data/07a_csci_prob_", met, ".csv"))
  # write_csv(CurrentProbsMedCSCI, file = paste0("output_data/07a_csci_median_prob_", met, ".csv"))
  
  finalCSCI_df_RB9 <- finalCSCI_df_RB9 %>%
    bind_rows(CurrentProbsCSCI)
  
}

finalCSCI_df_RB9 <- finalCSCI_df_RB9[-1,]

#### write to CSV
write.csv(finalCSCI_df_RB9, file = "output_data/07a_CSCI_prob.csv")

##########################################################################################################################
##########################################################################################################################
# Cleaning data for maps --------------------------------------------------------
# Combine CSCi and ASCI into one df 
ASCI <- read.csv("output_data/07a_ASCI_prob.csv")
CSCI <- read.csv("output_data/07a_CSCI_prob.csv")

comb <- ASCI %>% 
  bind_rows(CSCI) %>%
  rename(COMID = comid) %>%
  dplyr::select(-c(X))

write.csv(comb, file = "output_data/07a_predicted_probability.csv", row.names = FALSE)


## For Katie 
# combine datasets for maps
# asci_q99 <- read_csv("C:/Users/racheld/OneDrive - SCCWRP/Documents/GitHub/RB9_vulnerability_V2/output_data/07a_asci_prob_Q99.csv")
# asci_ds_mag_50 <- read_csv("C:/Users/racheld/OneDrive - SCCWRP/Documents/GitHub/RB9_vulnerability_V2/output_data/07a_asci_prob_DS_Mag_50.csv")
#   
# csci_q99 <- read_csv("C:/Users/racheld/OneDrive - SCCWRP/Documents/GitHub/RB9_vulnerability_V2/output_data/07a_csci_prob_Q99.csv")
# csci_bfl_mag_10 <- read_csv("C:/Users/racheld/OneDrive - SCCWRP/Documents/GitHub/RB9_vulnerability_V2/output_data/07a_csci_prob_Wet_BFL_Mag_10.csv")
# 
# combined_metrics_index <- asci_q99 %>%
#   bind_rows(asci_ds_mag_50) %>%
#   bind_rows(csci_q99) %>%
#   bind_rows(csci_bfl_mag_10) %>%
#   rename(COMID = comid)

# write_csv(combined_metrics_index, "output_data/07a_predicted_probability.csv")


###########################################################################
# Maps --------------------------------------------------------------------
###########################################################################
# install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
## read in csv 
combined_metrics_index <- read.csv("output_data/07a_predicted_probability.csv")

combined_metrics_index <- combined_metrics_index %>% 
  dplyr::select(-c(X.1, X))
  
## projection
prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
### upload RB9 nhds

calinhd <- readOGR('/Users/racheld/SCCWRP/SD Hydro Vulnerability Assessment - General/Data/SpatialData/NHDplus_RB9.shp') %>%
  spTransform(prj) %>%
  st_as_sf %>%
  st_simplify(dTolerance = 0.5, preserveTopology = T)
unique(calinhd$COMID)

## map

# fortified calinhd, joind with delta
# merge data set with comids to get spatial data
SynthNHD <- calinhd %>%
  filter(COMID %in% unique(combined_metrics_index$COMID)) %>%
  dplyr::select(COMID) %>% ## 46
  as('Spatial') %>% 
  st_as_sf(coords = c("long", "lat"), remove = FALSE)

# nhdplo <- SynthNHD %>%
#   fortify %>%
#   # left_join(comidid, by = 'id') %>%
#   full_join(synthesis.summary, by = 'COMID')

## Create bio-relevant flow alteration CSCI and ASCI maps 
# for appropriate prob and biol threshold combos, altered dependent on mdeian, altered 1 or 2 metrics

# set colors for alteration categories used in legend and maps
# set colors for alteration categories used in legend and maps
# colors <- c("#ca0020", "#0571b0", "white")
# alteration <- c("Altered",  "Unaltered", NA)
# categories <- c("Likely Altered", "Likely Unaltered", "Not evaluated")
# # lookup table used for legend and maps
# lookup <- data.frame(cbind(colors, alteration, categories))
# 
# # create title for plot (metric threshold)
# metric.threshold <- "2 Metric Altered Threshold"

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

names <- c("Magnitude of largest annual storm", "Dry-season median baseflow", "Magnitude of largest annual storm", "Wet-season low baseflow")

# z = "ASCI_Q99"
for(z in met_ind){
  #subset 
  subset.index <- subset[subset$metric_index == z,]
  subset.index
  
  iteration <- grep(z, met_ind)
  metric_z <- names[iteration]
  
  # for(i in metrics){
  #   # # set probability threshold label
  #   # # prob <- "Probability Threshold at 25%"
  #   subset.index.metrics <- subset.index[subset.index$hydro.endpoints == i,]
    
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
    
    # subset lookup categories and tables
    
    # lookup.sub <- lookup[lookup$alteration %in% unique(subset.join$`Alteration - Biology`),]
    # 
    # # save as factor to sort categories in legend
    # lookup.sub$alteration <- factor(lookup.sub$alteration, levels = unique(lookup.sub$alteration))
    # subset.join$`Alteration - Biology` <- factor(subset.join$`Alteration - Biology`, levels = unique(lookup.sub$alteration))
    # 
    
    # synthesis map for bio index z
    syn.plot <- study + geom_sf(data = subset.join, aes(color= PredictedProbabilityScaled, geometry = geometry)) +
      scale_color_distiller(palette = "Spectral", direction = 1) +
      labs(color = "Predicted Probability")
    # scale_color_viridis_c(option = "A") 
    
    # print
    print(syn.plot)
    
    # write plot
    out.filename <- paste0("figures/maps/07a_", z, "_predicted_prob_Current.jpg") 
    ggsave(syn.plot, file = out.filename, dpi=300, height=4, width=6)
    
  # }
}


############ summary of predicted probabilities ############
## ASCI
## greater than or = to 0.7 or 70% of good score ------ changed this from 50%
asciq99_stats <- combined_metrics_index %>%
  filter(index == "ASCI" & PredictedProbabilityScaled >= 0.70 & hydro.endpoints == "Q99") %>%
  distinct()
## 413 comids 
413/2116 *100 ## 19.51796 %

## bad q99 < 0.7
asciq99_stats_bad <- combined_metrics_index %>%
  filter(index == "ASCI" & PredictedProbabilityScaled < 0.70 & hydro.endpoints == "Q99") %>%
  distinct()
## 1,703 comids 
1703/2116 *100  ## 80.48204 %

## ## greater than or = to 0.7 or 70% of good score ------ changed this from 50%
asci_dsmag50_stats <- combined_metrics_index %>%
  filter(index == "ASCI" & PredictedProbabilityScaled >= 0.70 & hydro.endpoints == "DS_Mag_50") %>%
  distinct()
## 335 comids
335/2116 *100  ## 15.83176 %

#bad ds mag 50
asci_dsmag50_stats_bad <- combined_metrics_index %>%
  filter(index == "ASCI" & PredictedProbabilityScaled < 0.70 & hydro.endpoints == "DS_Mag_50") %>%
  distinct()
## 1,781 comids
1781/2116 *100 ## 84.16824

#######################################
## CSCI
## ## greater than or = to 0.7 or 70% of good score ------ changed this from 50%
csciq99_stats <- combined_metrics_index %>%
  filter(index == "CSCI" & PredictedProbabilityScaled >= 0.70 & hydro.endpoints == "Q99") %>%
  distinct()
## 370 comids 
370/2116 *100  ## 17.48582%

## bad q99 < 0.5
csciq99_stats_bad <- combined_metrics_index %>%
  filter(index == "CSCI" & PredictedProbabilityScaled < 0.70 & hydro.endpoints == "Q99") %>%
  distinct()
## 1745 comids 
1745/2116 * 100 ## 82.46692 %

## ## greater than or = to 0.7 or 70% of good score ------ changed this from 50%
csci_wetbfl10_stats <- combined_metrics_index %>%
  filter(index == "CSCI" & PredictedProbabilityScaled >= 0.70 & hydro.endpoints == "Wet_BFL_Mag_10") %>%
  distinct()
## 1,347 comids
1347/2116 * 100 ## 63.65784 %

#bad ds mag 50
csci_wetbfl10_stats_bad <- combined_metrics_index %>%
  filter(index == "CSCI" & PredictedProbabilityScaled < 0.70 & hydro.endpoints == "Wet_BFL_Mag_10") %>%
  distinct()
## 769 comids
769/2116 * 100 ## 36.34216 %



####################################################################################################
# Deliverable submission ---------------------------------------------------------------------------
## clean file for deliverable (based on file created in section before maps are made)
## this is the clean CSV added to the deliverable readme 
clean <- combined_metrics_index %>%
  dplyr::select(-c(comid_wy, wayr, WYT, FlowMetric, Scenario, abs_FFM_median_cfs, hydro, PredictedProbability)) %>%
  rename(FlowMetric = hydro.endpoints, Index = index) %>%
  mutate(IndexCode = paste(Index, FlowMetric, sep= "_")) %>% 
  mutate(PredictedProbabilityScaled = format(PredictedProbabilityScaled))

write_csv(clean, "output_data/07a_predicted_probability_combined_CSCI_ASCI.csv")

#### create shape file of clean data set for deliverabl by joing shapefile from map code to the clean data
deliverable_shp_file <-  clean %>%
  full_join(SynthNHD, by = "COMID")

## write out the shapefile
sf::st_write(deliverable_shp_file, "output_data/07a_predicted_probability_combined_CSCI_ASCI.shp", 
         driver = "ESRI Shapefile")

############################## QA test ######
# load(file = "models/01a_ASCI_negative_GLM_all_delta_mets_April2021.RData")
# neg.glm <- neg.glm[ind1]
# neg.glm
# 
# load(file = "models/01a_ASCI_positive_GLM_all_delta_mets_April2021.RData")
# pos.glm <- pos.glm[ind1]
# pos.glm
# 
# #
# load(file = "models/01_CSCI_negative_GLM_all_delta_mets_April2021.RData")
# neg.glm <- neg.glm[ind1]
# neg.glm 
# load(file = "models/01_CSCI_positive_GLM_all_delta_mets_April2021.RData")
# pos.glm <- pos.glm[ind1]
# pos.glm
# 
# i = 70
# 
# # negMod <- neg.glm[i][[1]]
# 
# hydroxx <- delta_long %>%
#   filter(hydro.endpoints == met)
# # hydroxx <- "q99"
# unique(hydroxx$FlowMetric)
# 
# ## get models for pos and neg
# posMod <- pos.glm[i][[1]]
# negMod <- neg.glm[i][[1]]
# negMod$data
# posMod$data
# ## rename to match models, separate scenarios and delta positive and negative
# new_data_current_pos <- hydroxx %>%
#   # pivot_wider(names_from = Scenario, values_from = DeltaH)
#   rename(hydro =  DeltaH) %>%
#   filter(Scenario == "Current",
#          !hydro < 0)
# 
# negModCurrent <- predict(negMod, new_data_current_pos, type = "response")

# Old code  
# #######################################################################################################################
# #######################################################################################################################
# #######################################################################################################################
# #######################################################################################################################
# ##old loop
# ## Loop through CSCI and ASCI thresholds and metrics
# indices <- c("ASCI", "CSCI")
# metrics <- c("Q99", "DS_Mag_50", "Wet_BFL_Mag_10")
# # i = "Q99"
# # z = "ASCI"
# for(z in indices){
#   #subset either csci or asci
#   subset.index <- subset[subset$index == z,]
#   subset.index
#   
#   for(i in metrics){
#     # # set probability threshold label
#     # # prob <- "Probability Threshold at 25%"
#     subset.index.metrics <- subset.index[subset.index$hydro.endpoints == i,]
#     
#     # merge with comids to get spatial data for map
#     subset.join <- subset.index.metrics %>% 
#       full_join(SynthNHD, by = c('COMID'))
#     subset.join
#     # set title and subtitle
#     title <- paste0(z)
#     subtitle <- paste("Predicted Probability", i, sep = "\n") 
#     
#     ## Plot
#     # Set up base map 
#     study <- ggplot(SynthNHD) + 
#       geom_sf(color = "lightgrey", fill="white") +
#       labs(title=title, subtitle = subtitle, x ="", y = "")  + 
#       annotation_scale() +
#       annotation_north_arrow(pad_y = unit(0.9, "cm"),  height = unit(.8, "cm"),
#                              width = unit(.8, "cm")) +
#       theme(panel.background = element_rect(fill = "white"),
#             axis.ticks = element_blank(),
#             axis.text = element_blank(),
#             panel.grid = element_line(color = "white", size = 0.8),
#             plot.title = element_text(size=20)) 
#     #print map
#     study
#     
#     # subset lookup categories and tables
#     
#     # lookup.sub <- lookup[lookup$alteration %in% unique(subset.join$`Alteration - Biology`),]
#     # 
#     # # save as factor to sort categories in legend
#     # lookup.sub$alteration <- factor(lookup.sub$alteration, levels = unique(lookup.sub$alteration))
#     # subset.join$`Alteration - Biology` <- factor(subset.join$`Alteration - Biology`, levels = unique(lookup.sub$alteration))
#     # 
#     
#     # synthesis map for bio index z
#     syn.plot <- study + geom_sf(data = subset.join, aes(color= PredictedProbabilityScaled, geometry = geometry)) +
#       scale_color_distiller(palette = "Spectral", direction = 1)
#       # scale_color_viridis_c(option = "A") 
#     
#     # print
#     print(syn.plot)
#     
#     # write plot
#     out.filename <- paste0("figures/maps/07a_", z, "_", i, "_predicted_prob_Current.jpg") 
#     ggsave(syn.plot, file = out.filename, dpi=300, height=4, width=6)
#     
#   }
# }






