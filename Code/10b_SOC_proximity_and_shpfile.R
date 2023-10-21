############################################################################################################################################
#### This script is looking at the different threshold/deltaH combinations 
#### The script will identify whether current probability values are within the threshold and how close said values are the ranges of the thresholds 

library(tidyverse)
library(tidyr)
library(tidylog)
#packages for shp file 
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

### read in look up table of FFMs and thresholds 
lookup_table <- read_csv("output_data/Manuscript/10_SOC_ALL_delta_thresholds_scaled.csv")

### reading in delta long from script 10a for df_II
delta_long_SOC_data <- read.csv("ignore/10a_SOC_delta_h_long.csv") %>% 
  dplyr::select(-c(X, FlowMetric)) 

### read in current predicted probabilities 
#### Edited 10/20
### Quick and ugly editing of the df. It was decided that some metrics would be plotted with scaled values and 
# some with unscaled values as follows: ASCI: DS_Dur_WS & SP_Dur ; CSCI DS_Dur_WS = scaled - the rest unscaled 
# Editing this so that all columns say "Predicted probability scaled" even when they are not for ease of re-running 
# the code below as it was written. The filters can be changed as metrics change 
current_predicted_prob_all <- read_csv("output_data/Manuscript/10a_SOC_predicted_probability_ASCIandCSCI.csv")

# scaled metrics -> ASCI: DS_Dur_WS & SP_Dur ; CSCI DS_Dur_WS
current_predicted_df_scaled <- current_predicted_prob_all %>% 
  filter(hydro.endpoints %in% c("DS_Dur_WS", "SP_Dur")) %>% 
  dplyr::select(-PredictedProbability)

# UNSCALED but renaming the unscaled column to PredictedProbScaled for ease of running the code below 
# (I apologize for doing it this way) 
current_predicted_df_UNscaled <- current_predicted_prob_all %>% 
  filter(!hydro.endpoints %in% c("DS_Dur_WS", "SP_Dur")) %>% 
  dplyr::select(-PredictedProbabilityScaled) %>%
  # renaming just for the sake of not having to edit all the dfs I make below (this is not actually the scaled column)
  rename(PredictedProbabilityScaled = PredictedProbability)

### FINAL DF TO USE IN THE CODE BELOW 
current_predicted_prob <- current_predicted_df_scaled %>% 
  bind_rows(current_predicted_df_UNscaled)

# create lookup table 
lookuptable <- lookup_table %>%
  dplyr::select(-c(...1, X, metric, n)) %>% 
  pivot_longer(Threshold70:Threshold99, names_to = "Threshold", values_to = "value") %>%
  group_by(Hydro_endpoint, Threshold) %>% 
  pivot_wider(names_from = "Type", values_from = "value") %>% 
  mutate(IndexCode = paste(Biol, Hydro_endpoint, sep= "_")) %>% 
  filter(IndexCode %in% c("CSCI_DS_Dur_WS", "CSCI_SP_Tim", "CSCI_Wet_BFL_Mag_50",
                          "CSCI_FA_Mag", "CSCI_Peak_10", "CSCI_DS_Mag_50", 
                          "ASCI_SP_Dur", "ASCI_DS_Dur_WS", "ASCI_DS_Mag_50", 
                          "ASCI_Wet_BFL_Mag_50", "ASCI_SP_Mag", "ASCI_Peak_2")) %>% 
  mutate(ThreshIndexCode = paste(Biol, Hydro_endpoint, Bio_threshold, sep= "_")) #%>% 
  # mutate(metric = paste(Bio_endpoint, "_", Hydro_endpoint, "_", Bio_threshold, "_", Type, sep = ""))
  

  
#### edit the combined database table & join to look up table
current_pred_prob <- current_predicted_prob %>%
  mutate(IndexCode = paste(index, hydro.endpoints, sep= "_")) %>%
  full_join(lookuptable, by = "IndexCode") %>%
  dplyr::select(-c(FlowMetric, Biol, hydro.endpoints, IndexCode))  # took out useless and/or duplicate columns 


############################################################################################################################################
### adding the proximity and threshold classification columns to df 
proximity_df <- current_pred_prob %>%
  mutate(Classification = case_when(hydro <= Positive & hydro >= Negative ~ "Within",
                                    hydro > Positive ~ "Augmented",
                                    hydro < Negative ~ "Depleted", 
                                    is.na(PredictedProbabilityScaled) ~ "Indeterminant",
                                    T ~ "Bad")) %>%
  mutate(Prox_Positive = Positive - hydro) %>%
  mutate(Prox_Negative = Negative - hydro) %>%
  
  mutate(Prox_Positivev2 = ifelse(abs(Prox_Positive) >= 1, round(Prox_Positive),
                                  ifelse(abs(Prox_Positive) < 1, round(Prox_Positive, digits = 2), "Broken"))) %>%
  
  mutate(Prox_Negativev2 = ifelse((abs(Prox_Negative)) >= 1, round(Prox_Negative),
                                  ifelse(abs(Prox_Negative) < 1, round(Prox_Negative, digits = 2), "Broken")))

### adding text column to explain in words what the outputs are saying 
final_dfv2 <- proximity_df %>%
  mutate(Prox_Positivev2 = as.numeric(Prox_Positivev2)) %>%
  mutate(Prox_Negativev2 = as.numeric(Prox_Negativev2)) %>%
  mutate(Type = case_when(Hydro_endpoint == "DS_Dur_WS" ~ "Dur", 
                          Hydro_endpoint == "SP_Dur" ~ "Dur", 
                          Hydro_endpoint == "SP_Tim" ~ "Tim", 
                          Hydro_endpoint == "Q99" ~ "Mag", 
                          Hydro_endpoint == "DS_Mag_50" ~ "Mag", 
                          Hydro_endpoint == "Wet_BFL_Mag_10" ~ "Mag", 
                          Hydro_endpoint == "FA_Mag" ~ "Mag",
                          Hydro_endpoint == "Peak_10" ~ "Mag",
                          Hydro_endpoint == "Peak_2" ~ "Mag",
                          Hydro_endpoint == "Peak_5" ~ "Mag",
                          Hydro_endpoint == "SP_Mag" ~ "Mag",
                          Hydro_endpoint == "Wet_BFL_Mag_50" ~ "Mag",
                          TRUE ~ "None")) %>% 
  mutate(Results = case_when(
    # Mag metrics 
    Classification == "Within" & Type == "Mag" ~
      paste("Current Delta FFM is within limits. Proposed project should not increase by more than", Prox_Positivev2, "cfs or decrease by more than",  abs(Prox_Negativev2), "cfs."), 
            
    Classification == "Augmented" & Type == "Mag" ~
      paste("Current Delta FFM is augmented. Proposed project can decrease by", abs(Prox_Positivev2), "to",  abs(Prox_Negativev2), "cfs."), 
    
    Classification == "Depleted" & Type == "Mag" ~
      paste("Current Delta FFM is depleted. Proposed project can increase by", Prox_Negativev2, "to",  Prox_Positivev2, "cfs."),
    
    Classification == "Indeterminant" ~ "A probability of NA indicates augmented peak metric.  Not enough data for flow ecology curve peak augmentation.",
    
    #Dur metrics 
    Classification == "Within" & Type == "Dur" ~
      paste("Current Delta FFM is within limits. Proposed project should not increase duration by more than", Prox_Positivev2, "days or decrease duration by more than",  abs(Prox_Negativev2), "days."), 
    
    Classification == "Augmented" & Type == "Dur" ~
      paste("Current Delta FFM is augmented (long). Proposed project can decrease duration by", abs(Prox_Positivev2), "days to",  abs(Prox_Negativev2), "days."), 
    
    Classification == "Depleted" & Type == "Dur" ~
      paste("Current Delta FFM is depleted (short). Proposed project can increase duration by", Prox_Negativev2, " days to",  Prox_Positivev2, "days."),
      
    #Tim metric 
    Classification == "Within" & Type == "Tim" ~
      paste("Current Delta FFM is within limits. Proposed project should not change timing by more than", Prox_Positivev2, "days later or by more than",  abs(Prox_Negativev2), "days earlier."), 
    
    Classification == "Augmented" & Type == "Tim" ~
      paste("Current Delta FFM is augmented (late). Proposed project can change timing by", abs(Prox_Positivev2), "to",  abs(Prox_Negativev2), "days earlier."), 
    
    Classification == "Depleted" & Type == "Tim" ~
      paste("Current Delta FFM is depleted (early). Proposed project can change timing by", Prox_Negativev2, "to",  Prox_Positivev2, "days later."),
  
    TRUE ~ "bad")) %>%
  dplyr::select(-c(Prox_Positivev2, Prox_Negativev2, Type))

formatting_final_dfv2 <- final_dfv2 %>% 
  dplyr:: select(c(site, hydro, PredictedProbabilityScaled, Threshold, index, Hydro_endpoint, Results)) 

# for testing and QA'ing
# write.csv(formatting_final_dfv2, "C:/Users/racheld/Downloads/SOC_prox_threshold.csv", row.names = FALSE)

## df for II
df_II <- delta_long_SOC_data %>% 
  dplyr:: select(c(site, DeltaH, hydro.endpoints)) %>%  #, index
  distinct() %>% 
  pivot_wider(names_from = hydro.endpoints, values_from = DeltaH, names_prefix = "II.DeltaFFM_") 

# df for III
df_III <- formatting_final_dfv2 %>% 
  dplyr:: select(c(site, Threshold)) %>% 
  mutate(`III.Threshold` = case_when(
    Threshold == "Threshold70" ~ "0.70", 
    Threshold == "Threshold90" ~ "0.90",
    Threshold == "Threshold99" ~ "0.99", 
    TRUE ~ "1")) %>% 
  dplyr::select(-Threshold)


## df for IV
df_IV <-  formatting_final_dfv2 %>% 
  dplyr::select(c(site, PredictedProbabilityScaled, Threshold, index, Hydro_endpoint)) %>% 
  mutate(IndexMetric = paste(index, Hydro_endpoint, sep= "_")) %>% 
  dplyr::select(c(site, IndexMetric, PredictedProbabilityScaled)) %>% 
  distinct() %>% 
  pivot_wider(names_from = IndexMetric, values_from = PredictedProbabilityScaled, names_prefix = "IV.Prob_")
  

## df for V
df_V <-  formatting_final_dfv2 %>% 
  dplyr::select(c(site, PredictedProbabilityScaled, Threshold, index, Hydro_endpoint, Results)) %>% 
  mutate(IndexMetric = paste(index, Hydro_endpoint, sep= "_")) %>% 
  dplyr::select(c(site, IndexMetric, Results, Threshold)) %>% 
  distinct() %>% 
  pivot_wider(names_from = IndexMetric, values_from = Results, names_prefix = "V.Result_") %>%
  mutate(`III.Threshold` = case_when(
    Threshold == "Threshold70" ~ "0.70", 
    Threshold == "Threshold90" ~ "0.90",
    Threshold == "Threshold99" ~ "0.99", 
    TRUE ~ "1")) %>% 
  dplyr::select(-c(Threshold))


combined_dfs <- df_II %>% 
  left_join(df_III, by = "site") %>% 
  distinct() %>%
  left_join(df_IV, by = "site") %>% 
  distinct() %>% 
  left_join(df_V, by = c("site", "III.Threshold")) %>% 
  distinct() %>% 
  rename(`I.Site` = site) %>% 
#Rachel added/edited 10/2
  mutate(V.Result_CSCI_FA_Mag = if_else(is.na(V.Result_CSCI_FA_Mag), "A probability of NA means no median delta FFM value is available.", as.character(V.Result_CSCI_FA_Mag)))
  
# for shapefile
combined_dfs_shp <- df_II %>% 
  left_join(df_III, by = "site") %>% 
  distinct() %>%
  left_join(df_IV, by = "site") %>% 
  distinct() %>% 
  left_join(df_V, by = c("site", "III.Threshold")) %>% 
  distinct() %>% 
  rename(`I.Site` = site) %>% 
  #Rachel added/edited 10/2
  mutate(V.Result_CSCI_FA_Mag = if_else(is.na(V.Result_CSCI_FA_Mag), "A probability of <Null> means no median delta FFM value is available.", as.character(V.Result_CSCI_FA_Mag))) %>% 
  # can comment this line in or out depending on whether the -9999 is needed for ArcGIS
  #Rachel added/edited 9/29
  mutate(II.DeltaFFM_FA_Mag = if_else(is.na(II.DeltaFFM_FA_Mag), -9999, as.numeric(II.DeltaFFM_FA_Mag))) %>%
  mutate(IV.Prob_ASCI_Peak_2 = if_else(is.na(IV.Prob_ASCI_Peak_2), -9999, as.numeric(IV.Prob_ASCI_Peak_2))) %>%
  mutate(IV.Prob_CSCI_FA_Mag = if_else(is.na(IV.Prob_CSCI_FA_Mag), -9999, as.numeric(IV.Prob_CSCI_FA_Mag))) %>% 
  mutate(IV.Prob_CSCI_Peak_10 = if_else(is.na(IV.Prob_CSCI_Peak_10), -9999, as.numeric(IV.Prob_CSCI_Peak_10))) %>% 
  mutate(V.Result_CSCI_Peak_10 = if_else(V.Result_CSCI_Peak_10 == "A probability of NA indicates augmented peak metric.  Not enough data for flow ecology curve peak augmentation.", 
         "A probability of <Null> indicates augmented peak metric.  Not enough data for flow ecology curve peak augmentation.", as.character(V.Result_CSCI_Peak_10))) %>% 
  mutate(V.Result_ASCI_Peak_2 = if_else(V.Result_ASCI_Peak_2 == "A probability of NA indicates augmented peak metric.  Not enough data for flow ecology curve peak augmentation.", 
                                         "A probability of <Null> indicates augmented peak metric.  Not enough data for flow ecology curve peak augmentation.", as.character(V.Result_ASCI_Peak_2)))
  


# write.csv(combined_dfs, "C:/Users/racheld/Downloads/SOC_prox_threshold_test.csv", row.names = FALSE)
write.csv(combined_dfs, "output_data/Manuscript/SOC_RiskFramework_Data_Final.csv", row.names = FALSE)

### paired with -9999 code 
write.csv(combined_dfs_shp, "output_data/Manuscript/SOC_RiskFramework_Data_Final_forshapefile.csv", row.names = FALSE)

############################################################################
# Shp file ----------------------------------------------------------------
### making shp file 
#re-read in csv due to shpfile making problems 
final_SOC <- read.csv("output_data/Manuscript/SOC_RiskFramework_Data_Final_forshapefile.csv")

#ArcGIS truncates column names renaming so shpefile will write
final_SOC <- final_SOC %>%
  rename("site" = "I.Site", 
         "DDSDurWS" = "II.DeltaFFM_DS_Dur_WS", 
         "DSPDur" = "II.DeltaFFM_SP_Dur", 
         "DDSMag50" = "II.DeltaFFM_DS_Mag_50", 
         "DPeak2" = "II.DeltaFFM_Peak_2", 
         "DSPMag" =  "II.DeltaFFM_SP_Mag",
         "DWetBFLMag50" = "II.DeltaFFM_Wet_BFL_Mag_50", 
         "DSPTim" = "II.DeltaFFM_SP_Tim", 
         "DFAMag" = "II.DeltaFFM_FA_Mag",
         "DPeak10" = "II.DeltaFFM_Peak_10", 
         "Threshold" = "III.Threshold", 
         "PASCIDSDurWS" = "IV.Prob_ASCI_DS_Dur_WS", 
         "PASCISPDur" = "IV.Prob_ASCI_SP_Dur", 
         "PASCIDSMag50" = "IV.Prob_ASCI_DS_Mag_50", 
         "PASCIPeak2" = "IV.Prob_ASCI_Peak_2", 
         "PASCISPMag" = "IV.Prob_ASCI_SP_Mag",
         "PASCIWetBFLMag50" = "IV.Prob_ASCI_Wet_BFL_Mag_50", 
         "PCSCIDSDurWS" = "IV.Prob_CSCI_DS_Dur_WS", 
         "PCSCISPTim" = "IV.Prob_CSCI_SP_Tim", 
         "PCSCIDSMag50" = "IV.Prob_CSCI_DS_Mag_50",
         "PCSCIFAMag" = "IV.Prob_CSCI_FA_Mag", 
         "PCSCIPeak10" = "IV.Prob_CSCI_Peak_10", 
         "PCSCIWetBFLMag50" = "IV.Prob_CSCI_Wet_BFL_Mag_50", 
         "RASCIDSDurWS" = "V.Result_ASCI_DS_Dur_WS", 
         "RASCISPDur" = "V.Result_ASCI_SP_Dur", 
         "RASCIDSMag50" = "V.Result_ASCI_DS_Mag_50", 
         "RASCIPeak2" = "V.Result_ASCI_Peak_2", 
         "RASCISPMag" = "V.Result_ASCI_SP_Mag", 
         "RASCIWetBFLMag50" = "V.Result_ASCI_Wet_BFL_Mag_50", 
         "RCSCIDSDurWS" = "V.Result_CSCI_DS_Dur_WS", 
         "RCSCISPTim" = "V.Result_CSCI_SP_Tim", 
         "RCSCIDSMag50" = "V.Result_CSCI_DS_Mag_50", 
         "RCSCIFAMag" = "V.Result_CSCI_FA_Mag", 
         "RCSCIPeak10" = "V.Result_CSCI_Peak_10", 
         "RCSCIWetBFLMag50" = "V.Result_CSCI_Wet_BFL_Mag_50"
         ) #%>% 
  # dplyr::select(-c(Column.Name, Lookup))

new_order = c("site", "DDSDurWS", "DSPDur", "DDSMag50", "DPeak2", "DSPMag", "DWetBFLMag50", "DSPTim", 
              "DFAMag", "DPeak10", "Threshold", "PASCIDSDurWS", "PASCISPDur", "PASCIDSMag50", "PASCIPeak2", 
              "PASCISPMag", "PASCIWetBFLMag50", "PCSCIDSDurWS", "PCSCISPTim", "PCSCIDSMag50", "PCSCIFAMag", 
              "PCSCIPeak10", "PCSCIWetBFLMag50", "RASCIDSDurWS", "RASCISPDur", "RASCIDSMag50", "RASCIPeak2", 
              "RASCISPMag", "RASCIWetBFLMag50", "RCSCIDSDurWS", "RCSCISPTim", "RCSCIDSMag50", "RCSCIFAMag", 
              "RCSCIPeak10", "RCSCIWetBFLMag50")

final_SOC <-  final_SOC[, new_order]

#read in information on subbasin and New_Name
basin_comid_lookup <- read.csv("SOC_Data/v13_pourpoints_NHD_comids.csv") 

#read in shapefiles subbasins and reaches
#subbasin polygon shapefile
basins <- st_read("SOC_Data/subbasin_boundaries_forSCCWRP.shp", quiet = T)


#lookuptable to convert subbasin codes for model output subbasin names - doesn't exist!!!
subbasin_lookup <- read.csv("SOC_Data/site_name_lookupletternumbers.csv")


#convert basin orig name to outputfile name (model subbasin name)
new.subbasinname <- basin_comid_lookup$Subbasin

for(z in 1:length(subbasin_lookup$Letter)){
  new.subbasinname <- gsub(subbasin_lookup$Letter[z], subbasin_lookup$Number[z], new.subbasinname)
}

#find and replace - in new.subbasinname with nothing, make consistent with file name
new.subbasinname <- gsub("-", "", new.subbasinname)
basin_comid_lookup$site <- as.numeric(new.subbasinname)

#join new subbasin name with the data 
SOC_shp <- final_SOC %>% 
  inner_join(basin_comid_lookup, by = 'site') %>% 
  dplyr::select(c(names(final_SOC), Subbasin)) %>% 
  rename(New_Name = Subbasin) %>% 
  inner_join(basins, by = "New_Name") %>% 
  dplyr::select(-c(FID_1, OWS))

#converting to sf due to error I was getting when reprojecting in "SOC_calinhd"
SOC_shp = st_as_sf(SOC_shp)

## projection
prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# load RB9 shp
SOC_calinhd <- SOC_shp %>%   #readOGR('SOC_Data/subbasin_boundaries_forSCCWRP.shp') %>%
  st_transform(prj) %>%
  st_as_sf %>%
  st_simplify(dTolerance = 0.5, preserveTopology = T)
# unique(calinhd$COMID)

# fortified calinhd, joind with delta
# merge data set with comids to get spatial data
SOC_SynthNHD <- SOC_calinhd %>%
  filter(site %in% unique(final_SOC$site)) %>%
  dplyr::select(site) %>% ## 46
  as('Spatial') %>% 
  st_as_sf(coords = c("long", "lat"), remove = FALSE)

## shp file join
SOC_deliverable_shp_file <-  final_SOC %>%
  full_join(SOC_SynthNHD, by = "site") %>% 
  distinct()

## write out the shapefile 
sf::st_write(SOC_deliverable_shp_file, "output_data/Manuscript/Shapefiles/South_OC_RiskFramework.shp", 
             driver = "ESRI Shapefile", append = FALSE)
