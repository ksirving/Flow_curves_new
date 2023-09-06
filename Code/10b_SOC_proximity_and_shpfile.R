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
lookup_table <- read_csv("C:/Users/racheld/Downloads/00_ALL_delta_thresholds_scaled_new_thresholds_formatted.csv")

# lookup_table <- read_csv("output_data/01_ALL_delta_thresholds_scaled.csv")
# 
# lookup_table <- read_csv("output_data/09a_SOC_ALL_delta_thresholds_scaled.csv") 

### read in current predicted probabilities 
current_predicted_prob <- read_csv("output_data/09a_SOC_predicted_probability_ASCIandCSCI.csv")

############################################################################################################################################
# filter look up table for what we want and reformat it 
# 
# lookuptable <- lookup_table %>%
#   filter(metric == "H_ASCI_Q99_0.86_Positive" | metric == "H_ASCI_Q99_0.86_Negative" | metric == "H_ASCI_DS_Mag_50_0.86_Positive"
#          | metric == "H_ASCI_DS_Mag_50_0.86_Negative" | metric == "H_ASCI_DS_Dur_WS_0.86_Positive" | metric == "H_ASCI_DS_Dur_WS_0.86_Negative"
#          | metric == "H_ASCI_SP_Dur_0.86_Positive" | metric == "H_ASCI_SP_Dur_50_0.86_Negative"
#          | metric == "CSCI_Q99_0.79_Positive" | metric == "CSCI_Q99_0.79_Negative"
#          | metric == "CSCI_Wet_BFL_Mag_10_0.79_Positive" | metric == "CSCI_Wet_BFL_Mag_10_0.79_Negative"
#          | metric == "CSCI_SP_Tim_0.79_Positive" | metric == "CSCI_SP_Tim_0.79_Negative"
#          | metric == "CSCI_DS_Dur_WS_10_0.79_Positive" | metric == "CSCI_DS_Dur_WS_0.79_Negative") %>%
#   dplyr::select(-c(...1, X, Bio_endpoint, n, metric)) %>%
#   pivot_longer(Threshold70:Threshold99, names_to = "Threshold", values_to = "value") %>%
#   group_by(Hydro_endpoint, Threshold) %>%
#   pivot_wider(names_from = "Type", values_from = "value") %>%
#   mutate(ThreshIndexCode = paste(Biol, Hydro_endpoint, Threshold, sep= "_")) %>%
#   mutate(IndexCode = paste(Biol, Hydro_endpoint, sep= "_"))


lookuptable <- lookup_table %>%
  mutate(ThreshIndexCode = paste(Biol, Hydro_endpoint, Threshold, sep= "_")) %>%
  mutate(IndexCode = paste(Biol, Hydro_endpoint, sep= "_")) %>%
  filter(IndexCode == "ASCI_Q99" | IndexCode == "ASCI_DS_Mag_50" | IndexCode == "ASCI_DS_Dur_WS"
         | IndexCode == "ASCI_SP_Dur" | IndexCode == "CSCI_Q99" | IndexCode == "CSCI_DS_Dur_WS"
         | IndexCode == "CSCI_SP_Tim" | IndexCode == "CSCI_Wet_BFL_Mag_10") %>% 
  dplyr::select(-c(...1))

  
#### edit the combined database table & join to look up table
current_pred_prob <- current_predicted_prob %>%
  mutate(IndexCode = paste(index, hydro.endpoints, sep= "_")) %>%
  full_join(lookuptable, by = "IndexCode") %>%
  dplyr::select(-c(FlowMetric, Biol, hydro.endpoints, IndexCode)) # took out useless and/or duplicate columns 


############################################################################################################################################
### adding the proximity and threshold classification columns to df 
proximity_df <- current_pred_prob %>%
  mutate(Classification = ifelse(hydro <= Positive & hydro >= Negative, "Within", 
                                 ifelse(hydro > Positive, "Augmented", 
                                        ifelse(hydro < Negative, "Depleted", "NA")))) %>%
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
                          TRUE ~ "None")) %>% 
  mutate(Results = case_when(
    # Mag metrics 
    Classification == "Within" & Type == "Mag" ~
      paste("Current Delta FFM is within limits. Proposed project should not increase by more than", Prox_Positivev2, "cfs or decrease by more than",  abs(Prox_Negativev2), "cfs."), 
            
    Classification == "Augmented" & Type == "Mag" ~
      paste("Current Delta FFM is augmented. Proposed project can decrease by", abs(Prox_Positivev2), "to",  abs(Prox_Negativev2), "cfs."), 
    
    Classification == "Depleted" & Type == "Mag" ~
      paste("Current Delta FFM is depleted. Proposed project can increase by", Prox_Negativev2, "to",  Prox_Positivev2, "cfs."),
    
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

## df for II
df_II <- formatting_final_dfv2 %>% 
  dplyr:: select(c(site, hydro, Hydro_endpoint)) %>%  
  distinct() %>% 
  pivot_wider(names_from = Hydro_endpoint, values_from = hydro, names_prefix = "II.DeltaFFM_") 

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
  rename(`I.Site` = site)
  

write.csv(combined_dfs, "C:/Users/racheld/Downloads/SOC_prox_threshold_test.csv", row.names = FALSE)
write.csv(combined_dfs, "C:/Users/racheld/Downloads/SOC_RiskFramework_Data_Final.csv", row.names = FALSE)


############################################################################
# Shp file for 2nd deliverable  -------------------------------------------
# Based off data above 

# reading in needed file for this sections 
# arroyo_toad <- read.csv("input_data/08_Arroyo_Toad_Prob_Occurrence_RB9.csv") # old csv
# arroyo_toad <- read.csv("input_data/NHD_Toads_prob.csv")
# 
# relative_alteration <- read.csv("input_data/08_Relative_Alteration_FFM_RB9.csv")
# 
# 
# ### DATAFRAME 1
# ## adding yes/no column based on whether threshold was satisfied or not & cleaning up columns
# 
# csci_asci_df <- final_dfv2 %>%
#   mutate(probs = ifelse(Threshold == "Threshold70", "0.70", 
#                         ifelse(Threshold == "Threshold90", "0.90", 
#                                ifelse(Threshold == "Threshold99", "0.99", "Bad")))) %>%
#   mutate(SatisfiedProbability = ifelse(PredictedProbabilityScaled >= probs, "Yes", "No")) %>%
#   dplyr::select(-c(PredictedProbability)) %>% # took out , index, Threshold, probs
#   rename(DeltaFFM = hydro)
# 
# csci_asci_df$Hydro_endpoint <- tolower(csci_asci_df$Hydro_endpoint)
# 
# ### join relative alteration to csci/asci df 
# relalt_csciasci <- csci_asci_df %>%
#   left_join(relative_alteration, by = c('Hydro_endpoint' = 'metric', 'COMID' = 'comid')) %>%
#   rename(FullMetricName = title_name2)
# 
# ### DATAFRAME 2
# ### joining and cleaning arroyo toad df
# arroyo_toadv2 <- arroyo_toad %>%
#   dplyr::select(c(MeanProb, COMID))
# 
# # taking mean of MeanProb
# arroyo_mean <- arroyo_toadv2 %>%
#   group_by(COMID) %>%
#   summarise(MeanProbToad = mean(MeanProb)) %>%
#   ungroup
# 
# ### DATAFRAME 3
# ## joining toad data to dataframe 2  - this final version sans pivot 
# final_df <- relalt_csciasci %>%
#   left_join(arroyo_mean, by = "COMID")  %>%
#   dplyr::select(-c(DeltaFFM, Bio_threshold, Negative, Positive, FullMetricName, Classification, Prox_Positive, Prox_Negative, Threshold)) %>%
#   rename(Probability_Threshold = probs) %>%
#   mutate(IndexMetric = paste(index, Hydro_endpoint, sep= "_")) %>%
#   dplyr::select(-c(index, Hydro_endpoint))
# 
# 
# # made a couple of extra/separate dfs to manipulate later on during the pivots and joins 
# final_df.2 <- relalt_csciasci %>%
#   left_join(arroyo_mean, by = "COMID")  %>%
#   dplyr::select(-c(DeltaFFM, Bio_threshold, Negative, Positive, FullMetricName, Classification, Prox_Positive, Prox_Negative, Threshold)) %>%
#   dplyr::rename(Probability_Threshold = probs) %>%
#   mutate(IndexMetric = paste(index, Hydro_endpoint, sep= "_")) %>%
#   dplyr::select(-c(index))
# 
# # this df was made to fix issues with duplicate csci/asci relative alteration values for q99 - joined later in "deliverable_df3" 
# q99_df <- relalt_csciasci %>%
#   left_join(arroyo_mean, by = "COMID")  %>%
#   dplyr::select(-c(DeltaFFM, Bio_threshold, Negative, Positive, FullMetricName, Classification, Prox_Positive, Prox_Negative, Threshold)) %>%
#   dplyr::rename(Probability_Threshold = probs) %>%
#   mutate(IndexMetric = paste(index, Hydro_endpoint, sep= "_")) %>%
#   dplyr::select(c(COMID, Probability_Threshold, Hydro_endpoint, Relative_Alteration)) %>%
#   filter(Hydro_endpoint == "q99") %>%
#   distinct() %>%
#   pivot_wider(names_from = Hydro_endpoint, values_from = Relative_Alteration, names_prefix = "II.Rel_Alt_")
# 
# # re-ordering columns for visualizing - getting closer to final product
# new_order = c("COMID", "Relative_Alteration", "PredictedProbabilityScaled", "IndexMetric", "ThreshIndexCode", "MeanProbToad", "Probability_Threshold", 
#               "SatisfiedProbability", "Results")
# 
# final_df <-  final_df[, new_order]
# 
# 
# # had to do several pivots/joins to make final product
# # there was probably a better way to do this but this is what made the most sense to me 
# deliverable_df1 <- final_df %>%
#   dplyr::select(-c(SatisfiedProbability, Relative_Alteration, MeanProbToad, ThreshIndexCode, PredictedProbabilityScaled)) %>%
#   pivot_wider(names_from = IndexMetric, values_from = Results, names_prefix = "V.Result_")
# 
# deliverable_df2 <- final_df %>%
#   dplyr::select(-c(SatisfiedProbability, Relative_Alteration, MeanProbToad, ThreshIndexCode, Results)) %>%
#   pivot_wider(names_from = IndexMetric, values_from = PredictedProbabilityScaled, names_prefix = "IV.Prob_")
# 
# # had to do a work around to get unique values for q99 (had to make new df for this which is the "q99_df" from above)
# # doing a join with q99 df to fix problem of duplicates in q99 column - cleaned 
# deliverable_df3 <- final_df.2 %>%
#   dplyr::select(-c(SatisfiedProbability,  MeanProbToad, ThreshIndexCode, Results, PredictedProbabilityScaled, ThreshIndexCode, IndexMetric)) %>%
#   filter(Hydro_endpoint != "q99") %>%
#   pivot_wider(names_from = Hydro_endpoint, values_from = Relative_Alteration, names_prefix = "II.Rel_Alt_") %>%
#   left_join(q99_df, by = c("COMID", "Probability_Threshold"))
# 
# deliverable_df4 <- final_df %>%
#   dplyr::select(c(COMID, MeanProbToad, Probability_Threshold))  
# 
# ## the final df that will go into the making of the shp file 
# final_product <- deliverable_df1 %>%
#   left_join(deliverable_df2, by = c("COMID", "Probability_Threshold")) %>%
#   left_join(deliverable_df3, by = c("COMID", "Probability_Threshold")) %>%
#   left_join(deliverable_df4, by = c("COMID", "Probability_Threshold")) %>%
#   distinct() %>%
#   dplyr::rename(`IV.Prob_Toad_Mean` = MeanProbToad, `I.COMID` = COMID, `III.Probability_Threshold` = Probability_Threshold) %>%
#   mutate(`IV.Prob_ASCI_q99` = round(`IV.Prob_ASCI_q99`, digits = 2), `IV.Prob_ASCI_ds_mag_50` = round(`IV.Prob_ASCI_ds_mag_50`, digits = 2), 
#          `IV.Prob_CSCI_q99` = round(`IV.Prob_CSCI_q99`, digits = 2), `IV.Prob_CSCI_wet_bfl_mag_10` = round(`IV.Prob_CSCI_wet_bfl_mag_10`, digits = 2), 
#          `IV.Prob_Toad_Mean` = round(`IV.Prob_Toad_Mean`, digits = 2))
# 
# # the final order for the deliverable 
# new_order = c("I.COMID", "II.Rel_Alt_q99", "II.Rel_Alt_ds_mag_50", "II.Rel_Alt_wet_bfl_mag_10", "III.Probability_Threshold",  
#               "IV.Prob_ASCI_q99", "IV.Prob_ASCI_ds_mag_50", "IV.Prob_CSCI_q99", "IV.Prob_CSCI_wet_bfl_mag_10", "IV.Prob_Toad_Mean", 
#               "V.Result_ASCI_q99", "V.Result_ASCI_ds_mag_50", "V.Result_CSCI_q99", "V.Result_CSCI_wet_bfl_mag_10")
# 
# ## the actual final, finished product that goes into making the shp file  
# final_product <-  final_product[, new_order]

# write.csv(final_product, "C:/Users/racheld/Downloads/RiskFramework_Data.csv")

# Shp file ----------------------------------------------------------------
### making shp file 
#re-read in csv due to shpfile making problems 
final_SOC <- read.csv("C:/Users/racheld/Downloads/SOC_RiskFramework_Data_Final.csv")

#renaming variables for shpfile because ArcGIS truncates it 
final_SOC <- final_SOC %>%
  rename("site" = "I.Site", 
         "DSDurWS" =  "II.DeltaFFM_DS_Dur_WS", 
         "DSMag50" = "II.DeltaFFM_DS_Mag_50",
         "Q99" = "II.DeltaFFM_Q99", 
         "SPDur" = "II.DeltaFFM_SP_Dur", 
         "SP_Tim" = "II.DeltaFFM_SP_Tim",
         "WetBFL10" = "II.DeltaFFM_Wet_BFL_Mag_10", 
         "PrbThrsh" = "III.Threshold", 
         "PbADsDur" = "IV.Prob_ASCI_DS_Dur_WS",
         "PbADsMg50" = "IV.Prob_ASCI_DS_Mag_50", 
         "PbAQ99" = "IV.Prob_ASCI_Q99", 
         "PbASpDur" = "IV.Prob_ASCI_SP_Dur",
         "PbCDsDur" = "IV.Prob_CSCI_DS_Dur_WS", 
         "PbCQ99" = "IV.Prob_CSCI_Q99", 
         "PbCSpTim" = "IV.Prob_CSCI_SP_Tim", 
         "PCWtBFL10" = "IV.Prob_CSCI_Wet_BFL_Mag_10", 
         "RADSDur" = "V.Result_ASCI_DS_Dur_WS", 
         "RADSMag50" = "V.Result_ASCI_DS_Mag_50", 
         "RAQ99" = "V.Result_ASCI_Q99", 
         "RASPDur" = "V.Result_ASCI_SP_Dur", 
         "RCDSDur" = "V.Result_CSCI_DS_Dur_WS", 
         "RCQ99" = "V.Result_CSCI_Q99", 
         "RCSPTim" = "V.Result_CSCI_SP_Tim", 
         "RCWetBFL10" = "V.Result_CSCI_Wet_BFL_Mag_10") %>%
  dplyr::select(-c(Column.Name, Lookup))

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
sf::st_write(SOC_deliverable_shp_file, "output_data/South_OC_RiskFramework.shp", 
             driver = "ESRI Shapefile", append = FALSE)






