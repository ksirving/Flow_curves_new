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
lookup_table <- read_csv("output_data/01_ALL_delta_thresholds_scaled.csv") 

### read in current predicted probabilities - data set with all mag metrics in one df
current_predicted_prob <- read_csv("output_data/07a_predicted_probability.csv") 

############################################################################################################################################
# filter look up table for what we want and reformat it 

lookuptable <- lookup_table %>%
  filter(metric == "H_ASCI_Q99_0.86_Positive" | metric == "H_ASCI_Q99_0.86_Negative" | metric == "H_ASCI_DS_Mag_50_0.86_Positive"
         | metric == "H_ASCI_DS_Mag_50_0.86_Negative" | metric == "CSCI_Q99_0.79_Positive" | metric == "CSCI_Q99_0.79_Negative" 
         | metric == "CSCI_Wet_BFL_Mag_10_0.79_Positive" | metric == "CSCI_Wet_BFL_Mag_10_0.79_Negative") %>%
  dplyr::select(-c(...1, X, Bio_endpoint, n, metric)) %>%
  pivot_longer(Threshold70:Threshold99, names_to = "Threshold", values_to = "value") %>%
  group_by(Hydro_endpoint, Threshold) %>%
  pivot_wider(names_from = "Type", values_from = "value") %>%
  mutate(ThreshIndexCode = paste(Biol, Hydro_endpoint, Threshold, sep= "_")) %>%
  mutate(IndexCode = paste(Biol, Hydro_endpoint, sep= "_"))
  

#### edit the combined database table & join to look up table
current_pred_prob <- current_predicted_prob %>%
  dplyr::select(-c(comid_wy, wayr, WYT, Scenario, abs_FFM_median_cfs)) %>%
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
  mutate(Results = ifelse(Classification == "Within", paste("Current Delta FFM is within limits. Proposed project should not increase by more than", Prox_Positivev2, "cfs or decrease by more than",  abs(Prox_Negativev2), "cfs."),
                          ifelse(Classification == "Augmented", paste("Current Delta FFM is augmented. Proposed project can decrease by", 
                                                                      abs(Prox_Positivev2), "to",  abs(Prox_Negativev2), "cfs."), 
                                 ifelse(Classification == "Depleted", paste("Current Delta FFM is depleted. Proposed project can increase by", 
                                                                             Prox_Negativev2, "to",  Prox_Positivev2, "cfs."), "NA")))) %>%
  dplyr::select(-c(Prox_Positivev2, Prox_Negativev2))


# write.csv(final_dfv2, "C:/Users/racheld/OneDrive - SCCWRP/Desktop/prox_threshold.csv")


############################################################################
# Shp file for 2nd deliverable  -------------------------------------------
# Based off data above 

# reading in needed file for this sections 
# arroyo_toad <- read.csv("input_data/08_Arroyo_Toad_Prob_Occurrence_RB9.csv") # old csv
arroyo_toad <- read.csv("input_data/NHD_Toads_prob.csv")

relative_alteration <- read.csv("input_data/08_Relative_Alteration_FFM_RB9.csv")


### DATAFRAME 1
## adding yes/no column based on whether threshold was satisfied or not & cleaning up columns

csci_asci_df <- final_dfv2 %>%
  mutate(probs = ifelse(Threshold == "Threshold70", "0.70", 
                        ifelse(Threshold == "Threshold90", "0.90", 
                               ifelse(Threshold == "Threshold99", "0.99", "Bad")))) %>%
  mutate(SatisfiedProbability = ifelse(PredictedProbabilityScaled >= probs, "Yes", "No")) %>%
  dplyr::select(-c(PredictedProbability)) %>% # took out , index, Threshold, probs
  rename(DeltaFFM = hydro)

csci_asci_df$Hydro_endpoint <- tolower(csci_asci_df$Hydro_endpoint)

### join relative alteration to csci/asci df 
relalt_csciasci <- csci_asci_df %>%
  left_join(relative_alteration, by = c('Hydro_endpoint' = 'metric', 'COMID' = 'comid')) %>% 
  rename(FullMetricName = title_name2)

### DATAFRAME 2
### joining and cleaning arroyo toad df
arroyo_toadv2 <- arroyo_toad %>%
  dplyr::select(c(MeanProb, COMID))

# taking mean of MeanProb
arroyo_mean <- arroyo_toadv2 %>%
  group_by(COMID) %>%
  summarise(MeanProbToad = mean(MeanProb)) %>%
  ungroup

### DATAFRAME 3
## joining toad data to dataframe 2  - this final version sans pivot 
final_df <- relalt_csciasci %>%
  left_join(arroyo_mean, by = "COMID")  %>%
  dplyr::select(-c(DeltaFFM, Bio_threshold, Negative, Positive, FullMetricName, Classification, Prox_Positive, Prox_Negative, Threshold)) %>%
  rename(Probability_Threshold = probs) %>%
  mutate(IndexMetric = paste(index, Hydro_endpoint, sep= "_")) %>%
  dplyr::select(-c(index, Hydro_endpoint))


# made a couple of extra/separate dfs to manipulate later on during the pivots and joins 
final_df.2 <- relalt_csciasci %>%
  left_join(arroyo_mean, by = "COMID")  %>%
  dplyr::select(-c(DeltaFFM, Bio_threshold, Negative, Positive, FullMetricName, Classification, Prox_Positive, Prox_Negative, Threshold)) %>%
  dplyr::rename(Probability_Threshold = probs) %>%
  mutate(IndexMetric = paste(index, Hydro_endpoint, sep= "_")) %>%
  dplyr::select(-c(index))

# this df was made to fix issues with duplicate csci/asci relative alteration values for q99 - joined later in "deliverable_df3" 
q99_df <- relalt_csciasci %>%
  left_join(arroyo_mean, by = "COMID")  %>%
  dplyr::select(-c(DeltaFFM, Bio_threshold, Negative, Positive, FullMetricName, Classification, Prox_Positive, Prox_Negative, Threshold)) %>%
  dplyr::rename(Probability_Threshold = probs) %>%
  mutate(IndexMetric = paste(index, Hydro_endpoint, sep= "_")) %>%
  dplyr::select(c(COMID, Probability_Threshold, Hydro_endpoint, Relative_Alteration)) %>%
  filter(Hydro_endpoint == "q99") %>%
  distinct() %>%
  pivot_wider(names_from = Hydro_endpoint, values_from = Relative_Alteration, names_prefix = "II.Rel_Alt_")

# re-ordering columns for visualizing - getting closer to final product
new_order = c("COMID", "Relative_Alteration", "PredictedProbabilityScaled", "IndexMetric", "ThreshIndexCode", "MeanProbToad", "Probability_Threshold", 
              "SatisfiedProbability", "Results")

final_df <-  final_df[, new_order]


# had to do several pivots/joins to make final product
# there was probably a better way to do this but this is what made the most sense to me 
deliverable_df1 <- final_df %>%
  dplyr::select(-c(SatisfiedProbability, Relative_Alteration, MeanProbToad, ThreshIndexCode, PredictedProbabilityScaled)) %>%
  pivot_wider(names_from = IndexMetric, values_from = Results, names_prefix = "V.Result_")

deliverable_df2 <- final_df %>%
  dplyr::select(-c(SatisfiedProbability, Relative_Alteration, MeanProbToad, ThreshIndexCode, Results)) %>%
  pivot_wider(names_from = IndexMetric, values_from = PredictedProbabilityScaled, names_prefix = "IV.Prob_")

# had to do a work around to get unique values for q99 (had to make new df for this which is the "q99_df" from above)
# doing a join with q99 df to fix problem of duplicates in q99 column - cleaned 
deliverable_df3 <- final_df.2 %>%
  dplyr::select(-c(SatisfiedProbability,  MeanProbToad, ThreshIndexCode, Results, PredictedProbabilityScaled, ThreshIndexCode, IndexMetric)) %>%
  filter(Hydro_endpoint != "q99") %>%
  pivot_wider(names_from = Hydro_endpoint, values_from = Relative_Alteration, names_prefix = "II.Rel_Alt_") %>%
  left_join(q99_df, by = c("COMID", "Probability_Threshold"))

deliverable_df4 <- final_df %>%
  dplyr::select(c(COMID, MeanProbToad, Probability_Threshold))  

## the final df that will go into the making of the shp file 
final_product <- deliverable_df1 %>%
  left_join(deliverable_df2, by = c("COMID", "Probability_Threshold")) %>%
  left_join(deliverable_df3, by = c("COMID", "Probability_Threshold")) %>%
  left_join(deliverable_df4, by = c("COMID", "Probability_Threshold")) %>%
  distinct() %>%
  dplyr::rename(`IV.Prob_Toad_Mean` = MeanProbToad, `I.COMID` = COMID, `III.Probability_Threshold` = Probability_Threshold) %>%
  mutate(`IV.Prob_ASCI_q99` = round(`IV.Prob_ASCI_q99`, digits = 2), `IV.Prob_ASCI_ds_mag_50` = round(`IV.Prob_ASCI_ds_mag_50`, digits = 2), 
         `IV.Prob_CSCI_q99` = round(`IV.Prob_CSCI_q99`, digits = 2), `IV.Prob_CSCI_wet_bfl_mag_10` = round(`IV.Prob_CSCI_wet_bfl_mag_10`, digits = 2), 
         `IV.Prob_Toad_Mean` = round(`IV.Prob_Toad_Mean`, digits = 2))

# the final order for the deliverable 
new_order = c("I.COMID", "II.Rel_Alt_q99", "II.Rel_Alt_ds_mag_50", "II.Rel_Alt_wet_bfl_mag_10", "III.Probability_Threshold",  
              "IV.Prob_ASCI_q99", "IV.Prob_ASCI_ds_mag_50", "IV.Prob_CSCI_q99", "IV.Prob_CSCI_wet_bfl_mag_10", "IV.Prob_Toad_Mean", 
              "V.Result_ASCI_q99", "V.Result_ASCI_ds_mag_50", "V.Result_CSCI_q99", "V.Result_CSCI_wet_bfl_mag_10")

## the actual final, finished product that goes into making the shp file  
final_product <-  final_product[, new_order]

write.csv(final_product, "C:/Users/racheld/Downloads/final_product.csv")

#################################################################################################
# Adding columns that I missed from relative alteration to final DF

## read in final data set
to_edit <- read.csv("C:/Users/racheld/Downloads/final_product.csv")

# reformatting table 
relative_alteration_edited <- relative_alteration %>% 
  dplyr::select(comid, metric, Relative_Alteration) %>% 
  pivot_wider(names_from = metric, values_from = Relative_Alteration, names_prefix = "II.Rel_Alt_") %>% 
  dplyr::select(-c(II.Rel_Alt_q99, II.Rel_Alt_ds_mag_50, II.Rel_Alt_wet_bfl_mag_10)) %>% 
  right_join(to_edit, by = c('comid' = 'I.COMID')) %>%
  rename(`I.COMID` = comid)

relative_alteration_edited_order = c("I.COMID", "II.Rel_Alt_fa_mag", "II.Rel_Alt_wet_bfl_mag_10", "II.Rel_Alt_wet_bfl_mag_50", "II.Rel_Alt_peak_2",  "II.Rel_Alt_peak_5", "II.Rel_Alt_peak_10",
              "II.Rel_Alt_q99",  "II.Rel_Alt_sp_mag", "II.Rel_Alt_ds_mag_50", "III.Probability_Threshold", "IV.Prob_ASCI_q99", "IV.Prob_ASCI_ds_mag_50", "IV.Prob_CSCI_q99", 
              "IV.Prob_CSCI_wet_bfl_mag_10", "IV.Prob_Toad_Mean", "V.Result_ASCI_q99", "V.Result_ASCI_ds_mag_50", "V.Result_CSCI_q99", "V.Result_CSCI_wet_bfl_mag_10")

## the actual final, finished product that goes into making the shp file  
relative_alteration_edited <-  relative_alteration_edited[, relative_alteration_edited_order]

write.csv(relative_alteration_edited, "C:/Users/racheld/Downloads/RiskFramework_Data_Final.csv", row.names=FALSE)


 

# Shp file ----------------------------------------------------------------
### making shp file 
#re-read in csv due to shpfile making problems 
relative_alteration_edited <- read.csv("C:/Users/racheld/Downloads/RiskFramework_Data_Final.csv")

#renaming variables for shpfile because ArcGIS truncates it 
relative_alteration_edited <- relative_alteration_edited %>%
  rename("COMID" = "I.COMID", "AltFaMag" = "II.Rel_Alt_fa_mag", "AltWBFL10" = "II.Rel_Alt_wet_bfl_mag_10", "AltWBFL50" = "II.Rel_Alt_wet_bfl_mag_50", 
         "AltPeak2" = "II.Rel_Alt_peak_2", "AltPeak5" = "II.Rel_Alt_peak_5", "AltPeak10" = "II.Rel_Alt_peak_10", "AltQ99" = "II.Rel_Alt_q99", "AltSpMag" = "II.Rel_Alt_sp_mag", 
         "AltDsMag50" = "II.Rel_Alt_ds_mag_50", "ProbThld" = "III.Probability_Threshold", "ProbAQ99" = "IV.Prob_ASCI_q99",
         "ProbADM50" = "IV.Prob_ASCI_ds_mag_50", "ProbCQ99" = "IV.Prob_CSCI_q99", "ProbCWBM10" = "IV.Prob_CSCI_wet_bfl_mag_10",
         "PrbTdMn" = "IV.Prob_Toad_Mean", "ResAQ99" = "V.Result_ASCI_q99", "ResADM50" = "V.Result_ASCI_ds_mag_50",
         "ResCQ99" = "V.Result_CSCI_q99", "ResCWBM10" = "V.Result_CSCI_wet_bfl_mag_10")

## projection
prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# load RB9 shp
calinhd <- readOGR('/Users/racheld/SCCWRP/SD Hydro Vulnerability Assessment - General/Data/SpatialData/NHDplus_RB9.shp') %>%
  spTransform(prj) %>%
  st_as_sf %>%
  st_simplify(dTolerance = 0.5, preserveTopology = T)
# unique(calinhd$COMID)

# fortified calinhd, joind with delta
# merge data set with comids to get spatial data
SynthNHD <- calinhd %>%
  filter(COMID %in% unique(relative_alteration_edited$COMID)) %>%
  dplyr::select(COMID) %>% ## 46
  as('Spatial') %>% 
  st_as_sf(coords = c("long", "lat"), remove = FALSE)

## shp file join
deliverable_shp_file_january <-  relative_alteration_edited %>%
  full_join(SynthNHD, by = "COMID")


## write out the shapefile 
sf::st_write(deliverable_shp_file_january, "output_data/RiskFramework_Data_Final.shp", 
              driver = "ESRI Shapefile", append = FALSE)


