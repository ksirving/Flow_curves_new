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
lookup_table <- read_csv("output_data/Manuscript/07_ALL_delta_thresholds_scaled_updated.csv") %>% 
  mutate(metric = paste(Bio_endpoint, "_", Hydro_endpoint, "_", Bio_threshold, "_", Type, sep = "")) #edited by Rachel 9/7

### read in current predicted probabilities - data set with all mag metrics in one df
current_predicted_prob <- read_csv("output_data/Manuscript/07b_predicted_probability.csv") 

############################################################################################################################################
# filter look up table for what we want and reformat it 

lookuptable <- lookup_table %>%
  # filter(metric == "H_ASCI_Q99_0.86_Positive" | metric == "H_ASCI_Q99_0.86_Negative" | metric == "H_ASCI_DS_Mag_50_0.86_Positive"
  #        | metric == "H_ASCI_DS_Mag_50_0.86_Negative" | metric == "CSCI_Q99_0.79_Positive" | metric == "CSCI_Q99_0.79_Negative" 
  #        | metric == "CSCI_Wet_BFL_Mag_10_0.79_Positive" | metric == "CSCI_Wet_BFL_Mag_10_0.79_Negative") %>%
  filter(metric %in% c("H_ASCI_Peak_2_0.86_Positive", "H_ASCI_Peak_2_0.86_Negative", "H_ASCI_SP_Mag_0.86_Positive", "H_ASCI_SP_Mag_0.86_Negative", 
                       "H_ASCI_Wet_BFL_Mag_50_0.86_Positive", "H_ASCI_Wet_BFL_Mag_50_0.86_Negative", "H_ASCI_DS_Mag_50_0.86_Positive", "H_ASCI_DS_Mag_50_0.86_Negative", 
                       "CSCI_DS_Mag_50_0.79_Positive", "CSCI_DS_Mag_50_0.79_Negative", "CSCI_Peak_10_0.79_Positive", "CSCI_Peak_10_0.79_Negative", 
                       "CSCI_FA_Mag_0.79_Positive", "CSCI_FA_Mag_0.79_Negative", "CSCI_Wet_BFL_Mag_50_0.79_Positive", "CSCI_Wet_BFL_Mag_50_0.79_Negative")) %>% 
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
  # Rachel edited 9/13
  mutate(Classification = case_when(hydro <= Positive & hydro >= Negative ~ "Within", 
                                    hydro > Positive ~ "Augmented", 
                                    hydro < Negative ~ "Depleted",
                                    is.na(PredictedProbability) ~ "Indeterminant", 
                                    str_detect(Hydro_endpoint, "^Peak") & hydro < 0 & hydro > Negative ~ "Within",
                                    T ~ "Wrong")) %>% 
  
  # mutate(Classification = ifelse(hydro <= Positive & hydro >= Negative, "Within", 
  #                                ifelse(hydro > Positive, "Augmented", 
  #                                       ifelse(hydro < Negative, "Depleted", "Bad")))) %>%
  
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
  mutate(Results = case_when(Classification == "Within" ~ paste("Current Delta FFM is within limits. Proposed project should not increase by more than", Prox_Positivev2, "cfs or decrease by more than",  abs(Prox_Negativev2), "cfs."),
                             Classification == "Augmented" ~ paste("Current Delta FFM is augmented. Proposed project can decrease by", abs(Prox_Positivev2), "to",  abs(Prox_Negativev2), "cfs."),
                             Classification == "Depleted" ~ paste("Current Delta FFM is depleted. Proposed project can increase by", Prox_Negativev2, "to",  Prox_Positivev2, "cfs."),
                             Classification == "Indeterminant" ~ "A probability of <Null> indicates augmented peak metric.  Not enough data for flow ecology curve peak augmentation.",
                             T ~ "Bad"
                             )) %>% 
  mutate(Results = case_when(Classification == "Within" & str_detect(Hydro_endpoint, "^Peak") ~ paste("Current Delta FFM is within limits. Proposed project should not decrease by more than", abs(Prox_Negativev2), "cfs and can increase by up to", abs(round(hydro,  digits = 2)), "cfs. However, any increase beyond", abs(round(hydro,  digits = 2)), "cfs is uncertain."),
                             Classification == "Depleted" & str_detect(Hydro_endpoint, "^Peak") ~ paste("Current Delta FFM is depleted. Proposed project can increase by", abs(Prox_Negativev2), "to", abs(round(hydro,  digits = 2)), "cfs. However, any increase beyond", abs(round(hydro,  digits = 2)), "cfs is uncertain."),
                             T ~ as.character(Results))) %>% 
  
  
  # old code 
  # mutate(Results = ifelse(Classification == "Within", paste("Current Delta FFM is within limits. Proposed project should not increase by more than", Prox_Positivev2, "cfs or decrease by more than",  abs(Prox_Negativev2), "cfs."),
  #                         ifelse(Classification == "Augmented", paste("Current Delta FFM is augmented. Proposed project can decrease by", 
  #                                                                     abs(Prox_Positivev2), "to",  abs(Prox_Negativev2), "cfs."), 
  #                                ifelse(Classification == "Depleted", paste("Current Delta FFM is depleted. Proposed project can increase by", 
  #                                                                            Prox_Negativev2, "to",  Prox_Positivev2, "cfs."),
  #                                       # Rachel added lines below for Peak edits 9/14/2023
  #                                       ifelse(Classification == "Indeterminant", "A probability of 'NA' indicates augmented peak metric.  Not enough data for flow ecology curve peak augmentation.", 
  #                                              ifelse(Classification == "Within" & str_detect(Hydro_endpoint, "^Peak"), paste("Current Delta FFM is within limits. Proposed project should not decrease by more than", Prox_Negativev2, 
  #                                                                                                                             "cfs and can increase by up to", abs(hydro), ". However, any increase beyond", abs(hydro), "cfs is uncertain."), "Bad")))))) %>%
  dplyr::select(-c(Prox_Positivev2, Prox_Negativev2))

# 
# write.csv(final_dfv2, "C:/Users/racheld/Downloads/prox_threshold.csv", row.names = FALSE)
# 
# write.csv(current_pred_prob, "C:/Users/racheld/Downloads/current_pred_prob.csv")
############################################################################
# Shp file for 2nd deliverable  -------------------------------------------
# Based off data above 

# reading in needed file for this section
# arroyo_toad <- read.csv("input_data/08_Arroyo_Toad_Prob_Occurrence_RB9.csv") # old csv
arroyo_toad <- read.csv("input_data/NHD_Toads_prob.csv")

relative_alteration <- read.csv("input_data/08_Relative_Alteration_FFM_RB9.csv") # leaving this naming since this is a sheet Kris gave me



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
#### EDIT 9/7 by Rachel - this was a problem from the last run of data
# q99_df <- relalt_csciasci %>%
#   left_join(arroyo_mean, by = "COMID")  %>%
#   dplyr::select(-c(DeltaFFM, Bio_threshold, Negative, Positive, FullMetricName, Classification, Prox_Positive, Prox_Negative, Threshold)) %>%
#   dplyr::rename(Probability_Threshold = probs) %>%
#   mutate(IndexMetric = paste(index, Hydro_endpoint, sep= "_")) %>%
#   dplyr::select(c(COMID, Probability_Threshold, Hydro_endpoint, Relative_Alteration)) %>%
#   # filter(Hydro_endpoint == "q99") %>%
#   distinct() %>%
#   pivot_wider(names_from = Hydro_endpoint, values_from = Relative_Alteration, names_prefix = "II.Rel_Alt_")

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
# deliverable_df3 <- final_df.2 %>%
#   dplyr::select(-c(SatisfiedProbability,  MeanProbToad, ThreshIndexCode, Results, PredictedProbabilityScaled, ThreshIndexCode, IndexMetric)) %>%
#   # filter(Hydro_endpoint != "q99") %>%
#   # pivot_wider(names_from = Hydro_endpoint, values_from = Relative_Alteration, names_prefix = "II.Rel_Alt_") #%>%
#   left_join(q99_df, by = c("COMID", "Probability_Threshold"))

deliverable_df3 <- relalt_csciasci %>%
  left_join(arroyo_mean, by = "COMID")  %>%
  dplyr::select(-c(DeltaFFM, Bio_threshold, Negative, Positive, FullMetricName, Classification, Prox_Positive, Prox_Negative, Threshold)) %>%
  dplyr::rename(Probability_Threshold = probs) %>%
  mutate(IndexMetric = paste(index, Hydro_endpoint, sep= "_")) %>%
  dplyr::select(c(COMID, Probability_Threshold, Hydro_endpoint, Relative_Alteration)) %>%
  # filter(Hydro_endpoint == "q99") %>%
  distinct() %>%
  pivot_wider(names_from = Hydro_endpoint, values_from = Relative_Alteration, names_prefix = "II.Rel_Alt_")

deliverable_df4 <- final_df %>%
  dplyr::select(c(COMID, MeanProbToad, Probability_Threshold))  

## the final df that will go into the making of the shp file 
final_product <- deliverable_df1 %>%
  left_join(deliverable_df2, by = c("COMID", "Probability_Threshold")) %>%
  left_join(deliverable_df3, by = c("COMID", "Probability_Threshold")) %>%
  left_join(deliverable_df4, by = c("COMID", "Probability_Threshold")) %>%
  distinct() %>%
  dplyr::rename(`IV.Prob_Toad_Mean` = MeanProbToad, `I.COMID` = COMID, `III.Probability_Threshold` = Probability_Threshold) %>%
  mutate(`IV.Prob_ASCI_ds_mag_50` = round(`IV.Prob_ASCI_ds_mag_50`, digits = 2), 
         `IV.Prob_ASCI_peak_2` = round(`IV.Prob_ASCI_peak_2`, digits = 2), 
         `IV.Prob_ASCI_sp_mag` = round(`IV.Prob_ASCI_sp_mag`, digits = 2), 
         `IV.Prob_ASCI_wet_bfl_mag_50` = round(`IV.Prob_ASCI_wet_bfl_mag_50`, digits = 2), 
         `IV.Prob_CSCI_ds_mag_50` = round(`IV.Prob_CSCI_ds_mag_50`, digits = 2), 
         `IV.Prob_CSCI_fa_mag` = round(`IV.Prob_CSCI_fa_mag`, digits = 2), 
         `IV.Prob_CSCI_peak_10` = round(`IV.Prob_CSCI_peak_10`, digits = 2), 
         `IV.Prob_CSCI_wet_bfl_mag_50` = round(`IV.Prob_CSCI_wet_bfl_mag_50`, digits = 2), 
         `IV.Prob_Toad_Mean` = round(`IV.Prob_Toad_Mean`, digits = 2))

# the final order for the deliverable 
# new_order = c("I.COMID", "II.Rel_Alt_q99", "II.Rel_Alt_ds_mag_50", "II.Rel_Alt_wet_bfl_mag_10", "III.Probability_Threshold",  
#               "IV.Prob_ASCI_q99", "IV.Prob_ASCI_ds_mag_50", "IV.Prob_CSCI_q99", "IV.Prob_CSCI_wet_bfl_mag_10", "IV.Prob_Toad_Mean", 
#               "V.Result_ASCI_q99", "V.Result_ASCI_ds_mag_50", "V.Result_CSCI_q99", "V.Result_CSCI_wet_bfl_mag_10")

new_order <- c("I.COMID", "II.Rel_Alt_ds_mag_50", "II.Rel_Alt_peak_2", "II.Rel_Alt_sp_mag", "II.Rel_Alt_wet_bfl_mag_50", 
               "II.Rel_Alt_fa_mag", "II.Rel_Alt_peak_10", "III.Probability_Threshold", "IV.Prob_ASCI_ds_mag_50", "IV.Prob_ASCI_peak_2",         
               "IV.Prob_ASCI_sp_mag", "IV.Prob_ASCI_wet_bfl_mag_50", "IV.Prob_CSCI_ds_mag_50", "IV.Prob_CSCI_fa_mag", "IV.Prob_CSCI_peak_10", 
               "IV.Prob_CSCI_wet_bfl_mag_50", "IV.Prob_Toad_Mean", "V.Result_ASCI_ds_mag_50", "V.Result_ASCI_peak_2", "V.Result_ASCI_sp_mag", 
               "V.Result_ASCI_wet_bfl_mag_50", "V.Result_CSCI_ds_mag_50", "V.Result_CSCI_fa_mag", "V.Result_CSCI_peak_10", "V.Result_CSCI_wet_bfl_mag_50")

## the actual final, finished product that goes into making the shp file  
final_product <-  final_product[, new_order]

write.csv(final_product, "output_data/Manuscript/09_final_product.csv", row.names = FALSE)

#################################################################################################
# Adding columns that I missed from relative alteration to final DF

## read in final data set
to_edit <- read.csv("output_data/Manuscript/09_final_product.csv")

# reformatting table 
relative_alteration_edited <- relative_alteration %>% 
  dplyr::select(comid, metric, Relative_Alteration) %>% 
  pivot_wider(names_from = metric, values_from = Relative_Alteration, names_prefix = "II.Rel_Alt_") %>% 
  dplyr::select(-c(II.Rel_Alt_ds_mag_50, II.Rel_Alt_peak_2, II.Rel_Alt_sp_mag, II.Rel_Alt_wet_bfl_mag_50, II.Rel_Alt_fa_mag, II.Rel_Alt_peak_10)) %>% 
  right_join(to_edit, by = c('comid' = 'I.COMID')) %>%
  rename(`I.COMID` = comid)

relative_alteration_edited_order = c("I.COMID", "II.Rel_Alt_ds_mag_50", "II.Rel_Alt_peak_2", "II.Rel_Alt_sp_mag", "II.Rel_Alt_wet_bfl_mag_50", 
                                     "II.Rel_Alt_fa_mag", "II.Rel_Alt_peak_10", "II.Rel_Alt_peak_5", "II.Rel_Alt_wet_bfl_mag_10", "II.Rel_Alt_q99",
                                     "III.Probability_Threshold", "IV.Prob_ASCI_ds_mag_50", "IV.Prob_ASCI_peak_2", "IV.Prob_ASCI_sp_mag", "IV.Prob_ASCI_wet_bfl_mag_50", 
                                     "IV.Prob_CSCI_ds_mag_50", "IV.Prob_CSCI_fa_mag", "IV.Prob_CSCI_peak_10", "IV.Prob_CSCI_wet_bfl_mag_50", "IV.Prob_Toad_Mean", "V.Result_ASCI_ds_mag_50", 
                                     "V.Result_ASCI_peak_2", "V.Result_ASCI_sp_mag", "V.Result_ASCI_wet_bfl_mag_50", "V.Result_CSCI_ds_mag_50", "V.Result_CSCI_fa_mag", "V.Result_CSCI_peak_10", 
                                     "V.Result_CSCI_wet_bfl_mag_50")

## the actual final, finished product that goes into making the shp file  
relative_alteration_edited <-  relative_alteration_edited[, relative_alteration_edited_order]


# For CSV -----------------------------------------------------------------
relative_alteration_edited_forCSV <- relative_alteration %>% 
  dplyr::select(comid, metric, Relative_Alteration) %>% 
  pivot_wider(names_from = metric, values_from = Relative_Alteration, names_prefix = "II.Rel_Alt_") %>% 
  dplyr::select(-c(II.Rel_Alt_ds_mag_50, II.Rel_Alt_peak_2, II.Rel_Alt_sp_mag, II.Rel_Alt_wet_bfl_mag_50, II.Rel_Alt_fa_mag, II.Rel_Alt_peak_10)) %>% 
  right_join(to_edit, by = c('comid' = 'I.COMID')) %>%
  rename(`I.COMID` = comid) %>% 
  # edited by Rachel 10/2 - adding NAs instead of "Null" for the CSV - comment in or out if needed
  mutate(V.Result_ASCI_peak_2 = if_else(V.Result_ASCI_peak_2 == "A probability of <Null> indicates augmented peak metric.  Not enough data for flow ecology curve peak augmentation.",
                                        "A probability of NA indicates augmented peak metric. Not enough data for flow ecology curve peak augmentation.", as.character(V.Result_ASCI_peak_2))) %>%
  mutate(V.Result_CSCI_peak_10 = if_else(V.Result_CSCI_peak_10 == "A probability of <Null> indicates augmented peak metric.  Not enough data for flow ecology curve peak augmentation.",
                                        "A probability of NA indicates augmented peak metric. Not enough data for flow ecology curve peak augmentation.", as.character(V.Result_CSCI_peak_10))) #%>%
  
## the actual final, finished product that goes into making the csv
relative_alteration_edited_forCSV <-  relative_alteration_edited_forCSV[, relative_alteration_edited_order]

write.csv(relative_alteration_edited_forCSV, "output_data/Manuscript/RiskFramework_Data_Final.csv", row.names=FALSE)

# Shp file ----------------------------------------------------------------
### making shp file 
#re-read in csv due to shpfile making problems 
relative_alteration_edited <- read.csv("output_data/Manuscript/RiskFramework_Data_Final.csv")

#renaming variables for shpfile because ArcGIS truncates it 
relative_alteration_edited <- relative_alteration_edited %>%
  rename("COMID" = "I.COMID", "AltDM50" = "II.Rel_Alt_ds_mag_50", "AltP2" = "II.Rel_Alt_peak_2", "AltSpMag" = "II.Rel_Alt_sp_mag", "AltWBFL50" = "II.Rel_Alt_wet_bfl_mag_50",
         "AltFaMag" = "II.Rel_Alt_fa_mag", "AltP10" = "II.Rel_Alt_peak_10", "AltP5" = "II.Rel_Alt_peak_5", "AltWBFL10" = "II.Rel_Alt_wet_bfl_mag_10", "AltQ99" = "II.Rel_Alt_q99",
         "Prob_Thresh" = "III.Probability_Threshold", "PbASCIDM50" = "IV.Prob_ASCI_ds_mag_50", "PbASCIP2" = "IV.Prob_ASCI_peak_2", "PbASCISpMg" = "IV.Prob_ASCI_sp_mag", 
         "PbASCIBFL50" = "IV.Prob_ASCI_wet_bfl_mag_50", "PbCSCIDM50" = "IV.Prob_CSCI_ds_mag_50", "PbCSCIFMg" = "IV.Prob_CSCI_fa_mag", "PbCSCIP10" = "IV.Prob_CSCI_peak_10", "PbCSCIBFL50" = "IV.Prob_CSCI_wet_bfl_mag_50", 
         "ProbToadMn" = "IV.Prob_Toad_Mean", "ResADM50" = "V.Result_ASCI_ds_mag_50", "ResASCIP2" = "V.Result_ASCI_peak_2", "ResASCISMag" = "V.Result_ASCI_sp_mag", "ResASCIBFL50" = "V.Result_ASCI_wet_bfl_mag_50", 
         "ResCSCIDM50" = "V.Result_CSCI_ds_mag_50", "ResCSCIFMag" = "V.Result_CSCI_fa_mag", "ResCSCIP10" = "V.Result_CSCI_peak_10", "ResCSCIBFL50" = "V.Result_CSCI_wet_bfl_mag_50") %>% 
  # for shpfile pnly !!!!!!!!!!!!!!
  mutate(PbASCIP2 = if_else(is.na(PbASCIP2), -9999, as.numeric(PbASCIP2))) %>%
  mutate(PbCSCIP10 = if_else(is.na(PbCSCIP10), -9999, as.numeric(PbCSCIP10))) %>%
  mutate(ProbToadMn = if_else(is.na(ProbToadMn), -9999, as.numeric(ProbToadMn))) 


  # have to undo the edits made earlier for the CSV
relative_alteration_edited <- relative_alteration_edited %>% 
  mutate(ResASCIP2 = if_else(PbASCIP2 == -9999, "A probability of <Null> indicates augmented peak metric. Not enough data for flow ecology curve peak augmentation.", as.character(ResASCIP2))) %>%
  mutate(ResCSCIP10 = if_else(PbCSCIP10 == -9999, "A probability of <Null> indicates augmented peak metric.  Not enough data for flow ecology curve peak augmentation.", as.character(ResCSCIP10)))


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
sf::st_write(deliverable_shp_file_january, "output_data/Manuscript/Shapefiles/RiskFramework_Data_Final.shp", 
              driver = "ESRI Shapefile", append = FALSE)


