## projection into south OC study area

library(ggplot2)
library(dplyr)
library(tidyverse)



# Upload and format thresholds --------------------------------------------

thresholds <-read.csv( "output_data/Manuscript/07_ALL_delta_thresholds_scaled.csv")
head(thresholds)

## clean up df, make longer
thresholds <- thresholds %>%
  select(-X.1, -X, -n) %>%
  rename(Hydro_Metric = Hydro_endpoint) %>% 
  mutate(metric = paste0(Bio_endpoint, "_", Hydro_Metric, "_", Bio_threshold)) %>%
  pivot_longer(Threshold25:Threshold75, names_to = "Threshold") %>%
  rename(DeltaH = value) 

## make wider with type - pos/neg
thresholdsall <- thresholds %>%
  pivot_wider(names_from = Type, values_from = DeltaH)

## save data - all delta limits 
write.csv(thresholdsall, "output_data/Manuscript/08_all_delta_h_limits_all_combinations_thresh_combs.csv")

metrics <- unique(thresholdsall$Hydro_Metric)
metrics

head(thresholdsall)



# Upload SOC delta --------------------------------------------------------

## upload delta h 

delta <- read.csv("/Volumes/Biology-1/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/KTQ_flowalteration_assessment/00_Final_FFM_DeltaH_AH/SOC_deltaH_supp_final_12012021.csv")
head(delta)

## subset to only chosen metrics
delta <- delta %>%
  filter(flow_metric %in% c("Q99", "DS_Dur_WS", "SP_Tim", "SP_Dur"))

head(delta)

write.csv(delta, "output_data/Manuscript/08_delta_H_all_years_and_subbasins.csv")

## rename 
delta <- delta %>%
  rename(Hydro_Metric = flow_metric) #%>%

## join limits with delta data
delta_df <- full_join(delta, thresholdsall, by="Hydro_Metric")
head(delta_df)

## define alteration per subbasin, per year - within limits
delta_dfx <- delta_df %>%
  group_by(site, year, Hydro_Metric, Biol, Threshold, Bio_threshold) %>%
  mutate(Alteration_Current = ifelse(deltah_cur_ref_final <= Positive & deltah_cur_ref_final >= Negative, "Unaltered", "Altered")) %>%
  mutate(Alteration_WaterCon = ifelse(deltaH_watercon_ref_final <= Positive & deltaH_watercon_ref_final >= Negative, "Unaltered", "Altered"))


View(delta_dfx) 

write.csv(delta_dfx, "output_data/Manuscript/08_alteration_by_year_site_all_sites.csv")


# Alteration direction and percentage -------------------------------------

## current
names(delta_dfx)

##  direction per year
metric_tally_current_dir <- delta_dfx %>%
  group_by(site, Hydro_Metric, Biol, Threshold, Bio_threshold) %>%
  filter(Alteration_Current == "Altered") %>%
  mutate(Alteration_Current_direction = ifelse(deltah_cur_ref_final > Positive, "High", "Low")) %>%
  select(site, region, year, Hydro_Metric, Biol, Alteration_Current, Alteration_Current_direction)

write.csv(metric_tally_current_dir, "output_data/Manuscript/08_altered_metrics_direction_current_by_year.csv")
head(metric_tally_current_dir)

### count number of years altered - overall
metric_tally_current_dir <- delta_dfx %>%
  group_by(site, Hydro_Metric, Biol, Threshold, Bio_threshold) %>%
  filter(Alteration_Current == "Altered") %>%
  mutate(Alteration_Current_direction = ifelse(deltah_cur_ref_final > Positive, "High", "Low")) %>%
  count(Alteration_Current_direction) %>%
  mutate(Percentage = n/sum(n)*100) %>%
  select(-n) %>%
  pivot_wider(names_from = Alteration_Current_direction, values_from = Percentage) #%>%


## define overall altrration with criteria, between 40-60% both ways is uncertain, aobe 60% is high, beloe 40% is low
dirs <- seq(1, dim(metric_tally_current_dir)[1], 1)  
metric_tally_current_dir$Direction <- NULL

for(p in 1: length(dirs)) {
  
  metric_tally_current_dir$Direction[p] =  if(is.na(metric_tally_current_dir$High[p])) {
    paste("Low")
  } else if (metric_tally_current_dir$High[p] == 50) {
    paste("Uncertain")
  } else  if(metric_tally_current_dir$High[p] >= 60){
    paste("High")
  } else if (metric_tally_current_dir$High[p] <= 40) {
    paste("Low")
  } else if(is.na(metric_tally_current_dir$High[p])) {
    paste("Low")
  } else {
    paste("Uncertain")
  }
  
}

## save - direction of alteration overall per site
write.csv(metric_tally_current_dir, "output_data/Manuscript/08_altered_metrics_direction_current_overall.csv")



## count alteration by site from main delta df
metric_tally_current <- delta_dfx %>%
  group_by(site, Hydro_Metric, Biol, Threshold, Bio_threshold) %>%
  count(Alteration_Current)

metric_tally_current <- na.omit(metric_tally_current)
metric_tally_current

write.csv(metric_tally_current, "output_data/Manuscript/08_metric_suitability_tally_by_site_current.csv")

## make %
metric_tally_current <- metric_tally_current %>%
  group_by(site, Hydro_Metric, Biol, Threshold, Bio_threshold) %>%
  mutate(Percentage = n/sum(n)*100, YearsWithData = sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = Alteration_Current, values_from = Percentage)

metric_tally_current

YearsWithData <- metric_tally_current %>%
  ungroup() %>%
  select(site,Hydro_Metric, YearsWithData) %>%
  distinct()

YearsWithData

write.csv(YearsWithData , "output_data/Manuscript/08_sites_metrics_number_of_years_with_data.csv")

## replace NAs with zero - as 100% in other category
metric_tally_current[is.na(metric_tally_current)] <- 0

write.csv(metric_tally_current, "output_data/Manuscript/08_metric_suitability_tally_condensed_all_sites_current.csv")

## range of alteration
ranges <- metric_tally_current %>%
  group_by(Biol) %>%
  summarise(rg <- range(Altered))



# Water conservation ------------------------------------------------------

names(delta_dfx)
metric_tally_water_dir <- delta_dfx %>%
  group_by(site, Hydro_Metric, Biol, Threshold, Bio_threshold) %>%
  filter(Alteration_WaterCon == "Altered") %>%
  mutate(Alteration_WaterCon_direction = ifelse(deltaH_watercon_ref_final > Positive, "High", "Low"))  %>%
  select(site, region, year, Hydro_Metric, Biol, Alteration_WaterCon, Alteration_WaterCon_direction)

write.csv(metric_tally_water_dir, "output_data/Manuscript/08_altered_metrics_direction_watercon_by_year.csv")


### count number of years altered
metric_tally_water_dir <- delta_dfx %>%
  group_by(site, Hydro_Metric, Biol, Threshold, Bio_threshold) %>%
  filter(Alteration_WaterCon == "Altered") %>%
  mutate(Alteration_WaterCon_direction = ifelse(deltaH_watercon_ref_final > Positive, "High", "Low")) %>%
  count(Alteration_WaterCon_direction) %>%
  mutate(Percentage = n/sum(n)*100) %>%
  select(-n) %>%
  pivot_wider(names_from = Alteration_WaterCon_direction, values_from = Percentage) #%>%

dirs <- seq(1, dim(metric_tally_water_dir)[1], 1)  
metric_tally_water_dir$Direction <- NULL

for(p in 1: length(dirs)) {
  
  metric_tally_water_dir$Direction[p] =  if(is.na(metric_tally_water_dir$High[p])) {
    paste("Low")
  } else if (metric_tally_water_dir$High[p] == 50) {
    paste("Uncertain")
  } else  if(metric_tally_water_dir$High[p] >= 60){
    paste("High")
  } else if (metric_tally_water_dir$High[p] <= 40) {
    paste("Low")
  } else if(is.na(metric_tally_water_dir$High[p])) {
    paste("Low")
  } else {
    paste("Uncertain")
  }
  
}
head(metric_tally_water_dir)
write.csv(metric_tally_water_dir, "output_data/Manuscript/08_altered_metrics_direction_water_conservation.csv")

metric_tally_water <- delta_dfx %>%
  group_by(site, Hydro_Metric, Biol, Threshold, Bio_threshold) %>%
  count(Alteration_WaterCon)

metric_tally_water <- na.omit(metric_tally_water)
metric_tally_water

write.csv(metric_tally_water, "output_data/Manuscript/08_metric_suitability_tally_all_sites_watercons.csv")


## make %
metric_tally_water <- metric_tally_water %>%
  group_by(site, Hydro_Metric, Biol, Threshold, Bio_threshold) %>%
  mutate(Percentage = n/sum(n)*100) %>%
  select(-n) %>%
  pivot_wider(names_from = Alteration_WaterCon, values_from = Percentage)

metric_tally_water
metric_tally_water[is.na(metric_tally_water)] <- 0

write.csv(metric_tally_water, "output_data/Manuscript/08_metric_suitability_tally_condensed_all_sites_watercons.csv")



