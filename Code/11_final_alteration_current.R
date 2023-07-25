## Biological Flow Alteration - Current Scenario
# Level 2 - alteration summary based on various thresholds for CSCI and ASCI

## Thresholds
# CSCI - Bio_threshold = 0.92, Threshold = Threshold25
# ASCI - Bio_threshold = 0.94, Threshold = Threshold50

## Alteration determination:
# if >50% of time altered, metric is altered. 

## Overall prioritization based on CSCI and ASCI alteration:
# If 2-3 metrics altered (out of 3 important metrics), bio index considered altered

# Author: Kris Taniguchi-Quan, SCCWRP

###############################################################################
# Install packages - only need to install once
#install.packages("ggsn")
#install.packages("ggmap")
#install.packages("mapview")
#install.packages("geosphere")
#install.packages("rgeos")
#install.packages("ggspatial")
# To install spDataLarge
#devtools::install_github("robinlovelace/geocompr")
#install.packages("spDataLarge")
#install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")

#load libaries
library(spDataLarge)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(sf)
library(ggsn)
library(ggmap)
library(ggspatial)
library(mapview)
library(spData)      
library(geosphere)
library(rgeos)

###############################################################################
## Load in data
# ASCI/CSCI

# Read in CSCI/ASCI suitability data for current scenario
suit_data <- read.csv("output_data/Manuscript/08_metric_suitability_tally_condensed_all_sites_current.csv")

# set output directory where maps will be saved
out.dir <- "output_data/Manuscript/Figures/Maps/"
# create directory, only need to run once
dir.create(out.dir)

# read in information on subbasin and New_Name
basin_comid_lookup <- read.csv("Data/v13_pourpoints_NHD_comids.csv") 

# read in lookup table to convert subbasin codes for model output subbasin names
subbasin_lookup <- read.csv("Data/site_name_lookupletternumbers.csv")
subbasin_lookup
# read in shapefiles subbasins and reaches
# subbasin polygon shapefile
basins <- st_read("Data/Agg_Boundaries_v14.shp", quiet = T)
# reach polylines
reaches <- st_read('Data/reaches_forSCCWRP.shp', quiet = T)

############################################################
## Format the subbasin names to match the format in shapefiles

# convert basin orig name to outputfile name (model subbasin name) by replacing leading letter with associated model number code
new.subbasinname <- basin_comid_lookup$Subbasin

for(z in 1:length(subbasin_lookup$Letter)){
  new.subbasinname <- gsub(subbasin_lookup$Letter[z], subbasin_lookup$Number[z], new.subbasinname)
}

# find and replace "-" in new.subbasinname with nothing, make consistent with file name format
new.subbasinname <- gsub("-", "", new.subbasinname)
# save mew/subbasinname as site column in basin_comid_lookup
basin_comid_lookup$site <- as.numeric(new.subbasinname)

# join suit_data with basin_comid_lookup
suit_data2 <- suit_data %>% 
  inner_join(basin_comid_lookup, by = c('site')) %>% 
  select(c(names(suit_data), Subbasin)) %>% 
  rename(New_Name = Subbasin)

############################################################
## Post-process CSCI/ASCI Suitability data to get overall alteration
# Use the probability threshold and bio threshold combo that provides the most discriminatory power from sensitivity analysis
# CSCI - Bio_threshold = 0.92, Threshold = Threshold25
# ASCI - Bio_threshold = 0.94, Threshold = Threshold50

# subset to ASCI - Bio_threshold = 0.94, Threshold = Threshold50
subset.asci.094 <- suit_data2[suit_data2$Biol == "ASCI" & suit_data2$Bio_threshold == 0.94 & suit_data2$Threshold == "Threshold50",]
# subset to CSCI - Bio_threshold = 0.92, Threshold = Threshold25
subset.csci.092 <- suit_data2[suit_data2$Biol == "CSCI" & suit_data2$Bio_threshold == 0.92 & suit_data2$Threshold == "Threshold25",]

# combine subsetted csci and asci indices used
suit_data2_50 <- data.frame(rbind(subset.asci.094, subset.csci.092))

## set metric alteration based on time altered
# if >50% time Altered, altered
suit_data2_50$alteration_50pct_time[suit_data2_50$Altered > 50] <- "Altered"
suit_data2_50$alteration_50pct_time[suit_data2_50$Altered <= 50] <- "Unaltered"

#write.csv table of altered metrics per subbasin and bio/threshold
write.csv(suit_data2_50, file = "output_data/11_Level2_altered_metrics_results_50pcttimealtered_0.92csci_prob25_0.94asci_prob50_Current.csv")

## Set overall alteration for ASCI/CSCI (synthesizing across the 3 important metrics)
# If >1 metric is altered, then all altered

# aggregate by site, Biol, Threshold, alteration_50pct_time, get a count of n of altered and unaltered metrics for each
subset.50pct.time <- suit_data2_50 %>% 
  group_by(site, New_Name, Biol, Threshold, alteration_50pct_time) %>% 
  tally() %>% 
  ungroup() %>% 
  data.frame()

# if 2-3 metric altered, bio index is considered altered
# if number altered > 1 --> class it as overall altered
subset.50pct.time$overall.altered.2metric <- NA #create a new column as NA
# if >1 metric altered, save as altered
subset.50pct.time$overall.altered.2metric[which(subset.50pct.time$alteration_50pct_time == "Altered" & subset.50pct.time$n > 1)] <- "Altered"
# if 2-3 unaltered, consider unaltered                                                                                                                                                                                                                         #if 2-3 metrics unaltered --> class it as unaltered
subset.50pct.time$overall.altered.2metric[which(subset.50pct.time$alteration_50pct_time == "Unaltered" & subset.50pct.time$n > 1)] <- "Unaltered"

# save csv in output_data
file.name <- "output_data/11_summary.50pct.time.altered.2metrics.Current.csv"
write.csv(subset.50pct.time, file=file.name, row.names = FALSE)


####################################
## Summarize overall alteration across SOC
# number of altered and unaltered subbasins, percent of total subbasins using these thresholds

# find length of unique sites
site.length <- length(unique(subset.50pct.time$New_Name))

# summarize (count and %) of total subbasin in overall alteration categories
subset.50pct.time.summary2 <- subset.50pct.time %>% 
  group_by(Biol, Threshold, overall.altered.2metric) %>% 
  tally() %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(pct.2metric = 100*n/site.length) %>% 
  data.frame()

############################################################
## Create overall prioritization based on bio-relevant alteration of CSCI and ASCI
# if both altered, high priority; if one altered, medium priority; if unaltered, low priority

# remove NA values
subset.50pct.time.all2 <- na.omit(subset.50pct.time)

# tally number of biol indices (csci/asci) altered per site
subset.50pct.time.summary2.all <- subset.50pct.time.all2 %>% 
  group_by(New_Name,  overall.altered.2metric) %>% 
  tally() %>% 
  ungroup() %>% 
  na.omit()  

# save as overall.summary
overall.summary <- data.frame(subset.50pct.time.summary2.all)

# create new column for synthesis alteration, blank with NA values
overall.summary$synthesis_alteration <- NA
# designation prioritization categories
# if 2 bio indices altered, high priority
overall.summary$synthesis_alteration[which(overall.summary$overall.altered.2metric == "Altered" & overall.summary$n == 2)] <- "High Priority" 
# if one is unaltered, medium
overall.summary$synthesis_alteration[which(overall.summary$overall.altered.2metric == "Unaltered" & overall.summary$n == 1)] <- "Medium Priority" 
# if 2 bio indices unaltered, high priority
overall.summary$synthesis_alteration[which(overall.summary$overall.altered.2metric == "Unaltered" & overall.summary$n == 2)] <- "Low Priority" 

# remove NA rows (duplicate rows)
synthesis.summary <- na.omit(overall.summary)

#summary
synthesis.summary.table <- synthesis.summary %>% 
  na.omit()  %>% 
  group_by(synthesis_alteration) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(pct.2metric = 100*n/60) 
synthesis.summary.table <- data.frame(synthesis.summary.table)


############################################################
## Create overall synthesis maps for for 50% of time altered altered, 2 metrics altered


# colors and labels for suitability categories - used for legend and maps
colors <- c("#ca0020", "#fdae61","#0571b0", "white")
priority <- c("High Priority",  "Medium Priority","Low Priority", NA)
categories <- c("High (Alteration: CSCI & ASCI)", "Medium (Alteration: CSCI or ASCI)","Low (Alteration: None)","Not evaluated")
# lookup table for colors and categories for legend and maps
lookup <- data.frame(cbind(colors, priority, categories))

# merge synthesis.summary with basins to get spatial data
subset.join <- synthesis.summary %>% 
  full_join(basins, by = c('New_Name')) 

## plot
# Set up base map 
study <- ggplot(basins) + 
  labs(title="Prioritization for Additional Analysis", subtitle = "Based on Biologically-Relevant Flow Alteration",x ="", y = "")  + 
  geom_sf(color = "lightgrey", fill="white") +
  annotation_scale() +
  annotation_north_arrow(pad_y = unit(0.9, "cm"),  height = unit(.8, "cm"),
                         width = unit(.8, "cm")) +
  theme(panel.background = element_rect(fill = "white"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_line(color = "white", size = 0.8)) 

# print basemap
study

## subset lookup categories and tables
lookup.sub <- lookup[lookup$priority %in% unique(subset.join$synthesis_alteration),]

# save as factor for legend ordering
lookup.sub$priority <- factor(lookup.sub$priority, levels = unique(lookup.sub$priority))
subset.join$synthesis_alteration <- factor(subset.join$synthesis_alteration, levels = unique(lookup.sub$priority))

# synthesis map
syn.plot <- study + geom_sf(data = subset.join, color= "lightgrey", aes(fill=synthesis_alteration, geometry = geometry)) +
  scale_fill_manual(name = "Priority based on Biologic Flow Alteration", labels = lookup.sub$categories, values=lookup.sub$colors) +
  geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 

# print map
print(syn.plot)

# write plot
out.filename <- "output_data/Manuscript/Figures/Maps/11_Synthesis_Prioritization_map_LSPC_all_Aliso_Oso_small_creeks_SJ_Current.jpg"
ggsave(syn.plot, file = out.filename, dpi=500, height=6, width=8)


############################################################
## Create bio-relevant flow alteration CSCI and ASCI maps 
# for appropriate prob and biol threshold combos, for 50% of time altered, altered 2 metrics

# set colors for alteration categories used in legend and maps
colors <- c("#ca0020", "#0571b0", "white")
alteration <- c("Altered",  "Unaltered", NA)
categories <- c("Likely Altered", "Likely Unaltered", "Not evaluated")
# lookup table used for legend and maps
lookup <- data.frame(cbind(colors, alteration, categories))

# create title for plot (metric threshold)
metric.threshold <- "2 Metric Altered Threshold"

# subset to specific columns, rename columns
subset <- subset.50pct.time %>% 
  select("New_Name", "Biol", "Threshold","overall.altered.2metric") %>% 
  mutate(New_Name = as.character(New_Name)) %>% 
  data.frame() %>% 
  na.omit()
# update column names
names(subset) <- c("New_Name", "Biol", "Probability_Threshold","Alteration - Biology")

## Loop through CSCI and ASCI thresholds
indices <- c("ASCI", "CSCI")

for(z in indices){
  #subset either csci or asci
  subset.index <- subset[subset$Biol == z,]
  
  # set probability threshold label
  prob <- "Probability Threshold at 25%"
  
  # merge with basins to get spatial data for map
  subset.join <- subset.index %>% 
    full_join(basins, by = c('New_Name'))
  
  # set title and subtitle
  title <- paste0(z)
  subtitle <- "Biologically-Relevant Flow Alteration"
  
  ## Plot
  # Set up base map 
  study <- ggplot(basins) + 
    geom_sf(color = "lightgrey", fill="white") +
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
  lookup.sub <- lookup[lookup$alteration %in% unique(subset.join$`Alteration - Bio`),]
  
  # save as factor to sort categories in legend
  lookup.sub$alteration <- factor(lookup.sub$alteration, levels = unique(lookup.sub$alteration))
  subset.join$`Alteration - Bio` <- factor(subset.join$`Alteration - Bio`, levels = unique(lookup.sub$alteration))
  
  
  # synthesis map for bio index z
  syn.plot <- study + geom_sf(data = subset.join, color= "lightgrey", aes(fill=`Alteration - Bio`, geometry = geometry)) +
    scale_fill_manual(name = "Biologic Flow Alteration", labels = lookup.sub$categories, values=lookup.sub$colors) +
    geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 
  # print
  print(syn.plot)
  
  # write plot
  out.filename <- paste0("output_data/Manuscript/Figures/Maps/11_", z, "_alteration_map_LSPC_Aliso_Oso_small_creeks_SJ_Current.jpg")
  ggsave(syn.plot, file = out.filename, dpi=300, height=4, width=6)
  
}


# rename Alteration categories - Bio to likely unaltered and likely altered categories
subset$`Alteration - Biology` <- gsub("Altered", "Likely Altered", subset$`Alteration - Biology`)
subset$`Alteration - Biology` <- gsub("Unaltered", "Likely Unaltered", subset$`Alteration - Biology`)

# save the subset summary table with indices, subbasin
# pivot wider to get hydro.alteration.CSCI and hydro.alteration.ASCI columns
subset2 <- subset %>% 
  select(-c(Probability_Threshold)) %>% 
  pivot_wider(names_from = Biol, values_from = `Alteration - Biology`) %>% 
  rename(hydro.alteration.CSCI = CSCI) %>% 
  rename(hydro.alteration.ASCI = ASCI)

# remove na rows from overall summary
overall.summary2 <- na.omit(overall.summary)

# combine with overall summary
summary.csci.asci.synthesis <- subset2 %>% 
  inner_join(overall.summary2, by = c('New_Name')) %>% 
  select(c(names(subset2), "synthesis_alteration"))

# write csv summary for CSCI and ASCI
file.name.summary <- "output_data/11_SOC_CSCI_ASCI_HydroAlt_Synthesis_Summary_Current.csv"
write.csv(summary.csci.asci.synthesis, file = file.name.summary)

### tally
data <- read.csv("output_data/11_SOC_CSCI_ASCI_HydroAlt_Synthesis_Summary_Current.csv")

head(data)



table(data$hydro.alteration.ASCI)
# Likely Altered Likely Unaltered 
# 29               31 

table(data$hydro.alteration.CSCI)
# Likely Altered Likely Unaltered 
# 20               40 

table(data$synthesis_alteration)

# High Priority    Low Priority Medium Priority 
# 16              27              17 

data_med <- data %>%
  filter(synthesis_alteration == "Medium Priority")

sum(data_med$hydro.alteration.ASCI == "Likely Altered") #13

sum(data_med$hydro.alteration.CSCI == "Likely Altered") #4



