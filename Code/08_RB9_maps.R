## packages
library(tidyverse)
library(dplyr)
library(tidyr)

# Format data for map -----------------------------------------------
# 
# unique(Year_Tally$hydro.endpoint)
# 
# # head(delta_dfx_sub)
# head(delta_med)

## write in delta med created in script 02a  
delta_med <- read_csv("output_data/02a_delta_med.csv")
delta_med <- delta_med %>%
  dplyr::select(-...1)

## write in limits.RB9 created in script 02a 
limits <- read_csv("output_data/02a_limits.csv")
limits <- limits %>%
  dplyr::select(-...1)

## join limits with delta data median - changed this variable name from delta_df for clarity
delta_df_map <- left_join(delta_med, limits, by= "hydro.endpoint")
head(delta_df_map)

## define alteration per subbasin, per year - within limits
delta_dfx_map <- delta_df_map %>%
  group_by(comid, hydro.endpoint, Bio_endpoint, Bio_threshold, Threshold) %>%
  mutate(Alteration = ifelse(MedDelta <= Positive & MedDelta >= Negative, "Unaltered", "Altered")) 


write.csv(delta_dfx_map, "ignore/05_alteration_by_median_comid_all_sites.csv") 

head(delta_dfx_map) 

## count alteration by site from main delta df
# metric_tally_current <- delta_dfx %>%
#   group_by(comid, hydro.endpoint, Biol, Threshold, Bio_threshold) %>%
#   count(Alteration)
# 
# metric_tally_current <- na.omit(metric_tally_current)
# metric_tally_current


## make %
# metric_tally_current <- metric_tally_current %>%
#   group_by(comid, hydro.endpoint, Biol, Threshold, Bio_threshold) %>%
#   mutate(Percentage = n/sum(n)*100, YearsWithData = sum(n)) %>%
#   select(-n) %>%
#   pivot_wider(names_from = Alteration, values_from = Percentage)
# 
# metric_tally_current %>%   tally() 

## replace NAs with zero - as 100% in other category
# metric_tally_current[is.na(metric_tally_current)] <- 0

delta_dfx_sub <- delta_dfx_map %>%                                        #### edited this whole chunk of code - full metrics - "FA_Mag", "Wet_BFL_Mag_10", "Wet_BFL_Mag_50", "SP_Mag", "DS_Mag_50", "DS_Mag_90", "Q99"
  filter(metric %in% c("ASCI_DS_Mag_50_0.86", "ASCI_Q99_0.86", "CSCI_Wet_BFL_Mag_10_0.79", "CSCI_Q99_0.79"), # picked the chosen metrics that we picked with katie 
         Bio_threshold %in% c(0.86, 0.79)) %>%
  mutate(threshCode = paste(Bio_threshold, Threshold, sep= "_")) %>%
  filter(threshCode %in% c("0.79_Threshold25", "0.79_Threshold50", "0.79_Threshold75", "0.86_Threshold25", "0.86_Threshold50", "0.86_Threshold75"))

unique(delta_dfx_sub$threshCode)

write.csv(delta_dfx_sub, "ignore/05_metric_suitability_tally_condensed_all_sites_current.csv")

# Alteration criteria ----------------------------------------------------------------

delta_dfx_sub <- read.csv("ignore/05_metric_suitability_tally_condensed_all_sites_current.csv") 

head(delta_dfx_sub)

library(sf)
library(raster)
library(maptools)
library(rgdal)

## projection
prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

## set metric alteration based on median alteration
#  if 1 or 2 metrics altered then bio is altered
# if both bio altered = altered, of 1 bio altered = partially altered, if 0 bio altered = unaltered

## change names for comid column 
suit_data <- delta_dfx_sub %>%
  # dplyr::select(-X) %>%
  rename(COMID = comid)
head(suit_data)

# aggregate by site, Biol, Threshold, alteration_50pct_time, get a count of n of altered and unaltered metrics for each
subset.50pct.time <- suit_data %>% 
  group_by(COMID, Biol, Alteration, Threshold) %>% 
  tally() %>%    ### creates n column 
  ungroup() %>% 
  data.frame()

subset.50pct.time

# if 1-2 metric altered, bio index is considered altered
# if number altered > 1 --> class it as overall altered
subset.50pct.time$overall.altered.2metric <- NA #create a new column as NA

# if >=1 metric altered, save as altered, all others unaltered, then any unaltered with 1 metric also altered
subset.50pct.time <- subset.50pct.time %>%
  mutate(overall.altered.2metric = ifelse(Alteration == "Altered" & n >= 1, "Altered", "Unaltered")) %>%
  mutate(overall.altered.2metric = ifelse(overall.altered.2metric == "Unaltered" & n == 1, "Altered", overall.altered.2metric)) %>%
  dplyr::select(-Alteration) %>%
  distinct()

# save csv in output_data
file.name <- "ignore/05_summary.50pct.time.altered.2metrics.Current.csv" 

write.csv(subset.50pct.time, file=file.name, row.names = FALSE)

subset.50pct.time

# remove NA rows (duplicate rows) and remove calulation columns
separate.summary <- subset.50pct.time %>%
  na.omit() %>%
  dplyr::select(-n) ## removed threshold for this select function

## Summarize overall alteration across rb9
# number of altered and unaltered subbasins, percent of total subbasins using these thresholds

# find length of unique sites
site.length <- length(unique(subset.50pct.time$COMID))
site.length
#2116 added by Rachel, this is how many comids we have  

# summarize (count and %) of total comids in overall alteration categories
subset.50pct.time.summary2 <- subset.50pct.time %>% 
  group_by(Biol, Threshold, overall.altered.2metric) %>% 
  tally() %>% 
  ungroup() %>% 
  na.omit() %>% 
  mutate(pct.2metric = 100*n/site.length) %>% 
  data.frame()

subset.50pct.time.summary2

## code below added by Rachel 
#   Biol   Threshold overall.altered.2metric    n pct.2metric
# 1  ASCI Threshold25                 Altered 2008  94.8960302
# 2  ASCI Threshold25               Unaltered  108   5.1039698
# 3  ASCI Threshold50                 Altered 2060  97.3534972
# 4  ASCI Threshold50               Unaltered   56   2.6465028
# 5  ASCI Threshold75                 Altered 2107  99.5746692
# 6  ASCI Threshold75               Unaltered    9   0.4253308
# 7  CSCI Threshold25                 Altered 1918  90.6427221
# 8  CSCI Threshold25               Unaltered  198   9.3572779
# 9  CSCI Threshold50                 Altered 2002  94.6124764
# 10 CSCI Threshold50               Unaltered  114   5.3875236
# 11 CSCI Threshold75                 Altered 2074  98.0151229
# 12 CSCI Threshold75               Unaltered   42   1.9848771

# Overall synthesis -------------------------------------------------------

## Create overall prioritization based on bio-relevant alteration of CSCI and ASCI
# if both altered, high priority; if one altered, medium priority; if unaltered, low priority

# remove NA values
subset.50pct.time.all2 <- na.omit(subset.50pct.time)

# tally number of biol indices (csci/asci) altered per site
subset.50pct.time.summary2.all <- subset.50pct.time.all2 %>% 
  group_by(COMID, overall.altered.2metric, Threshold) %>% ####################################################### might be a problem here, had edited to group by biol but the problem might be above 
  tally() %>% 
  ungroup() %>% 
  na.omit()  

# save as overall.summary
overall.summary <- data.frame(subset.50pct.time.summary2.all)
overall.summary
# create new column for synthesis alteration, blank with NA values
overall.summary$synthesis_alteration <- NA
# designation prioritization categories
# if 2 bio indices altered, high priority
overall.summary$synthesis_alteration[which(overall.summary$overall.altered.2metric == "Altered" & overall.summary$n == 2)] <- "High Priority" 
# if one is unaltered, medium
overall.summary$synthesis_alteration[which(overall.summary$overall.altered.2metric == "Unaltered" & overall.summary$n == 1)] <- "Medium Priority" 
# # if one is altered, medium
# overall.summary$synthesis_alteration[which(overall.summary$overall.altered.2metric == "Altered" & overall.summary$n == 1)] <- "Medium Priority" 
# if 2 bio indices unaltered, low priority
overall.summary$synthesis_alteration[which(overall.summary$overall.altered.2metric == "Unaltered" & overall.summary$n == 2)] <- "Low Priority" 

# remove NA rows (duplicate rows) and remove calulation columns
synthesis.summary <- overall.summary %>%
  na.omit() %>%
  dplyr::select(-overall.altered.2metric, -n)

dim(synthesis.summary)
#summary
synthesis.summary.table <- synthesis.summary %>% 
  na.omit()  %>% 
  group_by(synthesis_alteration, Threshold) %>% 
  tally() %>% 
  ungroup() %>% 
  group_by(Threshold) %>% 
  mutate(pct.2metric = 100*n/sum(n)) 

synthesis.summary.table <- data.frame(synthesis.summary.table)
synthesis.summary.table

# Maps --------------------------------------------------------------------
# install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")

library(spDataLarge)
library(readxl)
library(sf)
library(ggsn)
library(ggmap)
library(ggspatial)
library(spData)      
library(geosphere)
library(rgeos)
### upload RB9 nhds

calinhd <- readOGR('/Users/racheld/SCCWRP/SD Hydro Vulnerability Assessment - General/Data/SpatialData/NHDplus_RB9.shp') %>%
  spTransform(prj) %>%
  st_as_sf %>%
  st_simplify(dTolerance = 0.5, preserveTopology = T)
unique(calinhd$COMID)
unique(overall.summary$COMID)


## synthesis map

# fortified calinhd, joind with delta
# merge synthesis.summary with comids to get spatial data
SynthNHD <- calinhd %>%
  filter(COMID %in% unique(synthesis.summary$COMID)) %>%
  dplyr::select(COMID) %>% ## 46
  as('Spatial') %>% 
  st_as_sf(coords = c("long", "lat"), remove = FALSE)

nhdplo <- SynthNHD %>%
  fortify %>%
  # left_join(comidid, by = 'id') %>%
  full_join(synthesis.summary, by = 'COMID')


# colors and labels for suitability categories - used for legend and maps
colors <- c("#ca0020", "#fdae61","#0571b0", "white")
priority <- c("High Priority",  "Medium Priority","Low Priority", NA)
categories <- c("High (Alteration: CSCI & ASCI)", "Medium (Alteration: CSCI or ASCI)","Low (Alteration: None)","Not evaluated")

# lookup table for colors and categories for legend and maps
lookup <- data.frame(cbind(colors, priority, categories))

## plot
# Set up base map 
thresholds <- c("Threshold25", "Threshold50", "Threshold75") ## edited 11/16
t = "Threshold25"

for(t in thresholds){
  title = paste("Synthesized Biologic Flow Alteration", t, sep = "\n") ## edited 11/16
  
  nhdplo.m <- nhdplo[nhdplo$Threshold == t,]
  
  study <- ggplot(SynthNHD) + 
    labs(title = title, x ="", y = "")  + 
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
  lookup.sub <- lookup[lookup$priority %in% unique(nhdplo.m$synthesis_alteration),]
  
  # save as factor for legend ordering
  lookup.sub$priority <- factor(lookup.sub$priority, levels = unique(lookup.sub$priority))
  nhdplo.m$synthesis_alteration <- factor(nhdplo.m$synthesis_alteration, levels = unique(lookup.sub$priority))
  
  # synthesis map for bio index z
  syn.plot <- study + geom_sf(data = nhdplo.m, aes(color=synthesis_alteration, geometry = geometry)) +  
    scale_color_manual(name = "Alteration based on Biologic Flow Alteration", labels = lookup.sub$categories, values=lookup.sub$colors) 
  
  # print map
  print(syn.plot)
  
  # write plot
  out.filename <- paste0("figures/maps/05_Synthesis_Prioritization_map_RB9_Current_", t, ".jpg") ## edited 11/16
  ggsave(syn.plot, file = out.filename, dpi=500, height=6, width=8)
}

# CSCI & ASCI maps --------------------------------------------------------

separate.summary

## Create bio-relevant flow alteration CSCI and ASCI maps 
# for appropriate prob and biol threshold combos, altered dependent on mdeian, altered 1 or 2 metrics

# set colors for alteration categories used in legend and maps
colors <- c("#ca0020", "#0571b0", "white")
alteration <- c("Altered",  "Unaltered", NA)
categories <- c("Likely Altered", "Likely Unaltered", "Not evaluated")
# lookup table used for legend and maps
lookup <- data.frame(cbind(colors, alteration, categories))

# create title for plot (metric threshold)
metric.threshold <- "2 Metric Altered Threshold"

# subset to specific columns, rename columns
subset <- separate.summary %>% 
  dplyr::select("COMID", "Biol","overall.altered.2metric", "Threshold") %>% ### added threshold to this line 
  # mutate(COMID = as.character(COMID)) %>% 
  data.frame() %>% 
  na.omit()

# update column names
names(subset) <- c("COMID", "Biol","Alteration - Biology", "Threshold")

## Loop through CSCI and ASCI thresholds
indices <- c("ASCI", "CSCI")
thresholds <- c("Threshold25", "Threshold50", "Threshold75")
# i = "Threshold25"
# z = "ASCI"
for(z in indices){
  #subset either csci or asci
  subset.index <- subset[subset$Biol == z,]
  subset.index
  
  for(i in thresholds){
  # # set probability threshold label
  # # prob <- "Probability Threshold at 25%"
  subset.index.thresholds <- subset.index[subset.index$Threshold == i,]
    
  # merge with comids to get spatial data for map
  subset.join <- subset.index.thresholds %>% 
    full_join(SynthNHD, by = c('COMID'))
  subset.join
  # set title and subtitle
  title <- paste0(z)
  subtitle <- paste("Biologically-Relevant Flow Alteration", i, sep = "\n") 
  
  ## Plot
  # Set up base map 
  study <- ggplot(SynthNHD) + 
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
  
  lookup.sub <- lookup[lookup$alteration %in% unique(subset.join$`Alteration - Biology`),]
  
  # save as factor to sort categories in legend
  lookup.sub$alteration <- factor(lookup.sub$alteration, levels = unique(lookup.sub$alteration))
  subset.join$`Alteration - Biology` <- factor(subset.join$`Alteration - Biology`, levels = unique(lookup.sub$alteration))
  
  
  # synthesis map for bio index z
  syn.plot <- study + geom_sf(data = subset.join, aes(color=`Alteration - Biology`, geometry = geometry)) +
    scale_color_manual(name = "Biologic Flow Alteration", labels = lookup.sub$categories, values=lookup.sub$colors) 
  
  # print
  print(syn.plot)
  
  # write plot
  out.filename <- paste0("figures/maps/05_", z, i, "_alteration_map_RB9_Current.jpg") ## add i after z for the loop
  ggsave(syn.plot, file = out.filename, dpi=300, height=4, width=6)
  
}
}
  
# rename Alteration categories - Bio to likely unaltered and likely altered categories
subset$`Alteration - Biology` <- gsub("Altered", "Likely Altered", subset$`Alteration - Biology`)
subset$`Alteration - Biology` <- gsub("Unaltered", "Likely Unaltered", subset$`Alteration - Biology`)

# save the subset summary table with indices, subbasin
# pivot wider to get hydro.alteration.CSCI and hydro.alteration.ASCI columns

# subset1 <- subset %>%
#   unique()

subset2 <- subset %>%
  distinct() %>%
  # select(-c(Probability_Threshold)) %>%
  pivot_wider(names_from = Biol, values_from = `Alteration - Biology`) %>%
  rename(hydro.alteration.CSCI = CSCI) %>%
  rename(hydro.alteration.ASCI = ASCI)

## added by rachel to replace above code 
# subset2 <- subset %>% 
#   group_by(COMID) %>%
#   mutate(row = row_number()) %>%
#   pivot_wider(names_from = Biol, values_from = `Alteration - Biology`) %>%
#   rename(hydro.alteration.CSCI = CSCI) %>%
#   rename(hydro.alteration.ASCI = ASCI) %>%
#   dplyr::select(-row) %>%
#   ungroup() %>%
#   distinct()

  
  
# remove na rows from overall summary
overall.summary2 <- na.omit(overall.summary)

# combine with overall summary
summary.csci.asci.synthesis <- subset2 %>% 
  # dplyr::select(-Threshold) %>% ### removed threshold column so join could work properly 
  inner_join(overall.summary2, by = c('COMID', 'Threshold')) %>% 
  dplyr::select(c(names(subset2), "synthesis_alteration")) %>%
  distinct()

# write csv summary for CSCI and ASCI
file.name.summary <- "output_data/05_RB9_CSCI_ASCI_HydroAlt_Synthesis_Summary_Current.csv" 
write.csv(summary.csci.asci.synthesis, file = file.name.summary)

### tally
data <- read.csv("output_data/05_RB9_CSCI_ASCI_HydroAlt_Synthesis_Summary_Current.csv") 

# data <- filter(data, Threshold == "Threshold25")

head(data)



table(data$hydro.alteration.ASCI, data$Threshold) #edited by Rachel 12/5
#                     Threshold25 Threshold50 Threshold75
# Likely Altered          2008        2060        2107
# Likely Unaltered         108          56           9

table(data$hydro.alteration.CSCI, data$Threshold) ##edited to add threshold 
#                      Threshold25 Threshold50 Threshold75
# Likely Altered          1918        2002        2074
# Likely Unaltered         198         114          42

table(data$synthesis_alteration, data$Threshold) ## edited to add threshold 
#                     Threshold25 Threshold50 Threshold75
# High Priority          1903        1973        2065
# Low Priority             93          27           0
# Medium Priority         120         116          51

data_med <- data %>%
  filter(synthesis_alteration == "Medium Priority")

thresh25asci <- data_med %>%
  filter(Threshold == "Threshold50" & hydro.alteration.ASCI == "Likely Altered") 

thresh50asci <- data_med %>%
  filter(Threshold == "Threshold50" & hydro.alteration.ASCI == "Likely Altered") 

thresh25asci <- data_med %>%
  filter(Threshold == "Threshold25" & hydro.alteration.ASCI == "Likely Altered") 

# sum(data_med$hydro.alteration.ASCI == "Likely Altered") # 328

# sum(data_med$hydro.alteration.CSCI == "Likely Altered") # 235

thresh25csci <- data_med %>%
  filter(Threshold == "Threshold50" & hydro.alteration.CSCI == "Likely Altered") 

thresh50csci <- data_med %>%
  filter(Threshold == "Threshold50" & hydro.alteration.CSCI == "Likely Altered") 

thresh25csci <- data_med %>%
  filter(Threshold == "Threshold25" & hydro.alteration.CSCI == "Likely Altered") 




# shapefile all alteration ------------------------------------------------ 

# ## make sep summary wider to form asci/csci columns
# separate.summary <- separate.summary %>%
#   pivot_wider(names_from = Biol, values_from = overall.altered.2metric)
# 
# head(separate.summary) ## csci & asci
# head(synthesis.summary) ## synthesized
# 
# sum(separate.summary$ASCI == "Altered") ## 1461
# sum(separate.summary$CSCI == "Altered") ## 1368
# dim(separate.summary) ## 2116
# 
# (sum(separate.summary$ASCI == "Altered")/2116)*100
# 
# ## join alteration together
# 
# all_alt <- full_join(separate.summary, synthesis.summary, by = "COMID")
# head(all_alt)
# 
# all_alt <- rename(all_alt, ASCI.Alteration = ASCI, CSCI.Alteration = CSCI, Overall.Priority = synthesis_alteration)
# 
# ## NHD reaches
# 
# calinhd <- readOGR('/Users/racheld/SCCWRP/SD Hydro Vulnerability Assessment - General/Data/SpatialData/NHDplus_RB9.shp') %>%
#   spTransform(prj) %>%
#   st_as_sf %>%
#   st_simplify(dTolerance = 0.5, preserveTopology = T)
# head(calinhd)
# 
# # fortified calinhd, joind with delta
# # merge all_alt with basins to get spatial data
# altNHD <- calinhd %>%
#   filter(COMID %in% unique(all_alt$COMID)) %>%
#   dplyr::select(COMID, LENGTHKM) %>% ## 46
#   as('Spatial') %>% 
#   st_as_sf(coords = c("long", "lat"), remove = FALSE)
# 
# nhdalt <- altNHD %>%
#   fortify %>%
#   # left_join(comidid, by = 'id') %>%
#   full_join(all_alt, by = 'COMID')
# 
# ## save as shape
# st_write(nhdalt, "output_data/07_Bio_Alteration_RB9.shp") 
# 
# nhdalt <- st_read("output_data/07_Bio_Alteration_RB9.shp") 
# head(nhdalt)
# plot(nhdalt)
