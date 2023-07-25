
## maps to compare alterationa spatially
## packages

library(spDataLarge)
library(viridis)
library(readxl)
library(sf)
library(ggsn)
library(ggmap)
library(mapview)
library(spData)      
library(geosphere)
library(rgeos)
library(tidyverse)

#set output directory
out.dir <- "output_data/Manuscript/Figures/"


#read in information on subbasin and New_Name
basin_comid_lookup <- read.csv("/Volumes/Biology/San Juan WQIP_KTQ/Data/SpatialData/v13_pourpoints_NHD_comids.csv") 

#read in shapefiles subbasins and reaches
#subbasin polygon shapefile
basins <- st_read("Data/subbasin_boundaries_forSCCWRP.shp", quiet = T)
basins
#reach polylines
reaches <- st_read('Data/reaches_forSCCWRP.shp', quiet = T)

#lookuptable to convert subbasin codes for model output subbasin names - doesn't exist!!!
subbasin_lookup <- read.csv("/Volumes/Biology/San Juan WQIP_KTQ/Data/RawData/From_Geosyntec/South_OC_Flow_Ecology_for_SCCWRP/Old_Runs/191220_Interim_Calibration/site_name_lookupletternumbers.csv")

#convert basin orig name to outputfile name (model subbasin name)
new.subbasinname <- basin_comid_lookup$Subbasin

for(z in 1:length(subbasin_lookup$Letter)){
  new.subbasinname <- gsub(subbasin_lookup$Letter[z], subbasin_lookup$Number[z], new.subbasinname)
}

#find and replace - in new.subbasinname with nothing, make consistent with file name
new.subbasinname <- gsub("-", "", new.subbasinname)
basin_comid_lookup$site <- as.numeric(new.subbasinname)

#join new subbasin name with 
data <- read.csv("output_data/Manuscript/08_metric_suitability_tally_condensed_all_sites_current.csv")
head(data)

suit_data2 <- data %>% 
  inner_join(basin_comid_lookup, by = c('site')) %>% 
  select(c(names(data), Subbasin)) %>% 
  rename(New_Name = Subbasin) %>%
  select(-Unaltered)# %>%


##  upload ffm names
labels <- read.csv("Data/ffm_names.csv")
labels <- labels[1:24, ]
labels
labels <- labels %>% rename(Hydro_Metric = Flow.Metric.Code)
labels[25, 1] <- "Magnitude of largest annual storm"
labels[25, 2] <- "Q99"
labels[25, 3] <- "Peak Flow"
labels

## join data to labels
suit_data2 <- left_join(suit_data2, labels, by ="Hydro_Metric")

#names
cols <- names(suit_data2)

indices <- c("ASCI", "CSCI")
# z = "ASCI"
# h = "Q99"

for(z in indices){
  
  #subset either csci or asci
  subset.index <- suit_data2[suit_data2$Biol == z,]

  metrics <- na.omit(unique(subset.index$Hydro_Metric))


  ## loop through metrics
  for(h in metrics){ 
    
    #subset based on hydro metric

    subset.index.metric <- subset.index[subset.index$Hydro_Metric ==  h,]
    #bio value
    subset.index.metric
    metric <- paste(subset.index.metric$Flow.Metric.Name)[1]
    metric
    
    #merge with basins
    subset.join <- subset.index.metric %>% 
      full_join(basins, by = c('New_Name'))
    
    subset.join <- na.omit(subset.join)

    
    title <- paste0(z, ": ", metric)
    # New facet label names for probability variable
    prob.labs <-  c("Prob: 0.25", "Prob: 0.5", "Prob: 0.75")
    names(prob.labs) <- c("Threshold25", "Threshold50", "Threshold75")
    
    # New facet label names for bio variable
    
    if(z == "CSCI") {
      bio.labs <-  c("Bio: 0.63", "Bio: 0.79", "Bio: 0.92")
      names(bio.labs) <- c("0.63", "0.79", "0.92")
    } else {
      bio.labs <-  c("Bio: 0.75", "Bio: 0.86", "Bio: 0.94")
      names(bio.labs) <- c("0.75", "0.86", "0.94")
    }
    

    #plot
    #Set up base map 
    study <- ggplot(basins) + 
      geom_sf(color = "#969696", fill="white", size = 0.1) +
      labs(title=title,  x ="", y = "")  + 
      theme(panel.background = element_rect(fill = "white"),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_line(color = "white", size = 0.5),
            plot.title = element_text(size=20)) +
      geom_sf(data = reaches, color = "#67a9cf", size = 0.5) 
    
    study

    #synthesis map
    syn.plot <- study + geom_sf(data = subset.join, aes(fill=Altered, geometry = geometry), size = 0.2) +
      scale_fill_gradientn(colours=rev(viridis(6)), name = "Alteration (%)") +
      geom_sf(data = reaches, color = "#67a9cf", size = 0.1) +
      facet_grid(cols=vars(Threshold), rows = vars(Bio_threshold), labeller = labeller(Threshold = prob.labs, Bio_threshold = bio.labs))
  

    out.filename <- paste0("/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/conferences/JASM 2022/study_area_blank.jpg")
    ggsave(study, file = out.filename, dpi=600, height=4, width=6)
    #write plot
    
    # out.filename
    out.filename <- paste0(out.dir, z,"_", h, "_percentage_alteration_map.jpg")
    ggsave(syn.plot, file = out.filename, dpi=300, height=4, width=6)
    
    # }
    
  }
  
}


