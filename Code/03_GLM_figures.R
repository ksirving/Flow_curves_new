## create all GLM figures

library(ggplot2)
# library(purrr)
library(dplyr)
library(tidyverse)

out.dir <- "output_data/Manuscript/Figures/New_Figures/"

## full names for labels
labels <- read.csv("Data/ffm_names.csv")
labels <- labels[1:24, ]
labels <- labels %>% rename(hydro.endpoints = Flow.Metric.Code)
labels[25, 1] <- "Peak Flow Magnitude (Q99, cfs)"
labels[25, 2] <- "Q99"
labels[25, 3] <- "Peak Flow Magnitude"
labels


labels <- labels %>%
  mutate(hydro.endpoints = case_when(hydro.endpoints == "DS_Mag_50" ~ "d_ds_mag_50",
                                    hydro.endpoints == "FA_Mag" ~ "d_fa_mag",
                                    hydro.endpoints == "Peak_10" ~ "d_peak_10",
                                    hydro.endpoints == "Peak_2" ~ "d_peak_2",
                                    hydro.endpoints == "Peak_5" ~ "d_peak_5",
                                    hydro.endpoints == "SP_Mag" ~ "d_sp_mag",
                                    hydro.endpoints == "Wet_BFL_Mag_10" ~ "d_wet_bfl_mag_10",
                                    hydro.endpoints == "Wet_BFL_Mag_50" ~ "d_wet_bfl_mag_50",
                                    hydro.endpoints == "Q99" ~ "delta_q99"))
# data ASCI ---------------------------------------------------------------

## upload data
all_asci <- read.csv("output_data/01_h_asci_neg_pos_logR_metrics_figures_July2023.csv")

## scale probability
all_asci <- all_asci %>%
  select(-X) %>%
  group_by(comb_code, Type) %>%
  mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
           (max(PredictedProbability)-min(PredictedProbability))) 

all_asci <- left_join(all_asci, labels, by ="hydro.endpoints")

head(all_asci)


unique(all_asci$hydro.endpoints)
# data CSCI ---------------------------------------------------------------

## upload data
all_csci <- read.csv("output_data/01_CSCI_neg_pos_logR_metrics_figures_July2023.csv")

## scale probability
all_csci <- all_csci %>%
  select(-X) %>%
  group_by(comb_code, Type) %>%
  mutate(PredictedProbabilityScaled = (PredictedProbability-min(PredictedProbability))/
           (max(PredictedProbability)-min(PredictedProbability))) 

all_csci <- left_join(all_csci, labels, by ="hydro.endpoints")

head(all_csci)


# Figures CSCI-----------------------------------------------------------------

## filter to only 0.79
all_csci <- all_csci %>%
  mutate(Thresholds = as.character(thresholds)) %>%
  filter(thresholds == 0.79) 

head(all_csci)

## define FFM to loop through
HydroEnds <- unique(all_csci$hydro.endpoints)

HydroEnds
# m=2

for(m in 1:length(HydroEnds)) {
  
  ## title of FFM
  main.title <- all_csci %>%
    ungroup() %>%
    filter(hydro.endpoints == paste(HydroEnds[m])) %>%
    select(Flow.Metric.Name) %>%
    distinct(Flow.Metric.Name)
  
  ## subset data and put in order for geom.path
  all_cscix <- subset(all_csci,hydro.endpoints == paste(HydroEnds[m]))
  all_cscix <- all_cscix[order(all_cscix$PredictedProbabilityScaled, all_cscix$hydro),]
  
  # Rachel edited 9/20
  if(paste(HydroEnds[m]) %in% c("d_peak_10", "d_peak_2", "d_peak_5")){
    all_cscix <- all_cscix %>% 
      filter(Type == "Negative")
  }
  
  q3 <- ggplot(all_cscix, aes(x=hydro, y=PredictedProbabilityScaled, color=Thresholds))+
    geom_path()+
    facet_wrap(~Type, scales = "free_x") +
    theme(strip.background = element_blank(),
          strip.text.y = element_blank()) +
    scale_y_continuous(limits=c(0,1))+
    theme_minimal()+
    theme(text = element_text(size=15),axis.text.x = element_text(angle = 60,  vjust = 0.5, hjust=0.5)) +
    labs(title = paste(main.title),
         x = "Delta H",
         y = "Probability of Good CSCI") #+ theme_bw(base_size = 15)
  q3
  out.filename <- paste0(out.dir,"03_csci_", paste(HydroEnds[m]), "_0.79_updated_0920.jpg")
  ggsave(q3, file = out.filename, dpi=300, height=4, width=6, bg = "white")
  
  
}



# Figures ASCI ------------------------------------------------------------

## filter to 0.86
all_asci <- all_asci %>%
  mutate(Thresholds = as.character(thresholds)) %>%
  filter(thresholds == 0.86)

## define FFM to loop through
HydroEnds <- unique(all_asci$hydro.endpoints)
# m = 3

for(m in 1:length(HydroEnds)) {
  
  ## title of FFM
  main.title <- all_asci %>%
    ungroup() %>%
    filter(hydro.endpoints == paste(HydroEnds[m])) %>%
    select(Flow.Metric.Name) %>%
    distinct(Flow.Metric.Name)
  
  ## subset data and put in order for geom.path
  all_ascix <- subset(all_asci,hydro.endpoints == paste(HydroEnds[m]))
  all_ascix <- all_ascix[order(all_ascix$PredictedProbabilityScaled, all_ascix$hydro),]
  
  # Rachel edited 9/20
  if(paste(HydroEnds[m]) %in% c("d_peak_10", "d_peak_2", "d_peak_5")){
    all_ascix <- all_ascix %>% 
      filter(Type == "Negative")
  }

  
  q3 <- ggplot(all_ascix, aes(x=hydro, y=PredictedProbabilityScaled, color=Thresholds))+
    geom_path()+
    facet_wrap(~Type, scales = "free_x") +
    theme(strip.background = element_blank(),
          strip.text.y = element_blank()) +
    scale_y_continuous(limits=c(0,1))+
    theme_minimal()+
    theme(text = element_text(size=15),axis.text.x = element_text(angle = 60,  vjust = 0.5, hjust=0.5)) +
    labs(title = paste(main.title),
         x = "Delta H",
         y = "Probability of Good ASCI") #+ theme_bw(base_size = 15)
  q3

  out.filename <- paste0(out.dir,"03_asci_", paste(HydroEnds[m]), "_0.86_updated_0920.jpg")
  ggsave(q3, file = out.filename, dpi=300, height=4, width=6, bg = "white")
  
  
}


# Example Figure for report -----------------------------------------------

# ## filter to only 0.92 - chosen threshold
# all_csci <- all_csci %>%
#   mutate(Thresholds = as.character(thresholds)) %>%
#   filter(thresholds == 0.92) 
# 
# head(all_csci)
# 
# ## define FFM to loop through
# HydroEnds <- unique(all_csci$hydro.endpoints)
# 
# HydroEnds
# m=12 ## SP_Tim 
# 
#   ## title of FFM
#   main.title <- all_csci %>%
#     ungroup() %>%
#     filter(hydro.endpoints == paste(HydroEnds[m])) %>%
#     select(Flow.Metric.Name) %>%
#     distinct(Flow.Metric.Name)
#   
#   ## subset data and put in order for geom.path
#   all_cscix <- subset(all_csci,hydro.endpoints == paste(HydroEnds[m]))
#   all_cscix <- all_cscix[order(all_cscix$PredictedProbabilityScaled, all_cscix$hydro),]
#   
#   
#   q3 <- ggplot(all_cscix, aes(x=hydro, y=PredictedProbabilityScaled, color=Thresholds))+
#     geom_path()+
#     facet_wrap(~Type, scales = "free_x") +
#     theme(strip.background = element_blank(),
#           strip.text.y = element_blank()) +
#     scale_y_continuous(limits=c(0,1))+
#     theme_minimal()+
#     theme(text = element_text(size=15),axis.text.x = element_text(angle = 60,  vjust = 0.5, hjust=0.5)) +
#     theme(legend.position = "none") +
#     labs(title = paste(main.title),
#          x = "Delta H",
#          y = "Probability of Good CSCI") #+ theme_bw(base_size = 15)
#   q3
#   out.filename <- paste0(out.dir,"03_csci_", paste(HydroEnds[m]), "_0.92.jpg")
#   ggsave(q3, file = out.filename, dpi=300, height=4, width=6)
  
  

