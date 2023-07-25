## formatting hydro and extracting csci/asci sites
getwd()

library(CSCI)
library(reshape)
library(reshape2)
library(ggplot2)
library(lubridate)
library(tidyr)
library(dplyr)
library(tidyverse)

## SOC sites

soc <- read.csv("Data/CSCI_ASCI_SitesAll_for_SOC_FlowEcologyStudy.csv")
head(soc)
dim(soc) ## 841
unique(soc$BugID)

## bug sites 
csci<-read.csv("Data/Liesl_CSCI_deltaHsites_soCA_regionalcurves_060420.csv")
csci3
## select only columns needed
csci <- csci %>%
  select(stationcode, sampleid, sampledate, oovere, mmi, csci, latitude, longitude)

unique(csci$stationcode) ## 409

## new data from Liesl
csci3 <- read.csv("Data/Katie_CSCI_11122021_.csv")
csci3 <- csci3 %>%
  select(stationcode, sampleid, sampledate, oovere, mmi, csci, latitude, longitude)

unique(csci3$stationcode) ## 139

sum(unique(csci$stationcode) %in% unique(csci3$stationcode))

## combine
csci <- rbind(csci, csci3)
dim(csci) ## 548

# ## use duplicate to remove all duplicates except for last
csci<- csci[ !duplicated(csci[, c("stationcode")], fromLast=T),]
dim(csci) ## 434

## match sites

csci_sites <- unique(csci$stationcode)
soc_sites <- unique(soc$BugID)

soc_sites %in% csci_sites
sum(soc_sites %in% csci_sites) ## 434


## flow data
dh_data <- read.csv("/Volumes/Biology/San Juan WQIP_KTQ/Data/Working/Regional_Curves_CSCI_ASCI_Annie/Data/Flow/subset_woutLARsitematches/DeltaH_FINAL/SoCal_bio_deltaH_summary_supp_final.csv")

## match sites to csci sites

csci_sites <- unique(csci$stationcode)
delta_sites <- unique(dh_data$site)

## check number of matching sites
delta_sites %in% csci_sites
sum(delta_sites %in% csci_sites) # 427

csci_sites %in% delta_sites
sum(csci_sites %in% delta_sites) # 427

### extract only csci sites in delta sites

delta_csci <- subset(csci, stationcode %in% delta_sites)


## format delta to columns
## subset hydro data to only median for testing

dh_median <- subset(dh_data, summary.statistic =="median")
dim(dh_median) # 7236

head(dh_median)

### remove all sites with only one year delta

dh_median <- filter(dh_median, !n_year == 1)

## subset to only site, flow metric & delta H
dh_medianx <- dh_median[,c(1,3,8)]

## make wider
dh_medianx <- dcast(dh_medianx, site~flow_metric) # 443 ( waiting for additional bio sites)

## join with hydro data 

names(dh_medianx)
unique(dh_medianx$site) ## 529, 522

all_dat_med <- merge(delta_csci, dh_medianx, by.x="stationcode", by.y="site")
dim(all_dat_med) ## 420

head(all_dat_med)

## save - df to use in GLMs
write.csv(all_dat_med, "output_data/00_csci_delta_formatted_median_updated_RF2023.csv")


# ASCI --------------------------------------------------------------------

## upload and format asci data

algae <- read.csv("Data/ASCI.1.csv")
head(algae)

algae <- algae %>% 
  select(sampleid, stationcode, sampledate,replicate, assemblage, metric, result) %>%
  filter(metric == "ASCI", !assemblage == "SBA") %>%
  rename(StationCode = stationcode, SampleID = sampleid, SampleDate = sampledate, 
         Result = result, Replicate = replicate) %>%
  mutate(Index = ifelse(assemblage == "Hybrid", "H_ASCI", "D_ASCI")) %>%
  select(-assemblage, -metric) 
  

## upload 2nd algae data set
algae2 <- read.csv("Data/asci.scores.forRafi2.csv")

algae2 <- algae2 %>% 
  select(SampleID, StationCode, SampleDate,Replicate, H_ASCI, D_ASCI) %>%
  gather(key = "Index", value = "Result", H_ASCI, D_ASCI)

  ### new asci data from Liesl
  algae5 <- read.csv("Data/Katie_ASCI_11122021.csv")
  head(algae5)
  
  algae5 <- algae5 %>% 
    select(sampleid, stationcode, sampledate,replicate, assemblage, metric, result) %>%
    filter(metric == "ASCI", !assemblage == "SBA") %>%
    rename(StationCode = stationcode, SampleID = sampleid, SampleDate = sampledate, 
           Result = result, Replicate = replicate) %>%
    mutate(Index = ifelse(assemblage == "Hybrid", "H_ASCI", "D_ASCI")) %>%
    select(-assemblage, -metric) 
  

 ## merge dataframes
algae3 <- bind_rows(algae, algae2, algae5)

## take most recent sample
algae3$SampleDate <- as.Date(algae3$SampleDate)
algae3 <- algae3[ !duplicated(algae3[, c("StationCode", "Index")], fromLast=T),]


## remove rep column and make wide
algae4 <- algae3 %>% 
  select(-Replicate) %>%
  pivot_wider(id_cols=c(SampleID, SampleDate, StationCode), names_from = Index, values_from = Result) %>%
  distinct(SampleDate, StationCode, .keep_all = T)

dim(algae4) # 2324
head(algae4)

## save
save(algae4, file= "output_data/00_SOC_all_asci_sites.RData")

sum(is.na(algae4)) # 105

## upload flow data
dh_data <- read.csv("/Volumes/Biology/San Juan WQIP_KTQ/Data/Working/Regional_Curves_CSCI_ASCI_Annie/Data/Flow/subset_woutLARsitematches/DeltaH_FINAL/SoCal_bio_deltaH_summary_supp_final.csv")

## match sites to asci sites

asci_sites <- unique(algae4$StationCode)
delta_sites <- unique(dh_data$site)

delta_sites %in% asci_sites
sum(delta_sites %in% asci_sites) # 329

asci_sites %in% delta_sites
sum(asci_sites %in% delta_sites) # 329

### extract only asci sites in delta sites

delta_asci <- subset(algae4, StationCode %in% delta_sites)
dim(delta_asci) ## 329   5


## remobve duplicates
delta_asci <- delta_asci[ !duplicated(delta_asci[, c("StationCode")], fromLast=T),]

## subset to median delta   
dh_median <- subset(dh_data, summary.statistic =="median")

### remove all sites with only one year delta

dh_median <- filter(dh_median, !n_year == 1)
unique(dh_median$site)

## subset to only site, flow metric & delta H
dh_medianx <- dh_median[,c(1,3,8)] ### 
dh_medianx <- dcast(dh_medianx, site~flow_metric) # 


## merge asci with delta H
all_dat_med <- merge(delta_asci, dh_medianx, by.x="StationCode", by.y="site")
dim(all_dat_med) ## 324, 356

## save for GLMs

write.csv(all_dat_med, "output_data/00_asci_delta_formatted_median_Nov2021.csv")

## total sites

asci <- read.csv("output_data/00_asci_delta_formatted_median_Nov2021.csv")
csci <- read.csv("output_data/00_csci_delta_formatted_median_updated_Nov2021.csv")
names(csci)
asci <- asci %>%
  select(StationCode)

csci <- csci %>%
  select(stationcode)

csci <- csci %>%
  mutate(StationCode = stationcode) %>%
  select(StationCode)

all_sites <- rbind(asci, csci)

dim(all_sites) # 744
 
length(unique(all_sites$StationCode)) ## 480
