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

csci <- csci %>%
  rename(masterid = stationcode)


## flow data
dh_data <- read.csv("Data/2023-07-20_RFpred_output_alldata_biositesCOMIDs_med_dlt_FFM_test12_test2.csv") %>%
  select(-X)
head(dh_data)

csciFlow <- inner_join(csci, dh_data, by = c("masterid", "latitude", "longitude"))
head(csciFlow)

## save - df to use in GLMs
write.csv(csciFlow, "output_data/00_csci_delta_formatted_median_updated_RF2023.csv")


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
  distinct(SampleDate, StationCode, .keep_all = T) %>%
  rename(masterid = StationCode)

dim(algae4) # 2324
head(algae4)

## save
save(algae4, file= "output_data/00_SOC_all_asci_sites.RData")

sum(is.na(algae4)) # 2059

asciFlow <- inner_join(algae4, dh_data, by = c("masterid"))
head(asciFlow)

## save - df to use in GLMs
write.csv(asciFlow, "output_data/00_asci_delta_formatted_median_updated_RF2023.csv")

## total sites

length(unique(asciFlow$masterid)) ## 351
length(unique(csciFlow$masterid)) ## 353

length(unique(asciFlow$comid)) ## 286
length(unique(csciFlow$comid)) ## 286

