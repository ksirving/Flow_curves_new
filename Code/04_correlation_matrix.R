## correlation matrix


### CSCI
data <- read.csv("output_data/00_csci_delta_formatted_median_updated_RF2023.csv") #edited by Rachel 8/28/2023
head(data)

# data <- select(data, DS_Dur_WS:Wet_Tim) #edited by Rachel to reflect columns names 
data <- select(data, d_ds_mag_50:delta_q99)

data_cor <- cor(data, method="spearman", use="complete.obs") ## spearman

write.csv(data_cor, "output_data/manuscript/04_ffm_cor_csci_updated.csv") #edited name 8/28/2023


## ASCI
data <- read.csv("output_data/00_asci_delta_formatted_median_updated_RF2023.csv") #edited by Rachel 8/28/2023
head(data)

data <- select(data, d_ds_mag_50:delta_q99)

data_cor <- cor(data, method="spearman", use="complete.obs") ## spearman

write.csv(data_cor, "output_data/manuscript/04_ffm_cor_asci_updated.csv") #edited name 8/28/2023
