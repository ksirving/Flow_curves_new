## correlation matrix


### CSCI
data <- read.csv("output_data/00_csci_delta_formatted_median_updated_Nov2021.csv")
head(data)

data <- select(data, DS_Dur_WS:Wet_Tim)

data_cor <- cor(data, method="spearman", use="complete.obs") ## spearman

write.csv(data_cor, "output_data/manuscript/04_ffm_cor_csci.csv")


## ASCI
data <- read.csv("output_data/00_asci_delta_formatted_median_Nov2021.csv")
head(data)

data <- select(data, DS_Dur_WS:Wet_Tim)

data_cor <- cor(data, method="spearman", use="complete.obs") ## spearman

write.csv(data_cor, "output_data/manuscript/04_ffm_cor_asci.csv")