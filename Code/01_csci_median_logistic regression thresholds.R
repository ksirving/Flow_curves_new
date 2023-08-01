## glms for csci and median deltah

library(reshape)
library(CSCI)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(rcompanion)
library(tidyr)


## upload new formatted data

new_csci <- read.csv("output_data/00_csci_delta_formatted_median_updated_RF2023.csv")
head(new_csci)
dim(new_csci)
unique(new_csci$masterid)


# testing data - investigation --------------------------------------------

out.dir <- "output_data/Manuscript/Figures/"

## full names for labels
labels <- read.csv("Data/ffm_names.csv")
labels <- labels[1:24, ]
labels <- labels %>% rename(hydro.endpoints = Flow.Metric.Code)
labels[25, 1] <- "Magnitude of largest annual storm"
labels[25, 2] <- "Q99"
labels[25, 3] <- "Peak Flow"
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

## plot raw data
range(na.omit(new_csci$delta_q99))

q3 <- ggplot(new_csci, aes(x=delta_q99, y=csci))+
  # geom_stat()+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), linewidth = 1)+
  # facet_wrap(~Type, scales = "free_x") +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank()) +
  # scale_y_continuous(limits=c(0,1))+
  theme_minimal()+
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 60,  vjust = 0.5, hjust=0.5)) +
  labs(title = paste(main.title),
       x = "Delta H",
       y = "CSCI Score") #+ theme_bw(base_size = 15)
q3

file.name1 <- paste0(out.dir, "01_raw_csci_q99.jpg")
ggsave(q3, filename=file.name1, dpi=300, height=5, width=7.5)

## upload stationcode/masterid cross walk
load(file = "/Users/katieirving/Library/CloudStorage/OneDrive-SCCWRP/Documents - Katie’s MacBook Pro/git/Cannabis_Eflows/ignore/SMC_csci_cali_July2023.RData")
names(bug_tax_ca)
Sites
Sites <- bug_tax_ca %>%
  dplyr::select(stationcode, masterid) %>%
  rename(StationCode = stationcode)

## join to csci data - use inner join as a filter
csciRefs <- inner_join(new_csci, Sites, by = c( "masterid"), relationship = "many-to-many")
csciRefs

unique(csciRefs$masterid)

## upload ref sites
refSites <- read.csv("Data/ref sites.csv") 
refSites

## join to sites to cross walk masterid
refsCW <- inner_join(csciRefs, refSites, by ="StationCode") %>% distinct()
refsCW

unique(refsCW$StationCode)
unique(refsCW$masterid)

#### make new column in csci df for ref sites
new_csci <- new_csci %>%
  mutate(RefSite = ifelse(masterid %in% refsCW$masterid, "Yes", "No"))

names(new_csci)
## outliers investigation - seems to be driving the negative
sum(na.omit(new_csci$delta_q99 < -10000))

## filer outliers out
outliers <- new_csci %>%
  filter(delta_q99 < -10000) %>%
  dplyr::select(-X) %>% distinct()

write.csv(outliers, "Outliers.csv")
## remove outliers (n=5) and check plot
outliers$masterid

new_csci_sub <- new_csci %>% filter(!delta_q99 < -10000)  %>%
  pivot_longer(d_ds_mag_50:delta_q99, names_to = "hydro.endpoints", values_to = "hydro")
new_csci_sub

new_csci_sub <- left_join(new_csci_sub, labels, by ="hydro.endpoints")

## define FFM to loop through
HydroEnds <- unique(new_csci_sub$hydro.endpoints)

HydroEnds
m=1

for(m in 1:length(HydroEnds)) {
  
  ## title of FFM
  main.title <- new_csci_sub %>%
    ungroup() %>%
    filter(hydro.endpoints == paste(HydroEnds[m])) %>%
    dplyr::select(Flow.Metric.Name) %>%
    distinct(Flow.Metric.Name)
  
  ## subset data and put in order for geom.path
  all_cscix <- subset(new_csci_sub,hydro.endpoints == paste(HydroEnds[m]))
  # all_cscix <- all_cscix[order(all_cscix$PredictedProbabilityScaled, all_cscix$hydro),]
  
  
  ## plot 
  q3 <- ggplot(all_cscix, aes(x=hydro, y=csci, group = RefSite, col = RefSite))+
    geom_point(aes(colour = factor(RefSite)))+
    theme(strip.background = element_blank(),
          strip.text.y = element_blank()) +
    # scale_y_continuous(limits=c(0,1))+
    theme_minimal()+
    theme(text = element_text(size=15),axis.text.x = element_text(angle = 60,  vjust = 0.5, hjust=0.5)) +
    labs(title = paste(main.title),
         x = "Delta H",
         y = "CSCI Score") #+ theme_bw(base_size = 15)
  
  q3
  out.filename <- paste0(out.dir,"01_raw_csci_", paste(HydroEnds[m]), "_scatter.jpg")
  ggsave(q3, file = out.filename, dpi=300, height=4, width=6)
  
  
}


# plot with outliers
new_csci <- new_csci  %>%
  pivot_longer(d_ds_mag_50:delta_q99, names_to = "hydro.endpoints", values_to = "hydro")
new_csci

new_csci <- left_join(new_csci, labels, by ="hydro.endpoints")

## define FFM to loop through
HydroEnds <- unique(new_csci_sub$hydro.endpoints)

HydroEnds
m=1

for(m in 1:length(HydroEnds)) {
  
  ## title of FFM
  main.title <- new_csci %>%
    ungroup() %>%
    filter(hydro.endpoints == paste(HydroEnds[m])) %>%
    dplyr::select(Flow.Metric.Name) %>%
    distinct(Flow.Metric.Name)
  
  ## subset data and put in order for geom.path
  all_cscix <- subset(new_csci,hydro.endpoints == paste(HydroEnds[m]))
  # all_cscix <- all_cscix[order(all_cscix$PredictedProbabilityScaled, all_cscix$hydro),]
  
  
  ## plot 
  q3 <- ggplot(all_cscix, aes(x=hydro, y=csci, group = RefSite, col = RefSite))+
    geom_point(aes(colour = factor(RefSite)))+
    theme(strip.background = element_blank(),
          strip.text.y = element_blank()) +
    # scale_y_continuous(limits=c(0,1))+
    theme_minimal()+
    theme(text = element_text(size=15),axis.text.x = element_text(angle = 60,  vjust = 0.5, hjust=0.5)) +
    labs(title = paste(main.title),
         x = "Delta H",
         y = "CSCI Score") #+ theme_bw(base_size = 15)
  
  q3
  out.filename <- paste0(out.dir,"01_raw_csci_", paste(HydroEnds[m]), "_scatter_with_outliers.jpg")
  ggsave(q3, file = out.filename, dpi=300, height=4, width=6)
  
  
}





# mod <- read.csv("/Users/katieirving/OneDrive - SCCWRP/Documents - Katie’s MacBook Pro/Projects/ModifiedStreams/data/modchannels_katie.csv")
# 
# sum(mod$masterid %in% new_csci$stationcode)

# GLMs --------------------------------------------------------------------


## remove X column, change names
names(new_csci)
new_csci <- new_csci[, -c(1)]
names(new_csci)[4:6] <- c("OoverE", "MMI", "CSCI")

## define index thresholds, bio & hydro endpoints
thresholds <- c(0.63, 0.79, 0.92) 
biol.endpoints<-c("CSCI","OoverE","MMI")
hydro.endpoints<- colnames(new_csci)[10:18]

## create df with all combinations of threshold and endpoints
bio_h_summary<-  expand.grid(biol.endpoints=biol.endpoints,hydro.endpoints=hydro.endpoints, 
                             thresholds = thresholds,  stringsAsFactors = F)
bio_h_summary

## save
write.csv(bio_h_summary, "output_data/01_csci_hydro_endpoints_order_July2023.csv")


## GLMs per ffm, threshold and endpoints
## negative delta

neg.glm<-lapply(1:nrow(bio_h_summary), function(i)
  {
  
  hmet<-as.character(bio_h_summary[i,"hydro.endpoints"])
  bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
  bmet.thresh<- as.character(bio_h_summary[i,"thresholds"])
  
  mydat<-na.omit(new_csci[,c(hmet, bmet)])
  names(mydat)<-c( "hydro","bio")
  
  mydat <- mydat[which(mydat$hydro<0),]
  
  mydat$Condition<-ifelse(mydat$bio< bmet.thresh,0,1)
  mydat<-mydat[order(mydat$bio),]
  
  glm(Condition~hydro, family=binomial(link="logit"), data=mydat)
    
    
})
neg.glm
save(neg.glm, file="models/01_CSCI_negative_GLM_all_delta_mets_July2023.RData")


## extract data points from models
data <- NULL
data <- as.data.frame(data)
bio_h_summary$comb_code <- paste(bio_h_summary$biol.endpoints, "_", bio_h_summary$hydro.endpoints, "_", bio_h_summary$thresholds, sep="")
length(bio_h_summary)

code <- bio_h_summary$comb_code

  for(i in 1: length(code)) {
  
    hmet<-as.character(bio_h_summary[i,"hydro.endpoints"])
    bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
    bmet.thresh<- as.character(bio_h_summary[i,"thresholds"])
    
    mydat<-na.omit(new_csci[,c(hmet, bmet)])
    names(mydat)<-c( "hydro","bio")
    
    mydat <- mydat[which(mydat$hydro<0 ),]
    
    mydat$Condition<-ifelse(mydat$bio< bmet.thresh,0,1)
    mydat<-mydat[order(mydat$bio),]
    mydat$site_num <- rownames(mydat)

    mydat$hydro_code <- hmet
    mydat$bio <- bmet
    mydat$threshold <- bmet.thresh

    data <- bind_rows(data, mydat)
    # dim(data)
}

data_neg <- data
head(data_neg)
## positive delta
pos.glm<-lapply(1:nrow(bio_h_summary), function(i)
{
  hmet<-as.character(bio_h_summary[i,"hydro.endpoints"])
  bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
  bmet.thresh<-as.character(bio_h_summary[i,"thresholds"])
  
  mydat<-na.omit(new_csci[,c(hmet, bmet)])
  names(mydat)<-c( "hydro","bio")
  
  mydat <- mydat[which(mydat$hydro>=0),]
  
  mydat$Condition<-ifelse(mydat$bio< bmet.thresh,0,1)
  mydat<-mydat[order(mydat$bio),]

  glm(Condition~hydro, family=binomial(link="logit"), data=mydat)
  
 
})

# save
save(pos.glm, file="models/01_CSCI_positive_GLM_all_delta_mets_July2023.RData")

## extract datapoints from model
data <- NULL
data <- as.data.frame(data)
bio_h_summary$comb_code <- paste(bio_h_summary$biol.endpoints, "_", bio_h_summary$hydro.endpoints, "_", bio_h_summary$thresholds, sep="")
length(bio_h_summary)

code <- bio_h_summary$comb_code
i=52

for(i in 1: length(code)) {
  
  hmet<-as.character(bio_h_summary[i,"hydro.endpoints"])
  bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
  bmet.thresh<-as.character(bio_h_summary[i,"thresholds"])
  
  mydat<-na.omit(new_csci[,c(hmet, bmet)])
  names(mydat)<-c( "hydro","bio")
  
  mydat <- mydat[which(mydat$hydro>=0  ),]
  
  mydat$Condition<-ifelse(mydat$bio< bmet.thresh,0,1)
  mydat<-mydat[order(mydat$bio),]
  mydat$site_num <- rownames(mydat)
 
  mydat$hydro_code <- hmet
  mydat$bio <- bmet
  mydat$threshold <- bmet.thresh

  data <- bind_rows(data, mydat)
  dim(data)
  
}

data_pos <- data
data_pos

##### extract glm coeficients

coefs <- data.frame(matrix(ncol=11, nrow=81))
colnames(coefs) <- c("InterceptCoef", "VariableCoef", "Deviance", "AIC", "NullDeviance",
                     "InterceptPvalue", "VariablePvalue", "Delta", "McFadden", "Nagelkerke", "n")

for(i in 1:length(neg.glm)) {
  
  mod <- neg.glm[[i]]
  rsq <- nagelkerke(mod)

  coefs[i,1] <- coef(mod)[1]
  coefs[i,2] <- coef(mod)[2]
  coefs[i,3] <- mod$deviance
  coefs[i,4] <- mod$aic
  coefs[i,5] <- mod$null.deviance
  coefs[i,6:7] <- coef(summary(mod))[,4]
  coefs[i,8] <- "Negative"
  coefs[i,9] <- rsq$Pseudo.R.squared.for.model.vs.null[1]
  coefs[i,10] <- rsq$Pseudo.R.squared.for.model.vs.null[3]
  coefs[i,11] <- rsq$Number.of.observations[1]
}


## join with main summary df
bio_h_summary
cscinegcoefs <- cbind(bio_h_summary, coefs)
cscinegcoefs

## positive
coefs <- data.frame(matrix(ncol=11, nrow=81))
colnames(coefs) <- c("InterceptCoef", "VariableCoef", "Deviance", "AIC", "NullDeviance",
                     "InterceptPvalue", "VariablePvalue", "Delta", "McFadden", "Nagelkerke", "n")
pos.glm
for(i in 1:length(pos.glm)) {
  
  mod <- pos.glm[[i]]
  rsq <- nagelkerke(mod)
  
  coefs[i,1] <- coef(mod)[1]
  coefs[i,2] <- coef(mod)[2]
  coefs[i,3] <- mod$deviance
  coefs[i,4] <- mod$aic
  coefs[i,5] <- mod$null.deviance
  coefs[i,6:7] <- coef(summary(mod))[,4]
  coefs[i,8] <- "Positive"
  coefs[i,9] <- rsq$Pseudo.R.squared.for.model.vs.null[1]
  coefs[i,10] <- rsq$Pseudo.R.squared.for.model.vs.null[3]
  coefs[i,11] <- rsq$Number.of.observations[1]
}
  



## join with main summary df
bio_h_summary
csciposcoefs <- cbind(bio_h_summary, coefs)
csciposcoefs

### join pos and neg 

csci_coefs <- rbind(csciposcoefs, cscinegcoefs)

## save coefs
write.csv(csci_coefs, "output_data/manuscript/01_csci_glm_coefs.csv")


## extract predicted probability

head(data_neg)

code <- bio_h_summary$comb_code

## change names in df and make code
data_neg <- data_neg %>%
  select(-site_num) %>%
  rename(thresholds = threshold,  biol.endpoints = bio, hydro.endpoints = hydro_code ) %>%
  mutate(comb_code = paste(biol.endpoints, "_", hydro.endpoints,"_", thresholds, sep=""))

datx <- NULL
for(i in 1:length(code)) {
  
  dat <- data_neg %>%
    filter(comb_code == code[i]) 
  
  dat <- na.omit(dat)
  head(dat)
  
  modnum<-  which(bio_h_summary$comb_code== code[i])
  modnum
  
  
  mymod<-neg.glm[[modnum]]
  
  
  mydata<-data.frame(hydro = dat$hydro)
  mydata
  
  
  dat[,"PredictedProbability"] <-predict(mymod, newdata=mydata, type="response")
  
  datx <- rbind(datx, dat)
}

bio_h_summary_neg <- datx

## ad neg or pos
bio_h_summary_neg$Type<-"Negative"

## add code

data_pos <- data_pos %>%
  select(-site_num) %>%
  rename(thresholds = threshold,  biol.endpoints = bio, hydro.endpoints = hydro_code ) %>%
  mutate(comb_code = paste(biol.endpoints, "_", hydro.endpoints,"_", thresholds, sep=""))


## extract predicted probability
datx <- NULL
for(i in 1:length(code)) {
  
  dat <- data_pos %>%
    filter(comb_code == code[i]) 
  
  dat <- na.omit(dat)
  head(dat)
  
  modnum<-  which(bio_h_summary$comb_code== code[i])
  modnum
    

    mymod<-pos.glm[[modnum]]
    
    
    mydata<-data.frame(hydro = dat$hydro)
    mydata
    
    
    dat[,"PredictedProbability"] <-predict(mymod, newdata=mydata, type="response")
    
    datx <- rbind(datx, dat)
  }
  

bio_h_summary_pos <- datx

## ad neg or pos
bio_h_summary_pos$Type<-"Positive"
head(bio_h_summary_pos)

all_data <- rbind(bio_h_summary_pos, bio_h_summary_neg)

head(all_data)

all_data$thresholds <- as.factor(all_data$thresholds)

# save - df for BRTs
write.csv(all_data, "output_data/01_bugs_all_data_neg_pos_logR_metrics_figures_July2023.csv")

### CSCI endpoint only
all_data_csci <- subset(all_data,biol.endpoints=="CSCI")
head(all_data_csci)
write.csv(all_data_csci, "output_data/01_csci_neg_pos_logR_metrics_figures_July2023.csv")

str(bio_h_summary_neg)
