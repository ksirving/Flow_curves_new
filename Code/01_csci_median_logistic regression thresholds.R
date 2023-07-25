## glms for csci and median deltah

library(reshape)
library(CSCI)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(rcompanion)
library(tidyr)


## upload new formatted data

new_csci <- read.csv("output_data/00_csci_delta_formatted_median_updated_Nov2021.csv")
head(new_csci)
dim(new_csci)

mod <- read.csv("/Users/katieirving/OneDrive - SCCWRP/Documents - Katie’s MacBook Pro/Projects/ModifiedStreams/data/modchannels_katie.csv")

sum(mod$masterid %in% new_csci$stationcode)

## remove X column, change names
names(new_csci)
new_csci <- new_csci[, -c(1)]
names(new_csci)[4:6] <- c("OoverE", "MMI", "CSCI")

## define index thresholds, bio & hydro endpoints
thresholds <- c(0.63, 0.79, 0.92) 
biol.endpoints<-c("CSCI","OoverE","MMI")
hydro.endpoints<- colnames(new_csci)[9:24]

## create df with all combinations of threshold and endpoints
bio_h_summary<-  expand.grid(biol.endpoints=biol.endpoints,hydro.endpoints=hydro.endpoints, 
                             thresholds = thresholds,  stringsAsFactors = F)
bio_h_summary

## save
write.csv(bio_h_summary, "output_data/01_csci_hydro_endpoints_order_Nov2020.csv")


## GLMs per ffm, threshold and endpoints
## negative delta

i=52

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
  dim(mydat)
  glm(Condition~hydro, family=binomial(link="logit"), data=mydat)
    
    
})

save(neg.glm, file="models/01_CSCI_negative_GLM_all_delta_mets_April2021.RData")


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
save(pos.glm, file="models/01_CSCI_positive_GLM_all_delta_mets_April2021.RData")

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

coefs <- data.frame(matrix(ncol=11, nrow=144))
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
coefs <- data.frame(matrix(ncol=11, nrow=144))
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
write.csv(all_data, "output_data/01_bugs_all_data_neg_pos_logR_metrics_figures_April2021.csv")

### CSCI endpoint only
all_data_csci <- subset(all_data,biol.endpoints=="CSCI")
head(all_data_csci)
write.csv(all_data_csci, "output_data/01_csci_neg_pos_logR_metrics_figures_April2021.csv")

str(bio_h_summary_neg)
