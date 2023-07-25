## ASCI GLMs

library(reshape)
library(CSCI)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(rcompanion)
library(tidyr)

# Problem stationcodes from ASCI: 906M21738,     
# 902M20173

## thresholds for asci - Susie paper, table 8 - hybrid MMI 0.86
## thresholds - 0.75, 0.86, 0.94

## upload asci data
asci<-read.csv("output_data/00_asci_delta_formatted_median_Nov2021.csv")
head(asci)
dim(asci)
asci <- asci[, -c(1)]

## bio 

biol.endpoints<-c("H_ASCI","D_ASCI")#

## hydro
hydro.endpoints<- colnames(asci)[6:21]

## thresholds

thresholds <- c(0.75, 0.86, 0.94) ## hybrid and diatom are the same

bio_h_summary<-  expand.grid(biol.endpoints=biol.endpoints,hydro.endpoints=hydro.endpoints, thresholds = thresholds,  stringsAsFactors = F)
bio_h_summary

write.csv(bio_h_summary, "output_data/01_asci_hydro_endpoints_order_April2021.csv")
i=1
neg.glm<-lapply(1:nrow(bio_h_summary), function(i)
{
  
  hmet<-as.character(bio_h_summary[i,"hydro.endpoints"])
  bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
  bmet.thresh<- as.character(bio_h_summary[i,"thresholds"])

  mydat<-na.omit(asci[,c(hmet, bmet)])
  names(mydat)<-c( "hydro","bio")
  
  mydat <- mydat[which(mydat$hydro<0 ),]

  mydat$Condition<-ifelse(mydat$bio< bmet.thresh,0,1)
  mydat<-mydat[order(mydat$bio),]
  
  glm(Condition~hydro, family=binomial(link="logit"), data=mydat)
  
  
})
save(neg.glm, file="models/01a_ASCI_negative_GLM_all_delta_mets_April2021.RData")
length(neg.glm)
data <- NULL

data <- as.data.frame(data)
# data
bio_h_summary$comb_code <- paste(bio_h_summary$biol.endpoints, "_", bio_h_summary$hydro.endpoints, "_", bio_h_summary$thresholds, sep="")
length(bio_h_summary)

code <- bio_h_summary$comb_code
code
i=1

for(i in 1: length(code)) {
  
  hmet<-as.character(bio_h_summary[i,"hydro.endpoints"])
  bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
  bmet.thresh<- as.character(bio_h_summary[i,"thresholds"])
  
  mydat<-na.omit(asci[,c( hmet, bmet)])
  names(mydat)<-c( "hydro","bio")

  mydat <- mydat[which(mydat$hydro<0 ),]

  mydat$Condition<-ifelse(mydat$bio< bmet.thresh,0,1)
  mydat<-mydat[order(mydat$bio),]
  mydat$site_num <- rownames(mydat)


  mydat$hydro_code <- hmet
  mydat$bio <- bmet
  mydat$threshold <- bmet.thresh
  names(data)

  data <- bind_rows(data, mydat)
  
  
}

data_neg <- data


pos.glm<-lapply(1:nrow(bio_h_summary), function(i)
{
  hmet<-as.character(bio_h_summary[i,"hydro.endpoints"])
  bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
  bmet.thresh<-as.character(bio_h_summary[i,"thresholds"])
  
  mydat<-na.omit(asci[,c( hmet, bmet)])
  names(mydat)<-c( "hydro","bio")
  
  mydat <- mydat[which(mydat$hydro>=0),]

  mydat$Condition<-ifelse(mydat$bio< bmet.thresh,0,1)
  mydat<-mydat[order(mydat$bio),]

  
  glm(Condition~hydro, family=binomial(link="logit"), data=mydat)
  
})

save(pos.glm, file="models/01a_ASCI_positive_GLM_all_delta_mets_April2021.RData")

data <- NULL
data <- as.data.frame(data)


bio_h_summary$comb_code <- paste(bio_h_summary$biol.endpoints, "_", bio_h_summary$hydro.endpoints, "_", bio_h_summary$thresholds, sep="")
length(bio_h_summary)

code <- bio_h_summary$comb_code
code

for(i in 1: length(code)) {
  
  hmet<-as.character(bio_h_summary[i,"hydro.endpoints"])
  bmet<-as.character(bio_h_summary[i,"biol.endpoints"])
  bmet.thresh<-as.character(bio_h_summary[i,"thresholds"])
  
  mydat<-na.omit(asci[,c( hmet, bmet)])
  names(mydat)<-c( "hydro","bio")
  
  mydat <- mydat[which(mydat$hydro>=0  ),]

  mydat$Condition<-ifelse(mydat$bio< bmet.thresh,0,1)
  mydat<-mydat[order(mydat$bio),]
  mydat$site_num <- rownames(mydat)
  # write.csv(mydat, paste("output_data/glm_data/06_", bmet,"_neg_", hmet,  "_glm.csv", sep=""))
  
  # glm(Condition~hydro, family=binomial(link="logit"), data=mydat)
  
  mydat
  mydat$hydro_code <- hmet
  mydat$bio <- bmet
  mydat$threshold <- bmet.thresh
  names(data)
  names(mydat)
  data <- bind_rows(data, mydat)
  
  
}

data_pos <- data


##### glm coeficients
library(rcompanion)

coefs <- data.frame(matrix(ncol=11, nrow=96))
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
ascinegcoefs <- cbind(bio_h_summary, coefs)

## positive

coefs <- data.frame(matrix(ncol=11, nrow=96))
colnames(coefs) <- c("InterceptCoef", "VariableCoef", "Deviance", "AIC", "NullDeviance",
                     "InterceptPvalue", "VariablePvalue", "Delta", "McFadden", "Nagelkerke", "n")

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

asciposcoefs <- cbind(bio_h_summary, coefs)


### join pos and neg 

asci_coefs <- rbind(asciposcoefs, ascinegcoefs)
getwd()
write.csv(asci_coefs, "output_data/manuscript/01_asci_glm_coefs.csv")

## extract predicted probability

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
write.csv(all_data, "output_data/01_algae_all_data_neg_pos_logR_metrics_figures_April2021.csv")

### CSCI endpoint only
all_data_csci <- subset(all_data,biol.endpoints=="H_ASCI")
head(all_data_csci)
write.csv(all_data_csci, "output_data/01_h_asci_neg_pos_logR_metrics_figures_April2021.csv")

str(bio_h_summary_neg)
