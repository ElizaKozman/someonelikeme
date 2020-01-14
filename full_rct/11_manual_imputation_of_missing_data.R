library(stargazer)
library(kableExtra)
library(knitr)
library(plyr)
library(dplyr)
library(purrr)
library(stargazer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape)
library(BBmisc)
library(extrafont)
library("xtable")
library(rmarkdown)
library(mice)
library(VIM)

full_data<-read.csv("rand_matched_with_full_data_and_school_data_formatted_2.csv")

# Keep rows where sheets returned and not blank
full_data<-subset(full_data, sheet_returned==1)
full_data<-subset(full_data, p1_blank==0)
table(full_data$wwcb)
#### MEAN IMPUTATION - S1 ####

# Let's manually impute the isq_total for people who didn't complete all the items
full_data$n_unanswered<-sapply(full_data$isq_1, function(x) sum(is.na (x)))+sapply(full_data$isq_2, function(x) sum(is.na (x)))+sapply(full_data$isq_3, function(x) sum(is.na (x)))+sapply(full_data$isq_4, function(x) sum(is.na (x)))+sapply(full_data$isq_5, function(x) sum(is.na (x)))+sapply(full_data$isq_6, function(x) sum(is.na (x)))+sapply(full_data$isq_7, function(x) sum(is.na (x)))
table(full_data$n_unanswered)
table(full_data$n_unanswered, full_data$wwcb)

for (i in 1:nrow(full_data))
  
{
  full_data$sum_existing[i]<-sum(full_data$isq_1[i],full_data$isq_2[i],full_data$isq_3[i],full_data$isq_4[i],full_data$isq_5[i], full_data$isq_6[i], full_data$isq_7[i], na.rm=TRUE) 
}

full_data$n_answered<-7-full_data$n_unanswered

full_data$average[!(full_data$sum_existing==0)]<-full_data$sum_existing[!(full_data$sum_existing==0)]/(full_data$n_answered[!(full_data$sum_existing==0)])
full_data$isq_imputed<-round((full_data$average),2)

# The mean of the imputed totals is about the same for actual results
mean(full_data$isq_total, na.rm=T)
mean(full_data$isq_imputed, na.rm=T)

# We add 1168-875=293 more observations this way (overall)
summary(full_data$isq_total)
summary(full_data$isq_imputed)

# We add 283-231=52 more observations this way (for wwcbs)
full_data_wwcb<-subset(full_data, full_data$wwcb==1)
summary(full_data_wwcb$isq_total)
summary(full_data_wwcb$isq_imputed)

write.csv(full_data, "manually_imputed_data.csv")

#### MEAN IMPUTATION - S2 ####
full_data<-read.csv("rand_matched_with_full_data_and_school_data_formatted_2.csv")
# Keep rows where sheets returned and not blank
full_data<-subset(full_data, sheet_returned_s2==1)
full_data<-subset(full_data, p1_blank_s2==0)

# Let's manually impute the isq_total_s2 for people who didn't complete all the items
full_data$n_unanswered<-sapply(full_data$isq_1_s2, function(x) sum(is.na (x)))+sapply(full_data$isq_2_s2, function(x) sum(is.na (x)))+sapply(full_data$isq_3_s2, function(x) sum(is.na (x)))+sapply(full_data$isq_4_s2, function(x) sum(is.na (x)))+sapply(full_data$isq_5_s2, function(x) sum(is.na (x)))+sapply(full_data$isq_6_s2, function(x) sum(is.na (x)))+sapply(full_data$isq_7_s2, function(x) sum(is.na (x)))
table(full_data$n_unanswered, full_data$wwcb)
table(full_data$wwcb)

for (i in 1:nrow(full_data))
  
{
  full_data$sum_existing[i]<-sum(full_data$isq_1_s2[i],full_data$isq_2_s2[i],full_data$isq_3_s2[i],full_data$isq_4_s2[i],full_data$isq_5_s2[i], full_data$isq_6_s2[i], full_data$isq_7_s2[i], na.rm=TRUE) 
}

full_data$n_answered<-7-full_data$n_unanswered

full_data$average[!(full_data$sum_existing==0)]<-full_data$sum_existing[!(full_data$sum_existing==0)]/(full_data$n_answered[!(full_data$sum_existing==0)])
full_data$isq_imputed_s2<-round((full_data$average),2)

# The mean of the imputed totals is about the same for actual results
mean(full_data$isq_total_s2, na.rm=T)
mean(full_data$isq_imputed_s2, na.rm=T)

# We add 243-2=241 more observations this way (overall)
summary(full_data$isq_total_s2)
summary(full_data$isq_imputed_s2)

# We add 53 more observations this way (for wwcbs)
full_data_wwcb<-subset(full_data, full_data$wwcb==1)
summary(full_data_wwcb$isq_total_s2)
summary(full_data_wwcb$isq_imputed_s2)

write.csv(full_data, "manually_imputed_data_s2.csv")
