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
library(miceadds)

full_data<-read.csv("pilot_rand_matched_outcomes_formatted.csv")

#### UNI VERSUS CONTROL WWCB ####
#full_data_temp<-subset(full_data, full_data$wwcb==1&!is.na(full_data$maths_score))
full_data_temp<-subset(full_data, full_data$wwcb==1)

m1 <- miceadds::lm.cluster(data=full_data_temp, 
                             formula=isq_total~treatment+school,
                             cluster=full_data_temp$class_id)

m2 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total~treatment+fsm+sen+mob+school,
                           cluster=full_data_temp$class_id)

m3 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total~treatment+fsm+sen+mob+maths_score+school,
                           cluster=full_data_temp$class_id)

m4 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total~treatment+fsm+sen+mob+maths_score+prop_wwcb_class++school,
                           cluster=full_data_temp$class_id)


# Extract parameters for output
a<-as.data.frame((summary(m1)))
e_1<-c(a$`Std. Error`)
b<-as.data.frame((summary(m2)))
e_2<-c(b$`Std. Error`)
c<-as.data.frame((summary(m3)))
e_3<-c(c$`Std. Error`)
d<-as.data.frame((summary(m4)))
e_4<-c(d$`Std. Error`)

e_1
summary(m1)

# Make dummy models
m1_d <- lm(isq_total~treatment+school, data=full_data_temp)
m2_d <- lm(isq_total~treatment+fsm+sen+mob+school, data=full_data_temp)
m3_d <- lm(isq_total~treatment+fsm+sen+mob+maths_score+school, data=full_data_temp)
m4_d <- lm(isq_total~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school, data=full_data_temp)

#### UNI VERSUS CONTROL WWCG ####
full_data_temp<-subset(full_data, full_data$wwcg==1)
describe(full_data_temp)

m5 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total~treatment+school,
                           cluster=full_data_temp$class_id)
summary(m1)

m6 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total~treatment+fsm+sen+mob+school,
                           cluster=full_data_temp$class_id)

m7 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total~treatment+fsm+sen+mob+maths_score+school,
                           cluster=full_data_temp$class_id)

m8 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

# Extract parameters for output
a<-as.data.frame((summary(m5)))
e_5<-c(a$`Std. Error`)
b<-as.data.frame((summary(m6)))
e_6<-c(b$`Std. Error`)
c<-as.data.frame((summary(m7)))
e_7<-c(c$`Std. Error`)
d<-as.data.frame((summary(m8)))
e_8<-c(d$`Std. Error`)


# Make dummy models
m5_d <- lm(isq_total~treatment+school, data=full_data_temp)
m6_d <- lm(isq_total~treatment+fsm+sen+mob+school, data=full_data_temp)
m7_d <- lm(isq_total~treatment+fsm+sen+mob+maths_score+school, data=full_data_temp)
m8_d <- lm(isq_total~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school, data=full_data_temp)


#### UNI VERSUS CONTROL WWC ####
full_data_temp<-subset(full_data, full_data$wwc==1)
describe(full_data_temp)

m9 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total~treatment+school,
                           cluster=full_data_temp$class_id)

m10 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total~treatment+fsm+sen+mob+male+school,
                           cluster=full_data_temp$class_id)

m11 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total~treatment+fsm+sen+mob+male+maths_score+school,
                           cluster=full_data_temp$class_id)

m12 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total~treatment+fsm+sen+mob+male+maths_score+prop_wwcb_class+school+male*treatment,
                           cluster=full_data_temp$class_id)

# Extract parameters for output
a<-as.data.frame((summary(m9)))
e_9<-c(a$`Std. Error`)
b<-as.data.frame((summary(m10)))
e_10<-c(b$`Std. Error`)
c<-as.data.frame((summary(m11)))
e_11<-c(c$`Std. Error`)
d<-as.data.frame((summary(m12)))
e_12<-c(d$`Std. Error`)


# Make dummy models
m9_d <- lm(isq_total~treatment+school, data=full_data_temp)
m10_d <- lm(isq_total~treatment+fsm+sen+mob+male+school, data=full_data_temp)
m11_d <- lm(isq_total~treatment+fsm+sen+mob+male+maths_score+school, data=full_data_temp)
m12_d <- lm(isq_total~treatment+fsm+sen+mob+male+maths_score+prop_wwcb_class+school+male*treatment, data=full_data_temp)

#### CREATE TABLE ####
stargazer(m1_d, m2_d, m3_d, m4_d,m5_d, m6_d, m7_d, m8_d, m11_d, m12_d,      
          se = list(e_1, e_2, e_3, e_4, e_5, e_6, e_7, e_8, e_11, e_12) ,
          dep.var.labels.include = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("+", "*", "**"), 
          omit = c("school", "sen", "mob"),
          title="",
          notes.append = FALSE,
          notes.label = "",
          omit.stat = c("f", "rsq", "adj.rsq", "ser" ),
          header=FALSE,
          column.sep.width = "-1pt",
          font.size="small",
          type="text")

#### FIND COHEN'S D ####

# Use treatment coefficient from model 4
full_data_temp<-subset(full_data, full_data$wwcb==1)

temp<-summary(m4)
treatment_term<-temp[2, 1]
treatment_term
treatment_term<-as.numeric(treatment_term)

table(full_data_temp$treatment)
sd(full_data_temp$isq_total, na.rm = T)
sd1<-sd(full_data_temp$isq_total[(full_data_temp$treatment==1)], na.rm=T)
sd2<-sd(full_data_temp$isq_total[full_data_temp$treatment==0], na.rm=T)
sd1
sd2
sd<-sqrt((sd1^2+sd2^2)/2)
sd
Cohen_d<-treatment_term/sd
Cohen_d

control_mean<-mean(full_data_temp$isq_total[full_data_temp$treatment==0], na.rm=T)
control_mean


