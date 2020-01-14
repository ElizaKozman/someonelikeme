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


#### WWCBs ####
full_data_temp<-subset(full_data, full_data$wwcb==1)


m01 <- miceadds::lm.cluster(data=full_data_temp, 
                            formula=alevel_interest~treatment+school,
                            cluster=full_data_temp$class_id)

m1 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=alevel_interest~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m02 <- miceadds::lm.cluster(data=full_data_temp, 
                             formula=alevel_likely~treatment+school,
                             cluster=full_data_temp$class_id)

m2 <- miceadds::lm.cluster(data=full_data_temp, 
                            formula=alevel_likely~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school,
                            cluster=full_data_temp$class_id)


m03 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=uni_interest~treatment+school,
                           cluster=full_data_temp$class_id)
summa

m3 <- miceadds::lm.cluster(data=full_data_temp, 
                             formula=uni_interest~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school,
                             cluster=full_data_temp$class_id)

m04 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=uni_likely~treatment+school,
                           cluster=full_data_temp$class_id)

m4 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=uni_likely~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)



# Extract parameters for output
a<-as.data.frame((summary(m01)))
e_1<-c(a$`Std. Error`)
b<-as.data.frame((summary(m1)))
e_2<-c(b$`Std. Error`)
c<-as.data.frame((summary(m02)))
e_3<-c(c$`Std. Error`)
d<-as.data.frame((summary(m2)))
e_4<-c(d$`Std. Error`)
e<-as.data.frame((summary(m03)))
e_5<-c(e$`Std. Error`)
f<-as.data.frame((summary(m3)))
e_6<-c(f$`Std. Error`)
g<-as.data.frame((summary(m04)))
e_7<-c(g$`Std. Error`)
h<-as.data.frame((summary(m4)))
e_8<-c(h$`Std. Error`)

# Make dummy models
m01_d <- lm(alevel_interest~treatment+school, data=full_data_temp)
m1_d <- lm(alevel_interest~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school, data=full_data_temp)
m02_d <- lm(alevel_likely~treatment+school, data=full_data_temp)
m2_d <- lm(alevel_likely~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school, data=full_data_temp)
m03_d <- lm(uni_interest~treatment+school, data=full_data_temp)
m3_d <- lm(uni_interest~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school, data=full_data_temp)
m04_d <- lm(uni_likely~treatment+school,data=full_data_temp)
m4_d <- lm(uni_likely~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school, data=full_data_temp)



#### WWCGs ####
full_data_temp<-subset(full_data, full_data$wwcg==1)


m05 <- miceadds::lm.cluster(data=full_data_temp, 
                            formula=alevel_interest~treatment+school,
                            cluster=full_data_temp$class_id)

m5 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=alevel_interest~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m06 <- miceadds::lm.cluster(data=full_data_temp, 
                            formula=alevel_likely~treatment+school,
                            cluster=full_data_temp$class_id)

m6 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=alevel_likely~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)


m07 <- miceadds::lm.cluster(data=full_data_temp, 
                            formula=uni_interest~treatment+school,
                            cluster=full_data_temp$class_id)


m7 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=uni_interest~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m08 <- miceadds::lm.cluster(data=full_data_temp, 
                            formula=uni_likely~treatment+school,
                            cluster=full_data_temp$class_id)

m8 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=uni_likely~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)



# Extract parameters for output
a<-as.data.frame((summary(m05)))
e_9<-c(a$`Std. Error`)
b<-as.data.frame((summary(m5)))
e_10<-c(b$`Std. Error`)
c<-as.data.frame((summary(m06)))
e_11<-c(c$`Std. Error`)
d<-as.data.frame((summary(m6)))
e_12<-c(d$`Std. Error`)
e<-as.data.frame((summary(m07)))
e_13<-c(e$`Std. Error`)
f<-as.data.frame((summary(m7)))
e_14<-c(f$`Std. Error`)
g<-as.data.frame((summary(m08)))
e_15<-c(g$`Std. Error`)
h<-as.data.frame((summary(m8)))
e_16<-c(h$`Std. Error`)

# Make dummy models
m05_d <- lm(alevel_interest~treatment+school, data=full_data_temp)
m5_d <- lm(alevel_interest~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school, data=full_data_temp)
m06_d <- lm(alevel_likely~treatment+school, data=full_data_temp)
m6_d <- lm(alevel_likely~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school, data=full_data_temp)
m07_d <- lm(uni_interest~treatment+school, data=full_data_temp)
m7_d <- lm(uni_interest~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school, data=full_data_temp)
m08_d <- lm(uni_likely~treatment+school,data=full_data_temp)
m8_d <- lm(uni_likely~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school, data=full_data_temp)

m <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=alevel_likely~treatment+school,
                           cluster=full_data_temp$class_id)

#### TABLE###
stargazer(m1_d, m2_d, m3_d, m4_d, m5_d, m6_d, m7_d, m8_d, 
          se = list(e_2, e_4, e_6, e_8, e_10, e_12, e_14, e_16) ,
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
# Use treatment coefficient from model 7
full_data_temp<-subset(full_data, full_data$wwcg==1)

temp<-summary(m7)
treatment_term<-temp[2, 1]
treatment_term
treatment_term<-as.numeric(treatment_term)

table(full_data_temp$treatment)
sd1<-sd(full_data_temp$isq_total[(full_data_temp$treatment==1)], na.rm=T)
sd2<-sd(full_data_temp$isq_total[full_data_temp$treatment==0], na.rm=T)
sd<-sqrt((sd1^2+sd2^2)/2)
Cohen_d<-treatment_term/sd
Cohen_d


