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
                            formula=q_total~treatment+school,
                            cluster=full_data_temp$class_id)

m1 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=q_total~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m02 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery~treatment+school,family="binomial",
                            cluster=full_data_temp$class_id)

m2 <- miceadds::glm.cluster(data=full_data_temp, 
                           formula=lottery~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school, family="binomial",
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

# Make dummy models
m01_d <- lm(alevel_interest~treatment+school, data=full_data_temp)
m1_d <- lm(alevel_interest~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school, data=full_data_temp)
m02_d <- glm(lottery~treatment+school, family="binomial",data=full_data_temp)
m2_d <- glm(lottery~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school, family="binomial",data=full_data_temp)


#### WWCGs ####
full_data_temp<-subset(full_data, full_data$wwcg==1)

m03 <- miceadds::lm.cluster(data=full_data_temp, 
                            formula=q_total~treatment+school,
                            cluster=full_data_temp$class_id)

m3 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=q_total~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m04 <- miceadds::glm.cluster(data=full_data_temp, 
                             formula=lottery~treatment+school,family="binomial",
                             cluster=full_data_temp$class_id)

m4 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school, family="binomial",
                            cluster=full_data_temp$class_id)


# Extract parameters for output
a<-as.data.frame((summary(m03)))
e_5<-c(a$`Std. Error`)
b<-as.data.frame((summary(m3)))
e_6<-c(b$`Std. Error`)
c<-as.data.frame((summary(m04)))
e_7<-c(c$`Std. Error`)
d<-as.data.frame((summary(m4)))
e_8<-c(d$`Std. Error`)

# Make dummy models
m03_d <- lm(alevel_interest~treatment+school, data=full_data_temp)
m3_d <- lm(alevel_interest~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school, data=full_data_temp)
m04_d <- glm(lottery~treatment+school, family="binomial",data=full_data_temp)
m4_d <- glm(lottery~treatment+fsm+sen+mob+maths_score+prop_wwcb_class+school, family="binomial",data=full_data_temp)


#### TABLE###
stargazer(m1_d, m2_d, m3_d, m4_d, 
          se = list(e_2, e_4, e_6, e_8) ,
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
          font.size="small")

,
          type="text")

