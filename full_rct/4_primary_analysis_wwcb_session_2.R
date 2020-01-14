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
library(xtable)
library(rmarkdown)
library(mice)
library(miceadds)

#### ANY RM VERSUS CONTROL ####

full_data<-read.csv("rand_matched_with_full_data_and_school_data_formatted_2.csv")
full_data_temp<-subset(full_data, full_data$wwcb==1)

m1 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~any_rm+prime+school,
                           cluster=full_data_temp$class_id)

m2 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m3 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,
                           cluster=full_data_temp$class_id)

m4 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,
                           cluster=full_data_temp$class_id)


m5 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m6 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_high+any_rm*prop_wwcb_high+school,
                           cluster=full_data_temp$class_id)

m7 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m8 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_high+prime*prop_wwcb_high+school,
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
e<-as.data.frame((summary(m5)))
e_5<-c(e$`Std. Error`)
f<-as.data.frame((summary(m6)))
e_6<-c(f$`Std. Error`)
g<-as.data.frame((summary(m7)))
e_7<-c(g$`Std. Error`)
h<-as.data.frame((summary(m8)))
e_8<-c(h$`Std. Error`)


# Make dummy models
m1_d <- lm(isq_total_s2~any_rm+prime+school, data=full_data_temp)
m2_d <- lm(isq_total_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m3_d <- lm(isq_total_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school, data=full_data_temp)
m4_d <- lm(isq_total_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school, data=full_data_temp)
m5_d <- lm(isq_total_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school, data=full_data_temp)
m6_d <-lm(isq_total_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_high+any_rm*prop_wwcb_high+school, data=full_data_temp)
m7_d <- lm(isq_total_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_temp)
m8_d <-lm(isq_total_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_high+prime*prop_wwcb_high+school, data=full_data_temp)

stargazer(m1_d, m2_d, m3_d, m4_d, m5_d, m6_d,  
          se = list(e_1, e_2, e_3, e_4, e_5, e_6),
          dep.var.caption="ISQ score",
          dep.var.labels.include = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("+", "*", "**"), 
          omit = c("school", "fsm", "year", "sen", "mob", "maths_unified","maths_high"),
          covariate.labels = c("Any role model", "Similarity prime", "Proportion WWCB", "High WWCB", "Any role model x Proportion WWCB",  "Any role model x High WWCB"),
          title="",
          notes.append = FALSE,
          notes.label = "",
          omit.table.layout = "n",
          omit.stat = c("f", "rsq", "adj.rsq", "ser" ),
          header=FALSE,
          column.sep.width = "-1pt",
          font.size="scriptsize",
          no.space=TRUE)

#### UNI VERSUS APP ####
full_data<-read.csv("rand_matched_with_full_data_and_school_data_formatted_2.csv")
full_data_temp<-subset(full_data, full_data$wwcb==1)
full_data_temp<-subset(full_data_temp, full_data_temp$any_rm==1)


m1 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~uni_rm+prime+uni_rm*prime+school,
                           cluster=full_data_temp$class_id)

m2 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m3 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,
                           cluster=full_data_temp$class_id)

m4 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,
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


# Make dummy models
m1_d <- lm(isq_total_s2~uni_rm+prime+uni_rm*prime+school, data=full_data_temp)
m2_d <- lm(isq_total_s2~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m3_d <- lm(isq_total_s2~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school, data=full_data_temp)
m4_d <- lm(isq_total_s2~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school, data=full_data_temp)

stargazer(m1_d, m2_d, m3_d, m4_d,    
          se = list(e_1, e_2, e_3, e_4),
          dep.var.caption="ISQ score",
          dep.var.labels.include = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("+", "*", "**"), 
          omit = c("school", "fsm", "year", "sen", "mob", "maths_unified","maths_high", "prop_wwcb_class"),
          covariate.labels = c("University role model", "Similarity prime", "University role model x Similarity prime"),
          title="",
          notes.append = FALSE,
          notes.label = "",
          omit.table.layout = "n",
          omit.stat = c("f", "rsq", "adj.rsq", "ser" ),
          header=FALSE,
          column.sep.width = "-1pt",
          font.size="scriptsize",
          no.space=TRUE)

#### UNI VERSUS CONTROL ####

full_data<-read.csv("rand_matched_with_full_data_and_school_data_formatted_2.csv")
full_data_temp<-subset(full_data, full_data$wwcb==1)

m1 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~t_uni+prime+school,
                           cluster=full_data_temp$class_id)

m2 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m3 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,
                           cluster=full_data_temp$class_id)

m4 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,
                           cluster=full_data_temp$class_id)


m5 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m6 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_high+t_uni*prop_wwcb_high+school,
                           cluster=full_data_temp$class_id)

m7 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m8 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_high+prime*prop_wwcb_high+school,
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
e<-as.data.frame((summary(m5)))
e_5<-c(e$`Std. Error`)
f<-as.data.frame((summary(m6)))
e_6<-c(f$`Std. Error`)
g<-as.data.frame((summary(m7)))
e_7<-c(g$`Std. Error`)
h<-as.data.frame((summary(m8)))
e_8<-c(h$`Std. Error`)


# Make dummy models
m1_d <- lm(isq_total_s2~t_uni+prime+school, data=full_data_temp)
m2_d <- lm(isq_total_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m3_d <- lm(isq_total_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school, data=full_data_temp)
m4_d <- lm(isq_total_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school, data=full_data_temp)
m5_d <- lm(isq_total_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school, data=full_data_temp)
m6_d <-lm(isq_total_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_high+t_uni*prop_wwcb_high+school, data=full_data_temp)
m7_d <- lm(isq_total_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_temp)
m8_d <-lm(isq_total_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_high+prime*prop_wwcb_high+school, data=full_data_temp)

stargazer(m1_d, m2_d, m3_d, m4_d, m5_d, m6_d, 
          se = list(e_1, e_2, e_3, e_4, e_5, e_6),
          dep.var.caption="ISQ score",
          dep.var.labels.include = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("+", "*", "**"), 
          omit = c("school", "fsm", "year", "sen", "mob", "maths_unified","maths_high"),
          covariate.labels = c("University role model", "Similarity prime", "Proportion WWCB", "High WWCB", "University role model x Proportion WWCB",  "University role model x High WWCB"),
          title="",
          notes.append = FALSE,
          notes.label = "",
          omit.table.layout = "n",
          omit.stat = c("f", "rsq", "adj.rsq", "ser" ),
          header=FALSE,
          column.sep.width = "-1pt",
          font.size="scriptsize",
          no.space=TRUE)

#### FIND COHEN'S D ####
# Use treatment coefficient from model 3
temp<-summary(m3)
treatment_term<-temp[2, 1]
treatment_term
treatment_term<-as.numeric(treatment_term)

table(full_data_temp$treatment)
sd1<-sd(full_data_temp$isq_total_s2[(full_data_temp$t_uni==1)], na.rm=T)
sd2<-sd(full_data_temp$isq_total_s2[full_data_temp$t_uni==0], na.rm=T)
sd<-sqrt((sd1^2+sd2^2)/2)
Cohen_d<-treatment_term/sd
Cohen_d

control_mean<-mean(full_data_temp$isq_total_s2[full_data_temp$t_uni==0], na.rm=T)
control_mean

#### APP VERSUS CONTROL ####

full_data<-read.csv("rand_matched_with_full_data_and_school_data_formatted_2.csv")
full_data_temp<-subset(full_data, full_data$wwcb==1)

m1 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~t_app+prime+school,
                           cluster=full_data_temp$class_id)

m2 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m3 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,
                           cluster=full_data_temp$class_id)

m4 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,
                           cluster=full_data_temp$class_id)


m5 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m6 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_high+t_app*prop_wwcb_high+school,
                           cluster=full_data_temp$class_id)

m7 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m8 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=isq_total_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_high+prime*prop_wwcb_high+school,
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
e<-as.data.frame((summary(m5)))
e_5<-c(e$`Std. Error`)
f<-as.data.frame((summary(m6)))
e_6<-c(f$`Std. Error`)
g<-as.data.frame((summary(m7)))
e_7<-c(g$`Std. Error`)
h<-as.data.frame((summary(m8)))
e_8<-c(h$`Std. Error`)


# Make dummy models
m1_d <- lm(isq_total_s2~t_app+prime+school, data=full_data_temp)
m2_d <- lm(isq_total_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m3_d <- lm(isq_total_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school, data=full_data_temp)
m4_d <- lm(isq_total_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school, data=full_data_temp)
m5_d <- lm(isq_total_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school, data=full_data_temp)
m6_d <-lm(isq_total_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_high+t_app*prop_wwcb_high+school, data=full_data_temp)
m7_d <- lm(isq_total_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_temp)
m8_d <-lm(isq_total_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_high+prime*prop_wwcb_high+school, data=full_data_temp)

stargazer(m1_d, m2_d, m3_d, m4_d, m5_d, m6_d,   
          se = list(e_1, e_2, e_3, e_4, e_5, e_6),
          dep.var.caption="ISQ score",
          dep.var.labels.include = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("+", "*", "**"), 
          omit = c("school", "fsm", "year", "sen", "mob", "maths_unified","maths_high"),
          covariate.labels = c("Apprenticeship role model", "Similarity prime", "Proportion WWCB", "High WWCB", "Apprenticeship role model x Proportion WWCB",  "Apprenticeship role model x High WWCB"),
          title="",
          notes.append = FALSE,
          notes.label = "",
          omit.table.layout = "n",
          omit.stat = c("f", "rsq", "adj.rsq", "ser" ),
          header=FALSE,
          column.sep.width = "-1pt",
          font.size="scriptsize",
          no.space=TRUE)
