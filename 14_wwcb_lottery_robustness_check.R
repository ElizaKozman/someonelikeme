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

#### S1 DATA ####
full_data<-read.csv("rand_matched_with_full_data_and_school_data_formatted_2.csv")

full_data<-subset(full_data, full_data$wwcb==1)

temp<-full_data[full_data$sheet_returned==1&!is.na(full_data$isq_total)&!is.na(full_data$uni)&!is.na(full_data$app)&!is.na(full_data$first_family)&!is.na(full_data$alevel)&is.na(full_data$lottery),]
         
full_data$lottery[full_data$sheet_returned==1&!is.na(full_data$isq_total)&!is.na(full_data$uni)&!is.na(full_data$app)&!is.na(full_data$first_family)&!is.na(full_data$alevel)&is.na(full_data$lottery)]<-0
table(full_data$lottery)
full_data_temp<-full_data

m1 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery~any_rm+prime+school,family="binomial",
                            cluster=full_data_temp$class_id)


m2 <- miceadds::glm.cluster(data=full_data_temp, 
                           formula=lottery~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school,family="binomial",
                           cluster=full_data_temp$class_id)

m3 <- miceadds::glm.cluster(data=full_data_temp, 
                           formula=lottery~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,family="binomial",
                           cluster=full_data_temp$class_id)

m4 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,family="binomial",
                            cluster=full_data_temp$class_id)



# Make dummy models
m1_d <- glm(lottery~any_rm+prime+school, family="binomial",data=full_data_temp)
m2_d <- glm(lottery~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school, family="binomial",data=full_data_temp)
m3_d <- glm(lottery~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,family="binomial", data=full_data_temp)
m4_d <- glm(lottery~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,family="binomial", data=full_data_temp)

#### S2 DATA ####
full_data<-read.csv("rand_matched_with_full_data_and_school_data_formatted_2.csv")

full_data<-subset(full_data, full_data$wwcb==1)

temp<-full_data[full_data$sheet_returned_s2==1&!is.na(full_data$isq_total_s2)&!is.na(full_data$uni_s2)&!is.na(full_data$app_s2)&!is.na(full_data$alevel_s2)&is.na(full_data$lottery_s2),]

full_data$lottery_s2[full_data$sheet_returned_s2==1&!is.na(full_data$isq_total_s2)&!is.na(full_data$uni_s2)&!is.na(full_data$app_s2)&!is.na(full_data$alevel_s2)&is.na(full_data$lottery_s2)]<-0
table(full_data$lottery_s2)
full_data_temp<-full_data
prop.table(table(full_data$lottery_s2, full_data$any_rm),2)

m5 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery_s2~any_rm+prime+school,family="binomial",
                            cluster=full_data_temp$class_id)

m6 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school,family="binomial",
                            cluster=full_data_temp$class_id)

m7 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,family="binomial",
                            cluster=full_data_temp$class_id)

m8 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,family="binomial",
                            cluster=full_data_temp$class_id)


m9 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery_s2~any_rm+prime+lottery+school,family="binomial",
                            cluster=full_data_temp$class_id)

m10 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery_s2~any_rm+prime+lottery+fsm+sen+mob+year+prop_wwcb_class+school,family="binomial",
                            cluster=full_data_temp$class_id)

m11 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery_s2~any_rm+prime+lottery+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,family="binomial",
                            cluster=full_data_temp$class_id)

m12 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery_s2~any_rm+prime+lottery+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,family="binomial",
                            cluster=full_data_temp$class_id)


# Make dummy models
m5_d <- glm(lottery_s2~any_rm+prime+school, family="binomial",data=full_data_temp)
m6_d <- glm(lottery_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school, family="binomial",data=full_data_temp)
m7_d <- glm(lottery_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,family="binomial", data=full_data_temp)
m8_d <- glm(lottery_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,family="binomial", data=full_data_temp)
m9_d <- glm(lottery_s2~any_rm+prime+lottery+school, family="binomial",data=full_data_temp)
m10_d <- glm(lottery_s2~any_rm+prime+lottery+fsm+sen+mob+year+prop_wwcb_class+school, family="binomial",data=full_data_temp)
m11_d <- glm(lottery_s2~any_rm+prime+lottery+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,family="binomial", data=full_data_temp)
m12_d <- glm(lottery_s2~any_rm+prime+lottery+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,family="binomial", data=full_data_temp)


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
i<-as.data.frame((summary(m9)))
e_9<-c(i$`Std. Error`)
j<-as.data.frame((summary(m10)))
e_10<-c(j$`Std. Error`)
k<-as.data.frame((summary(m11)))
e_11<-c(k$`Std. Error`)
l<-as.data.frame((summary(m12)))
e_12<-c(l$`Std. Error`)

stargazer(m1_d, m2_d, m3_d, m4_d,m5_d, m6_d, m7_d,m8_d, m9_d,m10_d, m11_d, m12_d, 
          se = list(e_1, e_2, e_3, e_4, e_5, e_6,  e_7, e_8, e_9, e_10, e_11, e_12) ,
          dep.var.labels.include = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("+", "*", "**"), 
          omit = c("school", "sen", "mob", 'fsm', "year", "prop_wwcb_class", "maths_unified", "maths_high"),
          title="",
          notes.append = FALSE,
          notes.label = "",
          omit.stat = c("f", "rsq", "adj.rsq", "ser" ),
          header=FALSE,
          column.sep.width = "-1pt",
          font.size="small",
          type='text')


#### S1 DATA ####

full_data<-read.csv("rand_matched_with_full_data_and_school_data_formatted_2.csv")

full_data<-subset(full_data, full_data$wwcb==1)

temp<-full_data[full_data$sheet_returned==1&!is.na(full_data$isq_total)&!is.na(full_data$uni)&!is.na(full_data$app)&!is.na(full_data$first_family)&!is.na(full_data$alevel)&is.na(full_data$lottery),]

full_data$lottery[full_data$sheet_returned==1&!is.na(full_data$isq_total)&!is.na(full_data$uni)&!is.na(full_data$app)&!is.na(full_data$first_family)&!is.na(full_data$alevel)&is.na(full_data$lottery)]<-0
table(full_data$lottery)
full_data_temp<-full_data

m1 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery~t_uni+prime+school,family="binomial",
                            cluster=full_data_temp$class_id)


m2 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school,family="binomial",
                            cluster=full_data_temp$class_id)

m3 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,family="binomial",
                            cluster=full_data_temp$class_id)

m4 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,family="binomial",
                            cluster=full_data_temp$class_id)



# Make dummy models
m1_d <- glm(lottery~t_uni+prime+school, family="binomial",data=full_data_temp)
m2_d <- glm(lottery~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school, family="binomial",data=full_data_temp)
m3_d <- glm(lottery~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,family="binomial", data=full_data_temp)
m4_d <- glm(lottery~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,family="binomial", data=full_data_temp)

#### S2 DATA ####
full_data<-read.csv("rand_matched_with_full_data_and_school_data_formatted_2.csv")

full_data<-subset(full_data, full_data$wwcb==1)

temp<-full_data[full_data$sheet_returned_s2==1&!is.na(full_data$isq_total_s2)&!is.na(full_data$uni_s2)&!is.na(full_data$app_s2)&!is.na(full_data$alevel_s2)&is.na(full_data$lottery_s2),]

full_data$lottery_s2[full_data$sheet_returned_s2==1&!is.na(full_data$isq_total_s2)&!is.na(full_data$uni_s2)&!is.na(full_data$app_s2)&!is.na(full_data$alevel_s2)&is.na(full_data$lottery_s2)]<-0
table(full_data$lottery_s2)
full_data_temp<-full_data
prop.table(table(full_data$lottery_s2, full_data$t_uni),2)

m5 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery_s2~t_uni+prime+school,family="binomial",
                            cluster=full_data_temp$class_id)

m6 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school,family="binomial",
                            cluster=full_data_temp$class_id)

m7 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,family="binomial",
                            cluster=full_data_temp$class_id)

m8 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,family="binomial",
                            cluster=full_data_temp$class_id)


m9 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery_s2~t_uni+prime+lottery+school,family="binomial",
                            cluster=full_data_temp$class_id)

m10 <- miceadds::glm.cluster(data=full_data_temp, 
                             formula=lottery_s2~t_uni+prime+lottery+fsm+sen+mob+year+prop_wwcb_class+school,family="binomial",
                             cluster=full_data_temp$class_id)

m11 <- miceadds::glm.cluster(data=full_data_temp, 
                             formula=lottery_s2~t_uni+prime+lottery+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,family="binomial",
                             cluster=full_data_temp$class_id)

m12 <- miceadds::glm.cluster(data=full_data_temp, 
                             formula=lottery_s2~t_uni+prime+lottery+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,family="binomial",
                             cluster=full_data_temp$class_id)


# Make dummy models
m5_d <- glm(lottery_s2~t_uni+prime+school, family="binomial",data=full_data_temp)
m6_d <- glm(lottery_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school, family="binomial",data=full_data_temp)
m7_d <- glm(lottery_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,family="binomial", data=full_data_temp)
m8_d <- glm(lottery_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,family="binomial", data=full_data_temp)
m9_d <- glm(lottery_s2~t_uni+prime+lottery+school, family="binomial",data=full_data_temp)
m10_d <- glm(lottery_s2~t_uni+prime+lottery+fsm+sen+mob+year+prop_wwcb_class+school, family="binomial",data=full_data_temp)
m11_d <- glm(lottery_s2~t_uni+prime+lottery+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,family="binomial", data=full_data_temp)
m12_d <- glm(lottery_s2~t_uni+prime+lottery+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,family="binomial", data=full_data_temp)


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
i<-as.data.frame((summary(m9)))
e_9<-c(i$`Std. Error`)
j<-as.data.frame((summary(m10)))
e_10<-c(j$`Std. Error`)
k<-as.data.frame((summary(m11)))
e_11<-c(k$`Std. Error`)
l<-as.data.frame((summary(m12)))
e_12<-c(l$`Std. Error`)

stargazer(m1_d, m2_d, m3_d, m4_d,m5_d, m6_d, m7_d,m8_d, m9_d,m10_d, m11_d, m12_d, 
          se = list(e_1, e_2, e_3, e_4, e_5, e_6,  e_7, e_8, e_9, e_10, e_11, e_12) ,
          dep.var.labels.include = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("+", "*", "**"), 
          omit = c("school", "sen", "mob", 'fsm', "year", "prop_wwcb_class", "maths_unified", "maths_high"),
          title="",
          notes.append = FALSE,
          notes.label = "",
          omit.stat = c("f", "rsq", "adj.rsq", "ser" ),
          header=FALSE,
          column.sep.width = "-1pt",
          font.size="small",
          type="text")

#### S1 DATA ####

full_data<-read.csv("rand_matched_with_full_data_and_school_data_formatted_2.csv")

full_data<-subset(full_data, full_data$wwcb==1)

temp<-full_data[full_data$sheet_returned==1&!is.na(full_data$isq_total)&!is.na(full_data$uni)&!is.na(full_data$app)&!is.na(full_data$first_family)&!is.na(full_data$alevel)&is.na(full_data$lottery),]

full_data$lottery[full_data$sheet_returned==1&!is.na(full_data$isq_total)&!is.na(full_data$uni)&!is.na(full_data$app)&!is.na(full_data$first_family)&!is.na(full_data$alevel)&is.na(full_data$lottery)]<-0
table(full_data$lottery)
full_data_temp<-full_data

m1 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery~t_app+prime+school,family="binomial",
                            cluster=full_data_temp$class_id)


m2 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school,family="binomial",
                            cluster=full_data_temp$class_id)

m3 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,family="binomial",
                            cluster=full_data_temp$class_id)

m4 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,family="binomial",
                            cluster=full_data_temp$class_id)



# Make dummy models
m1_d <- glm(lottery~t_app+prime+school, family="binomial",data=full_data_temp)
m2_d <- glm(lottery~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school, family="binomial",data=full_data_temp)
m3_d <- glm(lottery~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,family="binomial", data=full_data_temp)
m4_d <- glm(lottery~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,family="binomial", data=full_data_temp)


#### S2 DATA ####
full_data<-read.csv("rand_matched_with_full_data_and_school_data_formatted_2.csv")

full_data<-subset(full_data, full_data$wwcb==1)

temp<-full_data[full_data$sheet_returned_s2==1&!is.na(full_data$isq_total_s2)&!is.na(full_data$uni_s2)&!is.na(full_data$app_s2)&!is.na(full_data$alevel_s2)&is.na(full_data$lottery_s2),]

full_data$lottery_s2[full_data$sheet_returned_s2==1&!is.na(full_data$isq_total_s2)&!is.na(full_data$uni_s2)&!is.na(full_data$app_s2)&!is.na(full_data$alevel_s2)&is.na(full_data$lottery_s2)]<-0
table(full_data$lottery_s2)
full_data_temp<-full_data
prop.table(table(full_data$lottery_s2, full_data$t_app),2)

m5 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery_s2~t_app+prime+school,family="binomial",
                            cluster=full_data_temp$class_id)

m6 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school,family="binomial",
                            cluster=full_data_temp$class_id)

m7 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,family="binomial",
                            cluster=full_data_temp$class_id)

m8 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,family="binomial",
                            cluster=full_data_temp$class_id)


m9 <- miceadds::glm.cluster(data=full_data_temp, 
                            formula=lottery_s2~t_app+prime+lottery+school,family="binomial",
                            cluster=full_data_temp$class_id)

m10 <- miceadds::glm.cluster(data=full_data_temp, 
                             formula=lottery_s2~t_app+prime+lottery+fsm+sen+mob+year+prop_wwcb_class+school,family="binomial",
                             cluster=full_data_temp$class_id)

m11 <- miceadds::glm.cluster(data=full_data_temp, 
                             formula=lottery_s2~t_app+prime+lottery+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,family="binomial",
                             cluster=full_data_temp$class_id)

m12 <- miceadds::glm.cluster(data=full_data_temp, 
                             formula=lottery_s2~t_app+prime+lottery+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,family="binomial",
                             cluster=full_data_temp$class_id)


# Make dummy models
m5_d <- glm(lottery_s2~t_app+prime+school, family="binomial",data=full_data_temp)
m6_d <- glm(lottery_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school, family="binomial",data=full_data_temp)
m7_d <- glm(lottery_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,family="binomial", data=full_data_temp)
m8_d <- glm(lottery_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,family="binomial", data=full_data_temp)
m9_d <- glm(lottery_s2~t_app+prime+lottery+school, family="binomial",data=full_data_temp)
m10_d <- glm(lottery_s2~t_app+prime+lottery+fsm+sen+mob+year+prop_wwcb_class+school, family="binomial",data=full_data_temp)
m11_d <- glm(lottery_s2~t_app+prime+lottery+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,family="binomial", data=full_data_temp)
m12_d <- glm(lottery_s2~t_app+prime+lottery+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,family="binomial", data=full_data_temp)


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
i<-as.data.frame((summary(m9)))
e_9<-c(i$`Std. Error`)
j<-as.data.frame((summary(m10)))
e_10<-c(j$`Std. Error`)
k<-as.data.frame((summary(m11)))
e_11<-c(k$`Std. Error`)
l<-as.data.frame((summary(m12)))
e_12<-c(l$`Std. Error`)

stargazer(m1_d, m2_d, m3_d, m4_d,m5_d, m6_d, m7_d,m8_d, m9_d,m10_d, m11_d, m12_d, 
          se = list(e_1, e_2, e_3, e_4, e_5, e_6,  e_7, e_8, e_9, e_10, e_11, e_12) ,
          dep.var.labels.include = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("+", "*", "**"), 
          omit = c("school", "sen", "mob", 'fsm', "year", "prop_wwcb_class", "maths_unified", "maths_high"),
          title="",
          notes.append = FALSE,
          notes.label = "",
          omit.stat = c("f", "rsq", "adj.rsq", "ser" ),
          header=FALSE,
          column.sep.width = "-1pt",
          font.size="small", 
          type='text')


