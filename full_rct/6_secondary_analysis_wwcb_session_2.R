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
                           formula=uni_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m2 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=uni_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m3 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=uni_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m4 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=app_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m5 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=app_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m6 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=app_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m7 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=q_total_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m8 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=q_total_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m9 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=q_total_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m10 <- miceadds::glm.cluster(data=full_data_temp, 
                           formula=lottery_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school,family="binomial",
                           cluster=full_data_temp$class_id)

m11 <- miceadds::glm.cluster(data=full_data_temp, 
                           formula=lottery_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school,family="binomial",
                           cluster=full_data_temp$class_id)

m12 <- miceadds::glm.cluster(data=full_data_temp, 
                             formula=lottery_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,family="binomial",
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
i<-as.data.frame((summary(m9)))
e_9<-c(i$`Std. Error`)
j<-as.data.frame((summary(m10)))
e_10<-c(j$`Std. Error`)
k<-as.data.frame((summary(m11)))
e_11<-c(k$`Std. Error`)
l<-as.data.frame((summary(m12)))
e_12<-c(l$`Std. Error`)


# Make dummy models
m1_d <- lm(uni_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m2_d <-lm(uni_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school, data=full_data_temp)
m3_d <- lm(uni_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_temp)

m4_d <- lm(app_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m5_d <-lm(app_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school, data=full_data_temp)
m6_d <- lm(app_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_temp)

m7_d <- lm(q_total_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m8_d <-lm(q_total_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school, data=full_data_temp)
m9_d <- lm(q_total_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_temp)

m10_d <- glm(lottery_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp, family="binomial")
m11_d <-glm(lottery_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school, data=full_data_temp, family="binomial")
m12_d <- glm(lottery_s2~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_temp, family="binomial")


stargazer(m1_d, m2_d, m4_d, m5_d, m10_d, m11_d,
          se = list(e_1, e_2,  e_4, e_5, e_10, e_11),
          dep.var.labels.include = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("+", "*", "**"), 
          omit = c("school", "fsm", "year", "sen", "mob", "maths_unified","maths_high"),
          covariate.labels = c("Any role model", "Similarity prime", "Proportion WWCB", "Any role model x Proportion WWCB"),
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
                           formula=uni_s2~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m2 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=app_s2~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m3 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=q_total_s2~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m4 <- miceadds::glm.cluster(data=full_data_temp, 
                             formula=lottery_s2~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school,family="binomial",
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
m1_d <- lm(uni_s2~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)

m2_d <- lm(app_s2~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)

m3_d <- lm(q_total_s2~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)

m4_d <- glm(lottery_s2~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp, family="binomial")


stargazer(m1_d, m2_d, m4_d, 
          se = list(e_1, e_2, e_4),
          dep.var.labels.include = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("+", "*", "**"), 
          omit = c("school", "fsm", "year", "sen", "mob", "maths_unified","maths_high", "prop_wwcb_class"),
          title="",
          covariate.labels = c("University role model", "Similarity prime", "University role model x Similarity prime"),
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
                           formula=uni_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m2 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=uni_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m3 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=uni_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m4 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=app_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m5 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=app_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m6 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=app_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m7 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=q_total_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m8 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=q_total_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m9 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=q_total_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m10 <- miceadds::glm.cluster(data=full_data_temp, 
                             formula=lottery_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school,family="binomial",
                             cluster=full_data_temp$class_id)

m11 <- miceadds::glm.cluster(data=full_data_temp, 
                             formula=lottery_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school,family="binomial",
                             cluster=full_data_temp$class_id)

m12 <- miceadds::glm.cluster(data=full_data_temp, 
                             formula=lottery_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,family="binomial",
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
i<-as.data.frame((summary(m9)))
e_9<-c(i$`Std. Error`)
j<-as.data.frame((summary(m10)))
e_10<-c(j$`Std. Error`)
k<-as.data.frame((summary(m11)))
e_11<-c(k$`Std. Error`)
l<-as.data.frame((summary(m12)))
e_12<-c(l$`Std. Error`)


# Make dummy models
m1_d <- lm(uni_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m2_d <-lm(uni_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school, data=full_data_temp)
m3_d <- lm(uni_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_temp)

m4_d <- lm(app_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m5_d <-lm(app_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school, data=full_data_temp)
m6_d <- lm(app_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_temp)

m7_d <- lm(q_total_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m8_d <-lm(q_total_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school, data=full_data_temp)
m9_d <- lm(q_total_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_temp)

m10_d <- glm(lottery_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp, family="binomial")
m11_d <-glm(lottery_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school, data=full_data_temp, family="binomial")
m12_d <- glm(lottery_s2~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_temp, family="binomial")


stargazer(m1_d, m2_d, m4_d, m5_d,m10_d, m11_d, 
          se = list(e_1, e_2, e_4, e_5, e_10, e_11),
          dep.var.labels.include = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("+", "*", "**"), 
          omit = c("school", "fsm", "year", "sen", "mob", "maths_unified","maths_high"),
          covariate.labels = c("University role model", "Similarity prime", "Proportion WWCB","University role model x Proportion WWCB"),
          title="",
          notes.append = FALSE,
          notes.label = "",
          omit.table.layout = "n",
          omit.stat = c("f", "rsq", "adj.rsq", "ser" ),
          header=FALSE,
          column.sep.width = "-1pt",
          font.size="scriptsize",
          no.space=TRUE)



#### APP VERSUS CONTROL ####

full_data<-read.csv("rand_matched_with_full_data_and_school_data_formatted_2.csv")
full_data_temp<-subset(full_data, full_data$wwcb==1)

m1 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=uni_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m2 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=uni_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m3 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=uni_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m4 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=app_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m5 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=app_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m6 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=app_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m7 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=q_total_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m8 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=q_total_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m9 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=q_total_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m10 <- miceadds::glm.cluster(data=full_data_temp, 
                             formula=lottery_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school,family="binomial",
                             cluster=full_data_temp$class_id)

m11 <- miceadds::glm.cluster(data=full_data_temp, 
                             formula=lottery_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school,family="binomial",
                             cluster=full_data_temp$class_id)

m12 <- miceadds::glm.cluster(data=full_data_temp, 
                             formula=lottery_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,family="binomial",
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
i<-as.data.frame((summary(m9)))
e_9<-c(i$`Std. Error`)
j<-as.data.frame((summary(m10)))
e_10<-c(j$`Std. Error`)
k<-as.data.frame((summary(m11)))
e_11<-c(k$`Std. Error`)
l<-as.data.frame((summary(m12)))
e_12<-c(l$`Std. Error`)


# Make dummy models
m1_d <- lm(uni_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m2_d <-lm(uni_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school, data=full_data_temp)
m3_d <- lm(uni_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_temp)

m4_d <- lm(app_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m5_d <-lm(app_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school, data=full_data_temp)
m6_d <- lm(app_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_temp)

m7_d <- lm(q_total_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m8_d <-lm(q_total_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school, data=full_data_temp)
m9_d <- lm(q_total_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_temp)

m10_d <- glm(lottery_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp, family="binomial")
m11_d <-glm(lottery_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school, data=full_data_temp, family="binomial")
m12_d <- glm(lottery_s2~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_temp, family="binomial")


stargazer(m1_d, m2_d, m4_d, m5_d,m10_d, m11_d, 
          se = list(e_1, e_2, e_4, e_5, e_10, e_11),
          dep.var.labels.include = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("+", "*", "**"), 
          omit = c("school", "fsm", "year", "sen", "mob", "maths_unified","maths_high"),
          covariate.labels = c("Apprenticeship role model", "Similarity prime", "Proportion WWCB","Apprenticeship role model x Proportion WWCB"),
          title="",
          notes.append = FALSE,
          notes.label = "",
          omit.table.layout = "n",
          omit.stat = c("f", "rsq", "adj.rsq", "ser" ),
          header=FALSE,
          column.sep.width = "-1pt",
          font.size="scriptsize",
          no.space=TRUE)


