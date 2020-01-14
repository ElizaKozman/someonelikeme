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
full_data_temp<-subset(full_data, full_data$wwcg==1)

# Make a df where survey data collected for ATE
# Keep rows where sheets returned and not blank
full_data_ATE<-subset(full_data_temp, sheet_returned==1)
full_data_ATE<-subset(full_data_ATE, p1_blank==0|p2_blank==0)

m1 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=effort~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m2 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=effort~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m3 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=effort~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m4 <- miceadds::lm.cluster(data=full_data_ATE, 
                           formula=effort~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_ATE$class_id)

m5 <- miceadds::lm.cluster(data=full_data_ATE, 
                           formula=effort~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school,
                           cluster=full_data_ATE$class_id)

m6 <- miceadds::lm.cluster(data=full_data_ATE, 
                           formula=effort~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,
                           cluster=full_data_ATE$class_id)

m7 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=attendance~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m8 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=attendance~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m9 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=attendance~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m10 <- miceadds::lm.cluster(data=full_data_ATE, 
                           formula=attendance~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_ATE$class_id)

m11 <- miceadds::lm.cluster(data=full_data_ATE, 
                           formula=attendance~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school,
                           cluster=full_data_ATE$class_id)

m12 <- miceadds::lm.cluster(data=full_data_ATE, 
                             formula=attendance~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,
                             cluster=full_data_ATE$class_id)



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
m1_d <- lm(effort~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m2_d <-lm(effort~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school, data=full_data_temp)
m3_d <- lm(effort~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_temp)

m4_d <- lm(effort~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_ATE)
m5_d <-lm(effort~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school, data=full_data_ATE)
m6_d <- lm(effort~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_ATE)

m7_d <- lm(attendance~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m8_d <-lm(attendance~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school, data=full_data_temp)
m9_d <- lm(attendance~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_temp)

m10_d <- lm(attendance~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_ATE)
m11_d <-lm(attendance~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+any_rm*prop_wwcb_class+school, data=full_data_ATE)
m12_d <- lm(attendance~any_rm+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_ATE)


stargazer(m1_d, m2_d, m4_d, m5_d,m7_d, m8_d, m10_d, m11_d,
          se = list(e_1, e_2, e_4, e_5, e_7, e_8, e_10, e_11),
          dep.var.labels.include = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("+", "*", "**"), 
          omit = c("school", "fsm", "year", "sen", "mob", "maths_unified","maths_high", "prop_wwcb_high"),
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
full_data_temp<-subset(full_data, full_data$wwcg==1)
full_data_temp<-subset(full_data_temp, full_data_temp$any_rm==1)

# Make a df where survey data collected for ATE
# Keep rows where sheets returned and not blank
full_data_ATE<-subset(full_data_temp, sheet_returned==1)
full_data_ATE<-subset(full_data_ATE, p1_blank==0|p2_blank==0)


m1 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=effort~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m2 <- miceadds::lm.cluster(data=full_data_ATE, 
                           formula=effort~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_ATE$class_id)

m3 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=attendance~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m4 <- miceadds::lm.cluster(data=full_data_ATE, 
                             formula=attendance~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school,
                             cluster=full_data_ATE$class_id)



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
m1_d <- lm(effort~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)

m2_d <- lm(effort~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_ATE)

m3_d <- lm(attendance~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)

m4_d <- lm(attendance~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_ATE)


stargazer(m1_d, m2_d,m3_d, m4_d, 
          se = list(e_1, e_2,e_3, e_4),
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
full_data_temp<-subset(full_data, full_data$wwcg==1)

# Make a df where survey data collected for ATE
# Keep rows where sheets returned and not blank
full_data_ATE<-subset(full_data_temp, sheet_returned==1)
full_data_ATE<-subset(full_data_ATE, p1_blank==0|p2_blank==0)

m1 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=effort~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m2 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=effort~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m3 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=effort~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m4 <- miceadds::lm.cluster(data=full_data_ATE, 
                           formula=effort~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_ATE$class_id)

m5 <- miceadds::lm.cluster(data=full_data_ATE, 
                           formula=effort~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school,
                           cluster=full_data_ATE$class_id)

m6 <- miceadds::lm.cluster(data=full_data_ATE, 
                           formula=effort~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,
                           cluster=full_data_ATE$class_id)

m7 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=attendance~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m8 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=attendance~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m9 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=attendance~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m10 <- miceadds::glm.cluster(data=full_data_ATE, 
                             formula=attendance~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school,family="binomial",
                             cluster=full_data_ATE$class_id)

m11 <- miceadds::lm.cluster(data=full_data_ATE, 
                             formula=attendance~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school,
                             cluster=full_data_ATE$class_id)

m12 <- miceadds::lm.cluster(data=full_data_ATE, 
                             formula=attendance~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,
                             cluster=full_data_ATE$class_id)



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
m1_d <- lm(effort~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m2_d <-lm(effort~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school, data=full_data_temp)
m3_d <- lm(effort~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_temp)

m4_d <- lm(effort~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_ATE)
m5_d <-lm(effort~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school, data=full_data_ATE)
m6_d <- lm(effort~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_ATE)

m7_d <- lm(attendance~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m8_d <-lm(attendance~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school, data=full_data_temp)
m9_d <- lm(attendance~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_temp)

m10_d <- lm(attendance~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_ATE)
m11_d <-lm(attendance~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+t_uni*prop_wwcb_class+school, data=full_data_ATE)
m12_d <-lm(attendance~t_uni+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_ATE)


stargazer(m1_d, m2_d, m4_d, m5_d, m7_d, m8_d, m10_d, m11_d,
          se = list(e_1, e_2, e_4, e_5, e_7, e_8, e_10, e_11),
          dep.var.labels.include = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("+", "*", "**"), 
          omit = c("school", "fsm", "year", "sen", "mob", "maths_unified","maths_high"),
          covariate.labels = c("University role model", "Similarity prime", "Proportion WWCB",  "University role model x Proportion WWCB"),
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
full_data_temp<-subset(full_data, full_data$wwcg==1)

# Make a df where survey data collected for ATE
# Keep rows where sheets returned and not blank
full_data_ATE<-subset(full_data_temp, sheet_returned==1)
full_data_ATE<-subset(full_data_ATE, p1_blank==0|p2_blank==0)

m1 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=effort~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m2 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=effort~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m3 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=effort~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m4 <- miceadds::lm.cluster(data=full_data_ATE, 
                           formula=effort~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_ATE$class_id)

m5 <- miceadds::lm.cluster(data=full_data_ATE, 
                           formula=effort~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school,
                           cluster=full_data_ATE$class_id)

m6 <- miceadds::lm.cluster(data=full_data_ATE, 
                           formula=effort~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,
                           cluster=full_data_ATE$class_id)

m7 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=attendance~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m8 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=attendance~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m9 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=attendance~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m10 <- miceadds::glm.cluster(data=full_data_ATE, 
                             formula=attendance~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school,family="binomial",
                             cluster=full_data_ATE$class_id)

m11 <- miceadds::lm.cluster(data=full_data_ATE, 
                            formula=attendance~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school,
                            cluster=full_data_ATE$class_id)

m12 <- miceadds::lm.cluster(data=full_data_ATE, 
                            formula=attendance~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school,
                            cluster=full_data_ATE$class_id)



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
m1_d <- lm(effort~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m2_d <-lm(effort~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school, data=full_data_temp)
m3_d <- lm(effort~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_temp)

m4_d <- lm(effort~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_ATE)
m5_d <-lm(effort~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school, data=full_data_ATE)
m6_d <- lm(effort~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_ATE)

m7_d <- lm(attendance~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m8_d <-lm(attendance~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school, data=full_data_temp)
m9_d <- lm(attendance~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_temp)

m10_d <- lm(attendance~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_ATE)
m11_d <-lm(attendance~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+t_app*prop_wwcb_class+school, data=full_data_ATE)
m12_d <-lm(attendance~t_app+prime+fsm+sen+mob+year+prop_wwcb_class+prime*prop_wwcb_class+school, data=full_data_ATE)


stargazer(m1_d, m2_d, m4_d, m5_d, m7_d, m8_d, m10_d, m11_d,
          se = list(e_1, e_2, e_4, e_5, e_7, e_8, e_10, e_11),
          dep.var.labels.include = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("+", "*", "**"), 
          omit = c("school", "fsm", "year", "sen", "mob", "maths_unified","maths_high"),
          covariate.labels = c("Apprenticeship role model", "Similarity prime", "Proportion WWCB",  "Apprenticeship role model x Proportion WWCB"),
          title="",
          notes.append = FALSE,
          notes.label = "",
          omit.table.layout = "n",
          omit.stat = c("f", "rsq", "adj.rsq", "ser" ),
          header=FALSE,
          column.sep.width = "-1pt",
          font.size="scriptsize",
          no.space=TRUE)
