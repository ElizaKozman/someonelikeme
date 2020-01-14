library(dplyr)
library(plyr)
library(psych)
library(mediation)
library(stargazer)
library(mice)
library(stargazer)
library(kableExtra)
library(knitr)
library(purrr)
library(tidyr)
library(ggplot2)
library(reshape)
library(BBmisc)
library(extrafont)
library("xtable")
library(rmarkdown)
library(miceadds)

full_data<-read.csv("rand_matched_with_full_data_and_school_data_formatted_1.csv")
full_data$id<-seq.int(nrow(full_data))
full_data$similar[full_data$treatment==5]<-NA

#### SIMILAR/EXAMPLE - WWCB ####
full_data_temp<-subset(full_data, full_data$any_rm==1)
full_data_temp<-subset(full_data_temp, full_data_temp$wwcb==1)


m1 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=similar~uni_rm+prime+uni_rm*prime+school,
                           cluster=full_data_temp$class_id)

m2 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=similar~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m3 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=similar~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,
                           cluster=full_data_temp$class_id)

m4 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=similar~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,
                           cluster=full_data_temp$class_id)

              
# Make dummy models
m1_d <- lm(similar~uni_rm+prime+uni_rm*prime+school, data=full_data_temp)
m2_d <- lm(similar~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m3_d <- lm(similar~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school, data=full_data_temp)
m4_d <- lm(similar~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school, data=full_data_temp)


full_data_temp<-subset(full_data, full_data$any_rm==1)
full_data_temp<-subset(full_data_temp, full_data_temp$wwcb==1)


m5 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=example~uni_rm+prime+uni_rm*prime+school,
                           cluster=full_data_temp$class_id)

m6 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=example~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m7 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=example~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,
                           cluster=full_data_temp$class_id)

m8 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=example~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,
                           cluster=full_data_temp$class_id)

m9 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=example~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school+uni_rm*region,
                           cluster=full_data_temp$class_id)
summary(m9)


# Make dummy models
m5_d <- lm(example~uni_rm+prime+uni_rm*prime+school, data=full_data_temp)
m6_d <- lm(example~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m7_d <- lm(example~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school, data=full_data_temp)
m8_d <- lm(example~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school, data=full_data_temp)

m8_d <- lm(example~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school+uni_rm*region, data=full_data_temp)
summary(m8_d)
summary(m8_d)

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

stargazer(m1_d, m2_d, m3_d, m4_d, m5_d, m6_d, m7_d, m8_d,    
          se = list(e_1, e_2, e_3, e_4, e_5, e_6, e_7, e_8),
          dep.var.caption="ISQ score",
          dep.var.labels.include = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("+", "*", "**"), 
          omit = c("school", "fsm", "year", "sen", "mob", "maths_unified","maths_high", "prop_wwcb_class"),
          title="",
          notes.append = FALSE,
          notes.label = "",
          omit.table.layout = "n",
          omit.stat = c("f", "rsq", "adj.rsq", "ser" ),
          header=FALSE,
          column.sep.width = "-1pt",
          font.size="scriptsize",
          no.space=TRUE,type="text")

#### SIMILAR/EXAMPLE - WWCG ####
full_data_temp<-subset(full_data, full_data$any_rm==1)
full_data_temp<-subset(full_data_temp, full_data_temp$wwcg==1)


m1 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=similar~uni_rm+prime+uni_rm*prime+school,
                           cluster=full_data_temp$class_id)

m2 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=similar~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m3 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=similar~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,
                           cluster=full_data_temp$class_id)

m4 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=similar~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,
                           cluster=full_data_temp$class_id)

# Make dummy models
m1_d <- lm(similar~uni_rm+prime+uni_rm*prime+school, data=full_data_temp)
m2_d <- lm(similar~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m3_d <- lm(similar~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school, data=full_data_temp)
m4_d <- lm(similar~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school, data=full_data_temp)


full_data_temp<-subset(full_data, full_data$any_rm==1)
full_data_temp<-subset(full_data_temp, full_data_temp$wwcg==1)


m5 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=example~uni_rm+prime+uni_rm*prime+school,
                           cluster=full_data_temp$class_id)

m6 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=example~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m7 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=example~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,
                           cluster=full_data_temp$class_id)

m8 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=example~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,
                           cluster=full_data_temp$class_id)

# Make dummy models
m5_d <- lm(example~uni_rm+prime+uni_rm*prime+school, data=full_data_temp)
m6_d <- lm(example~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m7_d <- lm(example~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school, data=full_data_temp)
m8_d <- lm(example~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school, data=full_data_temp)


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

stargazer(m1_d, m2_d, m3_d, m4_d, m5_d, m6_d, m7_d, m8_d,    
          se = list(e_1, e_2, e_3, e_4, e_5, e_6, e_7, e_8),
          dep.var.caption="ISQ score",
          dep.var.labels.include = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("+", "*", "**"), 
          omit = c("school", "fsm", "year", "sen", "mob", "maths_unified","maths_high", "prop_wwcb_class"),
          title="",
          notes.append = FALSE,
          notes.label = "",
          omit.table.layout = "n",
          omit.stat = c("f", "rsq", "adj.rsq", "ser" ),
          header=FALSE,
          column.sep.width = "-1pt",
          font.size="scriptsize",
          no.space=TRUE, type="text")


#### SIMILAR/EXAMPLE - All PUPILS ####
full_data_temp<-subset(full_data, full_data$any_rm==1)

m1 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=similar~uni_rm+prime+uni_rm*prime+school,
                           cluster=full_data_temp$class_id)

m2 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=similar~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m3 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=similar~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,
                           cluster=full_data_temp$class_id)

m4 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=similar~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,
                           cluster=full_data_temp$class_id)

# Make dummy models
m1_d <- lm(similar~uni_rm+prime+uni_rm*prime+school, data=full_data_temp)
m2_d <- lm(similar~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m3_d <- lm(similar~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school, data=full_data_temp)
m4_d <- lm(similar~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school, data=full_data_temp)


full_data_temp<-subset(full_data, full_data$any_rm==1)

m5 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=example~uni_rm+prime+uni_rm*prime+school,
                           cluster=full_data_temp$class_id)

m6 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=example~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school,
                           cluster=full_data_temp$class_id)

m7 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=example~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school,
                           cluster=full_data_temp$class_id)

m8 <- miceadds::lm.cluster(data=full_data_temp, 
                           formula=example~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school,
                           cluster=full_data_temp$class_id)

# Make dummy models
m5_d <- lm(example~uni_rm+prime+uni_rm*prime+school, data=full_data_temp)
m6_d <- lm(example~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+school, data=full_data_temp)
m7_d <- lm(example~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_unified+school, data=full_data_temp)
m8_d <- lm(example~uni_rm+prime+uni_rm*prime+fsm+sen+mob+year+prop_wwcb_class+maths_high+school, data=full_data_temp)


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

stargazer(m1_d, m2_d, m3_d, m4_d, m5_d, m6_d, m7_d, m8_d,    
          se = list(e_1, e_2, e_3, e_4, e_5, e_6, e_7, e_8),
          dep.var.caption="ISQ score",
          dep.var.labels.include = FALSE,
          star.cutoffs = c(0.1, 0.05, 0.01),
          star.char = c("+", "*", "**"), 
          omit = c("school", "fsm", "year", "sen", "mob", "maths_unified","maths_high", "prop_wwcb_class"),
          title="",
          notes.append = FALSE,
          notes.label = "",
          omit.table.layout = "n",
          omit.stat = c("f", "rsq", "adj.rsq", "ser" ),
          header=FALSE,
          column.sep.width = "-1pt",
          font.size="scriptsize",
          no.space=TRUE,type="text")


