library(dplyr)
library(plyr)
library(BBmisc)

full_data<-read.csv("pilot_rand_matched_outcomes.csv")

#### FORMAT ISQ ####
# Make it between 1 and 5 like other survey items
summary(full_data$isq_total)
full_data$isq_total<-full_data$isq_total/7
summary(full_data$isq_total)

#### FORMAT MOB ####

# At the moment is formatted according to calendar
# We want to shift so want August to be formatted as 0 and July as 12
table(full_data$mob)
full_data_temp_1<-subset(full_data, !is.na(full_data$mob))
full_data_temp_2<-subset(full_data, is.na(full_data$mob))
full_data_temp_1$mob<-full_data_temp_1$mob-8
full_data_temp_1$mob[full_data_temp_1$mob<0]<-12+full_data_temp_1$mob[full_data_temp_1$mob<0]
table(full_data_temp_1$mob)
full_data<-rbind(full_data_temp_1, full_data_temp_2)
rm(full_data_temp_1, full_data_temp_2)


#### Make WWCG marker ####
full_data$wwcg<-0
full_data$wwcg[full_data$white_british==1&full_data$wc==1&full_data$female==1]<-1

#### Make male marker ####
full_data$male<-0
full_data$male[full_data$female==0]<-1

#### Make wwc marker ####
full_data$wwc<-0
full_data$wwc[full_data$white_british==1&full_data$wc==1]<-1

#### Convert maths to numeric and scale ####
full_data$maths_score<-as.numeric(as.character(full_data$maths_score))
hist(full_data$maths_score)
full_data$maths_score<-scale(full_data$maths_score,center = TRUE, scale = TRUE)
hist(full_data$maths_score)
sd(full_data$maths_score, na.rm=T)

#### Scale prop_wwcb_class ####
hist(full_data$prop_wwcb_class)
full_data$prop_wwcb_class<-scale(full_data$prop_wwcb_class)
hist(full_data$prop_wwcb_class)

#### Scale mob ####
hist(full_data$mob)
full_data$mob<-scale(full_data$mob)
hist(full_data$mob)

#### Final format ####
# Get rid of all the X columns and add an id column
cols<-c("X")
full_data<-full_data[,!names(full_data) %in% cols]

full_data$id<-seq.int(nrow(full_data))

#### WRITE OUT ####

write.csv(full_data, "pilot_rand_matched_outcomes_formatted.csv")
