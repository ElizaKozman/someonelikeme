library(dplyr)
library(plyr)
library(xtable)

full_data<-read.csv("rand_matched_with_s1_s2_school.csv")


### Wave label is missing for some ###
temp<-full_data[is.na(full_data$wave_1),]
table(temp$school)

c_1<- c("Boughey", "Bridgemary", "Chamberlaine", "Henley", "Houghton","Kidsgrove", "Longhill", "Mountbatten", "New",  "Romsey","Shirebrook", "Wheelers")   
c_2<- c( "Derby", "Excel", "Hartsdown", "HCC", "Holgate", "Knyvett", "Neale","Passmores", "Priory", "Royal", "SHL", "Westbourne")   

table(full_data$school)
full_data$wave_1[full_data$school%in%c_1]<-1
full_data$wave_1[full_data$school%in%c_2]<-0
table(full_data$wave_1)
summary(full_data$wave_1)
table(full_data$wave_1, full_data$school)

#### FORMAT ISQ ####
# Make it between 1 and 5 like other survey items
summary(full_data$isq_total)
full_data$isq_total<-full_data$isq_total/7
summary(full_data$isq_total)

summary(full_data$isq_total_s2)
full_data$isq_total_s2<-full_data$isq_total_s2/7
summary(full_data$isq_total_s2)

#### FORMAT SCORES OF 6 ####
# A score of 6 is 'already doing this' for some measures - make into a 5

full_data$applied_app_s1<-0
full_data$applied_app_s1[full_data$app==6]<-1
full_data$applied_uni_s1<-0
full_data$applied_uni_s1[full_data$uni==6]<-1

full_data$applied_app_s2<-0
full_data$applied_app_s2[full_data$app_s2==6]<-1
full_data$applied_uni_s2<-0
full_data$applied_uni_s2[full_data$uni_s2==6]<-1

full_data$alevel[full_data$alevel==6]<-5
full_data$uni[full_data$uni==6]<-5
full_data$app[full_data$app==6]<-5

full_data$alevel_s2[full_data$alevel_s2==6]<-5
full_data$uni_s2[full_data$uni_s2==6]<-5
full_data$app_s2[full_data$app_s2==6]<-5

#### FORMAT MATHS SCORE ####
full_data$q_attempted<-apply(full_data[36:45], 1, function(x) sum(!is.na(x)))
# Where no questions attempted, make NA
full_data$q_total[full_data$q_attempted==0]<-NA


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

#### FORMAT MATHS ####

# Create a unified maths marker
table(full_data$maths_score)
table(full_data$maths_level)
summary(full_data$maths_score)
summary(full_data$maths_level)
nrow(full_data[!is.na(full_data$maths_level),])
nrow(full_data[!is.na(full_data$maths_score),])

# There is one entry of 18 in the scores which doesn't seem valid - remove
full_data$maths_score[full_data$maths_score=="18"]<-NA
table(full_data$maths_score)

# We need to convert these scores into levels by splitting into buckets
# Get the weighting from existing levels
full_data_temp<-subset(full_data, !is.na(full_data$maths_level))
table(full_data_temp$year)
# Ignore the random Year 9 in data
full_data_temp_10<-subset(full_data_temp, full_data_temp$year==10)
full_data_temp_11<-subset(full_data_temp, full_data_temp$year==11)
prop.table(table(full_data_temp_10$maths_level))
prop.table(table(full_data_temp_11$maths_level))
rm(full_data_temp_10, full_data_temp_11)
# Take average across both years
c_basic<-c(prop.table(table(full_data$maths_level[full_data$year==10|full_data$year==11])))
c_basic
# Assume distribution by year is similar
# So handle year 8 and 9 separately

full_data_temp_8<-subset(full_data, full_data$year==8)

# Put rows in order and calculate breakpoints from percentages and then cut that way
full_data_temp_8<-subset(full_data_temp_8, !is.na(full_data_temp_8$maths_score))
full_data_temp_8<-full_data_temp_8 %>% arrange(maths_score)
nrow<-as.numeric(nrow(full_data_temp_8))
c<-c_basic*nrow
c
cut_1<-as.numeric(c[1])
cut_2<-as.numeric(c[1]+c[2])
cut_3<-as.numeric(c[1]+c[2]+c[3])
cut_4<-as.numeric(c[1]+c[2]+c[3]+c[4])
cut_5<-as.numeric(c[1]+c[2]+c[3]+c[4]+c[5])

full_data_temp_8$maths_unified<-"NA"
full_data_temp_8$maths_unified[(as.numeric(rownames(full_data_temp_8)))<cut_1]<-"1"
full_data_temp_8$maths_unified[((as.numeric(rownames(full_data_temp_8)))>=cut_1)&((as.numeric(rownames(full_data_temp_8)))<cut_2)]<-"2"
full_data_temp_8$maths_unified[((as.numeric(rownames(full_data_temp_8)))>=cut_2)&((as.numeric(rownames(full_data_temp_8)))<cut_3)]<-"3"
full_data_temp_8$maths_unified[((as.numeric(rownames(full_data_temp_8)))>=cut_3)&((as.numeric(rownames(full_data_temp_8)))<cut_4)]<-"4"
full_data_temp_8$maths_unified[((as.numeric(rownames(full_data_temp_8)))>=cut_4)&((as.numeric(rownames(full_data_temp_8)))<cut_5)]<-"5"
full_data_temp_8$maths_unified[((as.numeric(rownames(full_data_temp_8)))>=cut_5)]<-"6"

table(full_data_temp_8$maths_unified)
prop.table(table(full_data_temp_8$maths_unified))

# Compare back to maths levels - levels match
full_data_temp<-subset(full_data, !is.na(full_data$maths_level))
prop.table(table(full_data_temp$maths_level))

full_data_temp_9<-subset(full_data, full_data$year==9)

# Put rows in order and calculate breakpoints from percentages and then cut that way
full_data_temp_9<-subset(full_data_temp_9, !is.na(full_data_temp_9$maths_score))
full_data_temp_9<-full_data_temp_9 %>% arrange(maths_score)
nrow<-as.numeric(nrow(full_data_temp_9))
c<-c_basic*nrow

cut_1<-as.numeric(c[1])
cut_2<-as.numeric(c[1]+c[2])
cut_3<-as.numeric(c[1]+c[2]+c[3])
cut_4<-as.numeric(c[1]+c[2]+c[3]+c[4])
cut_5<-as.numeric(c[1]+c[2]+c[3]+c[4]+c[5])

full_data_temp_9$maths_unified<-"NA"
full_data_temp_9$maths_unified[(as.numeric(rownames(full_data_temp_9)))<cut_1]<-"1"
full_data_temp_9$maths_unified[((as.numeric(rownames(full_data_temp_9)))>=cut_1)&((as.numeric(rownames(full_data_temp_9)))<cut_2)]<-"2"
full_data_temp_9$maths_unified[((as.numeric(rownames(full_data_temp_9)))>=cut_2)&((as.numeric(rownames(full_data_temp_9)))<cut_3)]<-"3"
full_data_temp_9$maths_unified[((as.numeric(rownames(full_data_temp_9)))>=cut_3)&((as.numeric(rownames(full_data_temp_9)))<cut_4)]<-"4"
full_data_temp_9$maths_unified[((as.numeric(rownames(full_data_temp_9)))>=cut_4)&((as.numeric(rownames(full_data_temp_9)))<cut_5)]<-"5"
full_data_temp_9$maths_unified[((as.numeric(rownames(full_data_temp_9)))>=cut_5)]<-"6"

table(full_data_temp_9$maths_unified)
prop.table(table(full_data_temp_9$maths_unified))

# Compare back to maths levels - levels match
full_data_temp<-subset(full_data, !is.na(full_data$maths_level))
prop.table(table(full_data_temp$maths_level))

# Combine
full_data_temp_3<-full_data[is.na(full_data$maths_score)&is.na(full_data$maths_level),]
full_data_temp_3$maths_unified<-NA
full_data_temp$maths_unified<-full_data_temp$maths_level
full_data<-rbind(full_data_temp, full_data_temp_8, full_data_temp_9, full_data_temp_3)
table(full_data$maths_unified)
full_data$maths_unified<-as.numeric(as.character(full_data$maths_unified))
table(full_data$maths_unified)
prop.table(table(full_data$maths_unified))
hist(full_data$maths_unified)

# Compare back to maths levels - levels match
full_data_temp<-subset(full_data, !is.na(full_data$maths_level))
prop.table(table(full_data_temp$maths_level))
t<-prop.table(table(full_data$year, full_data$maths_unified),2)
print(xtable(t, type="latex"))

#### SCALE MATHS ####
full_data$maths_score<-scale(full_data$maths_score, center = TRUE, scale = TRUE)
hist(full_data$maths_score)
sd(full_data$maths_score, na.rm=T)
full_data$maths_unified<- full_data$maths_unified-4
hist(full_data$maths_unified)
full_data$maths_level<- full_data$maths_level-4
hist(full_data$maths_level)


#### CUT PROP_WWCB INTO CATEGORIES####
temp<-mean(full_data$prop_wwcb_class)
hist(full_data$prop_wwcb_class)
full_data$prop_wwcb_high[full_data$prop_wwcb_class>=temp]<-1
full_data$prop_wwcb_high[full_data$prop_wwcb_class<temp]<-0


#### SCALE PROP_WWCB ####
full_data$prop_wwcb_class<-scale(full_data$prop_wwcb_class, center = TRUE, scale = TRUE)
hist(full_data$prop_wwcb_class)

#### HIGH AND LOW MATHS ####
# Do by year
# Let's derive a marker which is equivalent to being in the top two levels  5-6
prop.table(table(full_data$maths_level))
proportion_high<-prop.table(table(full_data$maths_level))[5]+prop.table(table(full_data$maths_level))[6]

temp_10<-subset(full_data, full_data$year==10)
table(temp_10$maths_level)
prop.table(table(temp_10$maths_level))
temp_10$maths_high<-0
temp_10$maths_high[temp_10$maths_level==5|temp_10$maths_level==6]<-1
temp_10$maths_high[is.na(temp_10$maths_level)]<-NA
table(temp_10$maths_high)
summary(temp_10$maths_high)
prop.table(table(temp_10$maths_high))

temp_11<-subset(full_data, full_data$year==11)
table(temp_11$maths_level)
prop.table(table(temp_11$maths_level))
temp_11$maths_high<-0
temp_11$maths_high[temp_11$maths_level==5|temp_11$maths_level==6]<-1
temp_11$maths_high[is.na(temp_11$maths_level)]<-NA
table(temp_11$maths_high)
summary(temp_11$maths_high)
prop.table(table(temp_11$maths_high))

temp_12<-subset(full_data, full_data$year==12)
table(temp_12$maths_level)
prop.table(table(temp_12$maths_level))
temp_12$maths_high<-0
temp_12$maths_high[temp_12$maths_level==5|temp_12$maths_level==6]<-1
temp_12$maths_high[is.na(temp_12$maths_level)]<-NA
table(temp_12$maths_high)
prop.table(table(temp_12$maths_high))


temp_13<-subset(full_data, full_data$year==13)
table(temp_13$maths_level)
prop.table(table(temp_13$maths_level))
temp_13$maths_high<-0
temp_13$maths_high[temp_13$maths_level==5|temp_13$maths_level==6]<-1
temp_13$maths_high[is.na(temp_13$maths_level)]<-NA
table(temp_13$maths_high)
prop.table(table(temp_13$maths_high))

temp_8<-subset(full_data, full_data$year==8)
temp_8<-temp_8[order(-temp_8$maths_score),] 
# Label top ~40% of rows as high maths
temp_8$maths_quintile<-cut(as.numeric(as.character(temp_8$maths_score)),breaks = 5, label=c("1", "2", "3", "4", "5"))
table(temp_8$maths_quintile)
temp_8$maths_high<-0
temp_8$maths_high[temp_8$maths_quintile==4|temp_8$maths_quintile==5]<-1
temp_8$maths_high[is.na(temp_8$maths_quintile)]<-NA
table(temp_8$maths_high)
summary(temp_8$maths_high)
prop.table(table(temp_8$maths_high))
temp_8<-temp_8[ , -which(names(temp_8) %in% c("maths_quintile"))]

temp_9<-subset(full_data, full_data$year==9)
temp_9<-temp_9[order(-temp_9$maths_score),] 
# Label top ~40% of rows as high maths
temp_9$maths_quintile<-cut(as.numeric(as.character(temp_9$maths_score)),breaks = 5, label=c("1", "2", "3", "4", "5"))
table(temp_9$maths_quintile)
temp_9$maths_high<-0
temp_9$maths_high[temp_9$maths_quintile==4|temp_9$maths_quintile==5]<-1
temp_9$maths_high[is.na(temp_9$maths_quintile)]<-NA
table(temp_9$maths_high)
prop.table(table(temp_9$maths_high))
temp_9<-temp_9[ , -which(names(temp_9) %in% c("maths_quintile"))]

# Recombine
full_data_temp<-rbind(temp_8, temp_9, temp_10, temp_11, temp_12, temp_13)
table(full_data_temp$maths_high)
summary(full_data_temp$maths_high)
prop.table(table(full_data_temp$maths_high))

full_data<-full_data_temp

#### FORMAT SIMILAR AND EXAMPLE ####
#full_data$similar<-full_data$similar-4
#full_data$example<-full_data$example-4
#### MAKE WWCG  ####
full_data$wwcg<-0
full_data$wwcg[full_data$white_british==1&full_data$wc==1&full_data$female==1]<-1

#### MAKE MALE ####
full_data$male<-0
full_data$male[full_data$female==0]<-1

#### MAKE WWC ####
full_data$wwc<-0
full_data$wwc[full_data$white_british==1&full_data$wc==1]<-1

#### MAKE COMBINED TREATMENT ARMS ####
full_data$t_uni<-0
full_data$t_uni[full_data$treatment==1|full_data$treatment==2]<-1
full_data$t_uni[full_data$treatment==3|full_data$treatment==4]<-NA
table(full_data$t_uni)
table(full_data$treatment, full_data$t_uni)

full_data$t_app<-0
full_data$t_app[full_data$treatment==3|full_data$treatment==4]<-1
full_data$t_app[full_data$treatment==1|full_data$treatment==2]<-NA
table(full_data$treatment, full_data$t_app)

full_data$t_prime<-0
full_data$t_prime[full_data$treatment==1|full_data$treatment==3]<-1
full_data$t_prime[full_data$treatment==5]<-NA
table(full_data$treatment, full_data$t_prime)

full_data$treatment<-as.factor(full_data$treatment)
full_data$any_rm<-revalue(full_data$treatment, c("1"="1", "2"="1", "3"="1", "4"="1", "5"="0"))
full_data$uni_rm<-revalue(full_data$treatment, c("1"="1", "2"="1", "3"="0", "4"="0", "5"="0"))
full_data$app_rm<-revalue(full_data$treatment, c("1"="0", "2"="0", "3"="1", "4"="1", "5"="0"))
full_data$prime<-revalue(full_data$treatment, c("1"="1", "2"="0", "3"="1", "4"="0", "5"="0"))
full_data$any_rm<-as.numeric(as.character(full_data$any_rm))
full_data$uni_rm<-as.numeric(as.character(full_data$uni_rm))
full_data$app_rm<-as.numeric(as.character(full_data$app_rm))
full_data$prime<-as.numeric(as.character(full_data$prime))
table(full_data$any_rm, full_data$uni_rm)
table(full_data$uni_rm, full_data$prime)
table(full_data$app_rm, full_data$prime)


#### FORMAT YEAR ####
table(full_data$year)
# Make year 8s the reference category
full_data$year<-full_data$year-8
table(full_data$year)
