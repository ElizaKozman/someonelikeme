library(dplyr)
library(plyr)

full_data<-read.csv("rand_matched_with_full_data_and_school_data_formatted_1.csv")

#### FORMAT ATTENDANCE ####

table(full_data$attendance)
hist(full_data$attendance)
temp<-aggregate(full_data$attendance~full_data$school, FUN=mean)

# Attendance looks very low at Neale!
# Neale is actually a range which has been turned into a midpoint
# Drop
full_data$attendance[full_data$school=="Neale"]<-NA
# ALso drop for anyone who was absent all term as couldn't have been treated
summary(full_data$attendance)
hist(full_data$attendance)
full_data$attendance[full_data$attendance==0]<-NA

#### FORMAT EFFORT ####

# Several different coding strategies for effort - nearly all four categories
# This is the B4L score where 1 is good and 4 is bad
# These schools use this grading
c<-c( "Derby", "Holgate", "New", "Shirebrook", "Westbourne", "Wheelers")
# 4 use a continuous score
c<-c("New", "Westbourne", "Wheelers")

temp<-subset(full_data, full_data$school%in%c)
table(temp$effort)
temp$effort<-as.numeric(as.character(temp$effort))
hist(temp$effort)
summary(temp$effort)

# Does this distribution differ by school? Yes
m<-lm(effort~year+female+fsm+maths_unified+school, temp)
anova(m)

# Find other schools with continous scores and create z scores
c<-c("New", "Westbourne", "Wheelers", "Boughey", "Excel", "Longhill", "Mountbatten", "Passmores", "Romsey")

# For some high is bad
c_1<-c("New", "Westbourne", "Wheelers", "Boughey",  "Longhill", "Mountbatten","Romsey")
# For some low is bad
c_2<-c("Excel", "Passmores")

# Create dataframe to update
temp_updated<-data.frame()

for (i in 1:7) {
  temp<-subset(full_data, full_data$school==c_1[i])
  temp$effort<-as.numeric(as.character(temp$effort))
  temp$effort<-scale(temp$effort, center=TRUE, scale=TRUE)
  
  # Reverse scores to make a higher score better
  temp$effort<-(-1)*temp$effort
  temp_updated<-rbind(temp_updated, temp)
  
}

table(temp_updated$school)
for (i in 1:2) {
  
  temp<-subset(full_data, full_data$school==c_2[i])
  temp$effort<-as.numeric(as.character(temp$effort))
  temp$effort<-scale(temp$effort, center=TRUE, scale=TRUE)
  temp_updated<-rbind(temp_updated, temp)
  
}

temp<-subset(full_data, !(full_data$school%in%c_1)&!(full_data$school%in%c_2))
temp$effort<-NA

full_data_final<-rbind(temp_updated,temp)
table(full_data_final$school)

a<-aggregate(full_data_final$effort~full_data_final$school, FUN=mean)
a<-aggregate(full_data_final$effort~full_data_final$school, FUN=sd)

a<-c(full_data$X)
b<-c(full_data_final$X)
c<-a[!a %in%b ]
b[!b %in%a ]

hist(full_data_final$effort)
m<-lm(effort~fsm+female+sen+maths_unified+school, full_data_final)
summary(m)

# Get rid of all the X columns and add an id column
full_data_final<-select(full_data_final, c(-"X.2", -"X.1", -"X"))
full_data_final$id<-seq.int(nrow(full_data_final))


