#Do nearest neighbour from latlong and create graph from that 
#Can use text to do whatever we need too 
#PLaces have things in common... Maybe infer?? 

library(dplyr)
library(tidyr)
library(ggplot2)
library(rlist)

data <- read.csv("./crime.csv")

############### CLEANING ###############

#The shooting column
count <- 0
for (x in data$SHOOTING) {
  if (x == 'Y') {
    count = count + 1
  }
}
count

#Create new list assigning to N to empty obs
new_shooting <- c()

for (x in data$SHOOTING) {
  if (x != 'Y') {
    new_shooting <- c(new_shooting, 'N')
  } else {
    new_shooting <- c(new_shooting, x)
  }
}

data$Shooting <- new_shooting
data$SHOOTING <- NULL
data <- data %>% rename(LAT = Lat, LONG = Long, LOCATION = Location, SHOOTING = Shooting)
data <- data %>% rename(LAT = Lat, LONG = Long, LOCATION = Location)

#How may NA's in the dataset
sum(is.na(data))

#Per column if you so wish

#sum(is.na(data$INCIDENT_NUMBER))
#sum(is.na(data$OFFENSE_CODE))
#sum(is.na(data$OFFENSE_CODE_GROUP))
#sum(is.na(data$OFFENSE_DESCRIPTION))
#sum(is.na(data$DISTRICT))
#sum(is.na(data$REPORTING_AREA))
#sum(is.na(data$OCCURRED_ON_DATE))
#sum(is.na(data$YEAR))
#sum(is.na(data$MONTH))
#sum(is.na(data$DAY_OF_WEEK))
#sum(is.na(data$HOUR))
#sum(is.na(data$UCR_PART))
#sum(is.na(data$STREET))
#sum(is.na(data$LAT))
#sum(is.na(data$LONG))
#sum(is.na(data$LOCATION))
#sum(is.na(data$SHOOTING))

############### EXPLORATORY DATA ANALYSIS ##################

#Number of Crimes for each year
no_crimes <- ggplot(data, aes(YEAR)) +
  geom_bar(fill = "#0073C2FF") 
no_crimes

#Number of Distince Crimes
length(unique(data$OFFENSE_CODE_GROUP))

#The different crimes
unique(data$OFFENSE_CODE_GROUP)

#Crimes per year per offense group 
cpy <- data %>%
  group_by(OFFENSE_CODE_GROUP) %>%
  summarize(n())
cpy
