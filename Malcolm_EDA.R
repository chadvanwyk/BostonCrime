#Do nearest neighbour from latlong and create graph from that 
#Can use text to do whatever we need too 
#PLaces have things in common... Maybe infer?? 

library(dplyr)
library(tidyr)
library(ggplot2)
library(rlist)

data <- read.csv("~/Desktop/Aldu/BostonCrime/crime.csv")

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
##################################--------Here's where I start bitch--------__##############################

#Unique Days -> Goes from Monday to Sunday 
unique(data$DAY_OF_WEEK)

#Which days are the the most dangerous
day_crimes <- ggplot(data, aes(DAY_OF_WEEK)) +
  geom_bar(fill = "#0073C2FF")+ggtitle("Which are the most dangerous days of the week")+xlab("Day of the week")+ylab("Amount of crimes")
day_crimes

#Which months are the the most dangerous
month_crimes <- ggplot(data, aes(MONTH)) +
  geom_bar(fill = "#0073C2FF")+ggtitle("Which are the most dangerous months")+xlab("Months of the year")+ylab("Amount of crimes")
month_crimes

#Change month 1-12 to January-December (perhaps a neater way could be a vector?)
data$MONTH <- as.character(data$MONTH)
data$MONTH[data$MONTH == "1"] <- "January"
data$MONTH <- as.character(data$MONTH)
data$MONTH[data$MONTH == "2"] <- "February"
data$MONTH <- as.character(data$MONTH)
data$MONTH[data$MONTH == "3"] <- "March"
data$MONTH <- as.character(data$MONTH)
data$MONTH[data$MONTH == "4"] <- "April"
data$MONTH <- as.character(data$MONTH)
data$MONTH[data$MONTH == "5"] <- "May"
data$MONTH <- as.character(data$MONTH)
data$MONTH[data$MONTH == "6"] <- "June"
data$MONTH <- as.character(data$MONTH)
data$MONTH[data$MONTH == "7"] <- "July"
data$MONTH <- as.character(data$MONTH)
data$MONTH[data$MONTH == "8"] <- "August"
data$MONTH <- as.character(data$MONTH)
data$MONTH[data$MONTH == "9"] <- "September"
data$MONTH <- as.character(data$MONTH)
data$MONTH[data$MONTH == "10"] <- "October"
data$MONTH <- as.character(data$MONTH)
data$MONTH[data$MONTH == "11"] <- "November"
data$MONTH <- as.character(data$MONTH)
data$MONTH[data$MONTH == "12"] <- "December"

#Removing longitude and latitude column (redundant information)
data$LOCATION <- NULL

#Removing Occured on date column (redundant information)
data$OCCURRED_ON_DATE <- NULL

#We can do this if need be, it does work, commenting it out for now though
#Downside my be we cant do calculations on this then
#data$HOUR <- as.character(data$HOUR)
#data$HOUR[data$HOUR == c("1","2","3")] <- c("1am", "2am", "3am")

#Checking for duplicates in the primary key
#There are - multiple offense codes for the same incident (one or more crimes)
unique_data_length <- length(data$INCIDENT_NUMBER) - length(unique(data$INCIDENT_NUMBER))
unique_data_length
#37 664 (If this value was 0, data would have all been unique)













