#GOOGLE DATA ANALYTICS CAPSTONE

#STEP 1: GETTING THE DATA

install.packages("tidyverse")
install.packages("lubridate")
install.packages("janitor")
install.packages("skimr")
install.packages("dplyr")

install.packages("geosphere")


library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)
library(geosphere)
library(dplyr)

April2020 <- read.csv("202004-divvy-tripdata.csv")
May2020 <- read.csv("202005-divvy-tripdata.csv")
June2020 <- read.csv("202006-divvy-tripdata.csv")
July2020 <- read.csv("202007-divvy-tripdata.csv")
August2020 <- read.csv("202008-divvy-tripdata.csv")
September2020 <- read.csv("202009-divvy-tripdata.csv")
October2020 <- read.csv("202010-divvy-tripdata.csv")
November2020 <- read.csv("202011-divvy-tripdata.csv")
December2020 <- read.csv("202012-divvy-tripdata.csv")
January2021 <- read.csv("202101-divvy-tripdata.csv")
Febraury2021 <- read.csv("202102-divvy-tripdata.csv")
March2021 <- read.csv("202103-divvy-tripdata.csv")
April2021 <- read.csv("202104-divvy-tripdata.csv")
May2021 <- read.csv("202105-divvy-tripdata.csv")
June2021 <- read.csv("202106-divvy-tripdata.csv")
July2021 <- read.csv("202107-divvy-tripdata.csv")
August2021 <- read.csv("202108-divvy-tripdata.csv")
September2021 <- read.csv("202109-divvy-tripdata.csv")
October2021 <- read.csv("202110-divvy-tripdata.csv")
November2021 <- read.csv("202111-divvy-tripdata.csv")
December2021 <- read.csv("202112-divvy-tripdata.csv")
January2022 <- read.csv("202201-divvy-tripdata.csv")
Febrauary2022 <- read.csv("202202-divvy-tripdata.csv")
March2022 <-  read.csv("202203-divvy-tripdata.csv")


#binding the monthly bike data into a collective data frame
bike_data <- rbind(April2020, May2020, June2020, July2020, August2020, September2020, 
                   October2020, November2020, December2020, January2021, Febraury2021, March2021,
                   April2021,May2021,June2021,July2021,August2021,September2021,
                   October2021,November2021,December2021,January2022,Febrauary2022,March2022)


#creating a copy of bike_data as backup
bike_data1 <- bike_data

#ACCESSING CURRENT DATAFRAME
str(bike_data1)
colnames(bike_data1) 
dim(bike_data1)
nrow(bike_data1)
summary(bike_data1)



#STEP 2: CLEANING DATA

bike_data1 <- janitor::remove_empty(bike_data1,which = c("cols"))
bike_data1 <- janitor::remove_empty(bike_data1,which = c("rows"))
remove_missing(bike_data1)

#adding relevant columns

#removing duplicatesbike_data1$date <- as.Date(bike_data1$started_at)

bike_data1$date <- as.Date(bike_data1$started_at) #add a date column
bike_data1$month <- format(as.Date(bike_data$started_at), "%b_%y")   #add a month column formatted as short hand_year (e.g. feb_2021)
bike_data1$day <- format(as.Date(bike_data1$date), "%d")  #add a day column
bike_data1$year <- format(as.Date(bike_data1$date), "%Y")  #add a year column
bike_data1$weekday <- format(as.Date(bike_data1$date), "%A")  #add a day of week column
bike_data1$time <- format(bike_data1$started_at, format = "%H:%M")  #add a time started column
bike_data1$time <- as.POSIXct(bike_data1$time, format = "%H:%M")  #change format for the time column for purposes later
bike_data1$ride_length <- (as.double(difftime(bike_data1$ended_at, bike_data1$started_at))) /60  #calculate ride length in minutes


bike_data1 <- distinct(bike_data1) 

#removes negative values
bike_data1 <- bike_data1[!bike_data1$ride_length<1,] 

#filtering out data that is not going to be used
bike_data1 <- bike_data1 %>% select(bike_type, member_casual, started_at, date, month, day, year, weekday, time, ride_length)
drop_na(bike_data1)

bike_data1$weekday <- ordered(bike_data1$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                           "Friday", "Saturday", "Sunday"))
bike_data1$month <- ordered(bike_data1$month, levels = c("April2020","May2020","June2020","July2020","August2020","September2020",
                                                         "October2020","November2020","December2020","January2021","Febraury2021","March2021",
                                                         "April2021","May2021","June2021","July2021","August2021","September2021","October2021","November2021",
                                                         "December2021","January2022","February2022","March2022"))


#DATA ANALYSIS
#We can start our analysis by finding summaries(5 point summaries of data columns) for important fields
summary(bike_data1$ride_length)

table(bike_data1$member_casual)

#Something I tried
#bike_data1$distances <- distm(c(bike_data1$start_lng, bike_data1$start_lat), c(bike_data1$end_lat, bike_data1$end_long), fun = distHaversine)

bike_data1 %>% 
  group_by(member_casual) %>%
  summarize(mean_ride_length = mean(ride_length, na.rm = TRUE))

bike_data1 %>%
  group_by(weekday,member_casual) %>%
  summarize(total_rides = n(),
            average_ride = mean(ride_length),
            min_rides = min(ride_length)) %>%
  arrange(weekday)



#DATA VISUALIZATIONS

#Total rides by weekday
bike_data1 %>%    #total rides broken down by weekday
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n() ) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge") + 
  labs(x= 'Day of Week', y='Total Number of Rides', title='Rides per Day of Week',subtitle = "Figure 1", fill = 'Type of Membership') +
  scale_y_continuous(breaks = c(250000, 400000, 550000), labels = c("250K", "400K", "550K"))

  
#Total rides by month
bike_data1 %>%   #total rides broken down by month
  group_by(member_casual, month) %>%  
  summarise(total_rides = n(),`average_duration_(mins)` = mean(ride_length)) %>% 
  arrange(customer_type) %>% 
  ggplot(aes(x=month, y=total_rides, fill = member_casual)) + geom_col(position = "dodge") + 
  labs(x= "Month", y= "Total Number of Rides", title = "Rides per Month", fill = "Type of Membership") + 
  scale_y_continuous(breaks = c(100000, 200000, 300000, 400000), labels = c("100K", "200K", "300K", "400K")) + theme(axis.text.x = element_text(angle = 45))

#Differentiation by rideable_type
bike_data1 %>%    
  ggplot(aes(x = rideable_type, fill = member_casual)) + geom_bar(position = "dodge") + 
  labs(x= 'Bike Type', y='Number of Rentals', title='Bike Type Breakdown',subtitle = "Figure 3", fill = 'Type of Membership') +
  scale_y_continuous(breaks = c(500000, 1000000, 1500000), labels = c("500K", "1Mil", "1.5Mil"))

#Differentiation by weekday
bike_data1 %>%
  ggplot(aes(x = weekday, y=n,fill= member_casual)) + geom_col() + 
  facet_wrap(~member_casual) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
  labs(x= "Weekday", y="Count", title = "Total Usage per day per rider type",subtitle = "Figure 4")




