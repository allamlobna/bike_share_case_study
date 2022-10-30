#Install Required Packages

install.packages(tidyverse)
install.packages(lubridate)
install.packages(ggplot)
install.packages(janitor)
library(tidyverse)
library(lubridate)
library(ggplot)
library(janitor)



#STEP 1: Collect Data
#Imported Data Sets

april_21_trip_data <- read_csv("202104-divvy-tripdata.csv")
may_21_trip_data <- read_csv("202105-divvy-tripdata.csv")
june_21_trip_data <- read_csv("202106-divvy-tripdata.csv")
july_21_trip_data <- read_csv("202107-divvy-tripdata.csv")
aug_21_trip_data <- read_csv("202108-divvy-tripdata.csv")
sep_21_trip_data <- read_csv("202109-divvy-tripdata.csv")
oct_21_trip_data <- read_csv("202110-divvy-tripdata.csv")
nov_21_trip_data <- read_csv("202111-divvy-tripdata.csv")
dec_21_trip_data <- read_csv("202112-divvy-tripdata.csv")
jan_22_trip_data <- read_csv("202201-divvy-tripdata.csv")
feb_22_trip_data <- read_csv("202202-divvy-tripdata.csv")
march_22_trip_data <- read_csv("202203-divvy-tripdata.csv")

#When importing CSV files, warning message about parsing issues arose.
#This was due to docked_bike rows having a ride_length of multiple days.
#Excel did not format these columns as HH:MM:SS, however, because 
#docked_bike rider_types were to be removed as part of the cleaning process,
#nothing was done during this step. problem() was used after removing 
#docked_bike rows and the data did not have any parsing issues.
problems(april_21_trip_data)
problems(may_21_trip_data)
problems(june_21_trip_data)
#...
problems(march_22_trip_data)

#STEP 2: Wrangle Data and Combine into Single File
#Using Janitor package, compared columns in dfs for inconsistency

compare_df_cols(april_21_trip_data,
                may_21_trip_data, 
                june_21_trip_data,
                july_21_trip_data, 
                aug_21_trip_data,
                sep_21_trip_data,
                oct_21_trip_data,
                nov_21_trip_data,
                dec_21_trip_data,
                jan_22_trip_data,
                feb_22_trip_data,
                march_22_trip_data,
                return = "mismatch")

#Combined all df's into one

total_trip_data <- rbind(april_21_trip_data,
                         april_21_trip_data,
                         may_21_trip_data, 
                         june_21_trip_data,
                         july_21_trip_data, 
                         aug_21_trip_data,
                         sep_21_trip_data,
                         oct_21_trip_data,
                         nov_21_trip_data,
                         dec_21_trip_data,
                         jan_22_trip_data,
                         feb_22_trip_data,
                         march_22_trip_data)


str(total_trip_data)

#STEP 3: Clean Data

#Confirmed there are two types of members and three types of bikes
#Members: casual, member
#Bikes: classic_bike, electric_bike, docked_bike
rider_types <- total_trip_data %>%
  group_by(rideable_type, member_casual)

summarize(rider_types)

#docked_bike is when bikes were taken out of docks and checked for quality
#This is irrelevant to the study, so removed from df
#Obtained total number of docked_bike to ensure that total removed is correct
table(total_trip_data$rideable_type)

#classic_bike   docked_bike electric_bike 
#2790047        321282       1272661

total_trip_data <- 
  total_trip_data[!grepl('docked_bike', total_trip_data$rideable_type),]

table(total_trip_data$rideable_type)

#classic_bike electric_bike 
#2790047       1272661

#No problems were found as expected in STEP 1
problems(total_trip_data)

#start_station_name and end_station_name have over 700,000 null values,
#so removed columns as lon/lat can be used for ride paths
sum(is.na(total_trip_data$start_station_name))
sum(is.na(total_trip_data$end_station_name))

total_trip_data = subset(total_trip_data, select = 
                           -c(start_station_name,end_station_name))

#start_station_id and end_station_id have over 700,000 null values,
#so removed columns as lon/lat can be used for ride paths
sum(is.na(total_trip_data$start_station_id))
sum(is.na(total_trip_data$end_station_id))

total_trip_data = subset(total_trip_data, select = 
                           -c(start_station_id,end_station_id))

#to find max and min ride_length, must convert to seconds. Originally in
# HMS then converted to the nearest second.

total_trip_data$ride_length <- seconds(hms(total_trip_data$ride_length))
total_trip_data$ride_length <- period_to_seconds(total_trip_data$ride_length)
sum(is.na(total_trip_data$ride_length))

# 145 NA ride_length values
total_trip_data <- drop_na(total_trip_data, ride_length)

#confirm that all NA rows have been removed
sum(is.na(total_trip_data$ride_length))

#No negative ride_length is found 
#  Min. 1st Qu.  Median    Mean     3rd Qu.    Max. 
#    0     384     677      1091    1207      93596
summary(total_trip_data$ride_length)

#Aggregated the relavant data into a df
agg_total_trip <- data.frame(
  "mean" =
    c(aggregate(total_trip_data$ride_length ~ 
                  total_trip_data$member_casual, FUN = mean)),
  "median" = 
    c(aggregate(total_trip_data$ride_length ~ 
                  total_trip_data$member_casual, FUN = median)),
  "max" =
    c(aggregate(total_trip_data$ride_length ~ 
                  total_trip_data$member_casual, FUN = max)),
  "min" = 
    c(aggregate(total_trip_data$ride_length ~ 
                  total_trip_data$member_casual, FUN = min)),
  "avg ride time per day" =
    c(aggregate(total_trip_data$ride_length ~ 
                  total_trip_data$member_casual 
                + total_trip_data$day_of_week, FUN = mean))
)

#Transposed aggregated data for easier readability
agg_total_trip <- data.frame (t(agg_total_trip))


write.csv(total_trip_data,"C:\\Users\\allam\\Documents\\GitHub\\bike_share_case_study\\total_trip_data.csv", row.names = FALSE)


#number of rides by rider type
total_trip_data %>% 
  mutate(weekday = wday(day_of_week, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Number of Rides by Rider Type", x = "Weekday", 
       y = "Number of Rides", fill = "Rider Type")


#average duration
total_trip_data %>% 
  mutate(weekday = wday(day_of_week, label = TRUE)) %>%
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Average Ride Length by Rider Type", x = "Weekday", 
       y = "Ride Length", fill = "Rider Type")





