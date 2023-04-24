## Install data analysis packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("janitor")
install.packages("skimr")
install.packages("writexl")
install.packages("codebook")
install.packages("naniar")
install.packages("future")
install.packages("Hmisc")
print("packages installed successfully")

## Load packages into environment 
library(tidyverse) ##A collection of packages for data manipulation, visualization, and modeling in R.
library(lubridate)## Provides tools for working with dates and times in R, making it easier to parse, manipulate, and format date-time objects.
library(janitor)##Contains functions for cleaning and tidying data in R, such as renaming columns, removing duplicates, and filling missing values.
library(skimr) ##Provides a compact summary of a dataset, including variable types, missing data, and basic descriptive statistics.
library(writexl) ##Allows you to export data frames to Excel files, with support for formatting, styling, and multiple sheets.
library(codebook) ##Generates a codebook that describes the variables in a dataset, including labels, value labels, and summary statistics.
library(naniar)##Provides tools for working with missing data, such as identifying missing values, visualizing patterns of missingness, and imputing missing values.
library(future)##Provides a framework for parallel and asynchronous programming in R, allowing you to execute code on multiple cores or even remote machines.
library(Hmisc) ##Contains various functions for data analysis and modeling in R, including tools for imputation, regression, and survival analysis.
print("packages loaded into enviornment successfully")


## Read in CSV files manually
X2201 <- read_csv("C:/R/Datasets/CDivvy Original Data/2201.csv")
X2202 <- read_csv("C:/R/Datasets/CDivvy Original Data/2202.csv")
X2203 <- read_csv("C:/R/Datasets/CDivvy Original Data/2203.csv")
X2204 <- read_csv("C:/R/Datasets/CDivvy Original Data/2204.csv")
X2205 <- read_csv("C:/R/Datasets/CDivvy Original Data/2205.csv")
X2206 <- read_csv("C:/R/Datasets/CDivvy Original Data/2206.csv")
X2207 <- read_csv("C:/R/Datasets/CDivvy Original Data/2207.csv")
X2208 <- read_csv("C:/R/Datasets/CDivvy Original Data/2208.csv")
X2209 <- read_csv("C:/R/Datasets/CDivvy Original Data/2209.csv")
X2210 <- read_csv("C:/R/Datasets/CDivvy Original Data/2210.csv")
X2211 <- read_csv("C:/R/Datasets/CDivvy Original Data/2211.csv")
X2212 <- read_csv("C:/R/Datasets/CDivvy Original Data/2212.csv")
print("files read into R successfully")

## Inspecting the data sets
View(X2201)
View(X2202)
View(X2203)
View(X2204)
View(X2205)
View(X2206)
View(X2207)
View(X2208)
View(X2209)
View(X2210)
View(X2211)
View(X2212)


## Inspect the column names 
colnames(X2201)
colnames(X2202)
colnames(X2203)
colnames(X2204)
colnames(X2205)
colnames(X2206)
colnames(X2207)
colnames(X2208)
colnames(X2209)
colnames(X2210)
colnames(X2211)
colnames(X2212)
print("column names successfully printed")

## Since data is standardized, inspect structure of data sets
str(X2201)
str(X2202)
str(X2203)
str(X2204)
str(X2205)
str(X2206)
str(X2207)
str(X2208)
str(X2209)
str(X2210)
str(X2211)
str(X2212)
print("Data set structure printed successfully")


## integrate all 12 data sets into 1
Xyearbikes <- rbind(X2201, X2202, X2203, X2204, X2205, X2206, X2207, X2208, X2209, X2210, X2211, X2212)
View(Xyearbikes)
print("Data sets successfully combined")

## Remove columns not conducive to the business objective (lat and long)
Xyearbikes <- Xyearbikes[, !(names(Xyearbikes) %in% c("start_lat", "start_lng", "end_lat", "end_lng"))]
print("columns removed successfully")

## identify the number of observations among columns
nrow(Xyearbikes)##5667717
ncol(Xyearbikes)##9
dim(Xyearbikes)##5667717       9
print("Count successful")

## identify number of missing values of data set 
n_miss(Xyearbikes)##3451612
print("missing value count calculated successfully")

## remove missing values & store in new data frame 
RMXyearbikes <- na.omit(Xyearbikes)
n_miss(RMXyearbikes) ##0
print("null value removal successful")


## identify # of duplicates in a DF
XRMXyearbikesdupes <- RMXyearbikes[duplicated(RMXyearbikes), ]
print("duplicate check successful")


## print # of duplicate values from  data set 
sum(Xyearbikes[duplicated(Xyearbikes), ]) 
print("duplicate count successful")

# summarize new data frame 
summary(RMXyearbikes)
## The mean ride start time is 14:07:47, indicating that most rides happen during daytime.
## The median ride start time is on 7/20/2022 at 21:24:09, indicating that there are more rides happening during the summer season.

## remove duplicate values
RMXyearbikes <<- distinct(RMXyearbikes)
print("duplicate removal successful")


## return a table of counts for each unique value
table(RMXyearbikes$member_casual)
## casual  member  
## 1758189 2611171


## returns a table that distributes counts for both values
table(RMXyearbikes$rideable_type, RMXyearbikes$member_casual)
##              casual  member
#classic_bike   888780 1708646
#docked_bike    174858       0
#electric_bike  694551  902525

# Convert character variable to date object
RMXyearbikes$date <- as.Date(RMXyearbikes$started_at)
# Extract month from date object
RMXyearbikes$month <- format(as.Date(RMXyearbikes$date), "%m")
# Extract day from date object
RMXyearbikes$day <- format(as.Date(RMXyearbikes$date), "%d")
# Extract year from date object
RMXyearbikes$year <- format(as.Date(RMXyearbikes$date), "%Y")
# Extract day of the week from date object
RMXyearbikes$day_of_week <- format(as.Date(RMXyearbikes$date), "%A")


## extract ride difference into new column ride_length
RMXyearbikes$ride_length <- difftime(RMXyearbikes$ended_at, RMXyearbikes$started_at)
print("ride_length column created")

# Check if ride_length is a factor
is.factor(RMXyearbikes$ride_length)
# Convert ride_length to a numeric variable (in seconds)
RMXyearbikes$ride_length <- as.numeric(difftime(RMXyearbikes$ended_at, RMXyearbikes$started_at, units = "secs"))
# Check if ride_length is numeric
is.numeric(RMXyearbikes$ride_length)


## create new table that removes any ride length less than 0 as that is an error in data
RMXyearbikes_v2 <- RMXyearbikes[RMXyearbikes$ride_length > 0, ]
RMXyearbikes_v2 <- RMXyearbikes_v2[RMXyearbikes_v2$ride_length != 0, ]


## Create Summary on New Chart
summary(RMXyearbikes_v2$ride_length) 


## aggregate among members and casuals 
aggregate(RMXyearbikes_v2$ride_length ~ RMXyearbikes_v2$member_casual, FUN = mean) ## On average, casuals rode longer than the members, by a little over 50%
aggregate(RMXyearbikes_v2$ride_length ~ RMXyearbikes_v2$member_casual, FUN = median) ## Casuals had a higher ride length median value, indicating once again they rode primarily longer times than the members: over 60%
aggregate(RMXyearbikes_v2$ride_length ~ RMXyearbikes_v2$member_casual, FUN = max) ## Casuals again have higher ride length maximum value, indicating casuals rode longer than members
aggregate(RMXyearbikes_v2$ride_length ~ RMXyearbikes_v2$member_casual, FUN = min) ## Both user types have a minimum ride of 1


## Aggregate avg member and casuals per day 
aggregate(RMXyearbikes$ride_length ~ RMXyearbikes$member_casual + RMXyearbikes$day_of_week, FUN = mean)
## Order the Days of week in order

## Fix days of week column in ascending order
RMXyearbikes_v2$day_of_week <- ordered(RMXyearbikes_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

## Run the average ride time by each day for members vs casual users
aggregate(RMXyearbikes_v2$ride_length ~ RMXyearbikes_v2$member_casual + RMXyearbikes_v2$day_of_week, FUN = mean)

## analyze ridership data by user type and weekday
RMXyearbikes_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)



## Identify most popular end station
RMXyearbikes_v2 %>% 
  count(end_station_name) %>% 
  slice_max(n = 1, order_by = n) %>% 
  pull(end_station_name)
## Streeter Dr & Grand Ave

## identify most popular bike
RMXyearbikes_v2 %>%
  count(rideable_type, member_casual) %>%
  group_by(member_casual) %>%
  slice_max(n = 1, order_by = n) %>%
  select(member_casual, rideable_type, n) %>%
  as_tibble()
## 1 casual        classic_bike   888728
## 2 member        classic_bike  1708573


## visualize the number of rides by rider type by weekday
RMXyearbikes_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")



## visualize the number of rides by rider type by month
RMXyearbikes_v2 %>%
  mutate(month = month(started_at, label = TRUE)) %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")



## create a visualization for average duration by weekday
RMXyearbikes_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


## create a visualization for average duration by month
RMXyearbikes_v2 %>%
  mutate(month = month(started_at, label = TRUE)) %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


# EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
counts <- aggregate(RMXyearbikes_v2$ride_length ~ RMXyearbikes_v2$member_casual + RMXyearbikes_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')
write.csv(RMXyearbikes_v2,"./RMXyearbikes_v2.csv")



# CONCLUSION  
Casual riders typically ride for longer times than Divvy's members, although the members have a much more steady ride usage while also contributing to more rides taken as well in contrast to the casuals. There isn't enough relevant data to discern why this is, and would require deeper data collection via surveys or more personally identifiable information. However, a case could be made for this result in that the casuals are riding for more leisurely reasons, like sight seeing or quick trips, while the members are riding for more routine ventures, such as school or work commutes. 

The most popular time of the year for cyclistics users were the summer months, specifically June, July and honorary mention August. The less popular were the colder months, as the avg bike use dwindles off in both casuals and members from the autumn into the winter months, having the lowest amount of rides in Nov, Dec, and Jan, when it becomes obviously less advantageous to bike outside. 

The members tend to ride steady during M - F, provoking the idea that they are commuting within a typical work or school schedule, while  the casuals ride less during the week and ramp up on the weekends, specifically Saturday being the casuals most popular day to ride, Sunday being the second. 

The most popular end station for the **casual riders** is *Streeter Dr & Grand Ave*. Likewise, the most popular bike type for them is the **classic bike**, comparatively this is also the *members* favorite bike as well, as they use this bike more than the *casuals* raising potential for casuals missing out on that bike type dependent on timing. 

The conclusion in the analysis is that while both type of Cyclistic users prefer the warmer months to colder to use the service, the members ride more and steadier than the casuals while the casuals ride longer than the members throughout the year. Therefore, the marketing strategies provided below, should be implemented around May - early June to capitalize on the heightening rides into the summer, with an emphasis on weekend rides and top popular bikes and end stations like **Streeter Dr** and *classics*.




# RECOMMENDATIONS
### How can we engage the casual riders to convert to members?
1.)Implement a rewards program leveraging popular bike types, like the **classic** for members who ride frequently, & providing additional incentives for them to continue using the service and potentially refer new members. 
2.) Offer promotions or discounts for casual riders who sign up for a membership during the summer months, when bike usage is highest.
3.) Create partnerships with local businesses or attractions near popular end stations to offer exclusive discounts or deals to members, encouraging them to ride to these locations.



# THANK YOU FOR YOUR TIME

### Questions / Comments ?




