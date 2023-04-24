## install data wrangling packages
install.packages("tidyverse")
install.packages("scales")
install.packages("lubridate")

## Load Packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

## Read in data sets
activitydaily <- read_csv("C:/R/Datasets/CFitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
sleep_day <- read_csv("C:/R/Datasets/CFitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
caloriesdaily <- read_csv("C:/R/Datasets/CFitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
intensitiesdaily <- read_csv("C:/R/Datasets/CFitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")
stepsdaily <- read_csv("C:/R/Datasets/CFitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")
weightlog <- read_csv("C:/R/Datasets/CFitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")
hourlysteps <- read_csv("C:/R/Datasets/CFitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")

### Exploring the tables

## Identify all the columns in the activitydaily data.
colnames(activitydaily)
summary(activitydaily)
### This data set shows users are spending a little over 16.50 hrs physically inactive on average, or 991.2 minutes.
### The maximum value for sedentary minutes is 1440, indicating that some users are inactive for the entire day.
### The mean VeryActiveMinutes for users in a day is 21.16, indicating that users are engaging in high-intensity activities for a small fraction of their day, and more engaged in LightlyActiveMinutes for the larger half of their day. 
### The ActivityDate column should be converted from chr class to Date class 


## Inspect activitydaily data.
head(activitydaily)
str(activitydaily)


## Identify all the columns in the sleep_day data.
colnames(sleep_day)
summary(sleep_day)
### SleepDay column to be converted from chr class to Date class 
### The average TotalMinutesAsleep is 419.5, indicating that on average users slept for about 7 hours.
### The data set shows that sleep habits vary greatly among users, with some users recording very short sleep times and others recording very long sleep times, promoting disporportionate disparity with scope of the sample 
### The average TotalTimeInBed is 458.6, indicating that on average users spent about 7.5 hours in bed.


## Inspect sleep_day data.
head(sleep_day)
str(sleep_day)

## Identify all the columns in the caloriesdaily data.
colnames(caloriesdaily)
summary(caloriesdaily)
### ActivityDay column to be converted from chr class to Date type
### The summary statistics show that the minimum calorie expenditure is 0, indicating that some users may not have engaged in any physical activity during that time

## Inspect caloriesdaily data.
head(caloriesdaily)
str(caloriesdaily)

## Identify all the columns in the intensitiesdaily data.
colnames(intensitiesdaily)
summary(intensitiesdaily)
### ActivityDay column to be converted from chr class to date type
### 25% of users have sedentary time below 729.8 mins, while 75% have sedentary time below 1229.5 mins. Even the Majority of active users spent most of their day sedent

## Inspect intensitiesdaily data.
head(intensitiesdaily)
str(intensitiesdaily)

## Identify all the columns in the stepsdaily data.
colnames(stepsdaily)
summary(stepsdaily)
### 25% of users take <3790 steps/day and 75% take <10727 steps/day
### ActivityDay column to be converted from chr class to Date type
### The minimum step count is 0, meaning that there are days where the user did not take any steps.


## Inspect stepsdaily data.
head(stepsdaily)
str(stepsdaily)

## Identify all the columns in the weightlog data.
colnames(weightlog)
summary(weightlog)
### Date column to be converted from Chr class to Date type
### About 65 records have missing values for the "Fat" column
### The frequent occurrence of TRUE in the IsManualReport column suggests significant manual reporting, potentially affecting data accuracy.

## Inspect weightlog data.
head(weightlog)
str(weightlog)

### Identify Columns in hourlysteps data set
colnames(hourlysteps)
summary(hourlysteps)
### There are some extreme outliers in the dataset, with a maximum step count of 10554 in a single hour.




## transform Date columns from each table, format from *chr to *date type in new Ymd column 
sleep_day$Ymd <- as.Date(sleep_day$SleepDay, format = "%m/%d/%Y")
activitydaily$Ymd <- as.Date(activitydaily$ActivityDate, format = "%m/%d/%Y")
caloriesdaily$Ymd <- as.Date(caloriesdaily$ActivityDay, format = "%m/%d/%Y")
intensitiesdaily$Ymd <- as.Date(intensitiesdaily$ActivityDay, format = "%m/%d/%Y")
stepsdaily$Ymd <- as.Date(stepsdaily$ActivityDay, format = "%m/%d/%Y")
weightlog$Ymd <- as.Date(weightlog$Date, format = "%m/%d/%Y")
hourlysteps$Ymd <- as.Date(hourlysteps$Ymd, format = "%m/%d/%Y") ### This is done after new column Ymd is created



## Identify N/A values in tables 
map(activitydaily, ~sum(is.na(.)))
map(caloriesdaily, ~sum(is.na(.)))
map(sleep_day, ~sum(is.na(.)))
map(intensitiesdaily, ~sum(is.na(.)))
map(stepsdaily, ~sum(is.na(.)))
map(weightlog, ~sum(is.na(.)))
map(hourlysteps, ~sum(is.na(.)))

### found multiple N/A values from weightlog data set in Fat column


## Replacing Null Values from Fat column in weightlog data set with the average of the 2 numbers
weightlog$Fat <- mean(weightlog$Fat, na.rm = TRUE)

## Separate Date and Time from Date column in weightlog / sleep_day / hourlysteps data sets into separate variables
weightlog <- separate(weightlog, Date, c("Date", "Time"), sep = " " )
sleep_day <- separate(sleep_day, SleepDay, c("Date", "Time"), sep = " " )
hourlysteps <- separate(hourlysteps, ActivityHour, c("Ymd", "Time"), sep = " " )


## Sort tables in order by Ymd
weightlog <- weightlog[order(as.Date(weightlog$Ymd, format = "%m/%d/%Y")), ]
activitydaily <- activitydaily[order(as.Date(activitydaily$Ymd, format = "%m/%d/%Y")), ]
caloriesdaily <- caloriesdaily[order(as.Date(caloriesdaily$Ymd, format = "%m/%d/%Y")), ]
intensitiesdaily <- intensitiesdaily[order(as.Date(intensitiesdaily$Ymd, format = "%m/%d/%Y")), ]
sleep_day <- sleep_day[order(as.Date(sleep_day$Ymd, format = "%m/%d/%Y")), ]
stepsdaily <- stepsdaily[order(as.Date(stepsdaily$Ymd, format = "%m/%d/%Y")), ]
hourlysteps <- hourlysteps[order(as.Date(hourlysteps$Ymd, format = "%m/%d/%Y")), ]



## Identify number of participants
n_distinct(activitydaily$Id) # 33
n_distinct(weightlog$Id)  # 8
n_distinct(sleep_day$Id) # 24
n_distinct(stepsdaily$Id) # 33
n_distinct(caloriesdaily$Id) # 33
n_distinct(intensitiesdaily$Id) # 33

#### Discard Small data set sample size of 8 users (weightlog) too small for Central Limit Theorum

## Identify Duplicates
sum(duplicated(activitydaily))
sum(duplicated(caloriesdaily))
sum(duplicated(sleep_day))
sum(duplicated(intensitiesdaily))
sum(duplicated(stepsdaily))


#### Found 3 Duplicates from sleep_day table

## Remove duplicates from sleep_day table
sleep_day <- sleep_day %>% distinct() %>% drop_na()
sum(duplicated(sleep_day)) # check 


### Review some summary statistcs of the data sets

# activity summary
activitydaily %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes, Calories) %>%
  summary()
### The dataset indicates that users were largely sedentary, with a median SedentaryMinutes of 1,057.5, indicating more than 17 hours spent being inactive daily, even for the most active users.


# explore num of active minutes per category
activitydaily %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  summary()
### Most users engaged in low-intensity activities throughout the day, as evidenced by a median of 199 lightly active minutes and a mean of 192.8.
### Even the least active users in the dataset had a minimum of 127 lightly active minutes per day, as indicated by the 25th percentile.


# sleep
sleep_day %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary() 
### The 75th percentile of TotalTimeInBed was 526.0, meaning that 75% of users spent more than 8.5 hours in bed per day.



#### Note that both 'activitydaily' and 'sleep_day' share the 'Id' and 'Ymd' columns - these can be used to merge the datasets 


## Merge dailyactivity and sleep_day datasets 
activitydaily2 <- merge(activitydaily, sleep_day, by = c("Id" , "Ymd"))
summary(activitydaily2)
### Moderate activity is most common, with higher focus on light activity.
### People are sedentary for 11.9 hours/day on average (712.1 min).
### Average sleep time is 6.98 hours (419.2 min) with 7.64 hours (458.5 min) in bed.

#### Visualize Insights -------------

## Plot Daily Sedentary Minutes
ggplot(activitydaily, aes(x = Ymd, y = SedentaryMinutes, fill = "mins")) +
  geom_bar(stat = "identity") +
  labs(x = "Ymd", y = "Sedentary Minutes", title = "Daily Sedentary Minutes") +
  scale_fill_manual(values = c("orange"))
## High Sendentary times


## Plot Steps Per Hour
ggplot(data=hourlysteps, aes(x=Time2, y=StepTotal)) + geom_bar(stat="identity", fill="purple") + labs(title = 'Activity per Hour')
## Most active hour of the day was 7:00 PM, could be utilized for the hydration bottle reminder based on bellabeat data

## Plot TotalSteps vs Calories Correlation
ggplot(data=activitydaily, aes(x=TotalSteps, y=Calories)) + 
  geom_point() + geom_smooth() + labs(title="Total Steps vs. Calories")
## Positive correlation between TotalSteps and Calories burned daily, the more steps taken, the more calories burned of course.


## Plot TotalSteps vs SedentaryMinutes
ggplot(data=activitydaily, aes(x=TotalSteps, y=SedentaryMinutes)) +
  geom_point() + geom_smooth() + labs(title="Total Steps vs. Sedentary Minutes")
## Negative correlation between steps taken in a day, and sedentary minutes, because as the sedentary minutes increase, total steps taken per day decreases.


## Plot the relationship between minutes asleep and time in bed
ggplot(data=sleep_day, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) +
  geom_point() + geom_smooth() + labs(title="Total Minutes Asleep vs. Total Time In Bed")
## Positive correlation between TotalMinutesAsleep and TotalTimeInBed, indicating that the more minutes spent in the bed, the higher likelihood of similar sleep patterns, however longer time spent in bed doesn't completely equal Total Minutes Asleep.



### Conclusion 
After conducting an analysis of the Fitbit health data provided by the participants, it can be inferred that the user adopted a moderately active lifestyle, primarily focused on light activity. It is worth noting, however, that the bedrock on which this analysis is based has several limitations, such as a substantial number of outliers, small sample sizes, and significant manual reporting, which could potentially lead to inaccurate records.
These findings provide valuable opportunities to assist the user in achieving and maintaining their current health and wellness goals while identifying areas for improvement and growth.
The Bellabeats Marketing Analytics Team can utilize this information to create customized fitness and nutrition plans, gamified challenges, and sleep tracking tools tailored to the user's needs.
By offering personalized recommendations and incentivizing healthy habits, users can be motivated to achieve their wellness goals and maintain a healthy lifestyle. 
Furthermore, these findings can be applied to the development of a social or community aspect of the Bellabeat app, providing users with opportunities to connect with like-minded individuals, participate in challenges and competitions, and earn awards. This fosters engagement, immersion, and longevity by promoting inclusivity.
Overall, while the reliability of the Fitbit health data set may be questionable due to its low quality, it serves as a helpful resource for understanding the general overview of the health and wellness market. The limitations and strengths identified in the data can be leveraged to inform marketing strategies that cater to the user's needs and interests, specifically the need for increased physical activity and optimal rest for healthy living. By utilizing the insights gained from the data, Bellabeat can design engaging, personalized experiences that motivate users to make positive changes in their lives and maintain their health and wellness goals.



### Recommendations for Marketing Strategies:

Bellabeat can take these insights and apply them to the Bellabeat app, and both tracking devices like the Timepiece, and the Leaf tracking device. 

Promote Light Activity: The user's Fitbit health data indicates a market trend where users tend to engage in more light activity compared to other categories. Bellabeat can capitalize on this by marketing products and services that promote and incentivize light physical activity, such as walking challenges, daily step goals, and outdoor adventure campaigns.

Offer Sleep-Focused Products: The user's data suggests that they maintain a consistent sleep pattern, which presents an opportunity for companies to market products that promote healthy sleep. This could include products such as sleep tracking apps, sleepwear, and mattresses.

Provide Health and Wellness Reminder Support: The user's maintained a high average of sedentary activity hours throughout the month, over 68 % of their day. This information can be used to implement reminder notifications excessive sedentary times to incentivize progressive steps towards decreasing the ammount of unhealthy sedentary time accumulated over a period of time. Likewise, this can be implemented for bed times customized to the level of optimal sleep users require, in accordance with the CDC and official health documents.

Interactive Fitness Goals: The user's Fitbit data shows that they engage in moderate physical activity levels. Bellabeat can incentivize increased activity levels by marketing gamified fitness challenges, daily workout routines, and rewards for hitting fitness goals. This, in conjunction with a social media / community implementation can help users stay motivated and engaged with their fitness goals

Optimal Hydration Monitoring: The Spring water bottle already tracks daily water intake, but Bellabeat could expand on this feature by providing personalized recommendations on how much water each user should be drinking based on their activity levels and other health data.

