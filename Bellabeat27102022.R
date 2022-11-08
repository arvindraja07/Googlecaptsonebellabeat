#Step 1: install and load the required package

require(ggplot2)
require(ggcorrplot)
require(ggstatsplot)
require(tidyverse)
require(lubridate)
require(dplyr)
require(readr)
require(tidyr)
require(skimr)
require(janitor)
require(scales)


#Step 2: Importing data

daily_activity = read.csv("E:/Desktop/Bellabeat27102022/dailyActivity_merged.csv", stringsAsFactors = TRUE)

weight_log = read.csv("E:/Desktop/Bellabeat27102022/weightLogInfo_merged.csv", stringsAsFactors = TRUE)

sleep_tracking = read.csv("E:/Desktop/Bellabeat27102022/sleepDay_merged.csv", stringsAsFactors = TRUE)

#Step 3: Data Cleaning
#Quick overview of the provided data

head(daily_activity)
head(weight_log)
head(sleep_tracking)

colnames(daily_activity)
colnames(weight_log)
colnames(sleep_tracking)

view(daily_activity)
view(weight_log)
view(sleep_tracking)

#Removing nulls

modi_daily_activity = daily_activity %>% filter(TotalSteps !=0)

view (modi_daily_activity)

weight_new <- weight_log %>% 
  separate(Date, c("Date", "Time"), " ")
view(weight_new)

modi_sleep_tracking <- sleep_tracking %>% separate(SleepDay, c("Date","Time"), " ")

view(modi_sleep_tracking)

#Removing duplicates

n_distinct(modi_daily_activity$Id)
n_distinct(modi_sleep_tracking$Id)
n_distinct(weight_new$Id)

nrow(modi_sleep_tracking)
nrow(unique(modi_sleep_tracking))

nrow(weight_new)
nrow(unique(weight_new))

modi_sleep_data = unique(modi_sleep_tracking)
nrow(modi_sleep_data)

nrow(modi_daily_activity)
nrow(unique(modi_daily_activity))

modi_daily_activity
modi_sleep_data
weight_new


#Creating a new dataset for analysis for the required columns

final_daily_activities_data <- modi_daily_activity %>% 
  select(Id, ActivityDate, TotalSteps, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes, Calories) %>% 
  rename(Date = ActivityDate)


final_weight_data <- weight_new %>% 
  select(Id, Date, BMI, WeightPounds, IsManualReport)

final_daily_sleep_data <- modi_sleep_data %>% 
  select(Id, Date, TotalMinutesAsleep, TotalTimeInBed)

final_daily_activities_data <- final_daily_activities_data %>% mutate( Weekday = weekdays(as.Date(Date, "%m/%d/%Y")))

final_weight_data <- final_weight_data %>% mutate( Weekday = weekdays(as.Date(Date, "%m/%d/%Y")))

final_daily_sleep_data <- final_daily_sleep_data %>% mutate( Weekday = weekdays(as.Date(Date, "%m/%d/%Y")))

summary(final_daily_activities_data)

summary(final_daily_sleep_data)

summary(final_weight_data)

#Step 4: Analysis and Share
#creating pie chart to find the contribution of activity level

VeryActiveMin <- sum(final_daily_activities_data$VeryActiveMinutes)
FairlyActiveMin <- sum(final_daily_activities_data$FairlyActiveMinutes)
LightlyActiveMin <- sum(final_daily_activities_data$LightlyActiveMinutes)
SedentaryMin <- sum(final_daily_activities_data$SedentaryMinutes)
slice <- c(VeryActiveMin,FairlyActiveMin,LightlyActiveMin,SedentaryMin)
lab <- c("VeryActive","FairlyActive","LightlyActive","Sedentary")
pct <- round(slice/sum(slice)*100)
lab <- paste(lab, pct)
lab <- paste(lab, "%", sep="")
pie(slice, labels = lab, col = rainbow(length(lab)), main = "Percentage of Activity Minutes")

#correlation between steps and calories
# Normality test
qqnorm(final_daily_activities_data$TotalSteps, pch = 1, frame = FALSE)
qqline(final_daily_activities_data$TotalSteps, col = "steelblue", lwd = 2)

qqnorm(final_daily_activities_data$Calories, pch = 1, frame = FALSE)
qqline(final_daily_activities_data$Calories, col = "steelblue", lwd = 2)

ggplot(data=final_daily_activities_data) +
  geom_point(mapping=aes(x=TotalSteps, y=Calories), color="brown") +
  geom_smooth(mapping=aes(x=TotalSteps, y=Calories)) +
  labs(title="The Relationship Between Total Steps and Calories", x="Total Steps", y="Calories Burned (kcal)")

ggcorrmat(
  data = final_daily_activities_data, ## data from which variable is to be taken
  cor.vars = c(TotalSteps,Calories) ## specifying correlation matrix variables
)

merged_data <- merge(final_daily_activities_data, final_daily_sleep_data, by="Id")

ggplot(data=merged_data) +
  geom_point(mapping=aes(x=TotalMinutesAsleep, y=VeryActiveMinutes, color="VeryActiveMinutes")) +
  geom_smooth(mapping=aes(x=TotalMinutesAsleep, y=VeryActiveMinutes, regLineColor="blue"))+
  labs(title="The Relationship Between Activity Levels and Total Minutes Asleep", x="Total Minutes Asleep", y="Minutes of Activity")

ggplot(data=merged_data) +
  geom_point(mapping=aes(x=TotalMinutesAsleep, y=FairlyActiveMinutes, color="FairlyActiveMinutes")) +
  geom_smooth(mapping=aes(x=TotalMinutesAsleep, y=FairlyActiveMinutes, regLineColor="blue")) +
  labs(title="The Relationship Between Activity Levels and Total Minutes Asleep", x="Total Minutes Asleep", y="Minutes of Activity")

ggplot(data=merged_data) +
  geom_point(mapping=aes(x=TotalMinutesAsleep, y=LightlyActiveMinutes, color="LightlyActiveMinutes")) +
  geom_smooth(mapping=aes(x=TotalMinutesAsleep, y=LightlyActiveMinutes, regLineColor="blue")) +
  labs(title="The Relationship Between Activity Levels and Total Minutes Asleep", x="Total Minutes Asleep", y="Minutes of Activity")

ggplot(data=merged_data) +
  geom_point(mapping=aes(x=TotalMinutesAsleep, y=SedentaryMinutes, color="SedentaryMinutes")) +
  geom_smooth(mapping=aes(x=TotalMinutesAsleep, y=SedentaryMinutes, regLineColor="blue")) +
  labs(title="The Relationship Between Activity Levels and Total Minutes Asleep", x="Total Minutes Asleep", y="Minutes of Activity")


