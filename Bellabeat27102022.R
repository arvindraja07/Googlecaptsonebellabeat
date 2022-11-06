#Step 1: install and load the required package

require(tidyverse)
require(lubridate)
require(ggplot2)
require(readr)
require(tidyr)
require(dplyr)
require(skimr)
require(janitor)
require(scales)
require(ggcorrplot)
require(ggstatsplot)

#Step 2: Importing data

daily_activity = read.csv("E:/Desktop/Bellabeat27102022/dailyActivity_merged.csv", stringsAsFactors = TRUE)

weight_log = read.csv("E:/Desktop/Bellabeat27102022/weightLogInfo_merged.csv", stringsAsFactors = TRUE)

sleep_tracking = read.csv("E:/Desktop/Bellabeat27102022/sleepDay_merged.csv", stringsAsFactors = TRUE)

head(daily_activity)
head(weight_log)
head(sleep_tracking)

colnames(daily_activity)
colnames(weight_log)
colnames(sleep_tracking)

view(daily_activity)
view(weight_log)
view(sleep_tracking)

modi_daily_activity = daily_activity %>% filter(TotalSteps !=0)

view (modi_daily_activity)

weight_new <- weight_log %>% 
  separate(Date, c("Date", "Time"), " ")
view(weight_new)

modi_sleep_tracking <- sleep_tracking %>% separate(SleepDay, c("Date","Time"), " ")

view(modi_sleep_tracking)

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

final_daily_activities_data <- modi_daily_activity %>% 
  select(Id, ActivityDate, TotalSteps, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes, Calories) %>% 
  rename(Date = ActivityDate)


final_weight_data <- weight_new %>% 
  select(Id, Date, BMI, WeightPounds, IsManualReport)

final_daily_sleep_data <- modi_sleep_data %>% 
  select(Id, Date, TotalMinutesAsleep, TotalTimeInBed)


summary(final_daily_activities_data)

summary(final_daily_sleep_data)

summary(final_weight_data)

VeryActiveMin <- sum(final_daily_activities_data$VeryActiveMinutes)
FairlyActiveMin <- sum(final_daily_activities_data$FairlyActiveMinutes)
LightlyActiveMin <- sum(final_daily_activities_data$LightlyActiveMinutes)
SedentaryMin <- sum(final_daily_activities_data$SedentaryMinutes)
TotalMin <- VeryActiveMin + FairlyActiveMin + LightlyActiveMin + SedentaryMin
slices <- c(VeryActiveMin,FairlyActiveMin,LightlyActiveMin,SedentaryMin)
lbls <- c("VeryActive","FairlyActive","LightlyActive","Sedentary")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls, "%", sep="")
pie(slices, labels = lbls, col = rainbow(length(lbls)), main = "Percentage of Activity in Minutes")

ggplot(data=final_daily_activities_data) +
  geom_point(mapping=aes(x=TotalSteps, y=Calories), color="red") +
  geom_smooth(mapping=aes(x=TotalSteps, y=Calories)) +
  labs(title="The Relationship Between Total Steps and Calories Burned", x="Total Steps", y="Calories Burned (kcal)")

ggcorrmat(
  data = final_daily_activities_data, ## data from which variable is to be taken
  cor.vars = c(TotalSteps,Calories) ## specifying correlation matrix variables
)

combined_data <- merge(final_daily_activities_data, final_daily_sleep_data, by="Id")

ggplot(data=combined_data) +
  geom_point(mapping=aes(x=TotalMinutesAsleep, y=VeryActiveMinutes, color="VeryActiveMinutes")) +
  geom_smooth(mapping=aes(x=TotalMinutesAsleep, y=VeryActiveMinutes, regLineColor="blue"))+
  labs(title="The Relationship Between Activity Levels and Total Minutes Asleep", x="Total Minutes Asleep", y="Minutes of Activity")

ggplot(data=combined_data) +
  geom_point(mapping=aes(x=TotalMinutesAsleep, y=FairlyActiveMinutes, color="FairlyActiveMinutes")) +
  geom_smooth(mapping=aes(x=TotalMinutesAsleep, y=FairlyActiveMinutes, regLineColor="blue")) +
  labs(title="The Relationship Between Activity Levels and Total Minutes Asleep", x="Total Minutes Asleep", y="Minutes of Activity")

ggplot(data=combined_data) +
  geom_point(mapping=aes(x=TotalMinutesAsleep, y=LightlyActiveMinutes, color="LightlyActiveMinutes")) +
  geom_smooth(mapping=aes(x=TotalMinutesAsleep, y=LightlyActiveMinutes, regLineColor="blue")) +
  labs(title="The Relationship Between Activity Levels and Total Minutes Asleep", x="Total Minutes Asleep", y="Minutes of Activity")

ggplot(data=combined_data) +
  geom_point(mapping=aes(x=TotalMinutesAsleep, y=SedentaryMinutes, color="SedentaryMinutes")) +
  geom_smooth(mapping=aes(x=TotalMinutesAsleep, y=SedentaryMinutes, regLineColor="blue")) +
  labs(title="The Relationship Between Activity Levels and Total Minutes Asleep", x="Total Minutes Asleep", y="Minutes of Activity")

ggplot(data=final_daily_activities_data) +
  geom_point(mapping=aes(x=TotalSteps, y=Calories), color="red") +
  geom_smooth(mapping=aes(x=TotalSteps, y=Calories)) +
  labs(title="The Relationship Between Total Steps and Calories Burned", x="Total Steps", y="Calories Burned (kcal)")

