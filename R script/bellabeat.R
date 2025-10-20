library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
setwd("C:/Users/mahaboob/Desktop/Fitabase Data 3.12.16-4.11.16")
activity<-read.csv("dailyActivity_merged.csv")
heartrate<-read.csv("heartrate_seconds_merged.csv")
intensities<-read.csv("dailyIntensities_merged.csv")
sleep<-read.csv("sleepday_merged.csv")
weight<-read.csv("weightLogInfo_merged.csv")


n_distinct(activity$Id)
n_distinct(intensities$Id)
n_distinct(sleep$Id)
n_distinct(heartrate$Id)




str(activity)
summary(activity)
head(activity)
#changing ID  from numerical to nominal
activity$Id<-as.character(activity$Id)
#changed date format from char to date
activity$ActivityDate<-as.Date(activity$ActivityDate,format = "%m/%d/%Y")
#remove duplicates
activity <- activity[!duplicated(activity), ]
#checking missing values
colSums(is.na(activity))

str(intensities)
summary(intensities)
head(intensities)
intensities$Id<-as.character(intensities$Id)
intensities$ActivityDay<-as.Date(intensities$ActivityDay,format="%m/%d/%Y")
intensities<-intensities[!duplicated(intensities), ]
colSums(is.na(intensities))




str(heartrate)
summary(heartrate)
head(heartrate)
heartrate$Id<-as.character(heartrate$Id)
#converting time to posixct format
heartrate$Time <- as.POSIXct(heartrate$Time, format="%m/%d/%Y %H:%M:%S")#Convert Time to POSIXct
heartrate$Date <- as.Date(heartrate$Time)
#remove duplicates and check missing values
heartrate<-heartrate[!duplicated(heartrate), ]
colSums(is.na(heartrate))




str(sleep)
summary(sleep)
head(sleep)
sleep$Id<-as.character(sleep$Id)
#convert sleepday to date format
sleep$SleepDay<-as.POSIXct(sleep$SleepDay,format="%m/%d/%Y %I:%M:%S %p")
sleep$Date<-as.Date(sleep$SleepDay)
#removing duplicates and check for missing values
sleep<-sleep[!duplicated(sleep), ]
colSums(is.na(sleep))



#analysing daily active patterns
activity$TotalActiveMinutes <- activity$VeryActiveMinutes +
  activity$FairlyActiveMinutes +
 activity$LightlyActiveMinutes
# creating categories to understand user behaviour:
activity$ActivityLevel <- ifelse(activity$TotalSteps < 5000, "Sedentary",
                                       ifelse(activity$TotalSteps < 10000, "Moderate", "Active"))
table(activity$ActivityLevel)
# exploring descriptive statistics
activity %>%
  summarise(
    avg_steps = mean(TotalSteps),
    median_steps = median(TotalSteps),
    avg_calories = mean(Calories),
    avg_active_minutes = mean(TotalActiveMinutes)
  )
# to find  percentage of users meeting the 10,000-step goal 

# Total number of records
total_records <- nrow(activity)
total_records
# Number of records meeting the 10,000-step goal
goal_met <- nrow(activity %>% filter(TotalSteps >= 10000))
goal_met
# Calculate percentage
percent_goal_met <- (goal_met / total_records) * 100
percent_goal_met
# Print result
cat("Percentage of users meeting 10,000-step goal:", round(percent_goal_met, 2), "%\n")



#correlate between movement and energy
cor(activity$TotalSteps,activity$Calories)
cor(activity$TotalActiveMinutes, activity$Calories)


#analysing sleep behaviour


#create derived metrics sleep effiency
sleep$SleepEfficiency <- sleep$TotalMinutesAsleep / sleep$TotalTimeInBed
#slepep duration categories
sleep$sleepquality<-ifelse(sleep$TotalMinutesAsleep<360,"Poor",ifelse(sleep$TotalMinutesAsleep<480,"Fair","Good"))

table(sleep$sleepquality)
#exploring descriptive statistics
library(dplyr)

sleep %>%
  summarise(
    avg_sleep = mean(TotalMinutesAsleep),
    avg_efficiency = mean(SleepEfficiency),
    good_sleepers = sum(sleepquality == "Good"),
    poor_sleepers = sum(sleepquality == "Poor")
  )




#analysing heart rate and stress

#summarize heart rate by day and user
library(dplyr)

daily_hr <- heartrate %>%
  group_by(Id, Date) %>%
  summarise(
    avg_hr = mean(Value),
    min_hr = min(Value),
    max_hr = max(Value),
    sd_hr = sd(Value) 
    # Heart rate variability (HRV)
  )
daily_hr





#activity plot
ggplot(activity, aes(x=TotalSteps, y=Calories,color=TotalSteps)) + 
  geom_point(size = 2) + 
  labs(title = "Bellabeat: Total Steps vs Total Calories")+
  theme_bw() +
  theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))

ggplot(data = activity, aes(x=TotalSteps, y=TotalDistance,color=TotalDistance)) +
  geom_line(mapping = aes(x=TotalSteps, y=TotalDistance)) +
  labs(title = "Correlation between Total Steps & Total Distance") +
  theme_bw() +
  theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))


ggplot(activity, aes(x=TotalSteps, y=SedentaryMinutes, color=TotalSteps)) +
  geom_point(size = 3, alpha = 0.5)+
  labs(title = "Sedentary Minutes vs Total Steps Taken") +
  theme_bw() +
  theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))


ggplot(sleep, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + 
  geom_point(shape = "circle", size = 3, alpha = 0.5, color = "#193E61") +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(title = "Bellabeat: Time Asleep vs Time in Bed", 
       caption = "source: https://htmlcolorcodes.com/") +
  theme_bw() +
  theme(plot.title = element_text(size = 15L, face = "bold", hjust = 0.5))





