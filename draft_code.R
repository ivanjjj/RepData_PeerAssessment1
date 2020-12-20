# Reproducible Research Course Project 1
# Author - Ivan Jennings
# 20/12/2020

# Load libraries to R
library(dplyr)
library(chron)

# Extract data and read data into R
unzip("activity.zip")
activity_data <- read.csv("activity.csv", stringsAsFactors = TRUE)

# Review data structure
head(activity_data)
str(activity_data)
summary(activity_data)

# Convert date column from Factor to Date format
activity_data$date <- as.Date(activity_data$date)

# What is mean total number of steps taken per day?
# Calculate the total number of steps taken per day

total_steps_per_day <- activity_data %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps, na.rm=TRUE))

# Make a histogram of the total number of steps taken each day
hist(total_steps_per_day$total_steps)

# Calculate and report the mean and median of the total number of steps taken per day
summary(total_steps_per_day$total_steps)

# What is the average daily activity pattern?
# Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

average_steps_per_interval <- activity_data %>%
  group_by(interval) %>%
  summarise(average_steps = mean(steps, na.rm=TRUE))

with(average_steps_per_interval,
     plot(interval, average_steps, type = "l"))

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
average_steps_per_interval$interval[max(average_steps_per_interval$average_steps)]

# Imputing missing values
# Calculate and report the total number of missing values in the dataset
summary(is.na(activity_data))
# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# Create a new dataset that is equal to the original dataset but with the missing data filled in.

#Check number of NAs per day
numberofNAs_data <- activity_data %>%
  group_by(date) %>%
  summarise(numberofNAs = mean(is.na(steps))) %>%
  filter(numberofNAs>0)

#Because only entire days of data is missing, I will impute by interval average
imputed_activity_data <- activity_data %>%
  group_by(interval) %>%
  mutate(steps=ifelse(is.na(steps),mean(steps,na.rm=TRUE),steps))

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

total_steps_per_day_imputed <- imputed_activity_data %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps, na.rm=TRUE))

hist(total_steps_per_day_imputed$total_steps)

summary(total_steps_per_day_imputed$total_steps)

# Are there differences in activity patterns between weekdays and weekends?
# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

imputed_activity_data_wd <- imputed_activity_data %>%
  mutate(weekday = ifelse(is.weekend(date),"weekend", "weekday" ))

# Make a panel plot containing a time series plot

average_steps_per_interval_wd <- imputed_activity_data_wd %>%
  group_by(interval, weekday) %>%
  summarise(average_steps = mean(steps, na.rm=TRUE))

weekend_steps_data <- average_steps_per_interval_wd %>%
  filter(weekday == "weekend")

weekday_steps_data <- average_steps_per_interval_wd %>%
  filter(weekday == "weekday")

par(mfrow = c(2,1))
with(weekend_steps_data,
     plot(interval, average_steps, type = "l"))
with(weekday_steps_data,
     plot(interval, average_steps, type = "l"))