---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---


## Loading and preprocessing the data
```r
##########necessary packages

library(knitr)
opts_chunk$set(echo = TRUE)

library(dplyr)
library(lubridate)
library(ggplot2)

activity_data <- read.csv("activity.csv", header = TRUE, sep = ',', colClasses = c("numeric", "character", "integer"))

#Tidying the data
activity_data$date <- ymd(activity_data$date)
str(activity_data)
head(activity_data)
```

## What is mean total number of steps taken per day?
```r
#1 Calculate the total number of steps per day using dplyr and group by date:
steps <- activity_data %>%  filter(!is.na(steps)) %>%  group_by(date) %>%  summarize(steps = sum(steps)) %>%
  print

#2 histogram
ggplot(steps, aes(x = steps)) +
  geom_histogram(fill = "firebrick", binwidth = 1000) +
  labs(title = "Histogram of Steps per day", x = "Steps per day", y = "Frequency")

dev.copy(png,"plot1.png",width=400,height=400)
dev.off()  
#3 Calculate the mean and median of the total number of steps taken per day:
mean_steps <- mean(steps$steps, na.rm = TRUE)
median_steps <- median(steps$steps, na.rm = TRUE)
mean_steps
median_steps
```
<p><a href="https://github.com/amitms/RepData_PeerAssessment1/blob/master/figures/plot1.png" target="_blank"><img src="https://github.com/amitms/RepData_PeerAssessment1/blob/master/figures/plot1.png" alt="plot1.png" style="max-width:100%;"></a> </p>

## What is the average daily activity pattern?
```r
#Calculate the average number of steps taken in each 5-minute interval per day
interval <- activity_data %>%  filter(!is.na(steps)) %>%  group_by(interval) %>%  summarize(steps = mean(steps))
#plot
ggplot(interval, aes(x=interval, y=steps)) +  geom_line(color = "firebrick")

dev.copy(png,"plot2.png",width=400,height=400)
dev.off()
interval[which.max(interval$steps),]

```
<p><a href="https://github.com/amitms/RepData_PeerAssessment1/blob/master/figures/plot2.png" target="_blank"><img src="https://github.com/amitms/RepData_PeerAssessment1/blob/master/figures/plot2.png" alt="plot1.png" style="max-width:100%;"></a> </p>

## Imputing missing values
```r
sum(is.na(activity_data$steps))
#Missing values are 2304.

data_full <- data
nas <- is.na(data_full$steps)
avg_interval <- tapply(data_full$steps, data_full$interval, mean, na.rm=TRUE, simplify=TRUE)
data_full$steps[nas] <- avg_interval[as.character(data_full$interval[nas])]

sum(is.na(data_full$steps))

steps_full <- data_full %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print
  
  ggplot(steps_full, aes(x = steps)) +
  geom_histogram(fill = "firebrick", binwidth = 1000) +
  labs(title = "Histogram of Steps per day, including missing values", x = "Steps per day", y = "Frequency")
  
activity_data_full <- activity_data
nas <- is.na(activity_data_full$steps)
avg_interval <- tapply(activity_data_full$steps, activity_data_full$interval, mean, na.rm=TRUE, simplify=TRUE)
activity_data_full$steps[nas] <- avg_interval[as.character(activity_data_full$interval[nas])]
sum(is.na(activity_data_full$steps))

steps_full <- activity_data_full %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print

ggplot(steps_full, aes(x = steps)) +
  geom_histogram(fill = "firebrick", binwidth = 1000) +
  labs(title = "Histogram of Steps per day, including missing values", x = "Steps per day", y = "Frequency")

dev.copy(png,"plot3.png",width=400,height=400)
dev.off()

#Calculate the mean and median steps with the filled in values:
mean_steps_full <- mean(steps_full$steps, na.rm = TRUE)
median_steps_full <- median(steps_full$steps, na.rm = TRUE)
mean_steps_full
median_steps_full
```
<p><a href="https://github.com/amitms/RepData_PeerAssessment1/blob/master/figures/plot3.png" target="_blank"><img src="https://github.com/amitms/RepData_PeerAssessment1/blob/master/figures/plot3.png" alt="plot3.png" style="max-width:100%;"></a> </p>


## Are there differences in activity patterns between weekdays and weekends?
```r
activity_data_full <- mutate(activity_data_full, weektype = ifelse(weekdays(activity_data_full$date) == "Saturday" | weekdays(activity_data_full$date) == "Sunday", "weekend", "weekday"))
activity_data_full$weektype <- as.factor(activity_data_full$weektype)
head(activity_data_full)
interval_full <- activity_data_full %>%  group_by(interval, weektype) %>%  summarise(steps = mean(steps))
s <- ggplot(interval_full, aes(x=interval, y=steps, color = weektype)) +  geom_line() +  facet_wrap(~weektype, ncol = 1, nrow=2)
print(s)

dev.copy(png,"plot4.png",width=400,height=400)
dev.off()
```
<p><a href="https://github.com/amitms/RepData_PeerAssessment1/blob/master/figures/plot4.png" target="_blank"><img src="https://github.com/amitms/RepData_PeerAssessment1/blob/master/figures/plot4.png" alt="plot4.png" style="max-width:100%;"></a> </p>

