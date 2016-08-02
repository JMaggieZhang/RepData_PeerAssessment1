---
title: "Reproducible Research Course Project 1"
author: "Jie Zhang"
date: "July 27, 2016"
output: word_document
---

This file is for the Reproducible Research Course Project 1. 

### Loading and processing the data  
    
1. Load the data(i.e. read.csv())  
```{r}
library(knitr)
opts_chunk$set(echo = TRUE)
setwd("/Users/Maggie/Desktop/Reproducible Research")
activity <- read.csv("activity.csv", header = TRUE, sep = ",")
```

2. Process/transform the data(if necessary) into a format suitable for your analysis
```{r}
library(lubridate)
activity$date <- ymd(activity$date)
```


#### What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day
```{r}
stepsperday <- aggregate(steps ~ date, activity, sum)
hist(stepsperday$steps, main = "Total number of steps taken each day",col = "lightblue", border = "pink", xlab = "Steps" )
```
 
2. Calculate and report the **mean** and **median** total number of steps taken per day.
```{r}
mean <- mean(stepsperday$steps)
mean
median <- median(stepsperday$steps)
median
```


#### What is the average daily activity pattern?  
1. Make a time series plot(i.e. type = "1") of the 5-minute interval(x-axis) and the average number of steps taken, averaged across all days(y-axis) 
```{r}
library(dplyr)
interval <- activity %>%
            filter(!is.na(steps)) %>%
            group_by(interval) %>%
           summarize(steps = mean(steps))

library(ggplot2)
ggplot(interval, aes(x=interval, y=steps)) +geom_line(color = "blue")
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  
```{r}
interval[which.max(interval$steps), ]
```
  So the No.835 5-minute interval, on average contains the maximum number of steps.

### Imputing missing values   
Note that there are a number of days/intervals where there are missing values (coded as 𝙽𝙰). The presence of missing days may introduce bias into some calculations or summaries of the data.  

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with 𝙽𝙰s)  
```{r, echo = TRUE }
sum(is.na(activity$steps))
```
  The total number of missing values in the dataset is 2304.
  
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  
```{r}
activity2<- activity 
     nas <- is.na(activity2$steps)
avginterval<- tapply(activity2$steps, activity2$interval, mean, na.rm=TRUE)
activity2$steps[nas] <- avginterval[as.character(activity2$interval[nas])]
names(activity2)
```
  
3. Create a new dataset that is equal to the original dataset but with the missing data filled in. 
```{r}
activity2<- activity2[, c("date", "interval", "steps")]
head(activity2)
```
 
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r}
total2<- activity2%>%
        group_by(date)%>%
        summarise(total_steps = sum(steps, na.rm=TRUE))
total2

ggplot(total2, aes(x = total_steps)) +
        geom_histogram(fill = "pink", binwidth = 1000) +
        labs(title = "Daily Steps including Missing values", x = "Interval", y = "Number of Steps")
```
  
####Are there differences in activity patterns between weekdays and weekends?

Use the dataset with the filled-in missing values for this part.  

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.  
```{r}
activity2 <- mutate(activity2, weektype = ifelse(weekdays(activity2$date) == "Saturday" | weekdays(activity2$date) == "Sunday", "weekend", "weekday"))
activity2$weektype <- as.factor(activity2$weektype)
head(activity2)
```

2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
```{r}
newint <- activity2 %>%
  group_by(interval, weektype) %>%
  summarise(steps = mean(steps))
s <- ggplot(newint, aes(x=interval, y=steps, color = weektype)) +
  geom_line() +
  facet_wrap(~weektype, ncol = 1, nrow=2)
print(s)
```

