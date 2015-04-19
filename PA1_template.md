# Reproducible Research: Peer Assessment 1

This document describes the analysis of the data collected over a 2-year period by a fitness tracker. 

## Loading and preprocessing the data

The raw data is loaded into memory.

```r
unzip (zipfile="activity.zip")
activity <- read.csv("activity.csv")
```

The data needs to be properly formatted. For that the *interval* will be converted into a time representation and the *date* in a proper date. 

```r
activity$date <- as.Date(activity$date, "%Y-%m-%d")
militaryTime <- sprintf("%04d", activity$interval)
activity$time <- paste(substr(militaryTime,1,2), substr(militaryTime,3,4),sep=":")
```

## What is mean total number of steps taken per day?
We are going to analyse the distribution of the number of steps that are taken every day.

For that purpose the daily steps will be summed and the distribution every thousand steps will be observed.


```r
# We suppress the warning message saying that some objetcs are masked by this package
library(dplyr)
```

```r
daily.steps <- activity %>% 
        group_by(date) %>% 
        summarize (total=sum(steps, na.rm=TRUE))
hist(daily.steps$total, xlab ="Number of steps per day (grouped by thousands)", main ="Distribution of steps per day", col = "lightblue", breaks=seq(0, by=1000, len=23))

# Compute and round the mean
daily.steps.mean <- mean(daily.steps$total)
daily.steps.mean <- round(daily.steps.mean, digits=2)
abline( v = daily.steps.mean, col = 2, lty = 2)

# Compute and round the median
daily.steps.median <- median(daily.steps$total)
daily.steps.median <- round(daily.steps.median, digits=2)
abline( v = daily.steps.median, col = 3, lty = 2)

legend("topright", c(paste("Mean:",daily.steps.mean),paste("Median:",daily.steps.median)) , col = 2:3, lty = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 


It can be seen that the frequency of days when the subject walks less than 1000 steps a day is the same as when the subject walks between 1000-1100. However the average is **9354.23** steps with a median value of **1.0395\times 10^{4}** steps.

## What is the average daily activity pattern?
To answer this question the data will be averaged per 5-minute interval to show what would be an average day.


```r
library(ggplot2)
average.day <- activity %>% 
        group_by(time) %>% 
        summarize (mean=mean(steps, na.rm=TRUE))

# Get the max value
max.steps <- average.day[which.max(average.day$mean),] 

ggplot(average.day, aes (x = factor(time), y = mean, group="time")) +
        geom_line(colour="steelblue", size=1) +
        theme_bw() +
        scale_x_discrete(breaks=sprintf("%02d:00",seq(00,24, by=2))) + 
        ggtitle("Number of steps in an average day") + xlab("Time") + ylab("Steps") +
        geom_segment(colour="red", lty= 2, aes(x = max.steps$time, y = 0, xend = max.steps$time, yend = max.steps$mean))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

In an average day a maximum number of **206.17** steps were taken at **08:35**h

## Imputing missing values
The amount of missing values (NA) will be analyzed in order to see how much impact they have in the data. 
Firstly the total NA values will be computed.


```r
missing.values.total <- sum(is.na(activity$steps))
missing.values.percentage <- round (missing.values.total / nrow(activity) * 100, digits = 2)
```


```r
missing.values.per.day <- activity %>%
        group_by(date) %>%
        summarize(sum=sum(is.na(steps)))

days.missing.values <- sum(missing.values.per.day$sum != 0)

barplot(missing.values.per.day$sum, xlab="Days", ylab="Amount of NAs", xpd = FALSE, main="Number of NA values per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

There is a total of just 8 days missing information. 

As can be seen the days where values are missing, they correspond the whole day. 13.11% of the total data is missing. 

The approach to impute the missing values will be to replace them with the average day information.


```r
activity2 <- activity

# Create a function to fill the NAs with the average date
# The values need to be converted from int to numeric to keep the decimals
fillNA <- function(x) 
        {
                if (is.na(x[1])) 
                {
                        x.time <- x[4]
                        idx <- which(average.day[1] == x.time)
                        avg.row <- average.day[idx,]
                        as.numeric(avg.row[2])
                }
                else
                {
                        as.numeric(x[1])
                }
        }

activity2$steps <- apply(activity2, 1, fillNA)
```



The new data with imputed values is plotted. 


```r
daily2.steps <- activity2 %>% 
        group_by(date) %>% 
        summarize (total=sum(steps, na.rm=TRUE))
hist(daily2.steps$total, xlab ="Number of steps per day (grouped by thousands)", main ="Distribution of steps per day (with imputed values)", col = "lightblue", breaks=seq(0, by=1000, len=23))

# Compute and round the mean
daily2.steps.mean <- mean(daily2.steps$total)
daily2.steps.mean <- round(daily2.steps.mean, digits=2)
abline( v = daily2.steps.mean, col = 2, lty = 2)

# Compute and round the median
daily2.steps.median <- median(daily2.steps$total)
daily2.steps.median <- round(daily2.steps.median, digits=2)
abline( v = daily2.steps.median, col = 3, lty = 2)

legend("topright", c(paste("Mean:",daily2.steps.mean),paste("Median:",daily2.steps.median)) , col = 2:3, lty = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

Comparing to the original values, can be observed that the mean value has risen by imputing the NA values. However, as they were imputed with the average data, the median remains the same.

## Are there differences in activity patterns between weekdays and weekends?
First of all we have to factorize the dates and group them into *weekend* and *weekday*.


```r
# Factorize the dates into weekday and weekend
activity2$weekpart <- as.factor(ifelse(weekdays(activity2$date) %in% c("Saturday","Sunday"), "weekend", "weekday"))

average2.day <- activity2 %>% 
        group_by(weekpart,time) %>% 
        summarize (mean=mean(steps))

# Get the max value
max2.steps <- by(average2.day, average2.day$weekpart, function(x) { x[which.max(x$mean),] })

ggplot(average2.day, aes (x = factor(time), y = mean, group=c("time"))) +
        facet_grid(weekpart ~ . ) +
        geom_line(colour="steelblue", size=1) +
        theme_bw() +
        scale_x_discrete(breaks=sprintf("%02d:00",seq(00,24, by=2))) + 
        ggtitle("Number of steps in an average day") + xlab("Time") + ylab("Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 


During the weekday a maximum of 230.3781971 steps happen at 08:35h.
However during the weekend a maximum of 166.6391509 steps happen at 09:15h.


It can be observed that the maximum number of steps happen in the morning for the weekdays and the weekends. However there is a difference in the average number of steps, being higher during the weekdays than on the weekends. 
