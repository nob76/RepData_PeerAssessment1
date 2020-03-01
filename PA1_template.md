---
title: "Reproducible Research: Peer Assessment 1"
author: "Nate Brantley"
output: 
  html_document:
    keep_md: true
---

# Introduction
The purpose of this analysis is to analyze data from a personal activity monitoring device and determine if there are any differences in activity patterns between weekdays and weekends.

## Loading and preprocessing the data
The data I have utilized is contained in a file activity.csv. After extracting the .csv file from the .zip file, the data is loaded into a new data frame act_data.


```r
act_data <- read.table("activity.csv", header=TRUE, na.strings="NA", sep=",", quote="\"")
```
A review of the initial data set (using str function) is as follows:


```r
str(act_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

The variables steps and interval are integers which is suitable. The interval variable is divided into 5-minute segments that re-set every hour. For example, interval 55 is 55 minutes after midnight; 100 is 1AM; 105 is 1:05AM; 2355 is 11:55PM.

The date variable is a factor which needs to be transformed into a date for proper analysis. I've also added a variable for day so that the data can be analyzed based on weekday vs. weekend.


```r
library(lubridate)
```

```r
act_data$date <- as_date(act_data$date)
act_data$day <- wday(act_data$date, label=TRUE)
```

The summary of act_data is now as follows:


```r
summary(act_data)
```

```
##      steps             date               interval       day      
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Sun:2304  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   Mon:2592  
##  Median :  0.00   Median :2012-10-31   Median :1177.5   Tue:2592  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5   Wed:2592  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2   Thu:2592  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0   Fri:2592  
##  NA's   :2304                                           Sat:2304
```
Perhaps most interesting is that there were 0 steps taken in more than half of all 5-minute intervals. Also noted is that of 17568 observations, there were 2304 missing values (13.1% of the total).

A breakdown of missing values by day of week is as follows:


```r
library(dplyr)
```

```r
act_data %>% filter(is.na(steps)) %>%
            group_by(day) %>%
            summarize(no_na=sum(is.na(steps)))
```

```
## # A tibble: 6 x 2
##   day   no_na
##   <ord> <int>
## 1 Sun     288
## 2 Mon     576
## 3 Wed     288
## 4 Thu     288
## 5 Fri     576
## 6 Sat     288
```

## What is mean total number of steps taken per day?

To answer this question, I total the number of steps in the data set and then divide by the total number of days in the data set. **Note that this calculation excludes missing data.**


```r
round(sum(act_data$steps, na.rm=TRUE)/as.numeric(max(act_data$date)-min(act_data$date)+1), digits=0)
```

```
## [1] 9354
```

## What is the average daily activity pattern?

To examine the average daily activity pattern, I averaged the number of steps in each 5-minute interval across the study period with the results stored in act_data_by_interval. **Again, this calculation excludes missing data.**


```r
act_data_by_interval <- act_data %>% 
                        group_by(interval) %>%
                        summarize(avg_steps=mean(steps, na.rm=TRUE))
```

The daily activity pattern is plotted as follows:


```r
plot(act_data_by_interval$interval, act_data_by_interval$avg_steps, type="l", main="No. of Steps by Interval\n Over Study Period", xlab="5-Minute Interval", ylab="Avg. No. of Steps", col="blue", lwd=2)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

The interval with the highest number of average steps is determined as follows:


```r
filter(act_data_by_interval, avg_steps==max(avg_steps))
```

```
## # A tibble: 1 x 2
##   interval avg_steps
##      <int>     <dbl>
## 1      835      206.
```

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
