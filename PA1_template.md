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

The variables steps and interval are integers which is suitable. The date variable is a factor which needs to be transformed into a date for proper analysis.


```r
library(lubridate)
```

```r
act_data$date <- as_date(act_data$date)
```

The summary of act_data is now as follows:


```r
summary(act_data)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```
Perhaps most interesting is that there were 0 steps taken in more than half of all 5-minute intervals. Also noted is that of 17568 observations, there were 2304 missing values (13.1% of the total).

### What is mean total number of steps taken per day?

To answer this question, I total the number of steps in the data set and then divide by the total number of days in the data set. **Note that this calculation excludes missing data.**


```r
round(sum(act_data$steps, na.rm=TRUE)/as.numeric(max(act_data$date)-min(act_data$date)+1), digits=0)
```

```
## [1] 9354
```




## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
