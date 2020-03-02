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
        summarize(number_na=sum(is.na(steps)))
```

```
## # A tibble: 6 x 2
##   day   number_na
##   <ord>     <int>
## 1 Sun         288
## 2 Mon         576
## 3 Wed         288
## 4 Thu         288
## 5 Fri         576
## 6 Sat         288
```

A breakdown of missing values by date is as follows:


```r
act_data %>% 
        filter(is.na(steps)) %>%
        group_by(date, day) %>%
        summarize(number_na=sum(is.na(steps)))
```

```
## # A tibble: 8 x 3
## # Groups:   date [8]
##   date       day   number_na
##   <date>     <ord>     <int>
## 1 2012-10-01 Mon         288
## 2 2012-10-08 Mon         288
## 3 2012-11-01 Thu         288
## 4 2012-11-04 Sun         288
## 5 2012-11-09 Fri         288
## 6 2012-11-10 Sat         288
## 7 2012-11-14 Wed         288
## 8 2012-11-30 Fri         288
```

Missing values are associated with entire days. There are eight days with no observations. There are no missing values associated with Tuesdays whereas 1 or 2 of each of the other days of the week have missing data.

## What is mean total number of steps taken per day?

To answer this question, I created a data set (act_data_by_date) that aggregates the total number of steps on each observed date in the survey.


```r
act_data_by_date <- act_data %>% group_by(date) %>%
        summarize(steps=sum(steps, na.rm=FALSE))

head(act_data_by_date)
```

```
## # A tibble: 6 x 2
##   date       steps
##   <date>     <int>
## 1 2012-10-01    NA
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

**Note that act_date_by_date has 8 entries that are NA because there are 8 days with no observations. If na.rm had been set to TRUE, zeros would have been applied to these dates, which is misleading because in reality there were no observations made on those dates.**

The mean and median number of steps per day is as follows:

**Note that this calculation excludes days where there are no observations.**


```r
mean(act_data_by_date$steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(act_data_by_date$steps, na.rm=TRUE)
```

```
## [1] 10765
```
Finally I created a histogram to show the distribution of steps per day over the study period:


```r
library(ggplot2)
```

```r
ggplot(data=act_data_by_date, aes(act_data_by_date$steps)) + geom_histogram(binwidth=500) +
        labs(x= "No. of Steps per Day", y= "Count", title = "Histogram of Steps per Day") +
        theme(plot.title = element_text(hjust=0.5))
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

<img src="PA1_template_files/figure-html/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

**Note that 8 observations were excluded from the histogram as they are NA.**

## What is the average daily activity pattern?

To examine the average daily activity pattern, I averaged the number of steps in each 5-minute interval across the study period with the results stored in act_data_by_interval. The variable avg_spi is average steps per interval. **Again, this calculation excludes missing data.**


```r
act_data_by_interval <- act_data %>% 
                        group_by(interval) %>%
                        summarize(avg_spi=mean(steps, na.rm=TRUE))

summary(act_data_by_interval)
```

```
##     interval         avg_spi       
##  Min.   :   0.0   Min.   :  0.000  
##  1st Qu.: 588.8   1st Qu.:  2.486  
##  Median :1177.5   Median : 34.113  
##  Mean   :1177.5   Mean   : 37.383  
##  3rd Qu.:1766.2   3rd Qu.: 52.835  
##  Max.   :2355.0   Max.   :206.170
```

The daily activity pattern is plotted as follows:


```r
plot(act_data_by_interval$interval, act_data_by_interval$avg_spi, type="l", main="Average No. of Steps per Interval\n Over Study Period", xlab="5-Minute Interval", ylab="Avg. No. of Steps", col="blue", lwd=2)
```

<img src="PA1_template_files/figure-html/unnamed-chunk-14-1.png" style="display: block; margin: auto;" />

The interval with the highest number of average steps is calculated as follows:


```r
filter(act_data_by_interval, avg_spi==max(avg_spi))
```

```
## # A tibble: 1 x 2
##   interval avg_spi
##      <int>   <dbl>
## 1      835    206.
```
So the most active time of day is around 8:30AM.


## Imputing missing values

The next step of the analysis is to impute missing data associated with the number of steps. My approach was, for any given interval where the number of steps is NA, to impute the average number of steps for the same interval and day of week. For example, if interval 55 on a Sunday was missing, I imputed the average number of steps for all interval 55 observations on Sundays.

The total number of missing observations (which was also calculated previously in this report) is as follows:


```r
sum(is.na(act_data$steps))
```

```
## [1] 2304
```

I created a new variable in the act_data dataset named imputed_steps. For observations where steps was missing, imputed_steps contains the average steps for the given interval and day of week. If steps has an observation then imputed_steps uses the actual observation.

First, I grouped act_data by both day of week and interval and calculagted the average number of steps for each combination of day and interval (excluding NAs):


```r
act_data_by_day_interval <- act_data %>% 
                                group_by(day, interval) %>%
                                summarize(avg_spi=round(mean(steps, na.rm=TRUE), digits=2))
```

Then, I joined this data (avg_spi) to the original act_data dataset and added a new variable named imputed_steps:


```r
act_data <- left_join(act_data, act_data_by_day_interval)
```

```
## Joining, by = c("interval", "day")
```

```r
act_data$imputed_steps <- ifelse(is.na(act_data$steps),
                                 act_data$avg_spi,
                                 act_data$steps)

head(act_data)
```

```
##   steps       date interval day avg_spi imputed_steps
## 1    NA 2012-10-01        0 Mon    1.43          1.43
## 2    NA 2012-10-01        5 Mon    0.00          0.00
## 3    NA 2012-10-01       10 Mon    0.00          0.00
## 4    NA 2012-10-01       15 Mon    0.00          0.00
## 5    NA 2012-10-01       20 Mon    0.00          0.00
## 6    NA 2012-10-01       25 Mon    5.00          5.00
```

Next, I updated the act_data_by_date dataset to incorporate both the original total steps per date and the new imputed_steps variable:


```r
act_data_by_date <- act_data %>% group_by(date) %>%
        summarize(steps=sum(steps, na.rm=FALSE),
                  imputed_steps=sum(imputed_steps))

head(act_data_by_date)
```

```
## # A tibble: 6 x 3
##   date       steps imputed_steps
##   <date>     <int>         <dbl>
## 1 2012-10-01    NA         9975.
## 2 2012-10-02   126          126 
## 3 2012-10-03 11352        11352 
## 4 2012-10-04 12116        12116 
## 5 2012-10-05 13294        13294 
## 6 2012-10-06 15420        15420
```

The mean and median steps per day using the imputed_steps variable is as follows:


```r
mean(act_data_by_date$imputed_steps, na.rm=TRUE)
```

```
## [1] 10821.21
```

```r
median(act_data_by_date$imputed_steps, na.rm=TRUE)
```

```
## [1] 11015
```

The mean and median steps per day are slightly higher after missing data is imputed than in the original data set where missing data was excluded.

Finally, the histogram of steps per day using the imputed_steps variable is as follows:


```r
ggplot(data=act_data_by_date, aes(act_data_by_date$imputed_steps)) + geom_histogram(binwidth=500) +
        labs(x= "No. of Steps per Day", 
             y= "Count", 
             title = "Histogram of Steps per Day\n (Missing Data Imputed)") +
        theme(plot.title = element_text(hjust=0.5))
```

<img src="PA1_template_files/figure-html/unnamed-chunk-21-1.png" style="display: block; margin: auto;" />





## Are there differences in activity patterns between weekdays and weekends?
