## LOAD DATA
act_data <- read.table("activity.csv", header=TRUE, na.strings="NA", sep=",", quote="\"")

## CHANGE DATE TO DATE AND ADD DAY OF WEEK VARIABLE
library(lubridate)
act_data$date <- as_date(act_data$date)
act_data$day <- wday(act_data$date, label=TRUE)

## MISSING VALUES BY DAY OF WEEK
act_data %>% 
    filter(is.na(steps)) %>%
    group_by(day) %>%
    summarize(no_na=sum(is.na(steps)))

## MEAN NO. OF STEPS PER DAY
round(sum(act_data$steps, na.rm=TRUE)/as.numeric(max(act_data$date)-min(act_data$date)+1), digits=0)

## GROUP BY INTERVAL
act_data_by_interval <- act_data %>% group_by(interval) %>%
    summarize(avg_steps=mean(steps, na.rm=TRUE))

## PLOT DAILY ACTIVITY PATTERN
plot(act_data_by_interval$interval, act_data_by_interval$avg_steps, type="l", main="No. of Steps by Interval\n Over Study Period", xlab="5-Minute Interval", ylab="Avg. No. of Steps", col="blue", lwd=2)

## INTERVAL WITH HIGHEST AVERAGE NO. OF STEPS
filter(act_data_by_interval, avg_steps==max(avg_steps))


## METHOD TO IMPUTE MISSING STEPS DATA







act_data_by_interval_day <- act_data %>% group_by(day, interval) %>%
    summarize(avg_steps=mean(steps, na.rm=TRUE))
