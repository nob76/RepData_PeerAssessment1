## LOAD AND PREPROCESS DATA

    #LOAD DATA
    act_data <- read.table("activity.csv", header=TRUE, na.strings="NA", sep=",", quote="\"")

    # CHANGE DATE TO DATE AND ADD DAY OF WEEK VARIABLE
    library(lubridate)
    act_data$date <- as_date(act_data$date)
    act_data$day <- wday(act_data$date, label=TRUE)

    # MISSING VALUES BY DAY OF WEEK
    library(dplyr)
    act_data %>% 
        filter(is.na(steps)) %>%
        group_by(day) %>%
        summarize(number_na=sum(is.na(steps)))
    
    # MISSING VALUES BY DATE
    act_data %>% 
        filter(is.na(steps)) %>%
        group_by(date, day) %>%
        summarize(number_na=sum(is.na(steps)))

## ANALYSIS OF STEPS PER DAY

    # TOTAL STEPS TAKEN ON EACH DATE INCLUDING MISSING DATA
    act_data_by_date <- act_data %>% group_by(date) %>%
        summarize(steps=sum(steps, na.rm=FALSE))

    # HISTOGRAM OF STEPS TAKEN PER DAY EXCLUDING MISSING DATA
    library(ggplot2)
    ggplot(data=act_data_by_date, aes(act_data_by_date$steps)) + geom_histogram(binwidth=500) +
            labs(x= "No. of Steps per Day", y= "Count", title = "Histogram of Steps per Day") +
            theme(plot.title = element_text(hjust=0.5))

    # MEAN AND MEDIAN NUMBER OF STEPS PER DAY EXCLUDING MISSING DATA
    mean(act_data_by_date$steps, na.rm=TRUE)
    median(act_data_by_date$steps, na.rm=TRUE)

    # MEAN AND MEDIAN STEPS BY DAY OF WEEK INCLUDING AND EXCLUDING MISSING DATA
    act_data_by_day <- act_data %>% group_by(day) %>%
        summarize(total_steps=sum(steps, na.rm=TRUE),
                  ndays=n_distinct(date,day),
                  avg_spd_incl_na_days=sum(steps, na.rm=TRUE)/n_distinct(date,day),
                  avg_spi=mean(steps, na.rm=TRUE),
                  avg_spd_excl_na_days=mean(steps, na.rm=TRUE)*288,
                  med_spi=median(steps, na.rm=TRUE))

## ANALYSIS OF DAILY ACTIVITY PATTERN
    
    # GROUP BY INTERVAL
    act_data_by_interval <- act_data %>% 
                            group_by(interval) %>%
                            summarize(avg_spi=mean(steps, na.rm=TRUE))

    # PLOT DAILY ACTIVITY PATTERN
    plot(act_data_by_interval$interval, act_data_by_interval$avg_spi, type="l", 
         main="No. of Steps by Interval\n Over Study Period", xlab="5-Minute Interval", 
         ylab="Avg. No. of Steps", col="blue", lwd=2)

    # INTERVAL WITH HIGHEST AVERAGE NO. OF STEPS
    filter(act_data_by_interval, avg_steps==max(avg_steps))


## IMPUTING MISSING VALUES
    
    # NUMBER OF OBSERVATIONS WITH MISSING DATA
    sum(is.na(act_data$steps))

    # AVERAGE NO. OF STEPS BY DAY AND INTERVAL
    act_data_by_day_interval <- act_data %>% 
                                group_by(day, interval) %>%
                                summarize(avg_spi=round(mean(steps, na.rm=TRUE), digits=2))

    #IMPUTE NO. OF STEPS BASED ON AVERAGE FOR SIMILAR DAY AND INTERVAL

    na_rows <- which(is.na(act_data$steps)) #vector of rows with na's
    
    act_data <- left_join(act_data, act_data_by_day_interval)
    
    act_data$imputed_steps <- ifelse(is.na(act_data$steps),
                                     act_data$avg_spi,
                                     act_data$steps)

    # TOTAL STEPS TAKEN ON EACH DATE WITH IMPUTED DATA
    act_data_by_date <- act_data %>% group_by(date) %>%
        summarize(steps=sum(steps, na.rm=FALSE),
                  imputed_steps=sum(imputed_steps))
    
    # HISTOGRAM OF STEPS TAKEN PER DAY WITH IMPUTED DATA
    ggplot(data=act_data_by_date, aes(act_data_by_date$imputed_steps)) + geom_histogram(binwidth=500) +
        labs(x= "No. of Steps per Day", 
             y= "Count", 
             title = "Histogram of Steps per Day\n (Missing Data Imputed)") +
        theme(plot.title = element_text(hjust=0.5))
    
    
    # MEAN AND MEDIAN NUMBER OF STEPS PER DAY WITH IMPUTED MISSING DATA
    mean(act_data_by_date$imputed_steps, na.rm=TRUE)
    median(act_data_by_date$imputed_steps, na.rm=TRUE)
    
    # MEAN AND MEDIAN STEPS BY DAY OF WEEK INCLUDING AND EXCLUDING MISSING DATA
    act_data_by_day <- act_data %>% group_by(day) %>%
        summarize(total_steps=sum(steps, na.rm=TRUE),
                  ndays=n_distinct(date,day),
                  avg_spd_incl_na_days=sum(steps, na.rm=TRUE)/n_distinct(date,day),
                  avg_spi=mean(steps, na.rm=TRUE),
                  avg_spd_excl_na_days=mean(steps, na.rm=TRUE)*288,
                  med_spi=median(steps, na.rm=TRUE),
                  avg_impspd=mean(imputed_steps, na.rm=TRUE)*288
                  )












