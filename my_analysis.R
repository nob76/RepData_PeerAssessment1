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

## DAY TYPE ANALYSIS
    
    # CREATE NEW FACTOR VARIABLE
    act_data$day_type <- factor(ifelse(act_data$day %in% c('Sat', 'Sun'), "Weekend", "Weekday"),
                                levels=c("Weekday", "Weekend")
                                )

    # GROUP BY DAY TYPE AND INTERVAL
    act_data_by_day_type_interval <- act_data %>% 
                                     group_by(day_type, interval) %>%
                                     summarize(avg_impspi=mean(imputed_steps, na.rm=TRUE))
    
    # MAKE PANEL PLOT
    library(lattice)
    xyplot(avg_impspi~interval | day_type, data=act_data_by_day_type_interval, type="l", layout=c(1,2), xlab="Interval", ylab="Avg. No. of Steps")
    
    # ANALYZE DIFFERENCE IN STEPS FOR EACH INTERVAL
    weekday_pattern <- act_data_by_day_type_interval[act_data_by_day_type_interval$day_type=="Weekday", c("interval","avg_impspi")]
    
    weekend_pattern <- act_data_by_day_type_interval[act_data_by_day_type_interval$day_type=="Weekend", c("interval","avg_impspi")]
    
    delta <- weekend_pattern$avg_impspi - weekday_pattern$avg_impspi
    
    
    barplot(delta, names.arg= unique(weekend_pattern$interval), xlab="Interval", ylab="Weekend Steps - Weekday Steps", main="Weekend Pattern vs. Weekday Pattern", col="blue", border="black", ylim=c(-200, 200), axes=TRUE)
    
    ggplot(aes(x=unique(weekend_pattern$interval), y=delta)) + geom_bar(data=delta)
    
    qplot(x=unique(weekend_pattern$interval), 
          y=delta, geom="col", 
          xlab="Interval", 
          ylab="Weekend Steps - Weekday Steps", 
          main="Weekend Pattern vs. Weekday Pattern", 
          ylim=c(-200, 200)
    )
    
## CLUSTERING
    
    # CREATE NEW FACTOR VARIABLES
    act_data$tod <- factor(ifelse(act_data$interval <= 500, "Early Morning", 
                                  ifelse(between(act_data$interval, 505, 900), "Morning Rush",
                                  ifelse(between(act_data$interval, 905, 1700), "Work Hours",
                                  ifelse(between(act_data$interval, 1705,2000), "Early Evening",
                                  "Evening")))),
                                  levels=c("Early Morning", "Morning Rush", "Work Hours", "Early                                                 Evening", "Evening")
                            )
    
    act_data$dtype_tod <- paste(act_data$day_type, act_data$tod, sep=" ")
    
    
    # CREATE DENDROGRAM
    act_data %>%
        select(interval, imputed_steps) %>%
        dist() %>%
        hclust() %>%
        as.dendrogram() -> dg
    
    #MODIFY DENDROGRAM
    library(dendextend)
    
    dg %>% 
        set("labels_cex", 0.25) %>%
        plot()
    
    mycols_tod <- ifelse(act_data$tod == "Early Morning", "red",
                         ifelse(act_data$tod=="Morning Rush", "blue",
                                ifelse(act_data$tod=="Work Hours", "cyan",
                                       ifelse(act_data$tod=="Early Evening", "green",
                                              "brown"))))
    
    mycols_dtypetod <- ifelse(act_data$dtype_tod == "Weekday Early Morning", "pink",
                         ifelse(act_data$dtype_tod=="Weekday Morning Rush", "lightblue",
                                ifelse(act_data$dtype_tod=="Weekday Work Hours", "lightyellow",
                                       ifelse(act_data$dtype_tod=="Weekday Early Evening", "lightgreen",
                                              ifelse(act_data$dtype_tod=="Weekday Evening", "tan3",
                        ifelse(act_data$dtype_tod == "Weekend Early Morning", "darkred",
                               ifelse(act_data$dtype_tod=="Weekend Morning Rush", "darkblue",
                                      ifelse(act_data$dtype_tod=="Weekend Work Hours", "goldenrod",
                                             ifelse(act_data$dtype_tod=="Weekend Early Evening",                                                            "darkgreen",
                                                        "brown")))))))))
    
    the_bars <- cbind(mycols_tod, mycols_dtypetod)
    colored_bars(colors = the_bars, dend = dg)
    
    #KMEANS
    
    act_data[,c("interval","imputed_steps")] %>% 
        kmeans(centers=5, nstart=20) -> act_data_klust5
    
    act_data$klust5 <- as.factor(act_data_klust5$cluster)
    
    table(act_data_klust5$cluster, act_data$tod)
    
    ggplot(data=act_data, aes(interval, imputed_steps, color=act_data$klust5)) + geom_point()
    
    










