# Reproducible Research: Peer Assessment 1

## 1> Loading and preprocessing the data

  sample data is available at https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip  
  here is code used to read activity.zip file


```r
  fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileUrl, "activity.zip")  
  activity_df <- read.table(unzip("activity.zip"), sep = ",", na.strings = "NA", header = TRUE)
```

  data of activity.zip file is downloaded in a data frame "activity_df" using read.table function.. And displayed 


```r
  activity_df$date <- as.Date(activity_df$date)
  str(activity_df)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## 2> What is mean total number of steps taken per day?
      2.1 > Calculate the total number of steps taken per day
      2.2 > Make a histogram of the total number of steps taken each day
      2.3 > Calculate and report the mean and median of the total number of steps taken per day


```r
  library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
  activity_dt <- tbl_df(activity_df)
  activity_dt <- activity_dt %>% group_by(date) %>% filter(!is.na(steps))

## calculate sum of steps per day
  total_steps_by_day_dt <-  activity_dt %>% summarise(total_steps = sum(steps))

## make histogram
  hist(total_steps_by_day_dt$total_steps, breaks = 100)
```

![](PA1_template_files/figure-html/activity_per_day-1.png)<!-- -->

```r
## calculate mean and median on totalsteps across all days
  mean_totalsteps <- round(mean(total_steps_by_day_dt$total_steps), 2)
  median_totalsteps <- round(median(total_steps_by_day_dt$total_steps), 2)
```


  Mean and Median of total number of steps taken per day is 10766.19  and  10765 respectively.  
    

## 3> What is the average daily activity pattern?
      3.1 > Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps all day (y-axis)
      3.2 > Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?



```r
## Group by data on interval and plot timeseries
  activity_intvl_dt <- activity_dt %>% group_by(interval) %>% filter(!is.na(steps))
  mean_by_intvl_dt <-  activity_intvl_dt %>% summarise(total_steps = mean(steps))
  plot(mean_by_intvl_dt, type = "l")
```

![](PA1_template_files/figure-html/time_series_plot-1.png)<!-- -->

```r
  max_5min_intvl <- mean_by_intvl_dt[(which.max(mean_by_intvl_dt$total_steps)),][[1]]
```

        5-minute interval that has maximum steps is 835.   
  
  
## 4> Imputing missing values
    Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce
    bias into some calculations or summaries of the data.
    
    4.1 > Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
    4.2 > Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
    4.3 > Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
  activity_new_df <- activity_df
  missing_steps <- sum(is.na(activity_new_df$steps))
  percent_missing <- round((missing_steps/nrow(activity_new_df) * 100) , 2)
  index <- match(activity_new_df[is.na(activity_new_df$steps), ]$interval, mean_by_intvl_dt$interval )
  activity_new_df[is.na(activity_new_df$steps), ]$steps  <- mean_by_intvl_dt[index, ]$total_steps
```

    Missing values in the dataset is 2304 which is 13.11% of total observations.
    

   4.4 > Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
   

```r
  activity_new_df <- activity_new_df %>% group_by(date) %>% filter(!is.na(steps))

## calculate sum of steps per day
  total_steps_by_day_new_df <-  activity_new_df %>% summarise(total_steps = sum(steps))

## make histogram
  hist(total_steps_by_day_new_df$total_steps, breaks = 100)
```

![](PA1_template_files/figure-html/mean_on_new_dataframe-1.png)<!-- -->

```r
## calculate mean and median on totalsteps across all days
  mean_totalsteps_new <- round(mean(total_steps_by_day_new_df$total_steps), 2)
  median_totalsteps_new <- round(median(total_steps_by_day_new_df$total_steps), 2)
```

    Mean and Median of total number of steps taken per day is 10766.19  and  10766.19 respectively.  


## 5> Are there differences in activity patterns between weekdays and weekends?

    5.1 > For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day. 
    5.2 > Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
activity_new_df$date <- as.Date(activity_new_df$date)
wk_flg <- weekdays(activity_new_df$date) %in% c("Saturday", "Sunday")
activity_new_df$daytype <- as.factor(ifelse(wk_flg == TRUE, "Weekend", "Weekdays"))

activity_new_wkend_df <- activity_new_df %>% filter(daytype == "Weekend" ) %>% group_by(interval)
activity_new_wkdays_df <- activity_new_df %>% filter(daytype == "Weekdays" ) %>% group_by(interval)

mean_wkend_df <- activity_new_wkend_df %>% summarize(stepsN = mean(steps))
mean_wkend_df$daytype <- "Weekend"
mean_wkdays_df <- activity_new_wkdays_df %>% summarize(stepsN = mean(steps))
mean_wkdays_df$daytype <- "Weekdays"

mean_plot_df <- rbind(mean_wkend_df, mean_wkdays_df)


library(lattice)
xyplot(stepsN~interval|daytype, type="l", data=mean_plot_df, layout=c(1,2), aspect = "fill", xlab = "Interval", ylab ="Number of Steps")
```

![](PA1_template_files/figure-html/weekday_vs_weekend-1.png)<!-- -->


Assignment ready for review.  Appreciate your time, Thanks!
