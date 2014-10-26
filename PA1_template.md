# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
#setwd('..\\Desktop\\Work\\GitLocal\\RepData_PeerAssessment1')
ActivityMonitoringDF <- read.csv('activity.csv') ## This is the entire data set
```

## What is mean total number of steps taken per day?

```r
## Sum up the steps for all days
steps_per_day <- aggregate(ActivityMonitoringDF$steps, 
                           by=list(ActivityMonitoringDF$date), FUN=sum)
names(steps_per_day) <- c('date','steps')
mean_steps <- mean(steps_per_day$steps, na.rm=TRUE)
paste(c("The mean total steps per day is ", round(mean_steps,0)), collapse='')
```

```
## [1] "The mean total steps per day is 10766"
```

```r
median_steps <- median(steps_per_day$steps, na.rm=TRUE)
paste(c("The median total steps per day is ", round(median_steps,0)), collapse='')
```

```
## [1] "The median total steps per day is 10765"
```

## What is the average daily activity pattern?

```r
act_pattern_avg <- aggregate(ActivityMonitoringDF$steps,
                  by=list(ActivityMonitoringDF$interval),FUN=mean,na.rm=TRUE)
names(act_pattern_avg) <- c('interval', 'steps_per_interval')
#act_pattern is recorded in 5-minute intervals. 
nrows_to_sum <- 1 # For the course assignment will display all values
act_pattern <- colSums(matrix(act_pattern_avg$steps_per_interval, nrows_to_sum))
time_of_day <- format(round(act_pattern_avg$interval[seq(1,
        length(act_pattern_avg$interval), nrows_to_sum)]/100,digits=2),nsmall=2)
timelist <- strsplit(time_of_day, '[.]')

## Function to convert time to decimal hours (e.g. 2:15 gets set to 2.25)
convert_to_hours <- function(hours_minutes) {
  hm_numeric <- as.numeric(hours_minutes)
  fhours <- hm_numeric[1] + hm_numeric[2]/60.0
  return(fhours)
}

act_hours <- unlist(lapply(timelist,convert_to_hours))
plot(act_hours,act_pattern,main='Daily Activity Profile',xlab='Time of Day (hours)',
ylab='Average Steps Per 5-Minute Interval',type='l', lwd=2, col='blue')
```

![plot of chunk activity](./PA1_template_files/figure-html/activity.png) 

```r
max_val <- max(act_pattern_day)
max_idx <- which.max(act_pattern_day)
paste(c("The max activity averaged across all days is ",  round(max_val,1), 
        " and occurs at", timelist[[max_idx]][1], ':',
        timelist[[max_idx]][2]), collapse='')
```

```
## [1] "The max activity averaged across all days is 230.4 and occurs at 8:35"
```

## Imputing missing values

```r
## Determine number of missing values
total_na <- sum(is.na(ActivityMonitoringDF))
## Determine number of dates with missing values
dates_with_na_values <- levels(as.factor(as.character(
  ActivityMonitoringDF$date[is.na(ActivityMonitoringDF$steps)])))
total_dates_with_na_values <- length(dates_with_na_values)
paste(c("There are ", nrow(ActivityMonitoringDF), " total values with ",
        length(levels(ActivityMonitoringDF$date)), " total dates."), collapse='')
```

```
## [1] "There are 17568 total values with 61 total dates."
```

```r
paste(c("There are ", total_na, " total NA values corresponding to ", 
        total_dates_with_na_values, " separate dates."), collapse='')
```

```
## [1] "There are 2304 total NA values corresponding to 8 separate dates."
```

```r
## Do the simplest method, i.e. insert the average steps for each missing interval
for( i in 1:nrow(ActivityMonitoringDF)) {
  if(is.na(ActivityMonitoringDF$steps[i])) {
    suppressWarnings(ActivityMonitoringDF$steps[i] <- 
act_pattern_avg$steps[act_pattern_avg$interval==ActivityMonitoringDF$interval[i]])
  }
}

#Verify that the missing values have been imputed
paste(c("After imputing there are ", 
        sum(is.na(ActivityMonitoringDF$steps)), " missing values"), collapse = '')  
```

```
## [1] "After imputing there are 0 missing values"
```

## Are there differences in activity patterns between weekdays and weekends?

```r
day_of_wk <- strftime(ActivityMonitoringDF$date, format='%A')
#Create factor variable
day <- as.factor(day_of_wk=='Saturday' | day_of_wk=='Sunday')
levels(day) <- c('weekday', 'weekend')

ActMonitorWeekend <- ActivityMonitoringDF[(day=='weekend'),]
ActMonitorWeekday <- ActivityMonitoringDF[(day=='weekday'),]
act_pattern_end_avg<-aggregate(ActMonitorWeekend$steps,by=list(ActMonitorWeekend$interval),FUN=mean)
act_pattern_day_avg<-aggregate(ActMonitorWeekday$steps,by=list(ActMonitorWeekday$interval),FUN=mean)
names(act_pattern_day_avg) <- c('interval', 'steps_per_interval')
names(act_pattern_end_avg) <- c('interval', 'steps_per_interval')
#nrows_to_sum <- 3
nrows_to_sum <- 1
act_pattern_day <- colSums(matrix(act_pattern_day_avg$steps_per_interval, nrows_to_sum))
act_pattern_end <- colSums(matrix(act_pattern_end_avg$steps_per_interval, nrows_to_sum))
par(mfrow=c(2,1))
par(mar=c(0,5,0,5))
plot(act_hours, act_pattern_day, type='l', xlab = '', xaxt= 'n', 
     ylab = 'Weekday Activity', ylim = c(0,240), lwd=2, col='blue')
#, axis(3, at=seq(0,200,50)))
plot(act_hours, act_pattern_end, xlab='Time of Day (hours)', type='l', 
     ylab = 'Weekend Activity', ylim = c(0,240), lwd=2, col='red')
```

![plot of chunk days_ends](./PA1_template_files/figure-html/days_ends.png) 

```r
steps_per_day <- aggregate(ActivityMonitoringDF$steps, 
                            by=list(ActivityMonitoringDF$date), FUN=sum)
steps_per_weekday <- aggregate(ActMonitorWeekday$steps, 
                               by=list(ActMonitorWeekday$date), FUN=sum)
steps_per_weekend <- aggregate(ActMonitorWeekend$steps, 
                               by=list(ActMonitorWeekend$date), FUN=sum)
names(steps_per_day)     <- c('date','steps')
names(steps_per_weekday) <- c('date','steps')
names(steps_per_weekend) <- c('date','steps')
mean_steps_day     <- mean(steps_per_day$steps)
mean_steps_weekday <- mean(steps_per_weekday$steps)
mean_steps_weekend <- mean(steps_per_weekend$steps)
median_steps_day     <- median(steps_per_day$steps)
median_steps_weekday <- median(steps_per_weekday$steps)
median_steps_weekend <- median(steps_per_weekend$steps)
max_weekday_idx <- which.max(act_pattern_day)
max_weekend_idx <- which.max(act_pattern_end)
max_weekday_val <- max(act_pattern_day)
max_weekend_val <- max(act_pattern_end)

paste(c("The mean total steps per DAY (before/after) imputing is ", 
        round(mean_steps,0), '/', round(mean_steps_day,0)), collapse='')
```

```
## [1] "The mean total steps per DAY (before/after) imputing is 10766/10766"
```

```r
paste(c("The median total steps per DAY (before/after) imputing is ", 
        round(median_steps,0), '/', round(median_steps_day,0)), collapse='')
```

```
## [1] "The median total steps per DAY (before/after) imputing is 10765/10766"
```

```r
paste(c("The mean total steps per WEEKDAY is ",round(mean_steps_weekday,0)), collapse='')
```

```
## [1] "The mean total steps per WEEKDAY is 10256"
```

```r
paste(c("The median total steps per WEEKDAY is ",round(median_steps_weekday,0)), collapse='')
```

```
## [1] "The median total steps per WEEKDAY is 10765"
```

```r
paste(c("The max activity on a WEEKDAY is ",  round(max_weekday_val,1), 
        " steps per 5-minute interval and occurs at", timelist[[max_weekday_idx]][1], ':',
        timelist[[max_weekday_idx]][2]), collapse='')
```

```
## [1] "The max activity on a WEEKDAY is 230.4 steps per 5-minute interval and occurs at 8:35"
```

```r
paste(c("The mean total steps per WEEKEND is ", round(mean_steps_weekend,0)),collapse='')
```

```
## [1] "The mean total steps per WEEKEND is 12202"
```

```r
paste(c("The median total steps per WEEKEND is ",round(median_steps_weekend,0)),collapse='')
```

```
## [1] "The median total steps per WEEKEND is 11646"
```

```r
paste(c("The max activity on a WEEKEND is ",  round(max_weekend_val,1), 
        " steps per 5-minute interval and occurs at", timelist[[max_weekend_idx]][1], ':',
        timelist[[max_weekend_idx]][2]), collapse='')
```

```
## [1] "The max activity on a WEEKEND is 166.6 steps per 5-minute interval and occurs at 9:15"
```

```r
# Generate doc with knit2html("PA1_template.Rmd")
```
