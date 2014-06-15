
Reproducible Research Assignment 1
==================================

1. Loading library and data set and preprocessing the data

```r
library(ggplot2)
library(scales)
library(lattice)
data <- read.csv("activity.csv")
names(data)
```

```
## [1] "steps"    "date"     "interval"
```

```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Keep the original data as "data", then make a copy, as "data1"

```r
data1 <- data
```

Calculte the total number of steps taken per day.
For this part of the work, ignore the missing values in the dataset.

```r
sumofsteps <- aggregate(data$steps, by=list(data$date), FUN=sum)
```

Make a histogram of the total number of steps taken each day

```r
sumofsteps$days <- as.Date(sumofsteps$Group.1, format="%Y-%m-%d")
sumofsteps$Sum_of_steps_perday <- sumofsteps$x
ggplot(sumofsteps, aes(x=days, y=Sum_of_steps_perday)) + geom_bar(stat="identity")
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


Calculate and report the mean and median total number of steps taken per day

```r
meanofsteps <- aggregate(data$steps, by=list(data$date), FUN=mean)
meanofsteps
```

```
##       Group.1       x
## 1  2012-10-01      NA
## 2  2012-10-02  0.4375
## 3  2012-10-03 39.4167
## 4  2012-10-04 42.0694
## 5  2012-10-05 46.1597
## 6  2012-10-06 53.5417
## 7  2012-10-07 38.2465
## 8  2012-10-08      NA
## 9  2012-10-09 44.4826
## 10 2012-10-10 34.3750
## 11 2012-10-11 35.7778
## 12 2012-10-12 60.3542
## 13 2012-10-13 43.1458
## 14 2012-10-14 52.4236
## 15 2012-10-15 35.2049
## 16 2012-10-16 52.3750
## 17 2012-10-17 46.7083
## 18 2012-10-18 34.9167
## 19 2012-10-19 41.0729
## 20 2012-10-20 36.0938
## 21 2012-10-21 30.6285
## 22 2012-10-22 46.7361
## 23 2012-10-23 30.9653
## 24 2012-10-24 29.0104
## 25 2012-10-25  8.6528
## 26 2012-10-26 23.5347
## 27 2012-10-27 35.1354
## 28 2012-10-28 39.7847
## 29 2012-10-29 17.4236
## 30 2012-10-30 34.0938
## 31 2012-10-31 53.5208
## 32 2012-11-01      NA
## 33 2012-11-02 36.8056
## 34 2012-11-03 36.7049
## 35 2012-11-04      NA
## 36 2012-11-05 36.2465
## 37 2012-11-06 28.9375
## 38 2012-11-07 44.7326
## 39 2012-11-08 11.1771
## 40 2012-11-09      NA
## 41 2012-11-10      NA
## 42 2012-11-11 43.7778
## 43 2012-11-12 37.3785
## 44 2012-11-13 25.4722
## 45 2012-11-14      NA
## 46 2012-11-15  0.1424
## 47 2012-11-16 18.8924
## 48 2012-11-17 49.7882
## 49 2012-11-18 52.4653
## 50 2012-11-19 30.6979
## 51 2012-11-20 15.5278
## 52 2012-11-21 44.3993
## 53 2012-11-22 70.9271
## 54 2012-11-23 73.5903
## 55 2012-11-24 50.2708
## 56 2012-11-25 41.0903
## 57 2012-11-26 38.7569
## 58 2012-11-27 47.3819
## 59 2012-11-28 35.3576
## 60 2012-11-29 24.4688
## 61 2012-11-30      NA
```

```r
medianofsteps <- aggregate(data$steps, by=list(data$date), FUN=median)
medianofsteps
```

```
##       Group.1  x
## 1  2012-10-01 NA
## 2  2012-10-02  0
## 3  2012-10-03  0
## 4  2012-10-04  0
## 5  2012-10-05  0
## 6  2012-10-06  0
## 7  2012-10-07  0
## 8  2012-10-08 NA
## 9  2012-10-09  0
## 10 2012-10-10  0
## 11 2012-10-11  0
## 12 2012-10-12  0
## 13 2012-10-13  0
## 14 2012-10-14  0
## 15 2012-10-15  0
## 16 2012-10-16  0
## 17 2012-10-17  0
## 18 2012-10-18  0
## 19 2012-10-19  0
## 20 2012-10-20  0
## 21 2012-10-21  0
## 22 2012-10-22  0
## 23 2012-10-23  0
## 24 2012-10-24  0
## 25 2012-10-25  0
## 26 2012-10-26  0
## 27 2012-10-27  0
## 28 2012-10-28  0
## 29 2012-10-29  0
## 30 2012-10-30  0
## 31 2012-10-31  0
## 32 2012-11-01 NA
## 33 2012-11-02  0
## 34 2012-11-03  0
## 35 2012-11-04 NA
## 36 2012-11-05  0
## 37 2012-11-06  0
## 38 2012-11-07  0
## 39 2012-11-08  0
## 40 2012-11-09 NA
## 41 2012-11-10 NA
## 42 2012-11-11  0
## 43 2012-11-12  0
## 44 2012-11-13  0
## 45 2012-11-14 NA
## 46 2012-11-15  0
## 47 2012-11-16  0
## 48 2012-11-17  0
## 49 2012-11-18  0
## 50 2012-11-19  0
## 51 2012-11-20  0
## 52 2012-11-21  0
## 53 2012-11-22  0
## 54 2012-11-23  0
## 55 2012-11-24  0
## 56 2012-11-25  0
## 57 2012-11-26  0
## 58 2012-11-27  0
## 59 2012-11-28  0
## 60 2012-11-29  0
## 61 2012-11-30 NA
```


What is the average daily activity pattern?

Make a time series plot (type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
data$date <- as.Date(data$date)
data$time <- paste(data$interval%/%100,data$interval%%100, "00", sep=":")
data$DateTime <- paste(data$date, data$time)
data$DateTime <- as.POSIXlt(data$DateTime)
plot(data$DateTime, data$steps, type = "l", xlab = "Days", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 


Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
The result shows that row 16492 contains the maxinum number of steps (806).

```r
for(i in 1:nrow(data)){
    if(is.na(data$steps[i])){
        data$steps[i] = 0
    }
}
for(j in 1:nrow(data)){
    if(data$steps[j] == max(data$steps)){
        print(data[j,])
    }
}
```

```
##       steps       date interval    time            DateTime
## 16492   806 2012-11-27      615 6:15:00 2012-11-27 06:15:00
```


Imputing missing values
Check for the missing value in the columns, "steps", "date" and "iterval"

```r
nrow(data)
```

```
## [1] 17568
```

```r
length(data$steps[!is.na(data$steps)])
```

```
## [1] 17568
```

```r
length(data$date[!is.na(data$date)])
```

```
## [1] 17568
```

```r
length(data$interval[!is.na(data$interval)])
```

```
## [1] 17568
```

It is clear that only the column "steps" has missing value.
Calculate the total number of rows with NAs

```r
length(data$date[!is.na(data$date)]) - length(data$steps[!is.na(data$steps)])
```

```
## [1] 0
```

Because the median of everyday steps is 0, the strategy is filling in all the NA with 0.
Create a new dataset ("data1") that is equal to the original dataset but with the missing data filled in.


```r
head(data1)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
for(i in 1:nrow(data1)){
    if(is.na(data1$steps[i])){
        data1$steps[i] = 0
    }
}
head(data1)
```

```
##   steps       date interval
## 1     0 2012-10-01        0
## 2     0 2012-10-01        5
## 3     0 2012-10-01       10
## 4     0 2012-10-01       15
## 5     0 2012-10-01       20
## 6     0 2012-10-01       25
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```r
sumofsteps1 <- aggregate(data1$steps, by=list(data1$date), FUN=sum)
sumofsteps1$days <- as.Date(sumofsteps1$Group.1, format="%Y-%m-%d")
sumofsteps1$Sum_of_steps_perday <- sumofsteps1$x
ggplot(sumofsteps1, aes(x=days, y=Sum_of_steps_perday)) + geom_bar(stat="identity")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

```r
meanofsteps1 <- aggregate(data1$steps, by=list(data1$date), FUN=mean)
meanofsteps1
```

```
##       Group.1       x
## 1  2012-10-01  0.0000
## 2  2012-10-02  0.4375
## 3  2012-10-03 39.4167
## 4  2012-10-04 42.0694
## 5  2012-10-05 46.1597
## 6  2012-10-06 53.5417
## 7  2012-10-07 38.2465
## 8  2012-10-08  0.0000
## 9  2012-10-09 44.4826
## 10 2012-10-10 34.3750
## 11 2012-10-11 35.7778
## 12 2012-10-12 60.3542
## 13 2012-10-13 43.1458
## 14 2012-10-14 52.4236
## 15 2012-10-15 35.2049
## 16 2012-10-16 52.3750
## 17 2012-10-17 46.7083
## 18 2012-10-18 34.9167
## 19 2012-10-19 41.0729
## 20 2012-10-20 36.0938
## 21 2012-10-21 30.6285
## 22 2012-10-22 46.7361
## 23 2012-10-23 30.9653
## 24 2012-10-24 29.0104
## 25 2012-10-25  8.6528
## 26 2012-10-26 23.5347
## 27 2012-10-27 35.1354
## 28 2012-10-28 39.7847
## 29 2012-10-29 17.4236
## 30 2012-10-30 34.0938
## 31 2012-10-31 53.5208
## 32 2012-11-01  0.0000
## 33 2012-11-02 36.8056
## 34 2012-11-03 36.7049
## 35 2012-11-04  0.0000
## 36 2012-11-05 36.2465
## 37 2012-11-06 28.9375
## 38 2012-11-07 44.7326
## 39 2012-11-08 11.1771
## 40 2012-11-09  0.0000
## 41 2012-11-10  0.0000
## 42 2012-11-11 43.7778
## 43 2012-11-12 37.3785
## 44 2012-11-13 25.4722
## 45 2012-11-14  0.0000
## 46 2012-11-15  0.1424
## 47 2012-11-16 18.8924
## 48 2012-11-17 49.7882
## 49 2012-11-18 52.4653
## 50 2012-11-19 30.6979
## 51 2012-11-20 15.5278
## 52 2012-11-21 44.3993
## 53 2012-11-22 70.9271
## 54 2012-11-23 73.5903
## 55 2012-11-24 50.2708
## 56 2012-11-25 41.0903
## 57 2012-11-26 38.7569
## 58 2012-11-27 47.3819
## 59 2012-11-28 35.3576
## 60 2012-11-29 24.4688
## 61 2012-11-30  0.0000
```

```r
medianofsteps1 <- aggregate(data1$steps, by=list(data1$date), FUN=median)
medianofsteps1
```

```
##       Group.1 x
## 1  2012-10-01 0
## 2  2012-10-02 0
## 3  2012-10-03 0
## 4  2012-10-04 0
## 5  2012-10-05 0
## 6  2012-10-06 0
## 7  2012-10-07 0
## 8  2012-10-08 0
## 9  2012-10-09 0
## 10 2012-10-10 0
## 11 2012-10-11 0
## 12 2012-10-12 0
## 13 2012-10-13 0
## 14 2012-10-14 0
## 15 2012-10-15 0
## 16 2012-10-16 0
## 17 2012-10-17 0
## 18 2012-10-18 0
## 19 2012-10-19 0
## 20 2012-10-20 0
## 21 2012-10-21 0
## 22 2012-10-22 0
## 23 2012-10-23 0
## 24 2012-10-24 0
## 25 2012-10-25 0
## 26 2012-10-26 0
## 27 2012-10-27 0
## 28 2012-10-28 0
## 29 2012-10-29 0
## 30 2012-10-30 0
## 31 2012-10-31 0
## 32 2012-11-01 0
## 33 2012-11-02 0
## 34 2012-11-03 0
## 35 2012-11-04 0
## 36 2012-11-05 0
## 37 2012-11-06 0
## 38 2012-11-07 0
## 39 2012-11-08 0
## 40 2012-11-09 0
## 41 2012-11-10 0
## 42 2012-11-11 0
## 43 2012-11-12 0
## 44 2012-11-13 0
## 45 2012-11-14 0
## 46 2012-11-15 0
## 47 2012-11-16 0
## 48 2012-11-17 0
## 49 2012-11-18 0
## 50 2012-11-19 0
## 51 2012-11-20 0
## 52 2012-11-21 0
## 53 2012-11-22 0
## 54 2012-11-23 0
## 55 2012-11-24 0
## 56 2012-11-25 0
## 57 2012-11-26 0
## 58 2012-11-27 0
## 59 2012-11-28 0
## 60 2012-11-29 0
## 61 2012-11-30 0
```

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

It looks like there are no big different, and the reason is that the analysis is using 0 for replacing NA.

We will check whether there are differences in activity patterns between weekdays and weekends

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
data1$date <- as.Date(data$date)
days <- !(weekdays(data$date) %in% c('Saturday','Sunday'))
data1$day <- days
for(i in 1:nrow(data1)){
    if(data1$day[i]){
        data1$weekdays[i] = "weekdays"
    }
    else{
        data1$weekdays[i] = "weekend"
    }
}
```


Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
xyplot( steps ~ interval | weekdays, type = "l", data = data1, ylab = "Number of Steps", layout = c(1,2))
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 

