
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
hist(sumofsteps$x, breaks = 10)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


Calculate and report the mean and median total number of steps taken per day

```r
mean(sumofsteps$x)
```

```
## [1] NA
```

```r
mean(sumofsteps$x, na.rm = TRUE)
```

```
## [1] 10766
```

```r
median(sumofsteps$x)
```

```
## [1] NA
```

```r
median(sumofsteps$x, na.rm = TRUE)
```

```
## [1] 10765
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
hist(sumofsteps1$x, breaks = 10)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

```r
mean(sumofsteps1$x)
```

```
## [1] 9354
```

```r
median(sumofsteps1$x)
```

```
## [1] 10395
```

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

There are some different between the two histogram, and the mean and median are different too.

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

