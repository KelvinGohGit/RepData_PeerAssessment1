---
title: 'Reproducible Research: Peer Assessment 1'
author: "Kelvin"
date: "9/10/2020"
output: 
  html_document:
    keep_md: true
    self_contained: true
---


Introduction
=======================	
    
It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a
[Fitbit](http://www.fitbit.com), [Nike Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or
[Jawbone Up](https://jawbone.com/up). These type of devices are part of
the "quantified self" movement -- a group of enthusiasts who take
measurements about themselves regularly to improve their health, to
find patterns in their behavior, or because they are tech geeks. But
these data remain under-utilized both because the raw data are hard to
obtain and there is a lack of statistical methods and software for
processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012
and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web
site:
  
  * Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:
  
  * **steps**: Number of steps taking in a 5-minute interval (missing
                                                              values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
format

* **interval**: Identifier for the 5-minute interval in which
measurement was taken


The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
dataset.

## Assignment
This assignment will be described in multiple parts. You will need to
write a report that answers the questions detailed below. Ultimately,
you will need to complete the entire assignment in a **single R
markdown** document that can be processed by **knitr** and be
transformed into an HTML file.

Throughout your report make sure you always include the code that you
used to generate the output you present. When writing code chunks in
the R markdown document, always use `echo = TRUE` so that someone else
will be able to read the code. **This assignment will be evaluated via
peer assessment so it is essential that your peer evaluators be able
to review the code for your analysis**.

For the plotting aspects of this assignment, feel free to use any
plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the [GitHub repository created for this       assignment](http://github.com/rdpeng/RepData_PeerAssessment1). You
will submit this assignment by pushing your completed files into your
forked repository on GitHub. The assignment submission will consist of
the URL to your GitHub repository.

NOTE: The GitHub repository also contains the dataset for the
assignment so you do not have to download the data separately.

### Loading and preprocessing the data

Show any code that is needed to

1. Load the data (i.e. `read.csv()`)


```r
###loading dataset

ActivityData<-read.csv("./project_data/Activity.csv")
head(ActivityData,n=10)
```

```
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
```

```r
str(ActivityData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

### What is mean total number of steps taken per day?

```r
StepPerDay<-aggregate(steps~date, ActivityData, sum,na.rm=TRUE)
```



For this part of the assignment, you can ignore the missing values in
the dataset.

1. Make a histogram of the total number of steps taken each day

```r
hist(StepPerDay$steps,xlab="Daily Step", main="Total Number of Step Taken Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->



2. Calculate and report the **mean** and **median** total number of steps taken per day

```r
Meansteps<-mean(StepPerDay$steps)
Meansteps
```

```
## [1] 10766.19
```


```r
Mediansteps<-median(StepPerDay$steps)
Mediansteps
```

```
## [1] 10765
```



### What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
StepInterval<-aggregate(steps~interval, ActivityData, mean,na.rm=TRUE)
plot(steps~interval, data=StepInterval, type="l",xlab="5 minutes interval",ylab="Average Steps",main= "5 Mins Interval Plot")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->



2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
MaxStepsInterval<-StepInterval[which.max(StepInterval$steps),]$interval
MaxStepsInterval
```

```
## [1] 835
```
   
  

### Imputing missing values
  
Note that there are a number of days/intervals where there are missing
values (coded as `NA`). The presence of missing days may introduce
bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)

```r
TotalNA=sum(is.na(ActivityData$steps))
TotalNA
```

```
## [1] 2304
```



2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
ReplaceMean<-function(interval){
  StepInterval[StepInterval$interval==interval,]$steps
}
```




3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
FullAD<-ActivityData
for(i in 1:nrow(FullAD)){
   if(is.na(FullAD[i,]$steps)){
    FullAD[i,]$steps<-ReplaceMean(FullAD[i,]$interval)
    }
}
```


4. Make a histogram of the total number of steps taken each day and 

```r
StepPerDayNONA<-aggregate(steps~date, FullAD, sum)

hist(StepPerDayNONA$steps,xlab="Daily Step", main="Total Number of Step Taken Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->



Calculate and report the **mean** and **median** total number of steps taken per day. 

```r
MeanstepsNoNA<-mean(StepPerDayNONA$steps)
MeanstepsNoNA
```

```
## [1] 10766.19
```


```r
MedianstepsNONA<-median(StepPerDayNONA$steps)
MedianstepsNONA
```

```
## [1] 10766.19
```



Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

#### No different in mean as the missing value is imputed with mean value.Slightly different median value when missing value is excluded. After imputation, mean and median shared the same value 

   
### Are there differences in activity patterns between weekdays and weekends?
  For this part the `weekdays()` function may be of some help here. Use
the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

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
FullAD$date<-as.Date(FullAD$date)
FullAD1<-FullAD%>%
  mutate(daytype=ifelse(weekdays(FullAD$date)=="Saturday"|weekdays(FullAD$date)=="Sunday", "Weekend", "Weekday"))
```



1. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
FullAD2<-FullAD1%>%
  group_by(daytype,interval)%>%
  summarise(avgsteps=mean(steps))
```

```
## `summarise()` regrouping output by 'daytype' (override with `.groups` argument)
```

```r
library(lattice)
xyplot(avgsteps~interval |daytype, data=FullAD2, type="l",layout=c(1,2), xlab="5-minute interval", ylab="Average number of steps taken", main="Average number of steps taken, averaged across all weekday days or weekend days")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->



