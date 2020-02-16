---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

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
unzip("activity.zip")
df<-read.csv("activity.csv")
sum<-df %>% 
  group_by(date) %>%
  summarise(total_steps = sum(steps, na.rm = F))
```

## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day

```r
hist(sum$total_steps)
```

![](PA1_template_files/figure-html/histogram of the total number of steps-1.png)<!-- -->

2. Calculate and report the **mean** and **median** total number of steps taken per day

```r
#mean
print(mean(sum$total_steps,na.rm = T))
```

```
## [1] 10766.19
```

```r
#median
print(median(sum$total_steps,na.rm = T))
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
mean_interval<-df %>%
  group_by(interval) %>%
  summarise(mean_steps = mean(steps,na.rm = T))

plot(mean_interval,type = 'l')
```

![](PA1_template_files/figure-html/average daily activity time series plot-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
mean_interval %>% 
  filter(mean_steps==max(mean_steps)) %>% select(interval) %>% print()
```

```
## # A tibble: 1 x 1
##   interval
##      <int>
## 1      835
```


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)

```r
df %>% filter(is.na(steps)) %>% summarise(count = n())
```

```
##   count
## 1  2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
df1 <- df

df1$steps <- ifelse( is.na(df1$steps), round(mean_interval$mean_steps[which(mean_interval$interval==df1$interval)]), df1$steps)
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
sum1<-df1 %>% 
  group_by(date) %>%
  summarise(total_steps = sum(steps, na.rm = F))
hist(sum1$total_steps)
```

![](PA1_template_files/figure-html/histogram with simulated data-1.png)<!-- -->

```r
#mean
print(mean(sum1$total_steps,na.rm = T))
```

```
## [1] 10766.11
```

```r
#median
print(median(sum1$total_steps,na.rm = T))
```

```
## [1] 10763.5
```

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

1. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using **simulated data**:

![Sample panel plot](instructions_fig/sample_panelplot.png) 


```r
Sys.setlocale("LC_TIME", "English")
library(ggplot2)
```


```r
df2<-df1
df2$date <- as.Date(df2$date) 
df2$weekday <- ifelse(weekdays(df2$date) %in% c("Saturday","Sunday"),as.character("weekend"), as.character("weekday"))
#averaged
mean_interval2<-df2 %>%
  group_by(weekday,interval) %>%
  summarise(mean_steps = mean(steps,na.rm = T))

qplot(interval, mean_steps, data = mean_interval2, colour = weekday)+
  geom_line() + facet_wrap(~weekday,nrow = 2)
```

![](PA1_template_files/figure-html/panel plot-1.png)<!-- -->
final panel plot
