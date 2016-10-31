# Peer Assessment
Munshi Imran Hossain  
30 October 2016  




#### Load the data

```r
activity <- read.csv("activity.csv", header = T)
```

#### Remove NA values

```r
activity.narm <- activity[which(!is.na(activity$steps)), ]
```

#### Calculate the total number of steps taken per day

```r
activity.agg <- aggregate(activity$steps, by = list(activity$date), 
                       FUN = sum, na.rm = TRUE)
names(activity.agg) <- c("Date", "Steps")
```

#### Histogram of total number of steps taken per day

```r
hist(activity.agg$Steps, xlab = "Steps taken", main = "Distribution of steps 
     taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

#### Mean number of steps taken per day

```r
meanOfSTeps <- mean(activity.agg$Steps, na.rm = T)
```

#### Median number of steps taken per day

```r
medianOfSteps <- median(activity.agg$Steps, na.rm = T)
```

#### Aggregate data based on time intervals

```r
activity.agg.interval <- aggregate(activity$steps, by = list(activity$interval),
                                   FUN = mean, na.rm = TRUE)

names(activity.agg.interval) <- c("Interval", "AverageSteps")
```

#### Time series plot of the 5-minute interval and the average number of steps taken

```r
plot(activity.agg.interval$Interval, activity.agg.interval$AverageSteps, 
     type = 'l', xlab = "Time Interval", ylab = "Average steps",
     main = "Time series of average # of steps taken in each interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

#### Interval in which maximum number of steps are taken, on average

```r
max.step.interval <- activity.agg.interval$Interval[which.max(activity.agg.interval$AverageSteps)]
```

#### Total number of rows with NA

```r
numNAvalues <- nrow(activity) - nrow(activity.narm)
```

#### Filling in missing values with average steps in the time interval

```r
activity.imputed <- activity
for (i in which(is.na(activity.imputed$steps)))
{
  activity.imputed$steps[i] <- activity.agg.interval$AverageSteps[i] 
}
```

#### Calculate the total number of steps taken per day for new data

```r
activity.imputed.agg <- aggregate(activity.imputed$steps, 
                    by = list(activity.imputed$date), FUN = sum, na.rm = TRUE)

names(activity.imputed.agg) <- c("Date", "Steps")
```

#### Histogram of total number of steps taken per day

```r
hist(activity.imputed.agg$Steps, xlab = "Steps taken", main = "Distribution of steps 
     taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

There is a slight increase in frequency for the steps taken in the 
range 5000 to 1000

#### Mean number of steps taken per day

```r
meanOfSTeps.imputed <- mean(activity.imputed.agg$Steps, na.rm = T)
```

#### Median number of steps taken each day

```r
medianOfSteps.imputed <- median(activity.imputed.agg$Steps, na.rm = T)
```

#### Difference in mean and median of number of steps for original and imputed data

```r
diffMean <- meanOfSTeps - meanOfSTeps.imputed

diffMedian <- medianOfSteps - medianOfSteps.imputed
```

The mean number of steps has reduced by 176.4948964 and the median number of steps has reduced by 44.

#### Create a factor variable with 2 levels, one for weekday and the other for weekend

```r
days <- weekdays(as.Date(activity.imputed$date))

daytype <- c()  # a vector with 1 for weekday and 0 for weekend

daytype <- ifelse((days == "Sunday" | days == "Saturday"), 0, 1)
```

#### Add a coulmn of the day type to the data frame

```r
activity.imputed$daytype <- as.factor(daytype)
```

#### Aggregate data based on time intervals

```r
activity.agg <- aggregate(activity.imputed$steps, 
                by = list(activity.imputed$interval, activity.imputed$daytype),
                                   FUN = mean, na.rm = TRUE)

names(activity.agg) <- c("Interval", "DayType", "AverageSteps")
```

#### Separate the average data for weekday and weekend

```r
weekendActivity <- subset(activity.agg, activity.agg$DayType == 0)

weekdayActivity <- subset(activity.agg, activity.agg$DayType == 1)
```

#### Draw panel plot showing mean number of steps for each interval for weekdays and weekends

```r
par(mfrow = c(2, 1))
plot(weekendActivity$Interval, weekendActivity$AverageSteps, 
     type = 'l', xlab = "Time Interval", ylab = "Average steps",
     main = "Time series of average # of steps taken in each interval on weekend")

plot(weekdayActivity$Interval, weekdayActivity$AverageSteps, 
     type = 'l', xlab = "Time Interval", ylab = "Average steps",
     main = "Time series of average # of steps taken in each interval on weekday")
```

![](PA1_template_files/figure-html/unnamed-chunk-21-1.png)<!-- -->
