# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
# Read in activity file
step_DF <- read.csv(file="c:/coursera/ReproducibleData/week2/activity.csv",
                    sep = ",", header=TRUE)
```
## What is mean total number of steps taken per day?
####For this assignment ignore the missing values.
1. Calculate total daily steps.
2. Make a histogram of the total number of daily steps.
3. Calculate the mean and median of daily steps.


```r
# Ignore missing values
step_DF_na.omit <- na.omit(step_DF)

# Aggregate steps as per date to get total number of steps in a day
dailySteps <- aggregate(steps ~ date, step_DF_na.omit, sum)

# Create histogram of total number of steps in a day
hist(dailySteps$steps, col=1, main="Total number of steps per day", xlab="Total number of steps in a day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


```r
# Calculate mean and median of steps per day
meanSteps <- mean(dailySteps$steps)
meanSteps
```

```
## [1] 10766.19
```

```r
medianSteps <- median(dailySteps$steps)
medianSteps
```

```
## [1] 10765
```

```r
cat("Daily Steps mean = ", meanSteps, "and median = ", medianSteps)
```

```
## Daily Steps mean =  10766.19 and median =  10765
```
## What is the average daily activity pattern?
1. Make a time series plot (type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).
2. Which 5-minute interval contains the maximum number of steps?


```r
# aggregate steps to get average daily interval
 internalSteps<- aggregate(steps ~ interval, step_DF_na.omit, mean)

# Plot daily average interval
plot(internalSteps$interval, internalSteps$steps, type='l', col=1, 
     main="Average number of Daily Steps", xlab="Interval", 
     ylab="Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


```r
# Got lazy and just did which.max and max
maxSteps <- max(internalSteps$steps)
maxSteps
```

```
## [1] 206.1698
```

```r
maxInternal <- which.max(internalSteps$steps)
maxInternal
```

```
## [1] 104
```

```r
cat("5-minute interval value = ", maxInternal, "and maximum steps = ", maxSteps)
```

```
## 5-minute interval value =  104 and maximum steps =  206.1698
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate the total number of missing values in the dataset.
2. Devise a strategy for filling in all of the missing values in the dataset.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken daily and Calculate the mean and median. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# get number of rows with NA's
stepCnt_NA <- sum(is.na(step_DF))

cat("The total number of NA's = ", stepCnt_NA)
```

```
## The total number of NA's =  2304
```


```r
# Fill in missing values
# Will use the mean for like intervals to replace NA's.  
for (i in 1:nrow(step_DF))
  {
    if (is.na(step_DF$steps[i]))
      {
         rowNum <- which(internalSteps$interval == step_DF$interval[i])
         stepAvg <- round(internalSteps$steps[rowNum], digits=0)
         step_DF$steps[i] <- stepAvg
      }
  }

# Get total daily steps
dailyTotalSteps <- aggregate(steps ~ date, step_DF, sum)

# create histogram of total number of steps in a day
hist(dailyTotalSteps$steps, col=1,
    main="Total Daily Steps", xlab="Total Daily Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


```r
# Calculate mean and median of steps per day
meanTotalSteps <- mean(dailyTotalSteps$steps)
meanTotalSteps
```

```
## [1] 10765.64
```

```r
medianTotalSteps <- median(dailyTotalSteps$steps)
medianTotalSteps
```

```
## [1] 10762
```

```r
cat("Imputed: Daily Steps mean = ", meanTotalSteps, "and median = ", medianTotalSteps)
```

```
## Imputed: Daily Steps mean =  10765.64 and median =  10762
```


## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - weekday and weekend indicating whether a given date is a weekday or weekend day.
2.Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.


```r
# convert date from string to Date class
step_DF$date <- as.Date(step_DF$date, "%Y-%m-%d")

# day of the week 
step_DF$day <- weekdays(step_DF$date)

# add a new column called day type and initialize to weekday
step_DF$dayType <- c("weekday")

# If day is Saturday or Sunday, make day type as weekend
for (i in 1:nrow(step_DF))
  {
     if (step_DF$day[i] == "Saturday" || step_DF$day[i] == "Sunday")
       {
         step_DF$dayType[i] <- "weekend"
       }
  }

# convert from character to factor
step_DF$dayType <- as.factor(step_DF$dayType)

# aggregate steps as interval to get average number of steps in an interval across all days
intervalStepsImpute <- aggregate(steps ~ interval+dayType, step_DF, mean)
```


```r
# make the panel plot for weekdays and weekends
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.5
```

```r
qplot(interval, steps, data=intervalStepsImpute, geom=c("line"), xlab="Interval", 
      ylab="Number of steps", main="") + facet_wrap(~ dayType, ncol=1)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
