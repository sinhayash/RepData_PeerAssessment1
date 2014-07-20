# Reproducible Research: Peer Assessment 1
==========================================================
Make sure you have the packages `reshape2`, `chron` and `lattice` installed on your machine before running script.

## Loading and preprocessing the data
Loading the data through "read.csv".Make sure activity.csv lies in the directory.


```r
data <- read.csv("activity.csv")
```

Generating sequence of dates through "strptime".

```r
initialDate <- strptime(data$date[1], format = '%Y-%m-%d', tz = 'UTC')
finalDate   <- paste(tail(data, 1)$date, tail(data, 1)$interval)
finalDate   <- strptime(finalDate, format = '%Y-%m-%d %H%M', tz = 'UTC')

time_date  <- seq(initialDate,finalDate,length = nrow(data))
```

What we have done?

* Loaded data (with variable 'data')
* Start Date (with variable 'initialDate')
* End Date (with variable 'finalDate')

In a nutshell, we have completed preprocessing of data.

## What is mean total number of steps taken per day?
In the next phase, we come across a data with observations taken every five minutes. Thus we need to transform the data. For this task we will use `reshape` package. The functions used will be `melt` and `dcast`.


```r
library(reshape2)

melt_data  <- melt(data, id = c('date','interval'))
morph_data <- dcast(melt_data, date ~ variable, sum, na.rm=TRUE)
```

Generating the histogram for total no. of steps per day.
We use the `Base Graphic System`  for generation of histogram.
To generate legend , mean & median lines we use `legend` and `abline` respectively.


```r
meanSteps   <- mean(morph_data$steps)#Mean
medianSteps <- median(morph_data$steps)#Median

#Base Graphic System - Histogram
hist(morph_data$steps, col='gray', main='Total Number of Steps per Day', 
     xlab = 'Total Number of Steps per Day')


abline(v = meanSteps, col = 'springgreen', lwd = 3)
abline(v = medianSteps, col = 'orangered', lwd = 3)
legend('topright', legend = c('Mean', 'Median'), 
       col = c('springgreen','orangered'), 
       lty = 'solid', lwd = 3, bty = 'n')
```

![plot of chunk Hist](figure/Hist.png) 

Median and Mean for Total no. of steps per day is represented by the green line and red line respectively. 

## What is the average daily activity pattern?

For getting the activity pattern, we use `aggregate` function. This will in turn provide us the mean interval across each given day.


```r
avPerDay <- aggregate(data$steps,list(data$interval), mean, na.rm=TRUE)
names(avPerDay) <- c('interval','steps')
```

Next we plot the aquired results using `plot`. We will obtain the average value of steps during each 5 min interval.


```r
avPerDay$Time <- as.POSIXct(time_date[1:nrow(avPerDay)], format = '%H:%M', tz = 'UTC') #Aggregates the time column

max_stepsInterval <- format(avPerDay$Time[which.max(avPerDay$steps)],'%H:%M')
#Interval with maximum stepson a Day.

#Plot
with(avPerDay, plot(steps ~ Time, type = 'l', lwd = 2, main = 'Time Series on Average Day', xlab = 'Intervals - 5 min', ylab = 'Average Steps Across All Days'))
```

![plot of chunk timeSeries_avPerDay](figure/timeSeries_avPerDay.png) 


## Imputing missing data

First of all, our task is to calculate total no. of rows with NA data.


```r
NAs  <- sum(!complete.cases(data))


rowTotal <- nrow(data)
NA_percentage <- paste0(round(100*(NAs/rowTotal),2),"%")
```

As per the assignment we have to replace percentage with an estimate.



```r
values <- data

#Using Merge and avPerDay
steps <- merge(data[is.na(values$steps),],avPerDay, by = 'interval')$steps.y


values[is.na(values$steps),]$steps <- as.integer(steps)#Replace NA data
```

After the above mentioned task, the results are saved into `values`

Next we will compute the total steps per day without the missing data.

```r
melt_data_values  <- melt(values, id = c('date','interval'))
morph_data_values <- dcast(melt_data_values, date ~ variable, sum)

head(morph_data_values)
```

```
##         date steps
## 1 2012-10-01    64
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```


Note that `na.rm = TRUE` is not used for computing `morph_data_values`, unlike morph_data.
For this computation we use the same `Base Graphic System`.To generate legend , mean & median lines we use `legend` and `abline` respectively.


```r
meanSteps_values   <-   mean(morph_data_values$steps)#Mean
medianSteps_values <- median(morph_data_values$steps)#Median

hist(morph_data_values$steps, col = 'gray',main='Histogram of Total Number of Steps per Day - Replaced NAs', xlab = 'Total Number of Steps per Day')

abline(v = meanSteps_values, col = 'springgreen', lwd = 3)
abline(v = medianSteps_values, col = 'orangered', lwd = 3)
legend('topright', legend = c('Mean', 'Median'), col = c('springgreen','orangered'), lty = 'solid', lwd = 3, bty = 'n')
```

![plot of chunk Hist_NO_Missing data](figure/Hist_NO_Missing data.png) 


## Are there differences in activity patterns between weekdays and weekends?

We use the `chron` library to create a new factor variable to differentiate between weekend and weekday data. We use the function `is.weekend` for above mentioned task.


```r
library(chron)

weekend <- is.weekend(time_date)#Creating Logical Vector

type_day <- factor(weekend, labels = c('weekday','weekend'))

values$type_day <- type_day#Adding new column
```

Then we make use of the `aggregate` function to transform the data into mean over 5 min intervals for all weekends and weekdays.


```r
DayAvg_values <- aggregate(values$steps, list(values$type_day,values$interval), mean)

names(DayAvg_values) <- c('type_day','interval','steps')

str(DayAvg_values)
```

```
## 'data.frame':	576 obs. of  3 variables:
##  $ type_day: Factor w/ 2 levels "weekday","weekend": 1 2 1 2 1 2 1 2 1 2 ...
##  $ interval: int  0 0 5 5 10 10 15 15 20 20 ...
##  $ steps   : num  5.36 10.81 3.73 10.81 3.49 ...
```

Then we use the `lattice` package to plot graph.


```r
library(lattice)

xyplot(steps ~ interval | type_day, data = DayAvg_values, layout = c(1,2), type = c('l','r'), lwd = 2)
```

![plot of chunk TimeSeries_Average Day](figure/TimeSeries_Average Day.png) 

From the graphs, we conclude that `weekend` data is more balanced. As far as `weekday` data is considered it has very high data to the left, contrary to the right. We get the assurance of the above fact by a regression trend line. We observe that `weekend` data has a positive slope while `weekday` data has a lower slope.
