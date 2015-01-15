# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
activity <- read.csv("activity.csv",colClasses = c("integer", "character", "character"))

format_interval <- function(c){
  res <- c
  if(nchar(c) == 1) res <- paste("000", c, sep = "")
  if(nchar(c) == 2) res <- paste("00", c, sep = "")
  if(nchar(c) == 3) res <- paste("0", c, sep = "")
  
  res
}

activity$interval <- sapply(activity$interval, format_interval)
activity$date <- as.Date(activity$date, "%Y-%m-%d") # transform to Date
# look a bit of the data
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01     0000
## 2    NA 2012-10-01     0005
## 3    NA 2012-10-01     0010
## 4    NA 2012-10-01     0015
## 5    NA 2012-10-01     0020
## 6    NA 2012-10-01     0025
```

```r
tail(activity)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```

## What is mean total number of steps taken per day?

```r
total_number_steps <- sapply(split(activity$steps, activity$date), sum, na.rm = TRUE)
mean_tns <- mean(total_number_steps)
median_tns <- median(total_number_steps)
dates <- as.Date(names(total_number_steps), "%Y-%m-%d")
#plot(dates, total_number_steps, type = "h", main = "Total Number of Steps Taken Each Day (2012)")
hist(total_number_steps)
```

![](PA1_template_files/figure-html/totoal_numver_steps-1.png) 

The mean of total number of steps taken per day is 9354.2295082.  
The median of total number of steps taken per day is 10395.

## What is the average daily activity pattern?

```r
avg_daily <- sapply(split(activity$steps, activity$interval), mean, na.rm = TRUE)
hms <- strptime(names(avg_daily), "%H%M")
plot(hms, avg_daily, type = "l", main = "Average Daily Activity Pattern")
```

![](PA1_template_files/figure-html/avg_daily_pattern-1.png) 

```r
max_avg_daily <- max(avg_daily)
hms <- data.frame(avg_daily = avg_daily, time = names(avg_daily))
res <- subset(hms, avg_daily == max_avg_daily)
```
0835 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps (206.1698113).

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
