Reproducible Research - Assignment 1
Gary Lipton
========================================================

Number of steps per day:

```r
library(ggplot2)
activity <- read.csv("../DATA/activity.csv")
steps.per.day <- aggregate(activity$steps, by = list(Date = activity$date), 
    FUN = sum, na.rm = TRUE)
## Remove days that were all NAs.
steps.per.day <- steps.per.day[steps.per.day$x != 0, ]
hist(steps.per.day$x, main = "Mean number of steps per day", breaks = 10)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

```r
options(digits = 7)
cat("mean   = ", mean(steps.per.day$x), "\n", "median = ", median(steps.per.day$x), 
    "\n", sep = "")
```

```
## mean   = 10766.19
## median = 10765
```

Time series plot:

```r
time.series <- aggregate(activity$steps, by = list(interval = activity$interval), 
    mean, na.rm = T)
plot(time.series, type = "l", main = "Mean number of steps per interval vs. time", 
    ylab = "steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


Interval with maximum number of steps:

```r
time.series[which.max(time.series$x), ]
```

```
##     interval        x
## 104      835 206.1698
```


Imputation:

```r
num.na <- sum(is.na(activity$steps))
cat("Missing values: ", num.na, " ", round(100 * num.na/nrow(activity), 1), 
    "%\n", sep = "")
```

```
## Missing values: 2304 13.1%
```

```r
missing.dates <- unique(activity$date[is.na(activity$steps)])
cat("There are ", length(missing.dates), " out of ", length(unique(activity$date)), 
    " days with missing data.\n", sep = "")
```

```
## There are 8 out of 61 days with missing data.
```

```r
## This is to verify what seems apparent from inspection. Data is missing
## either for a whole day, or not at all.
if (any(!is.na(activity$steps[activity$date %in% missing.dates]))) {
    cat("There are dates with partially missing data.\n")
} else {
    cat("There are no dates with partially missing data.\n")
}
```

```
## There are no dates with partially missing data.
```

```r
activity.impt <- activity
na.ids <- which(is.na(activity.impt))
na.intvl <- activity.impt$interval[na.ids]
## Set each missing value to the average value for that interval.
activity.impt$steps[na.ids] <- time.series$x[match(na.intvl, time.series$interval)]
steps.per.day <- aggregate(activity.impt$steps, by = list(Date = activity.impt$date), 
    FUN = sum, na.rm = TRUE)
## Remove days that were all NAs.
steps.per.day <- steps.per.day[steps.per.day$x != 0, ]
hist(steps.per.day$x, main = "Mean number of steps per day", breaks = 10)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
cat("mean   = ", mean(steps.per.day$x), "\n", "median = ", median(steps.per.day$x), 
    "\n", sep = "")
```

```
## mean   = 10766.19
## median = 10766.19
```

Because the missing data is set to the average for the corresponding interval,
the total number of steps for each day of missing data is equal to the mean for
the entire data set. Because the distribution is approximately symmetrical, 
the mean becomes the median. Because there are now more values at the mean,
the histogram has become more sharply peaked.


Weekdays vs. weekends:

```r
days <- weekdays(as.POSIXlt(activity$date))
activity.impt$day <- factor("weekday", levels = c("weekday", "weekend"))
activity.impt$day[days == "Saturday" | days == "Sunday"] <- "weekend"
is.wkday <- activity.impt$day == "weekday"
time.series.wkday <- aggregate(activity.impt$steps[is.wkday], by = list(interval = activity.impt$interval[is.wkday]), 
    mean, na.rm = T)
time.series.wkend <- aggregate(activity.impt$steps[!is.wkday], by = list(interval = activity.impt$interval[!is.wkday]), 
    mean, na.rm = T)
par(mfrow = c(2, 1))
yrange <- c(0, max(c(time.series.wkend$x, time.series.wkday$x)))
plot(time.series.wkday, type = "l", ylim = yrange, main = "Mean number of steps per interval vs. time\nweekdays", 
    ylab = "steps")
plot(time.series.wkend, type = "l", ylim = yrange, main = "Mean number of steps per interval vs. time\nweekends", 
    ylab = "steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

```r
par(mfrow = c(1, 1))
```

