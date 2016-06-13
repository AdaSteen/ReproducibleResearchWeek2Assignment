
# A Steenekamp Reproducible Research
## Loading and preprocessing the data
### Read the data

```r
dat1<-read.csv("activity.csv")
```
### Load libraries used in Assignment

```r
library(dplyr)
library(ggplot2)
library(xtable)
library(knitr)
library(chron)
```

## What is mean total number of steps taken per day?
### Histogram

```r
dat2<-dat1
# Omit NA's
dat2<-na.omit(dat2)
# Group the total steps by date and calculate the sum of each day
totsteps<-dat2 %>% group_by(date) %>% summarise(tot=sum(steps))%>%arrange(date)
# Transform the date
totsteps<-totsteps %>% mutate(date = as.Date(date))
# Transform the steps
totsteps<-totsteps %>% mutate(tot = as.numeric(tot))
#Plot the Histogram
plot(totsteps$date,totsteps$tot,main="Total steps per day without imputed NA's",xlab="Date",ylab="Total Steps",type="h",col="blue")
```

![plot of chunk total number of steps](figure/total number of steps-1.png)


### The mean of steps per day

```r
dat2<-dat1
# Calculate the mean of steps per day
meansteps<-dat2 %>% mutate(date=as.Date(date),steps=as.numeric(steps))
meansteps<-meansteps %>% group_by(date) %>% summarise(avg=mean(steps))%>%arrange(date)
meansteps<-as.data.frame.AsIs(meansteps)
meansteps
```

```
##        x.date      x.avg
## 1  2012-10-01         NA
## 2  2012-10-02  0.4375000
## 3  2012-10-03 39.4166667
## 4  2012-10-04 42.0694444
## 5  2012-10-05 46.1597222
## 6  2012-10-06 53.5416667
## 7  2012-10-07 38.2465278
## 8  2012-10-08         NA
## 9  2012-10-09 44.4826389
## 10 2012-10-10 34.3750000
## 11 2012-10-11 35.7777778
## 12 2012-10-12 60.3541667
## 13 2012-10-13 43.1458333
## 14 2012-10-14 52.4236111
## 15 2012-10-15 35.2048611
## 16 2012-10-16 52.3750000
## 17 2012-10-17 46.7083333
## 18 2012-10-18 34.9166667
## 19 2012-10-19 41.0729167
## 20 2012-10-20 36.0937500
## 21 2012-10-21 30.6284722
## 22 2012-10-22 46.7361111
## 23 2012-10-23 30.9652778
## 24 2012-10-24 29.0104167
## 25 2012-10-25  8.6527778
## 26 2012-10-26 23.5347222
## 27 2012-10-27 35.1354167
## 28 2012-10-28 39.7847222
## 29 2012-10-29 17.4236111
## 30 2012-10-30 34.0937500
## 31 2012-10-31 53.5208333
## 32 2012-11-01         NA
## 33 2012-11-02 36.8055556
## 34 2012-11-03 36.7048611
## 35 2012-11-04         NA
## 36 2012-11-05 36.2465278
## 37 2012-11-06 28.9375000
## 38 2012-11-07 44.7326389
## 39 2012-11-08 11.1770833
## 40 2012-11-09         NA
## 41 2012-11-10         NA
## 42 2012-11-11 43.7777778
## 43 2012-11-12 37.3784722
## 44 2012-11-13 25.4722222
## 45 2012-11-14         NA
## 46 2012-11-15  0.1423611
## 47 2012-11-16 18.8923611
## 48 2012-11-17 49.7881944
## 49 2012-11-18 52.4652778
## 50 2012-11-19 30.6979167
## 51 2012-11-20 15.5277778
## 52 2012-11-21 44.3993056
## 53 2012-11-22 70.9270833
## 54 2012-11-23 73.5902778
## 55 2012-11-24 50.2708333
## 56 2012-11-25 41.0902778
## 57 2012-11-26 38.7569444
## 58 2012-11-27 47.3819444
## 59 2012-11-28 35.3576389
## 60 2012-11-29 24.4687500
## 61 2012-11-30         NA
```


### The median of steps per day (not sure if the median is zero, but running out of time...)

```r
dat2<-dat1
# Calculate the median of steps per day
mediansteps<-dat2 %>% mutate(date=as.Date(date),steps=as.numeric(steps))
mediansteps<-mediansteps %>% group_by(date) %>% summarise(med=median(steps))%>%arrange(date)
mediansteps<-as.data.frame.AsIs(mediansteps)
mediansteps
```

```
##        x.date x.med
## 1  2012-10-01    NA
## 2  2012-10-02     0
## 3  2012-10-03     0
## 4  2012-10-04     0
## 5  2012-10-05     0
## 6  2012-10-06     0
## 7  2012-10-07     0
## 8  2012-10-08    NA
## 9  2012-10-09     0
## 10 2012-10-10     0
## 11 2012-10-11     0
## 12 2012-10-12     0
## 13 2012-10-13     0
## 14 2012-10-14     0
## 15 2012-10-15     0
## 16 2012-10-16     0
## 17 2012-10-17     0
## 18 2012-10-18     0
## 19 2012-10-19     0
## 20 2012-10-20     0
## 21 2012-10-21     0
## 22 2012-10-22     0
## 23 2012-10-23     0
## 24 2012-10-24     0
## 25 2012-10-25     0
## 26 2012-10-26     0
## 27 2012-10-27     0
## 28 2012-10-28     0
## 29 2012-10-29     0
## 30 2012-10-30     0
## 31 2012-10-31     0
## 32 2012-11-01    NA
## 33 2012-11-02     0
## 34 2012-11-03     0
## 35 2012-11-04    NA
## 36 2012-11-05     0
## 37 2012-11-06     0
## 38 2012-11-07     0
## 39 2012-11-08     0
## 40 2012-11-09    NA
## 41 2012-11-10    NA
## 42 2012-11-11     0
## 43 2012-11-12     0
## 44 2012-11-13     0
## 45 2012-11-14    NA
## 46 2012-11-15     0
## 47 2012-11-16     0
## 48 2012-11-17     0
## 49 2012-11-18     0
## 50 2012-11-19     0
## 51 2012-11-20     0
## 52 2012-11-21     0
## 53 2012-11-22     0
## 54 2012-11-23     0
## 55 2012-11-24     0
## 56 2012-11-25     0
## 57 2012-11-26     0
## 58 2012-11-27     0
## 59 2012-11-28     0
## 60 2012-11-29     0
## 61 2012-11-30    NA
```

##What is the average daily activity pattern?
### Time-series Plot

```r
# Create a dataset for average steps per interval
avgstepsint<-dat2
# Omit NA's
avgstepsint<-na.omit(avgstepsint)
# Group and Summarise
avgstepsint<-avgstepsint %>% group_by(interval) %>% summarise(avg=mean(steps))%>%arrange(interval)
# Plot
plot(avgstepsint$interval,avgstepsint$avg,main="Time series average steps",xlab="Interval",ylab="Avg Steps",type="l",col="steelblue4")
```

![plot of chunk Timeseries plot](figure/Timeseries plot-1.png)

### Which 5-minute interval contains the max number of steps? (Answer: 835)

```r
# Get the maximum steps
max(avgstepsint$avg)
```

```
## [1] 206.1698
```

```r
# Determine which row number
which.max(avgstepsint$avg)
```

```
## [1] 104
```

```r
# Get the interval
maxint<-avgstepsint[104,1]
maxint
```

```
## Source: local data frame [1 x 1]
## 
##   interval
##      (int)
## 1      835
```
## Imputing missing values
### The strategy is to impute the mean of all means

```r
# Calculate the number of NA's
dat1na<-sum(is.na(dat1))
dat1na
```

```
## [1] 2304
```

```r
# Determine that only steps has NA's, none of the other variables
dat2na<-sum(is.na(dat2$steps))
dat2na
```

```
## [1] 2304
```

```r
# Calculate the average number of steps for all days
dat3<-dat1
dat3<-na.omit(dat3)
dat3<-dat3 %>% group_by(date) %>% summarise(avg=mean(steps))%>%arrange(date)
meanavg<-mean(dat3$avg)
# Impute into dat4 dataset
dat4<-dat1
dat4[is.na(dat4)] = meanavg
```
### Histogram with imputed values


```r
# Group and summarise imputed dataset
totisteps<-dat4 %>% group_by(date) %>% summarise(tot=sum(steps))%>%arrange(date)
# Mutate to date
totisteps<-totisteps %>% mutate(date = as.Date(date))
# Calculate the average number of steps for all days
totisteps<-totisteps %>% mutate(tot = as.numeric(tot))
# Impute into dat4 dataset
plot(totisteps$date,totisteps$tot,main="Total steps per day with imputed NA's",xlab="Date",ylab="Total Steps",type="h",col="red")
```

![plot of chunk Histogram with imputed values](figure/Histogram with imputed values-1.png)

## Are there differences in activity patterns between weekdays and weekends?

```r
# Determine weekend or not
dat5<-dat1
dat5<-na.omit(dat5)
dat5<-mutate(dat5,iswkend=is.weekend(dat5$date))
# Filter weekends
wkends<-filter(dat5,iswkend)
# Filter weekdays
wkdays<-filter(dat5,!iswkend)
# Determine averages
meanintwkd<-wkdays %>% group_by(interval) %>% summarise(avg=mean(steps))%>%arrange(interval)
meanintwke<-wkends %>% group_by(interval) %>% summarise(avg=mean(steps))%>%arrange(interval)
# Plot the graph
par(mfrow = c(1,2))
plot(meanintwkd$interval, meanintwkd$avg, type = "l", ylab = "Steps on Weekdays", xlab = "Interval",main="Weekdays",col="red")
plot(meanintwke$interval, meanintwke$avg, type = "l", ylab = "Steps on Weekends", xlab = "Interval",main="Weekends",col="blue") 
```

![plot of chunk weekends and weekdays](figure/weekends and weekdays-1.png)
