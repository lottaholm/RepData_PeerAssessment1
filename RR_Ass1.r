setwd("C:\\Users\\lotta.holm\\Documents\\Data Science\\RR\\RepData_PeerAssessment1\\")

## this script assumes that the data file activity.zip has been downloaded and unpacked to the working directory as a file named 


## 1.Loading and preprocessing the data

library(data.table) ## required for fread
data <-fread("activity\\activity.csv")

## Checking the structure of the data, looking for missing values, data types, the content of the  etc.
## Conclusion => Only steps are missing, the interval and date variables do not contain NA:s
## Conclusion => The variable steps seems ok. It is plausible to take 806 steps in five minutes and it seems ok that more than half of the time the person is not walking at all (sleeping or sitting down).
## Conclusion => The intervals seem to be labelled by hours and minutes starting with the initial minute the day in question, "0" stands for 00:00 "2055" for 20:55 etc.
## Conclusion => Only the correct days are included!
## Conclusion => The missing values are always missing for an entire day when they are missing. 

## 2. What is mean total number of steps taken per day?

library(dplyr)

dailysteps <- data %>% group_by(date) %>% summarise(sum(steps, na.rm=TRUE)) 
names(dailysteps) <- c("date", "totalsteps")

## Plain histogram that shows the amount of steps each day:
hist(dailysteps$totalsteps, breaks=61, main="Histogram of steps taken each day", xlab="Daily steps", col="lightblue")
dev.copy(png, file="figures\\RR_ass1_dailysteps.png", width = 1080, height = 480)
dev.off()

## Grouping the steps into intervals of 5000 steps gives a nice picture of how many days have been days of very little, moderate and heavy excercise respectively
hist(dailysteps$totalsteps, breaks=5, main="Histogram of steps taken each day, grouped in intervals of 5000 steps", xlab="Daily steps", col="lightblue")
dev.copy(png, file="figures\\RR_ass1_dailysteps_grouped.png", width = 1080, height = 480)
dev.off()

mean(dailysteps$totalsteps, na.rm=TRUE) 
median(dailysteps$totalsteps, na.rm=TRUE) 
 
## 3. What is the average daily activity pattern?

intervalsteps <- data %>% group_by(interval) %>% summarise(mean(steps, na.rm=TRUE)) 
names(intervalsteps)<- c("interval", "totalsteps")

library(lattice)
xyplot( totalsteps ~ interval, intervalsteps, type='l', main="Average number of steps taken per interval", xlab="Interval", ylab="Steps")
dev.copy(png, file="figures\\RR_ass1_intervalsteps.png", width = 1080, height = 480)
dev.off()

##Maximum number of steps are present in this interval:
intervalsteps$interval[intervalsteps$totalsteps==(max(intervalsteps$totalsteps))]

## 4. Imputing missing values

##total number of missing values in the data set:
sum(!is.na(data$steps))

## As the data for steos is always missing for an entire day I decided to impute the mean value of each interval into the appropriate missing values. This will not change the mean values calculated for the intervals and the days with missing values will now have values corresponding to the overall means.
data_new <- data


length <- length(data_new$steps)

for (i in 1:length) {
    if (is.na(data_new$steps[i])) { 
        a <- data_new$interval[i]
        data_new$steps[i] <- intervalsteps$totalsteps[intervalsteps$interval==a] 
    }
}

dailysteps_new <- data_new %>% group_by(date) %>% summarise(sum(steps, na.rm=TRUE)) 
names(dailysteps_new) <- c("date", "totalsteps")

## Plain histogram that shows the amount of steps each day
hist(dailysteps_new$totalsteps, breaks=61, main="Histogram of steps taken each day after imputation", xlab="Daily steps", col="lightblue")
dev.copy(png, file="figures\\RR_ass1_dailysteps_new.png", width = 1080, height = 480)
dev.off()
## Grouping the steps into intervals of 5000 steps gives a nice picture of how many days have been days of very little, moderate and heavy excercise respectively
hist(dailysteps_new$totalsteps, breaks=5, main="Histogram of steps taken each day, grouped in intervals of 5000 steps", xlab="Daily steps", col="lightblue")
dev.copy(png, file="figures\\RR_ass1_dailysteps_grouped_new.png", width = 1080, height = 480)
dev.off()

##Original mean of daily steps:
mean(dailysteps$totalsteps, na.rm=TRUE) 
##Original median of daily steps:
median(dailysteps$totalsteps, na.rm=TRUE) 

##New mean of daily steps:
mean(dailysteps_new$totalsteps, na.rm=TRUE) 
##New median of daily steps:
median(dailysteps_new$totalsteps, na.rm=TRUE) 

 ## Conclusion=> The mean is not affected at all as the values imputed ARE the mean values. The median is slightly affected as there now are days with 

## 5. Are there differences in activity patterns between weekdays and weekends?

library(lubridate)
data_new$date <- ymd(data_new$date)

data_new$wday <- wday(data_new$date) ## Will use wday() due to locale issues
data_new$wdaywkend[data_new$wday==1] <- "Weekend"
data_new$wdaywkend[data_new$wday==2] <- "Weekday"
data_new$wdaywkend[data_new$wday==3] <- "Weekday"
data_new$wdaywkend[data_new$wday==4] <- "Weekday"
data_new$wdaywkend[data_new$wday==5] <- "Weekday"
data_new$wdaywkend[data_new$wday==6] <- "Weekday"
data_new$wdaywkend[data_new$wday==7] <- "Weekend"

data_new$wdaywkend <- factor(data_new$wdaywkend)


intervalsteps_new <- data_new %>% group_by(wdaywkend, interval) %>% summarise(mean(steps, na.rm=TRUE)) 
names(intervalsteps_new)<- c("wdaywkend", "interval", "meansteps")

library(lattice)
xyplot( meansteps ~ interval|wdaywkend, intervalsteps_new, type='l', main="Average number of steps taken per interval, weekday vs. weekend", xlab="Interval", ylab="Steps")
dev.copy(png, file="figures\\RR_ass1_intervalsteps_wdaywkend.png", width = 1080, height = 480)
dev.off()




