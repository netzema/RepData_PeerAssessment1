library(ggplot2)
library(data.table)
library(DMwR)
## Loading and preprocessing the data
dat <- data.table(read.csv("activity.csv"))


## What is mean total number of steps taken per day?
NumOfDays <- length(unique(dat$date))
SumOfEachDay <- tapply(dat$steps, dat$date, sum, na.rm = TRUE)
MeanTotalSteps <- mean(SumOfEachDay)
MedianTotalSteps <- median(SumOfEachDay)
hist(x = SumOfEachDay)
MeanTotalSteps
MedianTotalSteps

## What is the average daily activity pattern?
IntMean <- tapply(dat$steps, dat$interval, mean, na.rm = TRUE)
plot(IntMean, type = "l", xlab = "5-Minute-Interval", ylab = "Mean Steps taken per day", main = "Mean steps taken per day in each interval")
MaxStepsInt <- which.max(IntMean)
MaxStepsInt

## Imputing missing values
LogicalNA <- is.na(dat)
NumOfNA <- sum(LogicalNA)
complete.cases(dat)
datImp <- knnImputation(dat, k = 10)

ImpSumOfEachDay <- tapply(datImp$steps, datImp$date, sum)
ImpMeanTotalSteps <- mean(ImpSumOfEachDay)
ImpMedianTotalSteps <- median(ImpSumOfEachDay)
hist(x = ImpSumOfEachDay)
ImpMeanTotalSteps
ImpMedianTotalSteps


## Are there differences in activity patterns between weekdays and weekends?
datImp2 <- cbind(datImp, weekdays(as.Date(datImp$date)))
names(datImp2) <- c("steps", "date", "interval", "weekday")
datImp2 <- data.table(datImp2)
datImp2[grepl(pattern = "Montag|Dienstag|Mittwoch|Donnerstag|Freitag", x = `weekday`), "weekday or weekend"] <- "weekday"
datImp2[grepl(pattern = "Samstag|Sonntag", x = `weekday`), "weekday or weekend"] <- "weekend"
datImp2[, `weekday or weekend` := as.factor(`weekday or weekend`)]

ggplot(datImp2 , aes(x = interval , y = steps, color=`weekday or weekend`)) + geom_line() + labs(title = "Mean Steps by Weekday/-end", x = "Interval", y = "Number of Steps") + facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2)