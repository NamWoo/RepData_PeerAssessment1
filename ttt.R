# Code for reading in the dataset and/or processing the data
# Histogram of the total number of steps taken each day
# Mean and median number of steps taken each day
# Time series plot of the average number of steps taken
# The 5-minute interval that, on average, contains the maximum number of steps
# Code to describe and show a strategy for imputing missing data
# Histogram of the total number of steps taken each day after missing values are imputed
# Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
# All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

library(ggplot2)

data <- read.csv("./activity/activity.csv", colClasses = c("integer", "Date", "factor"))
data$month <- as.numeric(format(data$date, "%m"))
data$day <- as.numeric(format(data$date, "%d"))

# Warning message:
# Removed 2304 rows containing missing values (position_stack).

data_nonull <- na.omit(data)
rownames(noNA) <- 1:nrow(noNA)
ggplot(data_nonull, aes(date, steps)) + geom_bar(stat = "identity", colour = "steelblue", fill = "steelblue", width = 0.7) + labs(title = "Histogram of Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")



#
step1_ag <- aggregate(data_nonull$steps, list(Date = data_nonull$date), sum)
step1_ta <- tapply(data_nonull$steps, data_nonull$date, sum, na.rm=TRUE)

mean(step1_ta)
mean(step1_ag$x)
# 10766.19

median(step1_ta)
median(step1_ag$x)
# 10765

#
stepavg <- aggregate(data_nonull$steps, list(interval = data_nonull$interval), mean)
names(stepavg)[2] <- "avgsteps"
ggplot(data=stepavg, aes(interval, stepavg)) + geom_line(color = "steelblue", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")


ggplot(data=stepavg, aes(x=interval, y=stepavg)) + geom_line(color = "steelblue", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")


#
avgSteps <- aggregate(data_nonull$steps, list(interval = as.numeric(as.character(data_nonull$interval))), FUN = "mean")
names(avgSteps)[2] <- "avgsteps"
ggplot(avgSteps, aes(interval, avgsteps)) + geom_line(color = "steelblue", size = 0.8) + labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")


stepavg <- aggregate(data_nonull$steps, list(interval = data_nonull$interval), mean)


averageStepsPerTimeBlock <- aggregate(x=list(meanSteps=data_nonull$steps), by=list(interval=data_nonull$interval), FUN=mean, na.rm=TRUE)

ggplot(data=averageStepsPerTimeBlock, aes(x=interval, y=meanSteps)) +
        geom_line() +
        xlab("5-minute interval") +
        ylab("average number of steps taken")

