require(dplyr)
require(ggplot2)
#Load data & pre-process
mdata <- read.csv("./data/activity.csv",header=TRUE)
clean_mdata <- mdata[complete.cases(mdata),]

#mean total number

total_steps_per_day <- with(clean_mdata, aggregate(steps ~ date, FUN = sum))
png("./out/Mean_tot_per_day.png",height = 500, width=500)
hist(x = total_steps_per_day$steps, xlab="Number of steps", ylab="Frequency",ylim=c(0,30),main="Histogram of steps per day")
dev.off()

#mean
mean_steps <- mean(total_steps_per_day$steps)
median_steps <- median(total_steps_per_day$steps)
#TODO: FIXME
print(paste("Mean: ", mean_steps, " Median: ", median_steps, sep=""))


##########################################################################
time_data <- with(clean_mdata, aggregate(steps ~ interval, FUN = mean))
mplot <- qplot(interval, steps, data=time_data) + geom_line() + ylab("Number of steps") + scale_x_continuous(seq(0,2355,500)) + ggtitle("Time series plot of mean steps taken across all days")
png("./out/Time_average_steps.png",width=500, height=500)
print(mplot)
dev.off()


#TODO: FIXME
#print(time_data[which(time_data$steps == max(time_data$steps),arr.ind=TRUE),1])

###########################################################################

num_NA <- nrow(mdata) - nrow(clean_mdata)

NA_rows <- which(is.na(mdata$steps), arr.ind=TRUE)

mdata_no_NA <- mdata
mdata_no_NA$interval <- as.numeric(mdata_no_NA$interval)

for(i in NA_rows) {
	mdata_no_NA[i,1] <- time_data[which(time_data$interval == mdata_no_NA[i,3], arr.ind=TRUE),2]
}

#print(nrow(mdata_no_NA) - nrow(mdata))

total_steps_per_day <- with(mdata_no_NA, aggregate(steps ~ date, FUN = sum))
png("./out/Mean_tot_per_day_no_NA.png",height = 500, width=500)
hist(x = total_steps_per_day$steps, xlab="Number of steps", ylab="Frequency",ylim=c(0, 40),main="Histogram of steps per day")
dev.off()

mean_steps <- mean(total_steps_per_day$steps)
median_steps <- median(total_steps_per_day$steps)
#TODO: FIXME
print(paste("Mean: ", mean_steps, " Median: ", median_steps, sep=""))

#############################################################################

mdata_no_NA$date <- as.Date(mdata_no_NA$date)
mdata_no_NA$day <- weekdays(mdata_no_NA$date)
mdata_no_NA$day_type <- as.factor(ifelse(mdata_no_NA$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))
#TODO: FINISH THIS
data <- with(mdata_no_NA, aggregate(steps ~ interval + day_type, FUN=mean))
png("./out/Activity_patterns.png", width=500, height =500)

mplot <- ggplot(data, aes(interval, steps)) + geom_point(size=0) + geom_line() + facet_grid(day_type ~ .)
print(mplot)
dev.off()

print("Done")
