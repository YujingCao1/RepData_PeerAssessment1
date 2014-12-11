#Load data and process data
activity=read.csv("C:/Users/y6u/Desktop/project1/repdata-data-activity/activity.csv",header=T,sep=",",colClasses=c("integer", "Date", "factor"))
activity$month <- as.numeric(format(activity$date, "%m"))
activity<- na.omit(activity)
rownames(activity) <- 1:nrow(activity)
head(activity)
dim(activity)

#What is mean total number of steps taken per day?
library(ggplot2)
##Make a histogram of total number of steps taken every day
ggplot(activity, aes(date, steps)) + geom_bar(stat = "identity", 
                                              colour = "navy",
                                              fill = "navy", 
                                              width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")
##Calculate and report the mean and median total number of steps taken per day
total_perday=tapply(activity$steps,activity$date,FUN=sum)
###mean of total number of steps taken every day
oldmean=mean(total_perday)
###median of total number of steps taken every day
oldmedian=median(total_perday)

#What is the average daily activity pattern?
total_interval=aggregate(activity$steps,by=list(interval=as.numeric(as.character(activity$interval))),FUN=mean)
names(total_interval)[2]="mean_steps"
##Make a time series plot
ggplot(total_interval, aes(interval,mean_steps)) + geom_line(colour = "navy",width = 0.8)+labs(title ="Time Series Plot of the 5-minute Interval", x = "5-minute interval", y = "Average number of steps taken")
##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
total_interval[total_interval$mean_steps==max(total_interval$mean_steps),]

#Imputing Missing Data
##import original data
orig_data=read.csv("C:/Users/y6u/Desktop/project1/repdata-data-activity/activity.csv",header=T,sep=",",colClasses=c("integer", "Date", "factor"))
orig_data$month <- as.numeric(format(orig_data$date, "%m"))
##the total number of missing values in the dataset
sum(is.na(orig_data))
##fill in all the missing data
##I choose mean of 5-minute interval to imput missing data and make a new dataset
newdata=orig_data
for(i in 1:dim(newdata)[1]){
  if(is.na(newdata$steps[i])){
    newdata$steps[i]=total_interval[which(newdata$interval[i]==total_interval$interval),]$mean_steps
    
  }
}

head(newdata)
sum(is.na(newdata))
##Make a histogram of the total number of steps taken each day based on newdata
ggplot(newdata, aes(date, steps)) + geom_bar(stat = "identity",
                                             colour = "navy",
                                             fill = "navy",
                                             width = 0.7) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day (no missing data)", x = "Date", y = "Total number of steps")
##Calculate and report the mean and median total number of steps taken per day
total_new=tapply(newdata$steps,newdata$date,FUN=sum)
###mean of total number of steps taken every day
newmean=mean(total_new)
###median of total number of steps taken every day
newmedian=median(total_new)
##compare old mean with new mean, and compare old median with new median
oldmean-newmean
oldmedian-newmedian
##After imputing missing data, the newmean equals to oldmean, however, newmedian is a little greater than oldmedian

#Are there differences in activity patterns between weekdays and weekends?
##Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
newdata$weekdays=factor(format(newdata$date,"%A"))
levels(newdata$weekdays)
levels(newdata$weekdays)=list(weekday=c("Monday","Tuesday","Wednesday","Thursday","Friday"),weekend=c("Saturday","Sunday"))
levels(newdata$weekdays)
table(newdata$weekdays)
##Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
interval_weekdays=aggregate(newdata$steps,by=list(interval=as.numeric(as.character(newdata$interval)),weekdays=newdata$weekdays),FUN=mean)
names(interval_weekdays)[3]="meansteps"
ggplot(interval_weekdays, aes(interval,meansteps)) + geom_line(colour = "navy",width = 0.8)+facet_grid(weekdays~ .)+xlab("5-minute interval")+ylab("Average number of steps taken")

library(knitr)
knit2html("PA1_template.Rmd")
