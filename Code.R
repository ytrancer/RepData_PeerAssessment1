# Load libraries

library(readr)
library(tidyverse)
library(Hmisc)

# Load the data

activity <- read_csv("activity.zip", col_types = cols(date = col_date(format = "%Y-%m-%d")))

# Question 1: Make a histogram of the total number of steps taken each day
#             Calculate and report the mean and median total number of steps taken per day

steps_per_day <- activity %>% group_by(date) %>% summarise(sum(steps)) %>%  
                            rename("sum_of_steps" = "sum(steps)")

qplot(steps_per_day$sum_of_steps, 
      geom = "histogram", 
      main = "Steps taken each day", 
      xlab = "Steps", 
      ylab = "Frequency", 
      fill = I("blue"), 
      col = I("red"), 
      alpha = I(.2))

# Mean and median

mean_of_steps <- round(mean(steps_per_day$sum_of_steps, na.rm = TRUE))
median_of_steps <- round(median(steps_per_day$sum_of_steps, na.rm = TRUE))

# Question 2: What is the average daily activity pattern?

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)

avg_steps <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)

ggplot(data = avg_steps, aes(x = interval, y = steps)) 
        + geom_line() 
        + xlab("Intervals") 
        + ylab("Average number of steps")

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

max_steps <- avg_steps %>% arrange(desc(steps)) %>% head(1)

# Question 3: Imputing missing values

# Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)

total_NAs <- sum(is.na(activity))

# Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, you could use the mean/median for that day,
# or the mean for that 5-minute interval, etc.

# Create a new dataset that is equal to the original dataset but with the missing data filled in.

activity_no_NAs <- activity
activity_no_NAs$steps <- impute(activity_no_NAs$steps, fun = mean)
head(activity_no_NAs)

# Make a histogram of the total number of steps taken each day and Calculate and report 
# the mean and median total number of steps taken per day. Do these values differ from the estimates
# from the first part of the assignment? What is the impact of imputing missing data on the estimates 
# of the total daily number of steps?

steps_no_NAs <- activity_no_NAs %>% group_by(date) %>% summarise(sum(steps)) %>% 
                                    rename("sum_of_steps" = "sum(steps)")

qplot(steps_no_NAs$sum_of_steps, 
      geom = "histogram", 
      main = "Steps taken each day (with imputed missing values)", 
      xlab = "Steps", 
      ylab = "Frequency", 
      fill = I("blue"), 
      col = I("red"), 
      alpha = I(.2))

mean_of_steps_no_NAs <- round(mean(steps_no_NAs$sum_of_steps, na.rm = TRUE))
median_of_steps_no_NAs <- round(median(steps_no_NAs$sum_of_steps, na.rm = TRUE))

# Question 4. Are there differences in activity patterns between weekdays and weekends?
# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
# indicating whether a given date is a weekday or weekend day.

activity_no_NAs$day_type <-  ifelse(as.POSIXlt(activity_no_NAs$date)$wday %in% c(0,6), 'weekend', 'weekday')
head(activity_no_NAs)

# Make a panel plot containing a time series plot

avg_steps_no_NAs <- aggregate(steps ~ interval + day_type, data=activity_no_NAs, mean)

ggplot(avg_steps_no_NAs, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(day_type ~ .) +
    xlab("Interval") + 
    ylab("Number of steps")