Reproducible Research: Peer Assessment 1
========================================

For this project, we will process human activity data recorded on a
cellular phone.

1. Loading and preprocessing the data
-------------------------------------

First we will load all the required libraries. We will read the file
activity.csv Ensure you modify your working directory.

    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(ggplot2)
    library(stringr)
    library(lattice)

    setwd("C:/Users/IP/Documents")
    activity<-read.table("activity.csv", sep=",",header=TRUE,  na.strings = "NA", 
    quote="'", stringsAsFactors = FALSE)

Let us have a look at the dataset "activity". Column names and values
are displayed with extra double quotes.

    head(activity)

    ##   X.steps      X..date.. X..interval...
    ## 1     "NA ""2012-10-01""             0"
    ## 2     "NA ""2012-10-01""             5"
    ## 3     "NA ""2012-10-01""            10"
    ## 4     "NA ""2012-10-01""            15"
    ## 5     "NA ""2012-10-01""            20"
    ## 6     "NA ""2012-10-01""            25"

Let us rename the column names and arrange the values by removing double
quotes and setting the right formats.

    names(activity)<-c("steps", "date", "interval")

    #removing quotes from values
    activity$steps<-gsub('"', "",activity$steps)
    activity$date<-gsub('"',"", activity$date)
    activity$interval<-gsub('"', "",activity$interval)

    #converting the dates to Date class
    activity$date<-as.Date(activity$date, "%Y-%m-%d")

    #converting the steps and  intyerval into numeric
    activity$steps<-as.numeric(activity$steps)

    ## Warning: NAs introduits lors de la conversion automatique

    activity$interval<-as.numeric(activity$interval)

Now the dataset look tidy.

    head(activity)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

2. What is mean total number of steps taken per day?
----------------------------------------------------

Let us have a look on the histogram of total daily steps.

    # Plotting histogram of total daily steps
    byday<-group_by(activity, date)
    activitybyday<-summarise(byday, dailysteps=sum(steps))
    hist(activitybyday$dailysteps, breaks=40, main="Daily steps histogram",
         xlab="number of daily steps", col="dark green")

![](PA1_template_files/figure-markdown_strict/histogram%20of%20total%20daimy%20steps-1.png)

3. What is the average daily activity pattern?
----------------------------------------------

    activitymeanmedian<-summarise(byday,meansteps=mean(steps, na.rm=TRUE), 
                                  mediansteps=median(steps, na.rm=TRUE))
    print(activitymeanmedian)

    ## Source: local data frame [61 x 3]
    ## 
    ##          date meansteps mediansteps
    ##        (date)     (dbl)       (dbl)
    ## 1  2012-10-01       NaN          NA
    ## 2  2012-10-02   0.43750           0
    ## 3  2012-10-03  39.41667           0
    ## 4  2012-10-04  42.06944           0
    ## 5  2012-10-05  46.15972           0
    ## 6  2012-10-06  53.54167           0
    ## 7  2012-10-07  38.24653           0
    ## 8  2012-10-08       NaN          NA
    ## 9  2012-10-09  44.48264           0
    ## 10 2012-10-10  34.37500           0
    ## ..        ...       ...         ...

Let us now plot the average number of steps by interval:

    byinterval<-group_by(activity, interval)
    activitybyinterval<-summarise(byinterval, averagesteps=mean(steps, na.rm=TRUE))
    plot(activitybyinterval$interval, activitybyinterval$averagesteps, type="h", main="Average number of steps by interval"
         , xlab="Interval", ylab="Average number of steps", col="dark blue")

![](PA1_template_files/figure-markdown_strict/plotting%20the%20average%20number%20of%20steps%20by%20interval-1.png)

We will also find the interval on which the maximum number of steps in
average occurs.

    print(paste("The maximum number of steps occurs at interval", 
          activitybyinterval$interval[which.max(activitybyinterval$averagesteps)], 
          "on average across all days."))

    ## [1] "The maximum number of steps occurs at interval 835 on average across all days."

4. Imputing missing values
--------------------------

First let us check how many values are missing:

    missing<-is.na(activity$steps)
    table(missing)

    ## missing
    ## FALSE  TRUE 
    ## 15264  2304

    print("There are 2304 missing values in the activity dataset.")

    ## [1] "There are 2304 missing values in the activity dataset."

Then, let us create a new dataset whith imputed missing values: NAs are
replaced by the mean valuee for the same interval. We will create and
use the dataset "activityfilled".

    activityfilled<-activity 
    fillingbyinterval<-function(x, intervalofx){
        if (is.na(x) | is.nan(x))   {
         x<-activitybyinterval$averagesteps[activitybyinterval$interval==intervalofx]   
        } else {
            x=x
        }
    }

    for(i in 1:17568) {activityfilled$steps[i]<-fillingbyinterval(activityfilled$steps[i], activityfilled$interval[i])}

    head(activityfilled)

    ##       steps       date interval
    ## 1 1.7169811 2012-10-01        0
    ## 2 0.3396226 2012-10-01        5
    ## 3 0.1320755 2012-10-01       10
    ## 4 0.1509434 2012-10-01       15
    ## 5 0.0754717 2012-10-01       20
    ## 6 2.0943396 2012-10-01       25

We can compare the "activityfilled" dataset to the "activity"" dataset
and check there are no more missing values:

    missingfilled<-is.na(activityfilled$steps)
    table(missingfilled)

    ## missingfilled
    ## FALSE 
    ## 17568

Now we can plot the histogram of daily total steps when all missing
values are replaced.

    bydayfilled<-group_by(activityfilled, date)
    activityfilledbyday<-summarise(bydayfilled, dailysteps=sum(steps))
    hist(activityfilledbyday$dailysteps, breaks=40, main="Daily steps histogram, no missing values",
         xlab="number of daily steps", col="dark blue")

![](PA1_template_files/figure-markdown_strict/plotting%20histogram%20of%20total%20daily%20steps,%20no%20missing%20values-1.png)

We can notice that the maximum frequency that occurs at the range
11000-12000 is now higher than in the original dataset (12 vs. 7). Let
us check the mean and median values once the NAs are replaced:

    activityfilledmeanmedian<-summarise(bydayfilled,meansteps=mean(steps, na.rm=TRUE), 
                                  mediansteps=median(steps, na.rm=TRUE))
    print(activityfilledmeanmedian)

    ## Source: local data frame [61 x 3]
    ## 
    ##          date meansteps mediansteps
    ##        (date)     (dbl)       (dbl)
    ## 1  2012-10-01  37.38260    34.11321
    ## 2  2012-10-02   0.43750     0.00000
    ## 3  2012-10-03  39.41667     0.00000
    ## 4  2012-10-04  42.06944     0.00000
    ## 5  2012-10-05  46.15972     0.00000
    ## 6  2012-10-06  53.54167     0.00000
    ## 7  2012-10-07  38.24653     0.00000
    ## 8  2012-10-08  37.38260    34.11321
    ## 9  2012-10-09  44.48264     0.00000
    ## 10 2012-10-10  34.37500     0.00000
    ## ..        ...       ...         ...

5. Are there differences in activity patterns between weekdays and weekends?
----------------------------------------------------------------------------

To answer this question, we will create a factor variable to check which
day is a weekday or a weekend.

    typeofday<-function(jour){
        if(tolower(weekdays(jour)) %in% c("samedi", "dimanche")) {
            "weekend"
        } else {
            "weekday"
        }
    }

We will add a new column "type" in which will be stored the type of
weekday according to its date:

    activityfilled<-mutate(activityfilled, type=NULL)
    for (j in 1:17568){
        activityfilled$type[j]<-typeofday(activityfilled$date[j])
    }
    head(activityfilled)

    ##       steps       date interval    type
    ## 1 1.7169811 2012-10-01        0 weekday
    ## 2 0.3396226 2012-10-01        5 weekday
    ## 3 0.1320755 2012-10-01       10 weekday
    ## 4 0.1509434 2012-10-01       15 weekday
    ## 5 0.0754717 2012-10-01       20 weekday
    ## 6 2.0943396 2012-10-01       25 weekday

Let us group by interval and type. Then we plot using lattice system.

    filledbyinterval<-group_by(activityfilled, interval, type)
    activityfilledbyinterval<-summarise(filledbyinterval, averagesteps=mean(steps, na.rm=TRUE))

    xyplot(averagesteps ~ interval | type, data= activityfilledbyinterval, layout=c(1, 2), type="l")

![](PA1_template_files/figure-markdown_strict/weekday%20vs.%20weekend%20number%20of%20steps-1.png)

    #releasing the graphical device
    dev.off()

    ## null device 
    ##           1

    dev.set(which = 2)

    ## null device 
    ##           1

**As a conclusion, we can see that on the weekends, the number of steps
is greater during the interval 800-950 by around 70 steps compared to
the weekend.**
