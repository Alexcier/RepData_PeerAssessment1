---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

#Part One
##Loading and Preprocessing the data
The first step in this assignment is to load and preprocess the data.
First however it is necessary to clear the workspace.
```{r}
rm(list=ls())
```

unzip the data
```{r}
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
```
The neccessary libraries are the first thing to load.
```{r}
library(knitr)
library(data.table)
library(ggplot2) 
library(Hmisc)
```

Next the chunk options are to be set to echo="TRUE" so as to allow the reader to 
read the code
```{r}
opts_chunk$set(echo = TRUE, results = 'hold')
```

Now the data is to be loaded, assigning the correct variable type to them
```{r}
activity <- read.csv('activity.csv', header = TRUE, sep = ",",
                  colClasses=c("numeric", "character", "numeric"))
```

Lets have a look at it, observing the first five rows and the names of the variables
```{r}
head(activity)
names(activity)
```

Next lets process the date variable to be read as a date
```{r}
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")
```

Then lets change the interval variable to a factor:
```{r}
activity$interval <- as.factor(activity$interval)
```

Finally for use later, lets create a derived variable from the date variable which
simply denotes the day of the week
```{r}
activity$day<- weekdays(activity$date)
```
And to this we pass an ifelse statement which will denote whether the day falls
on a weekend or weekday
```{r}
activity <- cbind(activity, 
                      weekPos=ifelse(activity$day == "Saturday" | 
                                     activity$day == "Sunday", "weekend", 
                                     "weekday"))
```

After all this processing lets have a quick look at the final results:
```{r}
head(activity)
names(activity)
str(activity)
```

#Part Two
##Mean Total Steps per day

First we perform some simple aggregations into the mask of steps_per_day
```{r}
stepsDate <- aggregate(steps ~ date, activity, sum, na.rm = TRUE)
```

Then we rename the column names, as the mask would have produced the aggregated 
variable as x, whereas we want it to be simply called steps
```{r}
colnames(stepsDate) <- c("date","steps")
```

Now we observe the total number of steps taken per day. 
```{r}
stepsDate
```

Now we create the histogram using ggplot
```{r}
#Create the histogram
ggplot(stepsDate, aes(x = steps)) + 
    geom_histogram(fill = "blue", binwidth = 1000) + 
    labs(title="Steps per Day", 
         x = "No. of Steps per Day", y = "Count within a day")
```

Here we calculate the summary statistics of mean and median
```{r}
meanStepsDate  <- mean(stepsDate$steps, na.rm=TRUE)
medianStepsDate <- median(stepsDate$steps, na.rm=TRUE)
```
As we can see the mean is 
```{r}
meanStepsDate
```
And the Median is
```{r}
medianStepsDate
```

#Part Three
##Average Daily Activity Pattern

Fist thing we need to do is another simple aggregation, using the mean function
to find the average steps taken per interval
```{r}
intSteps <- aggregate(activity$steps, 
                                by = list(interval = activity$interval),
                                FUN=mean, na.rm=TRUE)
```

Next we change the interval figure to a number in our intSteps mask, allowing it to
be plotted.
```{r}
intSteps$interval <- 
    as.integer(levels(intSteps$interval)[intSteps$interval])
```

Then we proceed to change the column names of our intSteps mask for easy referal.
```{r}
names(intSteps) <- c("interval", "steps")
```

Finally lets have a quick look at the intSteps mask
```{r}
head(intSteps)
```
As everthing seems alright, we will proceed

Now we once more use ggplot to plot the time-series analysis
```{r}
ggplot(intSteps, aes(x=interval, y=steps)) +   
    geom_line(color="blue", size=1) +  
    labs(title="Average Daily Activity Pattern", x="Interval", y="No. of steps") 
```

For the last of this section we will find the maximum steps interval
```{r}
maxInt <- intSteps[which.max(  
    intSteps$steps),]
maxInt
```
Thereby the maximum interval is the 835th with 206 steps

#Part Four
##Impute the missing values

Firstly we need to find the number of missing values/ rows with NA
```{r}
missing_values <- sum(is.na(activity$steps))
```

To impute the missing values we first create a mask to hold the newly create dataset
in, impMissVal
```{r}
impMissVal <- activity
```

From this we have choosen to use the Hmisc package in R to impute the missing values,
simply choosing the mean value as the imputed figure.
```{r}
impMissVal$steps <- impute(activity$steps, fun=mean)
```

As we have previously seen, the first several observations of steps have NA values.
Lets have a quick look to see if this is still the case
```{r}
head(impMissVal)
```
This dataset (impMissVal) is therefore equal to the original dataset with missing values filled in and additional derived variables for later use.

Now, lets compress the data to the granular level of the date variable, summing all 
the steps. Then, as before, lets change the column names to make them recognisable.
```{r}
totalStepsDate <- aggregate(steps ~ date, impMissVal, sum)
colnames(totalStepsDate) <- c("date","steps")
```

Finally lets create the histogram to compare with the first
```{r}
ggplot(totalStepsDate, aes(x = steps)) + 
    geom_histogram(fill = "red", binwidth = 1000) + 
    labs(title="Histogram of Steps Taken per Day", 
         x = "No. Steps per Day", y = "No. times in a day")
```

#Part Five
##Difference in activity patterns between weekends and weekdays

The factor variable "week position" (weekPos) was previously created in our data preparation and processing phase. To verify that it is a factor variable with two outputs still in the Imputed Missing values Dataset (impMissVal):
```{r}
str(impMissVal)
```

First we aggregate the activity patterns into the average imputed missing value mask 
(avImpMissVal) based on the averages for different days, keeping the week position variable (weekPos), the day variable (day), for now, and the interval itself.
```{r}
avImpMissVal <- aggregate(impMissVal$steps, 
                       by=list(impMissVal$weekPos, 
                               impMissVal$day, impMissVal$interval), mean)
```

Next we look at the final dataset, changing the names of the various variables for readabiliy
```{r}
head(avImpMissVal)
names(avImpMissVal) <- c("weekPos", "day", "interval", "average")
```


Finally lets knock together the graph, comparing weekends to weekdays
```{r}
ggplot(avImpMissVal, aes(interval, average)) + 
    geom_line(colour = "blue") + 
    facet_wrap(~ weekPos, nrow = 2, ncol = 1) +
    xlab("5-minute intervals") + 
    ylab("Average number of steps")
```

As we can see the both of these observation groupings roughly mirror each other, with slight discretions, in particuliar in the mornings where people seem more active at an early time.
