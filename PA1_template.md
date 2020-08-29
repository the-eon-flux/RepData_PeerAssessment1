---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---




## R Markdown report for analysing  personal activity dataset

Dataset obtained from here. [Activity Monitoring Data Link](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)   
Accessed on this date 2020-08-29  

Let's get into it in the following steps :

**1. Loading and preprocessing the data**


```r
#Downloading the data first
        InputFileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        download.file(InputFileUrl, "./Data.zip", method = "curl")
        unzip("./Data.zip")

# Reading the data 
        InputData <- read.csv("./activity.csv", header = T, stringsAsFactors = F)
        # Doing some checks
        dim(InputData)
```

```
## [1] 17568     3
```

```r
        summary(InputData)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

Data loading is completed as you can see.  
  
**2. What is mean total number of steps taken per day?**  


```r
# Reformatting the date column and extracting date
InputData$date <- as.Date(InputData$date)

# Histogram of ( total steps each day )
        Steps_Daywise <- aggregate(InputData[,1], list(InputData[,2]), sum)
        colnames(Steps_Daywise) <- c("Day", "Steps")
        
# Plotting 
        ggplot(Steps_Daywise, aes(x=Steps)) + geom_histogram(bins=10,color="white", fill="black") + geom_histogram(binwidth=2) +labs(title = "Histogram plot",
       subtitle = "For original data",
       y = "Frequency", x = "Number of steps")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).

## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/Mean_steps-1.png)<!-- -->

```r
# Mean & median (total steps each day)
        Mean_Of_DaywiseSteps <- summary(Steps_Daywise$Steps)[4]
        Median_Of_DaywiseSteps <- summary(Steps_Daywise$Steps)[3]
        summary(Steps_Daywise$Steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10765   10766   13294   21194       8
```

 
Moving on next thing to calculate.

**3. What is the average daily activity pattern ? **

a. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
New_df <- melt(InputData, id =2:3, na.rm = T)
Timeseries_Data <- dcast(New_df, interval~variable, mean)

ggplot( data = Timeseries_Data, aes(interval, steps)) + geom_line(color = "steelblue",size = 1) +
  geom_point(color="steelblue") + labs(title = "Timeseries plot",
       subtitle = "For original data",
       y = "Avg steps across days", x = "5-minute interval")
```

![](PA1_template_files/figure-html/Time series plot-1.png)<!-- -->


b. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
# Following 5 minutes interval has the max steps
Timeseries_Data[ which.max(Timeseries_Data$steps),1]
```

```
## [1] 835
```

**4. Imputing missing values**

As there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.  


a. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
# Total missing values (steps measurements)
NA_Indices <- is.na(InputData$steps)
sum(NA_Indices) # rows
```

```
## [1] 2304
```

b. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.



```r
# We will be replacing the missing values with mean for that 5-minute interval

# Following fn gets the mean for a particular 5-minute interval
Get_Steps_For_Interval <- function(Interval){ Timeseries_Data[ Timeseries_Data$interval == Interval, 2]}

# Creating a vec filling the missing data
Steps_For_Interval <- sapply(InputData[NA_Indices, 3 ], Get_Steps_For_Interval)

# Adding a vector of replaced values
TidyData <- InputData

# NewData with missing values filled
TidyData[NA_Indices, 1] <- mapply(function(x,y){x=y}, TidyData[NA_Indices, 1], ceiling(Steps_For_Interval))
```

Our new dataset is ready. Let's see the impact of imputing missing data.


```r
# Histogram of ( total steps each day )  
        Revised_Steps_Daywise <- aggregate(TidyData[,1], list(TidyData[,2]), sum, na.rm = T)  
        colnames(Revised_Steps_Daywise) <- c("Day", "Steps")  
# Plotting
        ggplot(Revised_Steps_Daywise, aes(x=Steps)) +
          geom_histogram(bins=10,color="white", fill="black") + 
                geom_histogram(binwidth=2) +labs(title = "Histogram plot",
       subtitle = "For newly imputated data", y = "Frequency", x = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
# Mean & median (total steps each day)  
        Rev_Mean_Of_DaywiseSteps <- summary(Revised_Steps_Daywise$Steps)[4]  
        Rev_Median_Of_DaywiseSteps <- summary(Revised_Steps_Daywise$Steps)[3]  
        summary(Revised_Steps_Daywise$Steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10909   10785   12811   21194
```

Impact of imputing missing data summarised :   
  - Y-Axis (Frequency) of Revised Histogram of ( total steps each day ) values have changed. Resemblance appears to be same as above ( but the revised plot seems to be more approaching to standard Normal distribution ).  
  - Mean appears to be shifted from 1.0766189\times 10^{4} to 1.0784918\times 10^{4} which is expected as new data points are added.  
  - Median also appears to be shifted from 1.0765\times 10^{4} to 1.0909\times 10^{4} which is expected as new data points are added.  

One interesting thing to know would be to ask this following question.    

**5. Are there differences in activity patterns between weekdays and weekends?**

Lets first create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.  We will be using the imputed data for this.  


```r
TidyData$WeekPeriod <- rep(NA, dim(TidyData)[1])
TidyData$WeekPeriod <- as.factor(sapply(TidyData$date, function(x){ifelse( (wday(x) %in% c(1,7) ),"weekend","weekday" )}))


New_df <- melt(TidyData, id =2:4, na.rm = T)
Rev_Timeseries_Data <- dcast(New_df, interval + WeekPeriod~variable , mean)


ggplot( data = Rev_Timeseries_Data, aes(interval, steps)) + geom_line(color = "steelblue",size = 1) +
  geom_point(color="steelblue") + labs(title = "Timeseries plot",
       subtitle = "For newly imputated data",
       y = "Avg steps across days", x = "5-minute interval") + facet_wrap(~WeekPeriod)
```

![](PA1_template_files/figure-html/Weeday_VS_Weekend_Activity-1.png)<!-- -->



**We can see that the person is more active on weekends than on weekdays. Probably working in the office most of the time in weekdays. **  


***









