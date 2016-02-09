#Title

###Introduction

Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

In this report, we explore the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. The database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

The data for this report come in the form of a comma-separated-value file compressed via the bzip2 algorithm to reduce its size. The data can be downloaded using the following link:

[Storm Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2) [47 Mb]

The following code downloads and saves the data to the user's working directory, then loads the data into R.

```{r, echo=TRUE}
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
  destfile="StormData.csv.bz2")
data<-read.csv("StormData.csv.bz2")
```

There is also some documentation of the database available. Within this documentation, we can see how some of the variables are constructed and/or defined.

[National Weather Service Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)  
[National Climatic Data Center Storm Events FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf)

The events in the database start in the year 1950 and end in November 2011. In the earlier years of the database there are generally fewer events recorded, most likely due to a lack of good records. More recent years should be considered more complete.

Once the data set is loaded into R, we can see the structure of the data using:

```{r, echo=TRUE}
str(data)
```

The data set consists of 902297 observations. Each observation is one storm event. We use the terms observation and event interchangeably.

To protect our raw data, we write it to a new CSV file in the working directory.

```{r, echo=TRUE]
write.csv(data, file="StormData.csv")
```

First, we will investigate which types of storm events have had the greatest impact on humans. From ```str(data)``` and from the documentation files, we know that  the variables ```FATALITIES``` and ```INJURIES``` represent the total number of reported human deaths and injuries respectively for each storm event.

We now create a new variable called ```totalHarmed``` which is the sum of the number of fatalities and of injuries for each event.

```{r, echo=TRUE]
data$totalHarmed<-rowSums(data[c,("FATALITIES","INJURIES")])
```

Since we are interested in knowing the storm type (```EVTYPE```) that has caused the most harm to humans, we find the sum of ```totalHarmed``` within each ```EVTYPE``` and then reorder the data set with ```totalHarmed``` in descending order.

```{r,. echo=TRUE}
x<-tapply(data$totalHarmed, data$EVTYPE, sum, na.rm=TRUE)
EVTYPES<-unique(data$EVTYPE)
x<-data.frame(EVTYPE=EVTYPES, totalHarmed=as.numeric(x))
x<-arrange(x, desc(totalHarmed))
```

From ```str(x)``` we see that there are 985 different types of storm events. 

```{r, echo=TRUE}
str(x)
```

While it may be possible to represent all 985 types of storm events graphically, we will plot only the top six most harmful event types for humans. We are choosing six somewhat arbitrarily; this yields a nice, readable plot while also clearly showing what storm types are the most harmful for humans.

```{r, echo=TRUE}
xtop<-x[1:6,]

barplot(xtop$totalHarmed, 
        col=rainbow(6, s = 1, v = 1, start = .5, end = .75, alpha = 1),
        xlab="Event Type",
        ylab="Number of People Injured or Killed",
        main="Storm Events with the Greatest Human Impact"
        )
legend("topright",
       tolower(xtop$EVTYPE),
       cex=.6, 
       fill=rainbow(6, s = 1, v = 1, start = .5, end = .75, alpha = 1)
       )
```

Again, the top six most harmful storm event types for humans, in order of severity, are:
```{r, echo=TRUE}
tolower(xtop$EVTYPE)
```
with the most injuries and deaths occurring as a result of:
```{r, echo=TRUE}
tolower(xtop$EVTYPE[1])
```

The next question we will investigate is what storm event type has had the greatest economic impact. To analyze this, we will consider ```PROPDMG``` and ```CROPDMG``` which show the dollar value of damage to property and agriculture respectively measured in US dollars. The variables ```PROPDMGEXP``` and ```CROPDMGEXP``` provide modifiers for the dollar values given in ```PROPDMG``` and ```CROPDMG```; "K" represents thousands of dollars, "M" represents millions of dollars, and "B" represents billions of dollars. 

To analyze the total economic impact of each storm type, two new variables are created: ```totalProp``` which represents the total value of property damage measured in thousands of dollars and ```totalCrop``` which represents the total value of agricultural damage measured in thousands of dollars

```{r, echo=TRUE}
data$totalProp[which(data$PROPDMGEXP=="")]<-0
data$totalProp[which(data$PROPDMGEXP=="K")]<-data$PROPDMG[which(data$PROPDMGEXP=="K")]
data$totalProp[which(data$PROPDMGEXP=="M")]<-data$PROPDMG[which(data$PROPDMGEXP=="M")]*1000
data$totalProp[which(data$PROPDMGEXP=="B")]<-data$PROPDMG[which(data$PROPDMGEXP=="B")]*1000000


data$totalCrop[which(data$CROPDMGEXP=="")]<-0
data$totalCrop[which(data$CROPDMGEXP=="K")]<-data$CROPDMG[which(data$CROPDMGEXP=="K")]
data$totalCrop[which(data$CROPDMGEXP=="M")]<-data$CROPDMG[which(data$CROPDMGEXP=="M")]*1000
data$totalCrop[which(data$CROPDMGEXP=="B")]<-data$CROPDMG[which(data$CROPDMGEXP=="B")]*1000000
```

Next, the values of ```totalProp``` and ```totalCrop``` are combined to form the value of ```totalDamage```. This variable represents the total dollar value of all damage resulting from each observation measured in thousands of dollars.

```{r, echo=TRUE}
data$totalDamage<-rowSums(data[,c("totalProp","totalCrop")])
```

As previously stated, we are interested in knowing what storm event type (```EVTYPE```) has had the greatest economic impact. To determine this, the values of ```totalDamage``` are summed within each ```EVTYPE```. The event types are  then sorted in decreasing order (most severe first).

```{r, echo=TRUE}
y<-tapply(data$totalDamage, data$EVTYPE, sum, na.rm=TRUE)

y<-data.frame(EVTYPE=EVTYPES, totalDamage=as.numeric(y))
y<-arrange(y, desc(totalDamage))
```

Again, from ```str(y)``` we know that there are 985 different types of storm events. 

```{r, echo=TRUE}
str(x)
```

And, as before when we considered human impact of storm types, while it may be possible to represent all 985 types of storm events graphically, we will plot only the top six most harmful event types for humans. To reiterate, we are choosing six somewhat arbitrarily; this yields a nice, readable plot while also clearly showing what storm types are the most harmful for humans.

```{r, echo=TRUE}
ytop<-y[1:6,]

barplot(ytop$totalDamage, 
        col=rainbow(6, s = 1, v = 1, start = .9, end = .1, alpha = 1),
        xlab="Event Type",
        ylab="Total Damage",
        main="Storm Events with the Greatest Economic Impact"
        )
legend("topright",
       tolower(ytop$EVTYPE),
       cex=.6, 
       fill=rainbow(6, s = 1, v = 1, start = .9, end = .1, alpha = 1)
       )
```