
# Download and read the data
#download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
              #destfile="StormData.csv.bz2")
data<-read.csv("StormData.csv.bz2")

#Find out what we have
str(data)

#Store original, unedited data as csv
write.csv(data, file="StormData.csv")

#Find total Property Damage for each obs in K $

data$totalProp[which(data$PROPDMGEXP!="K" & data$PROPDMGEXP!="M" & data$PROPDMGEXP!="B")]<-0
data$totalProp[which(data$PROPDMGEXP=="K")]<-data$PROPDMG[which(data$PROPDMGEXP=="K")]
data$totalProp[which(data$PROPDMGEXP=="M")]<-data$PROPDMG[which(data$PROPDMGEXP=="M")]*1000
data$totalProp[which(data$PROPDMGEXP=="B")]<-data$PROPDMG[which(data$PROPDMGEXP=="B")]*1000000

#Find total crop damage for each obs in K $

data$totalCrop[which(data$CROPDMGEXP!="K" & data$CROPDMGEXP!="M" & data$CROPDMGEXP!="B")]<-0
data$totalCrop[which(data$CROPDMGEXP=="K")]<-data$CROPDMG[which(data$CROPDMGEXP=="K")]
data$totalCrop[which(data$CROPDMGEXP=="M")]<-data$CROPDMG[which(data$CROPDMGEXP=="M")]*1000
data$totalCrop[which(data$CROPDMGEXP=="B")]<-data$CROPDMG[which(data$CROPDMGEXP=="B")]*1000000



#---------------------------------------------

#What types of events are most harmful to population health?
#We will look at FATALITIES and INJURIES by EVTYPE

#Create a new var of total people harmed
data$totalHarmed<-data$FATALITIES+data$INJURIES

#Find the sum of totalHarmed across each EVTYPE and make a df
x<-tapply(data$totalHarmed, data$EVTYPE, sum, na.rm=TRUE)
EVTYPES<-unique(data$EVTYPE)
x<-data.frame(EVTYPE=EVTYPES, totalHarmed=as.numeric(x))
require(dplyr)
x<-arrange(x, desc(totalHarmed))

#Look at the top six most harmful event types
#The fact that we choose top 6 is somewhat arbitrary. It just looks nice :)
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

#---------------------------------------------

#What types of events have the greatest economic impact?
#Need to look at PROPDMG and CROPDMG by EVTYPE

#Create a new var of total damage
data$totalDamage<-rowSums(data[,c("totalProp","totalCrop")])

#Find the sum of totalDamage across each EVTYPE and make a df
y<-tapply(data$totalDamage, data$EVTYPE, sum, na.rm=TRUE)

y<-data.frame(EVTYPE=EVTYPES, totalDamage=as.numeric(y))
y<-arrange(y, desc(totalDamage))

#Look at the top six most harmful event types
#The fact that we choose top 6 is somewhat arbitrary. It just looks nice :)
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