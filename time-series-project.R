library(readr)
library(lubridate)
library(ggplot2)
library(ggfortify)
library(dplyr)
library(forecast)

## load in data, reformat, subset

# load in data reformat
events <- read_csv("TrainingEvents.csv")
events$EndDate <- as.Date(events$EndDate, format = "%m/%d/%Y")

# pull out year and month
events <- events %>% mutate(month(EndDate), year(EndDate))
events$MonthYr <- format(as.Date(events$EndDate), "%Y-%m")

# subset only approved trainings
eventsSub <- subset(events, Hours != 0)
eventsSub <- subset(eventsSub, EventStatus == "Completed" | EventStatus == "Approved")

# convert data fields to factors
eventsSub$`month(EndDate)` <- format(as.factor(eventsSub$`month(EndDate)`))
eventsSub$`year(EndDate)` <- format(as.factor(eventsSub$`year(EndDate)`))
eventsSub$MonthYr <- format(as.factor(eventsSub$MonthYr))

# split hours into indicators for distance vs classroom
eventsSub$ClassroomHours <- 0
eventsSub$DistanceHours <- 0

for(i in 1:nrow(eventsSub)){
  if(eventsSub$LocationType[i] == "Classroom"){
    eventsSub$ClassroomHours[i] <- eventsSub$Hours[i]
  } else{
    eventsSub$DistanceHours[i] <- eventsSub$Hours[i]
  }
}

## aggregate data

# aggregate events by year
countYr <- aggregate(eventsSub$Hours, by = list(eventsSub$`year(EndDate)`), FUN = length)
fullCountYr <- subset(countYr[8:25,])
colnames(fullCountYr) <- c("year", "numEvents")
countYr <- subset(countYr[17:25,])
colnames(countYr) <- c("year", "numEvents")

# aggregate hours by year
hoursYr <- aggregate(eventsSub$Hours, by = list(eventsSub$`year(EndDate)`), FUN = sum)
hoursYr2 <- aggregate(eventsSub$ClassroomHours, by = list(eventsSub$`year(EndDate)`), FUN = sum)
hoursYr3 <- aggregate(eventsSub$DistanceHours, by = list(eventsSub$`year(EndDate)`), FUN = sum)
hoursYr <- cbind(hoursYr, hoursYr2, hoursYr3)
hoursYr <- subset(hoursYr[8:25,])
hoursYr <- subset(hoursYr, select = -c(Group.1.1, Group.1.2))
colnames(hoursYr) <- c("year", "totalHours", "classroomHours", "distanceHours")

# aggregate hours by month
hoursMonthYr <- aggregate(eventsSub$ClassroomHours, by = list(eventsSub$MonthYr), FUN = sum)
hoursMonthYr <- subset(hoursMonthYr[134:235,])
colnames(hoursMonthYr) <- c("year-month", "classroomHours")

## create possible indicator and regression variables

# create indicators for year and month
hoursMonthYr$year <- 0
hoursMonthYr$month <- 0

for(i in 1:nrow(hoursMonthYr)){
  hoursMonthYr$year[i] <- substr(hoursMonthYr$`year-month`[i], start = 1, stop = 4)
  hoursMonthYr$month[i] <- substr(hoursMonthYr$`year-month`[i], start = 6, stop = 7)
}

# create indicator for prevous year's distance hours
hoursMonthYr$prevYrDist <- 0
for(i in 1:nrow(hoursMonthYr)){
  for(y in 1:nrow(hoursYr)){
    if(hoursMonthYr$year[i] == hoursYr$year[y]){
      hoursMonthYr$prevYrDist[i] <- hoursYr$distanceHours[y]
    }
  }
}

# create indicator for annual training requirement
hoursMonthYr$PRRequire <- 23

for(i in 85:97){
  hoursMonthYr$PRRequire[i] <- 24
}

for(i in 98:102){
  hoursMonthYr$PRRequire[i] <- 16
}

# convert to factors
hoursMonthYr$year <- as.factor(hoursMonthYr$year)
hoursMonthYr$month <- as.factor(hoursMonthYr$month)
hoursMonthYr$PRRequire <- as.factor(hoursMonthYr$PRRequire)

# create time series objects and plot full dataset to get overall perspective
countYrTS <- ts(fullCountYr$numEvents, start = 2001)
autoplot(countYrTS, ylim = c(0, 2500),
         main = "Number of Training Events Offered Per Year") + ylab("Number of Training Events")

hoursYrTS <- ts(hoursYr$totalHours, start = 2001)
autoplot(hoursYrTS, ylim = c(0, 25000), 
         main = "Total Training Hours Offered Per Year") + ylab("Number of Training Hours")

# create time series of selected dataset
hoursMonYrTS <- ts(hoursMonthYr$classroomHours, start = c(2010, 1), frequency = 12)
autoplot(hoursMonYrTS, ylim = c(0, 1000), main = "Classroom-Based Training Hours Offered Per Month") + ylab("Number of Training Hours")

# look at plots to get ideas for models
plot(decompose(hoursMonYrTS))

par(mfrow = c(1, 2))

plot(hoursMonthYr$classroomHours ~ hoursMonthYr$year, xlab = "Year", 
     ylab = "Number of Training Hours", 
     main = "Training Hours as a Function of the Year")

plot(hoursMonthYr$classroomHours ~ hoursMonthYr$month, xlab = "Month of the Year", 
     ylab = "Number of Training Hours", 
     main = "Training Hours as a Function of the Month")

plot(hoursMonthYr$classroomHours ~ hoursMonthYr$prevYrDist, 
     xlab = "Total Distance Training Hours for Previous Year", ylab = "Number of Training Hours",
     main = "Monthly Classroom-Based Training Hours \n by Previous Year's Distance Training Hours")

plot(hoursMonthYr$classroomHours ~ hoursMonthYr$PRRequire, ylab = "Number of Training Hours",
     xlab = "Practitioner Registry Required Annual Training Hours",
     main = "Monthly Classroom-Based Training Hours \n by Annual Training Requirement for Practitioner Registry")

# look at time series autocorrelation plots
par(mfrow = c(1, 2))
acf(ts(hoursMonthYr$classroomHours, start = c(2010, 1)), xlab = "Lag (months)", main = "")
pacf(ts(hoursMonthYr$classroomHours, start = c(2010, 1)), xlab = "Lag (months)", main = "")

# fit regression models

# lm on year -- not significant
lmYear <- lm(classroomHours ~ year, data = hoursMonthYr)

# lm on month
lmMonth <- lm(classroomHours ~ month, data = hoursMonthYr)

# lm on previous year distance training hours -- not significant
lmPrevDist <- lm(classroomHours ~ prevYrDist, data = hoursMonthYr)

# lm on practitioner registry required annual training hours -- not significant
lmPRRequire <- lm(classroomHours ~ PRRequire, data = hoursMonthYr)

# monthly seasonal regression model output
lmMonthSum <- summary(lmMonth)
lmMonthSum

# use auto.arima to get best arima model
arimaHrs <- auto.arima(hoursMonthYr$classroomHours)
arimaHrs

# arima(0, 0, 4) with monthly indicator regression variable
arimaHrsMonth <- arima(hoursMonthYr$classroomHours, order = c(0, 0, 4), xreg = hoursMonthYr$month)
arimaHrsMonth

# fit exponential smoothing model
# ets
etsHrs <- ets(hoursMonYrTS)
etsHrs

# compare models with aic
aicResults <- AIC(etsHrs, lmMonth, arimaHrsMonth, arimaHrs)
aicResults

# model diagnostics
par(mfrow = c(1, 2))
acf(lmMonth$residuals, xlab = "Lag (months)", main = "")
pacf(lmMonth$residuals, xlab = "Lag (months)", main = "")

# compare plots
par(mfrow = c(1, 1))
Predicted <- ts(lmMonth$fitted.values, start = c(2010, 1), frequency = 12)
autoplot(hoursMonYrTS, ylab = "Number of Training Hours", ylim = c(0, 1000),
         main = "Classroom-Based Training Hours Offered Per Month: \n Realized vs. Predicted") + autolayer(Predicted, lwd = 0.8)
