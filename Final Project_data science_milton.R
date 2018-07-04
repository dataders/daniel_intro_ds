#Load required libraries
library(lubridate)
library(dplyr)
library(knitr)

#Import Chicago Crime Map data
#Raw data downloaded as CSV from here (https://data.cityofchicago.org/Public-Safety/Crimes-Map/dfnk-7re6)
ChiCrime <- read.csv(file.choose(), header=TRUE)


# Import Chicago Restaraunt data  
# Raw Data downloaded as CSV from here (https://data.cityofchicago.org/Health-Human-Services/Restaurant/5udb-dr6f/data)
ChicagoRest <- read.csv(file.choose(), header=TRUE)
# Create a dataframe from the source file. 
# Optimize Dataframe by creating vectors from columns 

##Food Inspection Database

RestName <- ChicagoRest$DBA.Name
License <- ChicagoRest$License..
Inspection.Date <- ChicagoRest$Inspection.Date
FastFood <- ChicagoRest$Fast.Food
Address <- ChicagoRest$Address
Zip <- ChicagoRest$Zip
InspectionDate <- ChicagoRest$Inspection.Date
Risk <- ChicagoRest$Risk
Results <- ChicagoRest$Results
Latitude <- ChicagoRest$Latitude
Longitude <- ChicagoRest$Longitude
Location <- ChicagoRest$Location

# Create a new dataframe
ChiRest2 <- data.frame(RestName,License, Inspection.Date, FastFood, Zip, Address, InspectionDate, Risk, Results, Latitude, Longitude, Location)

#The Dataframe will only focus on fast food restaruants 
## no longer doing this --ChiRest2 <- ChiRest2[ChicagoRest$Fast.Food == "TRUE", ]

## no longer doing this - ChiRest2 <- ChiRest2[unique(ChiRest2$Address)]


closures <- ChicagoRest %>%
  #convert times
  mutate(Inspection.Date = mdy(Inspection.Date)) %>%
  #filter for time range
  filter(Inspection.Date >= "2017-01-01" & Inspection.Date <= "2018-06-28") %>%
  #filter for restaurant closures
  filter(Results) %>% #gross not boolean
  #one row = 1 restaurant closure
  select(Results) %>%
  distinct(License, Inspection.Date) %>%
  #summarise by date %>%
  group_by(Inspection.Date) %>%
  summarise(Close.Count = n_distinct(License..)) %>%
  #rename Inspection.Date
  rename(Date = Inspection.Date)


closures <- ChiRest2 %>%
  #convert times
  mutate(Inspection.Date = mdy(Inspection.Date)) %>%
  #filter for time range
  filter(Inspection.Date >= "2017-01-01" & Inspection.Date <= "2018-06-28") %>%
  #filter for restaurant closures
  filter(Results == "Out of Business") %>% #gross not boolean
  #one row = 1 restaurant closure
  select(Results) %>%
  distinct(ChiRest2$Address, ChiRest2$Inspection.Date) %>%
  #summarise by date %>%
  group_by(ChiRest2$Inspection.Date) %>%
  summarise(Close.Count = n_distinct(ChiRest2$Address)) %>%
  #rename Inspection.Date
  rename(Date = ChiRest2$Inspection.Date)

## Chicago Crime Database

CrimeTime <- ChiCrime %>%
  #convert times
  mutate(DATE..OF.OCCURRENCE = mdy_hms(DATE..OF.OCCURRENCE)) %>%
  #create Date variable
  mutate(Date = date(DATE..OF.OCCURRENCE)) %>%
  #summarise by date %>%
  group_by(Date) %>%
  summarise(Book.Count = n_distinct(CASE.))


## Merge Chicago Crime and Restaurant Data
crime.rest <- ChiCrime %>% left_join(closures, by = "Date")


kable(head(crime.rest, n = 12), caption = "Crime Occurrence and Restaurant Closures By Date")



criticals <- ChiRest2 %>%
  #convert times
  mutate(Inspection.Date = mdy(Inspection.Date)) %>%
  #filter for time range
  filter(Inspection.Date >= "2017-01-01" & Inspection.Date <= "2018-06-28") %>%
  #filter for High Risk
  filter(Risk == "Risk 1 (High)") %>%
  #summarise by date %>%
  group_by(Inspection.Date) %>%
  summarise(Risk1.Count = n_distinct(License)) %>%
  #rename Inspection.Date
  rename(Date = Inspection.Date)

crime.high <- ChiCrime %>% left_join(criticals, by = "Date")

kable(head(crime.high, n = 12), caption = "Crime Occurrence and High Risk By Date")


plot(Crime.high, pch=16, col="blue", main="Matrix Scatterplot of Date, Crime, and High Risk")

## Trending and Correlations
library(plotly)

q <- plot_ly(crime.high, x = ~Date, y = ~Risk1.Count, z = ~Book.Count,  marker = list(size = 6))

## Crime and High Risk by Time
library(reshape2)
library(ggplot2)

#reshape data to long for graphing
crime.high.long <- crime.high %>%
  rename(Bookings = Book.Count, High.Risk = Risk1.Count) %>%
  melt(id.vars = "Date", variable.name = "Type", value.name = "Count") %>%
  #add Weekday & Weekend columns
  mutate(Weekday = wday(Date, label = TRUE)) %>%
  mutate(Weekend = ifelse((Weekday == "Sat" | Weekday == "Sun"), TRUE, FALSE))

#plot
ggplot(jail.red.long, aes(Date, Count)) +
  geom_point(aes(color = Weekend)) +
  facet_grid(.~Type)

## Predicting
#model predicting Booking Count using Red Count
book.count.lm1 <- lm(Book.Count ~ Risk1.Count, data = crime.high)
r1 <- summary(book.count.lm1)$r.squared

#model predicting Booking Count using Red Count and Date
book.count.lm2 <- lm(Book.Count ~ Red.Count + Date, data = jail.red)
r2 <- summary(book.count.lm2)$adj.r.squared

Crime.high.days <- crime.high %>%
  #add Weekday & Weekend columns
  mutate(Weekday = wday(Date, label = TRUE)) %>%
  mutate(Weekend = ifelse((Weekday == "Sat" | Weekday == "Sun"), TRUE, FALSE))

#model predicting Booking Count using high Count and Weekend
book.count.lm3 <- lm(Book.Count ~ Risk1.Count + factor(Weekend), data = jail.red.days)
r3 <- summary(book.count.lm3)$adj.r.squared


#model predicting Booking Count using Red Count and Weekend
book.count.lm4 <- lm(Book.Count ~ Risk1.Count + factor(Weekend) + Date, data = crime.high.days)
r4 <- summary(book.count.lm4)$adj.r.squared

Predictors <- c("Risk1.Count", "Risk1.Count + Date", "Risk1.Count + Weekend", "Risk1.Count + Weekend + Date")
r.squared <- c(r1, r2, r3, r4)
kable(data.frame(Predictors, r.squared))
