rm(list=ls())
macro <- read.csv(paste0(getwd(),"/macro.csv"))
train<- read.csv(paste0(getwd(),"/train.csv"))
test<- read.csv(paste0(getwd(),"/test.csv"))

#### Can not use Time series as data depends on size and all ##########
library(forecast)
acf(train$price_doc)
pacf(train$price_doc)
log_price<-log(train$price_doc)
acf(log_price)
pacf(log_price)
auto.arima(log_price)
plot(log_price)
plot()
plot(y=train$price_doc,x=train$timestamp)
first_diff<-diff(log_price)
acf(first_diff)
pacf(first_diff)
auto.arima(first_diff)

#######################################################################

train_merge<-merge(train,macro, by = "timestamp")
train1<-train
train_merge$log_price<-log(train_merge$price_doc)
train1$log_prices<-log(train1$price_doc)

### full_sq: total area in square meters, including loggias, balconies and other
### non-residential areas
summary(train1$full_sq)
plot(train1$full_sq)
hist(train1$full_sq)
density_full_sq<-density(train1$full_sq)
plot(density_full_sq)
density_full_sq$x[which.max(density_full_sq$y)]

#table(train1$full_sq)

## life_sq: living area in square meters, excluding loggias, balconies and other 
## non-residential areas
summary(train1$life_sq)
plot(train1$life_sq)
density_life_sq<-density(train1$full_sq)
plot(density_life_sq)
#table(train1$life_sq)


## Not possible case
View(train1[c(which(train1$full_sq<0)),])
View(train[c(which(train$full_sq<train$life_sq)),])
View(train1[-c(which(train1$full_sq==c(0,1))),])
## removed where full_sq=0 or 1 as that can not be the case
train1<-train1[-c(which(train1$full_sq==c(0,1))),]
## removed where full_sq can not be greater than life_sq as that can not be the case
train1<-train1[-c(which(train1$full_sq<train1$life_sq)),]

###### FOr NA's in Life Sq we place full_sq=life_sq
View(train1[c(which(is.na(train1$life_sq))),])
View(train1[c(which(is.na(train1$life_sq) & is.na(train1$max_floor))),])
View(train1[c(which(is.na(train1$life_sq) & !is.na(train1$max_floor))),])
train1[c(which(is.na(train1$life_sq))),"life_sq"]<-train1[c(which(is.na(train1$life_sq))),"full_sq"]


##floor: for apartments, floor of the building
summary(train1$floor)
summary(train1$max_floor)
View(train1[c(which(is.na(train1$floor))),])
View(train1[c(which(is.na(train1$floor) & is.na(train1$max_floor))),])
## All values where floor is na there max floor is also NA Therefore Replacing it by median
#train2<-train1[,c(1,2,3,4)]
train1[c(which(is.na(train1$floor))),"floor"]<-median(train1$floor,na.rm = TRUE)

##max_floor: number of floors in the building
summary(train1$max_floor)
summary(train1$max_floor-train1$floor)
density_diff_floor<-density(train1$max_floor-train1$floor,na.rm = TRUE)
plot(density_diff_floor)
View(train[c(which(train$max_floor<train$floor)),])
View(train1[c(which(train1$max_floor<train1$floor)),])
