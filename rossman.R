install.packages("readr")
library(readr)
library(randomForest)
library('ggplot2')
library('dplyr')

#Rossman
setwd("C:\\Users\\mbilal\\Desktop\\Rossman\\Data")
store<-read.csv("store.csv")
train<-read.csv("train.csv")
test<-read.csv("test.csv")
sample_submission<-read.csv("sample_submission.csv")

train <- merge(train,store)
test <- merge(test,store)

# There are some NAs in the integer columns so conversion to zero
train[is.na(train)]   <- 0
test[is.na(test)]   <- 0

# looking at only stores that were open in the train set
# may change this later
#train <- train[ which(train$Open=='1'),]

# seperating out the elements of the date column for the train set
train$Date<-as.Date(train$Date)
train$Day<-weekdays(train$Date)
train$Month<-months(train$Date)
#train$Year<-years(train$Date)

# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)
train <- train[,-c(3,8)]

# seperating out the elements of the date column for the test set
test$Date<-as.Date(test$Date)
test$Day<-weekdays(test$Date)
test$Month<-months(test$Date)
#test$Year<-year(test$Date)

# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)
test <- test[,-c(4,7)]

mean_sales<-summarize(group_by(train,Store,DayOfWeek,add=TRUE),mean(Sales))
train<-merge(train,mean_sales,by.x=c("Store","DayOfWeek"),by.y=c("Store","DayOfWeek"))
train$Sales_diff_mean<-round(train$Sales-train$`mean(Sales)`)
train<-train[,-c(20)]

