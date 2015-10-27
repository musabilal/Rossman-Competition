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

#Feature using Store,DayOfWeek and Sales
mean_sales<-summarize(group_by(train,Store,DayOfWeek,add=TRUE),mean(Sales))
train<-merge(train,mean_sales,by.x=c("Store","DayOfWeek"),by.y=c("Store","DayOfWeek"))
train$Sales_diff_mean<-round(train$Sales-train$`mean(Sales)`)
train<-train[,-c(19)]
qplot(train$Sales_diff_mean)
train$Sales_diff_mean<-abs(train$Sales_diff_mean)
train$Sales_diff_mean<-cut(train$Sales_diff_mean,breaks=c(-500,233,801,1631,7738,29829),labels=c("extremely_low","low","moderate","high","extremely_high"))
train$ref<-1
total_count<-summarize(group_by(train,Store,DayOfWeek),sum(ref))
names(total_count)<-c("Store","DayOfWeek","total_count")
train<-merge(train,total_count)
sales_diff_count<-summarize(group_by(train,Store,DayOfWeek,Sales_diff_mean),sum(ref))
names(sales_diff_count)<-c("Store","DayOfWeek","Sales_diff_mean","total_count_diff")
train<-merge(train,sales_diff_count)
train$avg_diff_prob<-train$total_count_diff/train$total_count
train<-train[,-c(20,21,22)] #Remove the temporary variables created
train<-train[,-c(3)] #Remove Sales_diff_mean

#Applying this new attribute to the test set
avg_diff_prob<-summarize(group_by(train,Store,DayOfWeek),max(avg_diff_prob))
test<-merge(test,avg_diff_prob)
names(test)[18]<-c("avg_diff_prob")

#Using Store,DayOfWeek,StoreType and checking Customers against it

#On train
mean_customers<-summarize(group_by(train,Store,DayOfWeek,StoreType,SchoolHoliday),mean(Customers))
names(mean_customers)<-c("Store","DayOfWeek","StoreType","SchoolHoliday","feature_2")
train<-merge(train,mean_customers)
train$Customer_difference<-abs(train$Customers-train$feature_2)/train$Customers
train<-train[,-which(names(train) %in% c("feature_2"))]
train$Customer_difference[is.infinite(train$Customer_difference)]<-0

#On test
mean_customers_test<-train[,which(names(train) %in% c("Store","DayOfWeek","StoreType","SchoolHoliday","Customer_difference"))]
test<-merge(test,mean_customers)