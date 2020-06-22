train <- read.csv("~/Desktop/train.csv")
test <- read.csv("~/Desktop/test.csv")
library(ggplot2)
library(dplyr)
library(caret)
library(lubridate)
library(geosphere)




any(is.na(train))  #no missing values

dim(distinct(train))
dim(train)

#no duplicates

barplot(table(train$vendor_id))    #more from vendor 2

barplot(table(train$passenger_count))

ggplot(train,aes(pickup_longitude,pickup_latitude))+geom_point(aes(color=vendor_id))
#mostly concentrated around the same place 
ggplot(train,aes(droppoff_longitude,dropoff_latitude))+geom_point(aes(color=vendor_id))



#splitting the date 



train$hour<-hour(train$pickup_datetime)
train$month<-month(train$pickup_datetime)


train$Date<-as.Date(train$pickup_datetime)
train$time<-format(as.POSIXct(train$pickup_datetime),format="%H:%M:%S")


train$days<-weekdays(train$Date)
train$days<-as.factor(train$days)

train$days<-ifelse(train$days=="Sunday" | train$days=="Saturday" ,"weekend","notweekend")

train<-train[,-15]
train<-train[,-14]

train<-train[,-3]
train<-train[,-3]


ggplot(train,aes(trip_duration))+geom_histogram(bins=40000,fill="red")+ylab("Density")+coord_cartesian(x=c(0,6000))

summary(train$trip_duration)

ggplot(train,aes(days))+geom_boxplot()

train%>%select(passenger_count,days)%>%group_by(passenger_count,days)%>%summarise(count=n())

#more travels during the weekdays

train%>%select(days,month)%>%group_by(month)%>%summarise(count=n())
#most travels during the month of march 


#handling outliers
Q<-quantile(train$trip_duration, probs=c(.25, .75))
iqr <- IQR(train$trip_duration)
train<- train%>% filter(trip_duration > (Q[1] - 1.5*iqr) &  +trip_duration< (Q[2] + 1.5*iqr))

ggplot(train,aes(days,trip_duration))+geom_boxplot()+facet_grid(~vendor_id)

#mean trip duration is slightly higher during the weekdays



long_pick<-train$pickup_longitude
lat_pick<-train$pickup_latitude
p1<-matrix(c(long_pick,lat_pick),ncol=2)


long_drop<-train$dropoff_longitude
lat_drop<-train$dropoff_latitude
p2<-matrix(c(long_drop,lat_drop),ncol=2)

distance<-distHaversine(p1,p2)/1000    #in km

train$distance<-distance                   #feature creation

train$speed<-(train$distance/(train$trip_duration/3600))


ggplot(train,aes(distance))+geom_histogram(bins=1000,fill="red")+ylab("Density")+coord_cartesian(x=c(0,25))

#most of the distances are between 2.5-3.5kms

ggplot(train,aes(speed))+geom_histogram(bins=9000,fill="red")+ylab("Density")+coord_cartesian(x=c(0,25))

#average speed is 11-12 km/hr

ggplot(train,aes(hour,speed))+geom_point()


corr_features <-train[,c(9,10,11,13,14,4,5,6,7)] 
corrplot(cor(corr_features, use='complete.obs'), type='lower')


train$days<-as.factor(train$days)
train$store_and_fwd_flag<-as.factor(train$store_and_fwd_flag)
train$vendor_id<-as.factor(train$vendor_id)

ggplot(train,aes(as.factor(month),trip_duration))+geom_point()+ geom_smooth(method = 'loess',color="grey10")


train<-train[,-14]
train<-train[,-1]

#adjusting the test set accordlingly


test$hour<-hour(test$pickup_datetime)
test$month<-month(test$pickup_datetime)


test$days<-as.Date(test$pickup_datetime)
test$days<-weekdays(test$days)
test$days<-ifelse(test$days=="Sunday" | test$days=="Saturday" ,"weekend","notweekend")


test<-test[,-3]


long_pick2<-test$pickup_longitude
lat_pick2<-test$pickup_latitude
p1_<-matrix(c(long_pick2,lat_pick2),ncol=2)


long_drop2<-test$dropoff_longitude
lat_drop2<-test$dropoff_latitude
p2_<-matrix(c(long_drop2,lat_drop2),ncol=2)

distance<-distHaversine(p1_,p2_)/1000    #in km

test$distance<-distance


id<-test[,1]

test<-test[,-1]

test$vendor_id<-as.factor(test$vendor_id)
test$store_and_fwd_flag<-as.factor(test$store_and_fwd_flag)
test$days<-as.factor(test$days)


#model
library(gbm)
model<-gbm(trip_duration~.,data=train,distribution="gaussian")
summary(model)

pred<-predict(model,newdata = test,n.trees = 100)

submission<- read.csv("~/Desktop/sample_submission.csv")

submission<-as.data.frame(submission)
names(submission)<-c("id","trip_duration")

write.csv(submission,file="submission1.csv",row.names = FALSE)
