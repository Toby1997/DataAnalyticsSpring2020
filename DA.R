
## Importing Data
library(Hmisc)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(kknn)
pollution <- read.csv(file.choose( "Desktop/DA/project/Beijing_Weather_Airpollution.csv"),header = T)
head(pollution)
str(pollution)

## Basic statistical infomation
describe(pollution)
dim(pollution)
sum(is.na(pollution)) # 0 ==> no missing data.
colnames(pollution)
sapply(pollution, class)
str(pollution)
sum(is.na(pollution)) # 0 ==> no missing data.
## pollution Visualization
#some density plot
ggplot(data=pollution,aes(x=PM2.5))+geom_density()
ggplot(data=pollution,aes(x=pollution$PM2.5))+geom_density(aes(x=PM2.5, cut=Type))
ggplot(data=pollution,aes(x=pollution$PM2.5))+geom_density(aes(x=PM2.5, fill=weather))

ggplot(data=pollution,aes(x=pollution$PM10))+geom_density()
ggplot(data=pollution,aes(x=pollution$NO2))+geom_density()
ggplot(data=pollution,aes(x=pollution$CO))+geom_density()
ggplot(data=pollution,aes(x=pollution$O3))+geom_density()
#some bar plot
ggplot(data=pollution,aes(x=pollution$Type))+geom_bar()
ggplot(data=pollution,aes(x=pollution$weather))+geom_bar()
ggplot(data=pollution,aes(x=pollution$Type))+geom_bar(aes(x=Type, fill=Type))+coord_flip()
ggplot(data=pollution,aes(x=pollution$Type))+geom_bar(aes(x=weather, fill=weather))+coord_flip()
ggplot(data=pollution,aes(x=pollution$Type))+geom_bar(aes(x=Type, fill=weather))+coord_polar()
library(caTools)
set.seed(123)
split = sample.split(pollution$PM2.5, SplitRatio = 0.8)
train1 = subset(pollution, split == TRUE)
test1 = subset(pollution, split == FALSE)
dim(train1);dim(test1)

plot(density(train1$PM2.5))
plot(density(log(train1$PM2.5)))
lm.model <- lm(log(PM2.5)~.,data = train1[,!names(train1)%in%c
('weather_station','utc_time','stationId')])
summary(lm.model)
plot(lm.model)
pred <- predict(lm.model,newdata=test1)
mean(abs(pred-test1$PM2.5)/test1$PM2.5)

set.seed(1)
nrow(pollution)
pollution <- pollution[sample(nrow(pollution),10000),]
train <- pollution[sample(nrow(pollution),1*nrow(pollution)/5),!names(pollution)%in%c('weather_station','utc_time',
                                                                                      'stationId')]
test <-  pollution[-sample(nrow(pollution),1*nrow(pollution)/5),!names(pollution)%in%c('weather_station','utc_time',
                                                                                       'stationId')]
colnames(train)
coly <- 1 
names(train)[coly] <- "y"


library(rpart)
dtree<-rpart(PM2.5~.,data = train1[,!names(train1)%in%c('weather_station','utc_time','stationId')])
printcp(dtree)
#visualization
library(rpart.plot)
rpart.plot(dtree,branch=1,type=2, fallen.leaves=T,cex=0.8)
pred <- predict(dtree,newdata=test1)
mean(abs(pred-test1$PM2.5)/test1$PM2.5)


##knn
set.seed(1)
nrow(pollution)
pollution <- pollution[sample(nrow(pollution),10000),]
train <- pollution[sample(nrow(pollution),4*nrow(pollution)/5),!names(pollution)%in%
c('weather_station','utc_time','stationId')]
test <-  pollution[-sample(nrow(pollution),4*nrow(pollution)/5),!names(pollution)%in%
c('weather_station','utc_time','stationId')]
colnames(train)
coly <- 1 
names(train)[coly] <- "y"

##Find the best k
model.kknn_2 <- kknn(y~.,train,test,k=2,scale=F,distance=1,kernel="rectangular")
sqrt(sum((model.kknn_2$fitted.values-test$PM2.5)^2)/length(test))
model.kknn_3 <- kknn(y~.,train,test,k=3,scale=F,distance=1,kernel="rectangular")
sqrt(sum((model.kknn_3$fitted.values-test$PM2.5)^2)/length(test))
model.kknn_4 <- kknn(y~.,train,test,k=4,scale=F,distance=1,kernel="rectangular")
sqrt(sum((model.kknn_4$fitted.values-test$PM2.5)^2)/length(test))

##Find the best kernel
model.kknn1 <- kknn(y~.,train,test,k=2,scale=F,distance=1,kernel="optimal")
sqrt(sum((model.kknn1$fitted.values-test$PM2.5)^2)/length(test))
model.kknn2 <- kknn(y~.,train,test,k=2,scale=F,distance=1,kernel="triangular")
sqrt(sum((model.kknn2$fitted.values-test$PM2.5)^2)/length(test))
model.kknn3 <- kknn(y~.,train,test,k=2,scale=F,distance=1,kernel="epanechnikov")
sqrt(sum((model.kknn3$fitted.values-test$PM2.5)^2)/length(test))

##Average error
model.kknn.best <- kknn(y~.,train,test,k=2,scale=F,distance=1,kernel="triangular")
mean(abs(model.kknn.best$fitted.values-test$PM2.5)/test$PM2.5)



plot(density(train$PM2.5))
plot(density(log(train$PM2.5)))
lm.model <- lm(log(PM2.5)~.,data = train[,!names(train)%in%c('weather_station','utc_time',
                                                             'stationId')])

summary(lm.model)
plot(lm.model)
pred <- predict(lm.model,newdata=test)
mean(abs(pred-test1$PM2.5)/test1$PM2.5)


