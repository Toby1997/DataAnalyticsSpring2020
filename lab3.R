EPI_data <-read.csv(( "~/Semster2/DA/HW1/EPI_data.csv"),header = T)
EPI_data

#central tendency: median, mean, mode
summary(EPI_data$EPI) 
fivenum(EPI_data$EPI, na.rm = TRUE)

summary(EPI_data$DALY) 
fivenum(EPI_data$DALY, na.rm = TRUE)

#Histogram
hist(EPI_data$EPI)
hist(EPI_data$DALY)

#boxplot
boxplot(EPI_data$ENVHEALTH,EPI_data$ECOSYSTEM)

#qqplot
qqplot(EPI_data$ENVHEALTH,EPI_data$ECOSYSTEM)

#Regression Exercises
attach(EPI_data)
boxplot(EPI_data$ENVHEALTH,EPI_data$DALY,EPI_data$AIR_H,EPI_data$WATER_H)
lmENVH<-lm(ENVHEALTH~DALY+AIR_H+WATER_H)

lmENVH
summary(lmENVH)
cENVH<-coef(lmENVH)

DALY<-c(seq(5,95,5))
AIR_H<-c(seq(5,95,5))
WATER_H<-c(seq(5,95,5))
NEW<-data.frame(DALY,AIR_H,WATER_H)
pENV<- predict(lmENVH,NEW,interval="prediction")
cENV<- predict(lmENVH,NEW,interval="confidence")

#-----
boxplot(EPI_data$AIR_E,EPI_data$DALY,EPI_data$AIR_H,EPI_data$WATER_H)
lmAIR_E<-lm(AIR_E~DALY+AIR_H+WATER_H)

lmAIR_E
summary(lmAIR_E)
cENVH<-coef(lmAIR_E)

DALY<-c(seq(5,95,5))
AIR_H<-c(seq(5,95,5))
WATER_H<-c(seq(5,95,5))
NEW<-data.frame(DALY,AIR_H,WATER_H)
pENV<- predict(lmAIR_E,NEW,interval="prediction")
cENV<- predict(lmAIR_E,NEW,interval="confidence")

#-----


#-------part2--------

Multivariate<-read.csv(file.choose( "/Desktop/DA/dataset_multipleRegression.csv"),header = T)
Multivariate

#EXERCISE 1
help(attach)
attach(Multivariate)
lmEnroll<-lm(ROLL~UNEM+HGRAD)
lmEnroll
summary(lmEnroll)
cEnroll<-coef(lmEnroll)
cEnroll

R1 <- function(a1,a2){        #a1:UNEM ,a2:HGRAD  
  R <- -8255.7510591 + 698.2681316*a1 + 0.9422769 *a2
  return(R)
}
ROLL1<- R1(7,90000)
ROLL1
#   add per capita income (INC) to the model

lmEnroll2<-lm(ROLL~UNEM+HGRAD+INC)
lmEnroll2
summary(lmEnroll2)
cEnroll2<-coef(lmEnroll2)
cEnroll2

R2 <- function(b1,b2,b3){   #b1:UNEM ,b2:HGRAD  ,b3:INC
  R <- -9153.2544627 + 450.1245037*b1 + 0.4064837*b2 + 4.2748577 *b3
  return(R)
}
ROLL2<- R2(7,90000,25000)
ROLL1

#EXERCISE 2
abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header = FALSE, sep = ",")
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' )
summary(abalone)
str(abalone)
summary(abalone$rings)

abalone$rings <- as.numeric(abalone$rings)
abalone$rings <- cut(abalone$rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
abalone$rings <- as.factor(abalone$rings)
summary(abalone$rings)
aba <- abalone
aba$sex <- NULL
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))
summary(aba$shucked_wieght)
ind <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- aba[ind==1,]
KNNtest <- aba[ind==2,]
sqrt(2918)
library(class)
help("knn") # Read the knn documentation on RStudio. 
KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$rings, k = 54)
KNNpred
table(KNNpred)

#EXERCISE 3

library(ggplot2) 
head(iris) # first 6 rows of the 
str(iris) 
summary(iris) 
help("sapply")
sapply(iris[,-5], var)
summary(iris)
ggplot(iris,aes(x = Sepal.Length, y = Sepal.Width, col= Species)) + geom_point()
ggplot(iris,aes(x = Petal.Length, y = Petal.Width, col= Species)) + geom_point()
set.seed(300)
k.max <- 12
wss<- sapply(1:k.max,function(k){kmeans(iris[,3:4],k,nstart = 20,iter.max = 20)$tot.withinss})
wss # within sum of squares.
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
icluster <- kmeans(iris[,3:4],3,nstart = 20)
table(icluster$cluster,iris$Species)


library(rpart)
library(rpart.plot)
iris
dim(iris) # check the dimensions of the iris dataset
s_iris <- sample(150,100) 
s_iris
iris_train <-iris[s_iris,]
iris_test <-iris[-s_iris,]
dim(iris_test)
dim(iris_train)
dectionTreeModel <- rpart(Species~., iris_train, method = "class")
dectionTreeModel
rpart.plot(dectionTreeModel)


