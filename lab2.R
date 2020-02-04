EPI_data<-read.csv("/Desktop/DA/data_EPI.csv",skip=1)
View(EPI_data)
attach(EPI_data)
fix(EPI_data) #launches a simple data editor
EPI
tf<-is.na(EPI)
E<-EPI[!tf]


plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 
par(pty="s") 
qqnorm(EPI); qqline(EPI)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)

plot(ecdf(EPI_data$EPI),do.points=FALSE,verticals = TRUE) 
plot(ecdf(EPI_data$EPI),do.points=TRUE,verticals = TRUE) 
par(pty="s")
qqnorm(EPI_data$EPI)
qqline(EPI_data$EPI) 
x <- seq(30,95,1)
x
x2 <-seq(30,95,2)
x2
x2 <-seq(30,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plo~//Desktop/DA/data_EPI.csvt")
qqline(x)

multivariate<-read.csv("/Desktop/DA/data_EPI.csv")
head(multivariate)
attach(multivariate)
mm<-lm(Homeowners~Immigrant)
mm
summary(mm)$coef 

plot(Homeowners~Immigrant)
abline(mm)
abline(mm,col=2,lwd=3)
#Creating Plots
#Chapter 2 -- R Graphics Cookbook
plot(mtcars$wt, mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data=mtcars)
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()
plot(pressure$temperature,pressure$pressure,type="l")
points(pressure$temperature,pressure$pressure)
lines(pressure$temperature,pressure$pressure/2, col="red")
points(pressure$temperature,pressure$pressure/2, col="blue") 
library(ggplot2)
qplot(pressure$temperature,pressure$pressure,geom="line")
qplot(temperature,pressure,data=pressure,geom="line")
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()+geom_point()

#Creating Bar graphs
barplot(BOD$demand,names.arg=BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl)) #generate a table of counts
qplot(mtcars$cyl)# cyl is continous here
qplot(factor(mtcars$cyl)) #treat cyl as discrete
#Bar graph of counts
qplot(facotr(cyl),data=mtcars)
ggplot(mtcars,aes(x=factor(cyl)))+geom_bar()


#Creating Histogram
#Vie the distribution of one-dimentional data with a histogram
hist(mtcars$mpg)
hist(mtcars$mpg,breaks=10) #specify apprioximate number of bins with breaks
hist(mtcars$mpg,breaks=5)
hist(mtcars$mpg,breaks=12)
qplot(mpg,data=mtcars,binwidth=4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth=4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth=5)

#Creating Box-plot
plot(ToothGrowth$supp,ToothGrowth$len)
#Formula Syntax
boxplot(len ~ supp,data=ToothGrowth) 

boxplot(len ~ supp+dose,data=ToothGrowth) 
qplot(ToothGrowth$supp,ToothGrowth$len,geom="boxplot")
qplot(supp,len,data=ToothGrowth,geom="boxplot")
ggplot(ToothGrowth,aes(x=supp,y=len))+geom_boxplot()
qplot(interaction(supp,dose),len,data=ToothGrowth,geom="boxplot")
ggplot(ToothGrowth,aes(x=interaction(supp,dose),y=len))+geom_boxplot()
