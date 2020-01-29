days <-c('Mon','Tue','Wed','Thur','Fri','Sat','Sun')
temp <-c(28,30.5,32,31.2,29.3,27.9,26.4)
snowed <- c('T','T','F','F','T','T','F')
help("data.frame")
RPI_Weather_Week <- data.frame(days,temp,snowed)

RPI_Weather_Week
head(RPI_Weather_Week)

str(RPI_Weather_Week)

summary(RPI_Weather_Week)
RPI_Weather_Week[1,]
RPI_Weather_Week[,1]
RPI_Weather_Week[,'snowed']
RPI_Weather_Week[,'days']
RPI_Weather_Week[,'temp']
RPI_Weather_Week[1:5,c("days","temp")]
RPI_Weather_Week$temp
subset(RPI_Weather_Week,subset=snowed==TRUE)
sorted.snowed <- order(RPI_Weather_Week['snowed'])
sorted.snowed
RPI_Weather_Week[sorted.snowed,]
dec.snow <- order(-RPI_Weather_Week$temp)
dec.snow
empty.DataFrame <- data.frame()
v1 <- 1:10
v1
letters
v2 <- letters[1:10]
df <- data.frame(col.name.1 = v1,col.name.2 = v2)
df
write.csv(df,file = 'saved_df1.csv')
df2 <- read.csv('saved_df1.csv')
df2
EPI_data <-read.csv(file.choose( "/Desktop/DA/data_EPI.csv"),header = T)
EPI_data
summary(EPI_data$EPI) 
fivenum(EPI_data$EPI, na.rm = TRUE)
summary(EPI_data$DALY) 
fivenum(EPI_data$DALY, na.rm = TRUE)
summary(EPI_data$DALY) 
fivenum(EPI_data$DALY, na.rm = TRUE)
hist(EPI_data$EPI)
hist(EPI_data$DALY)
boxplot(EPI_data$ENVHEALTH,EPI_data$ECOSYSTEM)
qqplot(EPI_data$ENVHEALTH,EPI_data$ECOSYSTEM)
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
boxplot(EPI_data$AIR_E,EPI_data$DALY,EPI_data$AIR_H,EPI_data$WATER_H)
lmAIR_E<-lm(AIR_E~DALY+AIR_H+WATER_H)

