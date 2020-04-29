rm(list=ls())

data("USArrests")
states=row.names(USArrests) 
states
names(USArrests )
apply(USArrests , 2, mean)
apply(USArrests , 2, var)
pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation
help("prcomp")
dim(pr.out$x)
biplot(pr.out, scale=0)
help("biplot")
pr.out$sdev
pr.var = pr.out$sdev^2
pr.var
pve = pr.var/sum(pr.var)
pve

#PCA on iris dataset


# PCA with iris dataset
data("iris")
head(iris)
# creating another dataset from iris dataset that contains the columns from 1 to 4 
irisdata1 <- iris[,1:4]
irisdata1

head(irisdata1)
principal_components <- princomp(irisdata1, cor = TRUE, score = TRUE)
summary(principal_components)
# using the plot() function, we can plot the principal components.
plot(principal_components)
# plotting the principal_components using the a line in plot() functions 
plot(principal_components, type = "l")
# using the biplot() function we can plot the components
biplot(principal_components)



data(Boston, package="MASS")
help(Boston)
help(prcomp)
pca_out <- prcomp(Boston,scale. = T)
pca_out
plot(pca_out)
help(biplot)

biplot(pca_out, scale = 0)
boston_pc <- pca_out$x
boston_pc
# boston_pc has the Princial Components having the same number of rows in the original dataset
head(boston_pc)
summary(boston_pc)

#====Titanic=====
library(rpart)
library(rpart.plot)
data("Titanic")
Titanic_new <- as.data.frame(Titanic)
help(Titanic)

Titanic1<-rpart(Survived ~.,data = Titanic,method = "anova")
  
Titanic1 
rpart.plot(Titanic1,type = 3,fallen.leaves = TRUE)

# Ctree

require(C50)  
#ctree, hclust, randomForest
set.seed(25)
Titanic
Titanic2= C5.0(Survived ~., data = Titanic_new[0:25,])
Titanic2
plot(Titanic2, main="Applying cTree on Titanic Dataset", tp_args = list(fill = c("green","red")))


#tab <- t(as.data.frame(apply(Titanic_new, 4:1, FUN=sum)))
#tab <- tab[apply(tab, 1, sum) > 0, ]  
#Titanic3<-hclust(tab)
#help("hclust")

set.seed(678910)
for(i in 1:40){
  # flipping a coin and getting the data
  Titanic_h <- rbinom(1, size = 1, prob = 0.5)
  # if the coin is "Heads", add a common pattern to that row,
  if(Titanic_h){
    data_Matrix[i, ] <- data_Matrix[i, ] + rep(c(0,3), each =5)
  }
}





library(randomForest)
Titanic4 <- randomForest(Survived ~.,   data=Titanic)
Titanic4
