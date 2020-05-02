library(datasets)
data("iris")
str(iris)
summary(iris)
install.packages("caret")
install.packages("tidyr")
library(caret)
library(tidyr)
i <- iris
library(ggplot2)
library(lattice)
library(caret)
library(ISLR)
library(pROC)
validation_index <- createDataPartition(i$Species, p=.80, list=FALSE)
validation <- i[-validation_index,]
i <- i[validation_index,]
dim(i)
dim(validation_index)
summary(validation_index)
head(validation_index)
head(validation)
set.seed(150)
sapply(i,class)
percentage <- prop.table(table(i$Species))*100
percentage
cbind(freq=table(i$Species), percentage=percentage)
x <- i[,1:4]
y <- i[,5]
par(mfrow=c(1,4))
for(a in 1:4) {
  boxplot(x[,a], main=names(iris)[a])
}
featurePlot(x=x,y=y,plot="density")
featurePlot(x=x,y=y,plot="box")
control <- trainControl(method = "cv", number=5)
metric="Accuracy"
set.seed(100)
fitlda <- train(Species~. ,data=i, method="lda", metric=metric, trcontrol=control)

fitqda <- train(Species~. ,data=i, method="qda", metric=metric, trcontrol=control)
#install.packages("rpart")
#fitrpart <- train(Species~. ,data=i, method="rpart", metric=metric, trcontrol=control)
results <- resamples(list(lda=fitlda, qda=fitqda))
summary(results)
print(fitlda)
prediction <- predict(fitlda,validation)
confusionMatrix(prediction, validation$Species)















                   