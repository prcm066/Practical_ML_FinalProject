data(iris)
library(ggplot2)
names(iris)


library(caret)
inTrain<-createDataPartition(y=iris$Species,p=0.7,list=FALSE)

training<-iris[inTrain,]
testing<-iris[-inTrain,]

dim(training)
dim(testing)


qplot(Petal.Width,Sepal.Width,  color=Species, data=training)

modelfit<-train(Species~., data=training, method="rpart")
print(modelfit$finalModel)

library("rattle")
fancyRpartPlot(modelfit$finalModel)
