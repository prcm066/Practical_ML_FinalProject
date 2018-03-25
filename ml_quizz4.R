library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)


set.seed(125)
training<-segmentationOriginal[segmentationOriginal$Case=="Train",]
testing<-segmentationOriginal[segmentationOriginal$Case=="Test",]
set.seed(125)
modelo<-train(Class~., method="rpart",data=training)
fancyRpartPlot(modelo$finalModel)
plot(modelo$finalModel)
text(modelo$finalModel)






library(pgmm)
data(olive)
olive = olive[,-1]



newdata = as.data.frame(t(colMeans(olive)))
arbol<-train(Area~. , data=olive, method="rpart")
fancyRpartPlot(arbol$finalModel)

pronosticos<-predict(arbol,newdata)




library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
modelo<-train(chd~age+alcohol+obesity+tobacco+typea+ldl,method="glm", family=binomial(),data=trainSA)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
pr<-predict(modelo,testSA)
missClass(testSA$chd, pr)
pr<-predict(modelo,trainSA)
missClass(trainSA$chd, pr)



library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
vowel.train$y<-as.factor(vowel.train$y)
vowel.test$y<-as.factor(vowel.test$y)

set.seed(33833)

forest<-train(y~., method="rf", data=vowel.train)
varImp(forest)




############ Quiz 4


library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
vowel.train$y<-as.factor(vowel.train$y)
vowel.test$y<-as.factor(vowel.test$y)

set.seed(33833)

forest<-train(as.factor(y)~., method="rf", data=vowel.train)
boosted<-train(as.factor(y)~., method="gbm", data=vowel.train)

pf<-predict(forest, vowel.test)
pb<-predict(boosted, vowel.test)

combinado<-data.frame(pf,pb,y=vowel.test$y)
combinado2<-combinado[pf==pb,]


with(combinado, confusionMatrix(pf,y))
with(combinado, confusionMatrix(pb,y))
with(combinado2, confusionMatrix(pb,y))





library(caret)
library(gbm)
set.seed(3433)


######### Quiz 4
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]


set.seed(62433)

rf<-train(diagnosis~., data=training, method="rf")
bs<-train(diagnosis~., data=training, method="gbm")
ld<-rf<-train(diagnosis~., data=training, method="lda")



stacked<-data.frame(prf=predict(rf,training),pbs=predict(bs,training),pld=predict(ld,training),diagnosis=training$diagnosis)
srf<-train(diagnosis~., data=stacked, method="rf")

prf<-predict(rf,testing)
pbs<-predict(bs,testing)
pld<-predict(ld,testing)
pstk<-predict(srf,data.frame(prf,pbs,pld))


confusionMatrix(pstk,testing$diagnosis)
confusionMatrix(prf,testing$diagnosis)
confusionMatrix(pbs,testing$diagnosis)
confusionMatrix(pld,testing$diagnosis)


##### quizz 4, pregunta 3

set.seed(3523)

library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)

modelo<-train(CompressiveStrength ~ ., method="lasso", data=training)


#### quizz 4, pregunta 4

library(lubridate) # For year() function below

dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
tstest<-ts(testing$visitsTumblr,start=366)

library("forecast")
modelo<-bats(tstrain)
plot(forecast(modelo,h=235))
lines(tstest)

datos<-forecast(modelo,h=235)

datos<-cbind(datos, tstest)

library(dplyr)
dfd<-as.data.frame(datos)
result<- dfd %>%
  mutate(dentro=ifelse(tstest<= `datos.Hi 95` & tstest >=`datos.Lo 95`,1,0))


### quiz 4, q5
set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(325)
modelo<-train(CompressiveStrength~., method='svmLinear2', data=training )
modelo<-e1071::svm(CompressiveStrength~ . , data=training)
postResample(predict(modelo,testing),testing$CompressiveStrength)


pred<-predict(modelo,testing)
mean((pred-testing$CompressiveStrength)^2)^0.5
