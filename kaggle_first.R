# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

setwd("C://Users/Pablo/Downloads")

# Leemos los archivos
training<-read.csv("train.csv",stringsAsFactors=FALSE)
testing <-read.csv("test.csv",stringsAsFactors=FALSE)

# Cargamos caret, que es contiene utilidades y es wrapper sobre diferentes librerias de ML

library("dplyr")
library("caret")

set.seed(20180326)
trn<-training

# Eliminamos near zeroes y variables con poca variación

trn2<- trn %>% select(-c(Survived,Name,Ticket,Cabin))
#Imputamos missings usando bagging
pp<-preProcess(trn2, method="knnImpute",k=10)
pptrain<-predict(pp,trn)

pptrain<- pptrain %>% mutate(survived=as.factor(Survived))


fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## 10 reps
  repeats = 3,
  search="grid")

tunegrid <- expand.grid(.mtry=c(1:15))

# Utilizamos clasificación por gradient boosting
modelo <- train(survived ~ Pclass+Sex+Age+SibSp+Embarked, data = pptrain, 
                method = "rf", 
                trControl = fitControl,
                tuneGrid=tunegrid,
                verbose = FALSE)

t2<- testing %>% select(-c(Name,Ticket,Cabin))
pptest<-predict(pp,t2)
resultado<-predict(modelo,pptest)
rf<-data.frame(PassengerId=testing$PassengerId,Survived=as.numeric(resultado)-1)
write.csv(rf,"finalPred.csv",row.names=FALSE)