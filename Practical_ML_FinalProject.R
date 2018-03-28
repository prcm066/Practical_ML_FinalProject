setwd("C://Users/Pablo/Dropbox/DataScience")
datos<-read.csv("pml-training.csv")

library("caret")

set.seed(20180325)

itrain<-createDataPartition(y=datos$classe,p=0.8, list=FALSE)

training<-datos[itrain,]
testing<-datos[-itrain,]

## quitamos variables con vacios
removeNaVars<-function(tabla,thres){
  tabla<-training
  nr<-nrow(tabla)
  nc<-ncol(tabla)
  lista<-rep(FALSE,nc)
  for(i in c(1:nc))
  {
    lista[i]<-ifelse(sum(is.na(tabla[,i]))/nr>thres, TRUE,FALSE)
  }
  return(lista)
}
nas<-removeNaVars(training,0.3)
tr2<-training[,!nas]

nzv <- nearZeroVar(tr2, saveMetrics= TRUE)
tr3<-tr2[,!nzv$nzv]

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## 10 reps
  repeats = 3)



pp<-preProcess(tr3, method="knnImpute",k=10)
pptrain<-predict(pp,tr3)
modelo <- train(classe ~ ., data = pptrain, 
         method = "xgbTree", 
         trControl = fitControl,
         verbose = FALSE)

