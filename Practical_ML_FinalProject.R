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
  repeats = 5)

xgbtree.grid<-expand.grid(nrounds = c(1, 10, 20),
                          max_depth = c(1, 4),
                          eta = c(.1, .4),
                          gamma = 0,
                          colsample_bytree = .7,
                          min_child_weight = 1,
                          subsample = c(.8, 1))


fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10)

modelo <- train(classe ~ ., 
                data = tr3, 
                method = "rf", 
                trControl = fitControl,
                verbose = TRUE,
                na.action=na.pass)


  