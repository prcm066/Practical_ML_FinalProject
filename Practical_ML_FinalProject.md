---
title: "PracticalML_FinalProject"
author: "Pablo Rodriguez Chavez"
date: "March 25, 2018"
output: html_document
---

## Introduction

The objective of this project is to develop a Human Activity Recognition classification model that can infere the type of activity based on data from accelerometers.

Data was downloaded from the following url:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

We load the libraries caret, and dplyr. The latter contains utilities for data transformation, the former, caret, contains tools to train Machine Learning models and is a wrapper over several other libraries.


```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

# Loading and Cleaning

As a first step, we load the data and split it in training and testing samples.


```r
setwd("C://Users/Pablo/Dropbox/DataScience")
datos<-read.csv("pml-training.csv")

set.seed(20180325)

itrain<-createDataPartition(y=datos$classe,p=0.8, list=FALSE)

training<-datos[itrain,]
testing<-datos[-itrain,]
```



```r
dim(training)
```

```
## [1] 15699   160
```


We have 160 variables, 159 without the target.A quick look at our data shows that there are many variavles win a high number of missings. We won't exclude them since we are going to use Xgboost tree classifier, which can handle NA's.

We build the following function in order to find the variables that have a percentage of NA's above a given threshold. 


```r
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
```

The usage of this function is almost like nearZeroVar function in caret.

```r
nas<-removeNaVars(training,0.9)
table(nas)
```

```
## nas
## FALSE  TRUE 
##    93    67
```

```r
tr2<-training[,!nas]
ncol(tr2)
```

```
## [1] 93
```

There were 67 variables with more thatn 90% of missings, we will remove them.


```r
nzv <- nearZeroVar(tr2, saveMetrics= TRUE)
table(nzv$nzv)
```

```
## 
## FALSE  TRUE 
##    59    34
```

Since Tree based methods can handle near zero variance variables, we will leave them.

# Model Fitting

We chose to fit a boosting classifier, specifically the Xgboost tree classifier, due to its good performance out of the box and its robustness in the precence of missings, sparse variables, etc.


## Training control and MetaParameter tuning

The control will be done using repeated cross validatio, we prepare a metaparameter grids for tuning.


```r
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
```






## Model Fitting

The training is done using Gradient Boosting Trees as implemented in xgboost ("Extreme Gradient Boosting")


```r
modelo <- train(classe ~ ., 
                data = pptrain, 
                method = "xgbTree", 
                trControl = fitControl,
                tuneGrid=xgbtree.grid, 
                verbose = FALSE,
                na.action=na.pass)
summary(modelo)
```



## Testing


```r
modelo <- predict(modelo, testing)

cm<-confusionMatrix()
```


# References

Ugulino, W., Cardador, D., Vega, K., Velloso, E., Milidiú, R., & Fuks, H. (2012). Wearable computing: Accelerometers' data classification of body postures and movements. In Advances in Artificial Intelligence-SBIA 2012 (pp. 52-61). Springer, Berlin, Heidelberg.

