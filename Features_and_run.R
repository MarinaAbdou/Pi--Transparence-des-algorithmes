rm(list=ls())
noiseval <- 3
percentage <- 0.7
path <- "/Users/alessandrobusato/Desktop/ESILV/Project/Pi2-Transparence-des-algorithmes-master"
setwd(path)
Train <- read.csv("sample_train.csv", header=TRUE)
Test <- read.csv("sample_test.csv", header=TRUE)
library(BBmisc)
library(fBasics)
library(tidyverse)
library(caret)

NData <- rbind(Train,Test)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
NData <- as.data.frame(lapply(NData, normalize))
rangetrain<-1:nrow(Train)
NTrain <- NData[rangetrain,]
NTest <- NData[-rangetrain,]
if (noiseval==0) {
  NTrain <- NTrain
} else if (noiseval==1) {
  r <- replicate(ncol(NTrain),rnorm(nrow(NTrain),0,0.02))
  NTrain <- NTrain+r;
  NTrain[length(NTrain)]<-Train[length(Train)]
} else if (noiseval==2) {
  r <- replicate(ncol(NTrain),rnorm(nrow(NTrain),0,0.11))
  NTrain <- NTrain+r;
  NTrain[length(NTrain)]<-Train[length(Train)]
} else if (noiseval==3) {
  r = replicate(ncol(NTrain),rnorm(nrow(NTrain),0,0.20))
  NTrain = NTrain+r;
  NTrain[length(NTrain)]<-Train[length(Train)]
} else if (noiseval==4) {
  for (i in 1:ncol(NTrain)) {
    r = replicate(ncol(NTrain),rbernoulli(nrow(NTrain),0.01))
    if (length(unique(NTrain[i]))==1){
      NTrain[i]<-(NTrain[i]+r[i])%%2
    } else {
      for (j in 1:length(r[i])){
        if (r[i][j]){
          NTrain[j,i]<-sample(NTrain[i],1)
        }
      }
    }
  }
}
randomSample <- function(x,perc){
  nobs=nrow(x)
  n=as.integer(perc*nobs)
  toRet=x[sample(nobs,n),]
  return(toRet)
}
NTrain <- randomSample(NTrain,percentage)
write.csv(NTrain[1:length(NTrain[,1]),],file="train_features.csv", row.names = FALSE)
write.csv(NTest[1:length(NTest[,1]),],file="test_features.csv", row.names = FALSE)

library(reticulate)

py_run_file("NN.py")
py_run_file("LR-code.py")

auc_NN <- py$auc
auc_LR <- py$logit_roc_auc

y_pred_keras_NN <- py$y_pred_keras
y_pred_LR <- py$y_pred

#Pour connecter R et Python quand il y a des graphes sur Python il y a une erreur dûe à Qt pluggin window... donc le code du plot en python a été commenté

graph <- function(name)
{
  if(name=="ROC")
  {
    ROCgraph <- plot(py$fpr_keras,py$tpr_keras,main="ROC curve", xlab="False Positive Rate", ylab="True Positive Rate",cex=.5,col="red", type ="s")
    lines(c(py$fpr),c(py$tpr),type="s",col="blue")
    abline(0,1)
    legend("topleft", legend=c("Neural Networks", "Logistic Regression"),
           col=c("red", "blue"), lty=1:1, cex=0.8)
  }
}
graph("ROC")

