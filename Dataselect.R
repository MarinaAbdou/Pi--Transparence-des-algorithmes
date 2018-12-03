install.packages('BBmisc')
library(BBmisc)
library(fBasics)
library(tidyverse)
library(caret)


rm(list=ls())
Data <- read.csv("/Users/alessandrobusato/Desktop/ESILV/Project/Git/Pi2-Transparence-des-algorithmes-master/clean_data.csv", header=TRUE)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
Data <- as.data.frame(lapply(Data, normalize))
yname="TARGET"
yind=match(yname,colnames(Data))
y=Data[yind]
testIndices=sample(sample(nrow(Data),0.3*nrow(Data)))
Test=Data[testIndices,]
Train=Data[-testIndices,]
yTrain=data.frame("TARGET"= y[-testIndices,1])
indexy1=which(yTrain[,1]==1)
datay1=Train[indexy1,]
datay0=Train[-indexy1,]
randomSampleofObs=function(x,perc){
  nobs=nrow(x)
  n=as.integer(perc*nobs)
  toRet=x[sample(nobs,n),]
  return(toRet)
}
data0sample=randomSampleofObs(datay0,1959/47891)
datasample=rbind(datay1,data0sample)
Train=randomSampleofObs(datasample,1)
write.csv(Train[1:length(Train[,1]),],file="/Users/alessandrobusato/Desktop/ESILV/Project/Git/Pi2-Transparence-des-algorithmes-master/sample_train.csv", row.names = FALSE)
write.csv(Test[1:length(Test[,1]),],file="/Users/alessandrobusato/Desktop/ESILV/Project/Git/Pi2-Transparence-des-algorithmes-master/sample_test.csv", row.names = FALSE)
