library(BBmisc)
library(fBasics)
library(tidyverse)
library(caret)
a<- as.data.frame(lapply(Data, normalize))
a<-NData-a
rm(list=ls())
noiseval <- 4
Data <- read.csv("/Users/alessandrobusato/Desktop/ESILV/Project/Git/Pi2-Transparence-des-algorithmes-master/sample_train.csv", header=TRUE)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
NData <- as.data.frame(lapply(Data, normalize))
if (noiseval==0) {
  NData <- NData
} else if (noiseval==1) {
  r <- replicate(ncol(NData),rnorm(nrow(NData),0,0.02))
  NData <- NData+r;
  NData <- as.data.frame(lapply(NData, normalize))
} else if (noiseval==2) {
  r <- replicate(ncol(NData),rnorm(nrow(NData),0,0.11))
  NData <- NData+r;
  NData <- as.data.frame(lapply(NData, normalize))
} else if (noiseval==3) {
  r = replicate(ncol(NData),rnorm(nrow(NData),0,0.20))
  NData = NData+r;
  NData <- as.data.frame(lapply(NData, normalize))
} else if (noiseval==4) {
  for (i in 1:ncol(NData)) {
    r = replicate(ncol(NData),rbernoulli(nrow(NData),0.01))
    if (length(unique(NData[i]))==1){
      NData[i]<-(NData[i]+r[i])%%2
    } else {
      for (j in 1:length(r[i])){
        if (r[i][j]){
          NData[j,i]<-sample(NData[i],1)
        }
      }
    }
  }
}
write.csv(NData[1:length(NData[,1]),],file="/Users/alessandrobusato/Desktop/ESILV/Project/Git/Pi2-Transparence-des-algorithmes-master/train_features.csv", row.names = FALSE)
