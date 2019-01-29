#path <- "C:/Users/user/Desktop/Pi2/app3"
#setwd(path)

library(shiny)
library(BBmisc)
library(fBasics)
library(tidyverse)
library(caret)
library(pROC)

results <- read.csv("results.csv", header=TRUE)
simdata <- read.csv("simdata.csv", header=FALSE)
sample_test <- read.csv("sample_test.csv")

results=data.frame(t(results))

prediction=results
prediction[prediction>=0.5]=1
prediction[prediction<0.5]=0

realval=sample_test["TARGET"]

time=simdata[,c(4,5)]
# create the data frame with precision
percGoodPred <- function(pred,realv){
  l=length(realv)
  true=0
  for(i in 1:l){
    if (pred[i]==realv[i]){
      true=true+1
    }
  }
  return (true/l)
}
areaRoC=function(results,realv){
  roc=roc(c(realv),c(results))
  return(as.numeric(auc(roc)))
}

GW_col <- function(predict,realv){
  GW1=c()
  GW2=c()

  nsim=length(predict[1,])/2

  for(i in 1:nsim){
    GW1=c(GW1,percGoodPred(predict[,2*i-1],realv[,1]))
    GW2=c(GW2,percGoodPred(predict[,2*i],realv[,1]))
  
    }
  
  
  d=data.frame("GW_M1"=GW1, "GW_M2"=GW2)
  return (d)
}
RoC_col <- function(results,realv){
  Roc1=c()
  Roc2=c()
  nsim=length(results[1,])/2
  
  for(i in 1:nsim){
    Roc1=c(Roc1,areaRoC(results[,2*i-1],realv[,1]))
    Roc2=c(Roc2,areaRoC(results[,2*i],realv[,1]))
  }

  d=data.frame("RoC_M1"=Roc1, "Roc_M2"=Roc2)
  return (d)
}

GW=GW_col(prediction,realval)
RoC_Area=RoC_col(results,realval)

AllDataApp3=function(simdata,GW,RoC,time){
  
  totData=list()
  M_index=simdata
  M_index[,1]=M_index[,1]+1
  M_index[,2]=as.integer(M_index[,2]*5)
  M_index[,3]=as.integer(M_index[,3]*5)
  
  
  GW_M1_Matr=array(NA,c(5,5,5))
  GW_M2_Matr=array(NA,c(5,5,5))
  RoC_M1_Matr=array(NA,c(5,5,5))
  RoC_M2_Matr=array(NA,c(5,5,5))
  Time_m1_Matr=array(NA,c(5,5,5))
  Time_m2_Matr=array(NA,c(5,5,5))
  
  for(i in 1:length(GW[,1])){
    GW_M1_Matr[M_index[i,1],M_index[i,2],M_index[i,3]]=GW[i,1]
    GW_M2_Matr[M_index[i,1],M_index[i,2],M_index[i,3]]=GW[i,2]
    RoC_M1_Matr[M_index[i,1],M_index[i,2],M_index[i,3]]=RoC[i,1]
    RoC_M2_Matr[M_index[i,1],M_index[i,2],M_index[i,3]]=RoC[i,2]
    Time_m1_Matr[M_index[i,1],M_index[i,2],M_index[i,3]]=time[i,1]
    Time_m2_Matr[M_index[i,1],M_index[i,2],M_index[i,3]]=time[i,2]
  }
  
  GWPerTime=list(GW_M1_Matr/Time_m1_Matr,GW_M2_Matr/Time_m2_Matr)
  RocPerTime=list(RoC_M1_Matr/Time_m1_Matr,RoC_M2_Matr/Time_m2_Matr)

  GWL=list(GW_M1_Matr,GW_M2_Matr)
  RocL=list(RoC_M1_Matr,RoC_M2_Matr)
  TimeL=list(Time_m1_Matr,Time_m2_Matr)
  
  totData=list(GWL,
               RocL,
               TimeL,
               GWPerTime,
               RocPerTime)
}

tabToPlotN=function(n,Matr){
  m=data.frame(Matr[n,1:5,1:5])
  row.names(m)=c("20%","40%","60%","80%","100%")
  colnames(m)=c("20%","40%","60%","80%","100%")
  return(m)
}
tabToPlotD=function(pd,Matr){
  m=data.frame(Matr[1:5,pd,1:5])
  
  row.names(m)=c(0:4)
  colnames(m)=c("20%","40%","60%","80%","100%")
  return(m)
}
tabToPlotV=function(pv,Matr){
  m=data.frame(Matr[1:5,1:5,pv])
  row.names(m)=c(0:4)
  colnames(m)=c("20%","40%","60%","80%","100%")
  return(m)
}

totData=AllDataApp3(simdata,GW,RoC_Area,time)
infoTab=data.frame(table(realval))
colnames(infoTab)=c("Value","Frequence")

findYlabel = function(selected){
 
  ylab = ""
  
  if(selected==1){
    ylab = "% good predictions"
  }else if(selected==2){
    ylab = "AUC"
  }else if(selected==3){
    ylab = "Time"
  }else if(selected==4){
    ylab = "AUC/Time"
  }else if(selected==5){
    ylab = "% good predictions/Time"
  }
  
  return(ylab)
}











 
