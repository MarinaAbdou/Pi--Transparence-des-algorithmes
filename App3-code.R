#path <- "C:/Users/user/Desktop/Pi2/App3 csv"
#path <- "C:/Users/Marina/Documents/Semestre 9 (2018-2019)/Pi²/Pi2-Transparence-des-algorithmes-AlessandroBusato-patch-1"
#setwd(path)

library(shiny)
library(BBmisc)
library(fBasics)
library(tidyverse)
library(caret)

results <- read.csv("App3/results.csv", header=TRUE)
simdata <- read.csv("App3/simdata.csv", header=TRUE)
sample_test <- read.csv("App3/sample_test.csv")



pred=results
pred[pred>=0.5]=1
pred[pred<0.5]=0

pred=data.frame(t(pred))
realval=sample_test["TARGET"]

# create the data frame with precision
precision <- function(pred,realv){
  l=length(realv)
  true=0
  for(i in 1:l){
    if (pred[i]==realv[i]){
      true=true+1
    }
  }
  return (true/l)
}
precisionM1andM2 <- function(predict,realv){
  prec1=c()
  prec2=c()
  nsim=length(predict[1,])/2
  for(i in 1:nsim){
    prec1=c(prec1,precision(predict[,2*i-1],realv[,1]))
    prec2=c(prec2,precision(predict[,2*i],realv[,1]))
  }
  d=data.frame("Prec_M1"=prec1, "Prec_M2"=prec2)
  return (d)
}


# create the matrix of the precisions
PrecXTrainFeatMatrix=function(simdata,precM1M2){
  M_index=simdata
  #change this for the new data
  M_index[,1]=M_index[,1]+1
  M_index[,2]=as.integer(M_index[,2]*5)
  M_index[,3]=as.integer(M_index[,3]*5)
  PrecXTrainFeatM1=array(NA,c(5,5,5))
  PrecXTrainFeatM2=array(NA,c(5,5,5))
  for(i in 1:length(precM1M2[,1])){
    PrecXTrainFeatM1[M_index[i,1],M_index[i,2],M_index[i,3]]=precM1M2[i,1]
    PrecXTrainFeatM2[M_index[i,1],M_index[i,2],M_index[i,3]]=precM1M2[i,2]
  }
  l=list(PrecXTrainFeatM1,PrecXTrainFeatM2)
  return (l)
}

matrix0=PrecXTrainFeatMatrix(simdata,precisionM1andM2(pred,realval))
matrix1=list(array(c(1,2,3,4,5,6),c(5,5,5)),array(1,c(5,5,5)))
matrix2=list(array(c(6,7,8,9,10,11),c(5,5,5)),array(2,c(5,5,5)))
matrix3=list(array(c(20,19,18,17,16,15),c(5,5,5)),array(3,c(5,5,5)))
matrix4=list(array(c(1,5,10,20,44,77),c(5,5,5)),array(4,c(5,5,5)))

totData=list(matrix0,
             matrix1,
             matrix2,
             matrix3,
             matrix4)





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


#m=data.frame(matrix[[2]][1,1:5,1:5])



# input
#input1 <- vector(mode="list", length=3)
#names(input1) <- c("All values", "1", "2","3")
#input1[[1]] <- -1
#input1[[2]] <- 1
#input1[[3]] <- 2
#input1[[4]] <- 3







#inpu=data.frame("All values",
                #      "1",
                 #     "2",
                  #    "3")
#dataInput=data.frame("All values"=c(-1),
                  #   "25%"=c(1),
                 ##    "50%"=c(2),
                ##     "75%"=c(3),
               #      "100%"=c(4)
              #       )
#varInput=data.frame("All values"=c(-1),
#                    "10%"=c(1),
 #####                   "20%"=c(2),
  #####                  "30%"=c(3),
   ####                 "40%"=c(4),
    ###                "50%"=c(5),
     ##               "60%"=c(6),
      #              "70%"=c(7),
        #            "80%"=c(8),
         #           "90%"=c(9),
          #          "100%"=c(10)
           #         )





  














