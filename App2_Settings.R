library(ROCR)
library(shiny)
library(BBmisc)
library(fBasics)
library(tidyverse)
library(caret)
library(data.table)
library(plotly)
library(rowr)

randomSample <- function(x,perc){
  nobs=nrow(x)
  n=as.integer(perc*nobs)
  toRet=x[sample(nobs,n),]
  return(toRet)
}
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
GoodWrong <- function(Tar,P1,P2){
  g=0
  g1=0
  g2=0
  ng=0
  l=length(Tar[,1])
  for (i in 1:l){
    if(Tar[i,1]==P1[i,1] &Tar[i,1]==P2[i,1] ){
      g=g+1
    }
    else if(Tar[i,1]==P1[i,1] &Tar[i,1]!=P2[i,1] ){
      g1=g1+1
    }
    else if(Tar[i,1]!=P1[i,1] &Tar[i,1]==P2[i,1] ){
      g2=g2+1
    }
    else {
      ng=ng+1
    }
  }
  g=g/l
  g1=g1/l
  g2=g2/l
  ng=ng/l
  
  dt=data.frame("Observations"=c(l),
                "Correct"=c(g*100),
                "Correct only for Model 1"=c(g1*100),
                "Correct only for Model 2"=c(g2*100),
                "Wrong"=c(ng*100)
    
  )
  return(dt)
}

PercGoodWrong2<- function(Tar,P1,P2){
  g=0
  g1=0
  g2=0
  ng=0
  l=length(Tar)
  if(l>0){
    for (i in 1:l){
      if(Tar[i]==P1[i] &Tar[i]==P2[i] ){
        g=g+1
      }
      else if(Tar[i]==P1[i] &Tar[i]!=P2[i] ){
        g1=g1+1
      }
      else if(Tar[i]!=P1[i] &Tar[i]==P2[i] ){
        g2=g2+1
      }
      else {
        ng=ng+1
      }
    }
  }
  dt=data.frame("Observations"=c(l),
                "Correct"=c(100*g/l),
                "Correct only for Model 1"=c(100*g1/l),
                "Correct only for Model 2"=c(100*g2/l),
                "Wrong"=c(100*ng/l)
                )
  return(dt)
}
clustIndCat=function(vect){
  un=unique(vect)
  l=length(un)
  indexes=list()
  for(i in 1:l){
    indexes[[i]]=which(vect==un[i])
  }
  return(indexes)
}
clustIndNum=function(vect){
  q=quantile(vect,c(.25,.50,.75))
  indexes=list()
  indexes[[1]]=which(vect<=q[1])
  indexes[[2]]=which(vect<=q[2] & vect>q[1])
  indexes[[3]]=which(vect<=q[3] & vect>q[2])
  indexes[[4]]=which(vect>q[3])
  return(indexes)
}
clResNum=function(var,TEST,pred1,pred2){
  vector=TEST[,var]
  clInd=clustIndNum(vector)
  t1=PercGoodWrong2(TEST[clInd[[1]],"TARGET"],pred1[clInd[[1]]],pred2[clInd[[1]]])
  t2=PercGoodWrong2(TEST[clInd[[2]],"TARGET"],pred1[clInd[[2]]],pred2[clInd[[2]]])
  t3=PercGoodWrong2(TEST[clInd[[3]],"TARGET"],pred1[clInd[[3]]],pred2[clInd[[3]]])
  t4=PercGoodWrong2(TEST[clInd[[4]],"TARGET"],pred1[clInd[[4]]],pred2[clInd[[4]]])
  clData=rbind(t1,t2,t3,t4)
  row.names(clData)=c("1st q","2nd q","3th q","4th q")
  return(clData)
}
clResCat=function(var,TEST,pred1,pred2){
  vector=TEST[,var]
  clInd=clustIndCat(vector)
  clData=data.frame()
  for(i in 1:length(unique(vector))){
    clData=rbind(clData,PercGoodWrong2(TEST[clInd[[i]],"TARGET"],pred1[clInd[[i]]],pred2[clInd[[i]]]))
  }
  row.names(clData)=c(unique(vector))
  return(clData)
}
clusterResults=function(var,TEST,pred1,pred2){
  l=length(unique(TEST[,var]))
  if (l>8){
    dt=clResNum(var,TEST,pred1,pred2)
  }
  else{
    dt=clResCat(var,TEST,pred1,pred2)
  }
  return(dt)
}
indexSubset=function(TEST,v1,v2,v3,v4,v5){
  if(v1!="No Filter"){ind<-which(TEST["ind_var30"]!=as.integer(v1))}
  else{ind<-c()}
  if(v2!="No Filter"){tmp<-which(TEST["num_meses_var5_ult3"]!=as.integer(v2))}
  else{tmp<-c()}
  ind<-c(ind,tmp)
  if(v5!="No Filter"){tmp<-which(TEST["ind_var5"]!=as.integer(v5))}
  else{tmp<-c()}
  ind<-c(ind,tmp)
  tmp<-which(TEST["num_var30"]<v3[1])
  ind<-c(ind,tmp)
  tmp<-which(TEST["num_var30"]>v3[2])
  ind<-c(ind,tmp)
  tmp<-which(TEST["num_var42"]<v4[1])
  ind<-c(ind,tmp)
  tmp<-which(TEST["num_var42"]>v4[2])
  ind<-c(ind,tmp)
  ind<-unique(ind)
  tmp<-1:length(TEST[,1])
  res<-tmp[-ind]
  return(res)
}

Misclassified <- function(Tar,P){
  l<-length(Tar)
  col<-c()
  for (i in 1:l){
    if(Tar[i]!=P[i]){
      col<-c(col,i)
    }
  }
  return(col)
}
DistVarNum=function(var,tes){
  vector=tes[,var]
  testmean <- mean(vector)
  testmedian <- median(vector)
  testvariance <- var(vector)
  clData=data.frame()
  clData=rbind(clData,testmean,testmedian,testvariance)
  row.names(clData)=c("Mean","Median","Variance")
  return(clData)
}
DistVarCat=function(var,tes){
  vector=tes[,var]
  clData=data.frame()
  for(i in 1:length(unique(vector))){
    clData=rbind(clData, sum(vector == unique(vector)[i])/length(vector)*100)
  }
  row.names(clData)=c(unique(vector))
  return(clData)
}
DistVarPred <- function(var,tes){
  l=length(unique(tes[,var]))
  if (l>8){
    dt=DistVarNum(var,tes)
  }
  else{
    dt=DistVarCat(var,tes)
  }
  return(dt)
}
Uni_Table <- function(var,tes1,tes2,tes3){
  a<-DistVarPred(var,tes1)
  b<-DistVarPred(var,tes2)
  c<-DistVarPred(var,tes3)
  d<-cbind.fill(a,b,c,fill=0)
  d<-data.frame(d)
  row.names(d)=row.names(c)
  colnames(d)=c("Model 1","Model 2","Distribution")
  return(d)
}

# setting of directory
#path <- "/Users/alessandrobusato/Desktop/ESILV/Project/Pi2-Transparence-des-algorithmes-master"
#setwd(path)

# import the data: sample_test(the same that we use for app1)
#                  result( fake data)
RESULTS<- read.csv("results.csv", header=TRUE)
TEST<- read.csv("sample_test.csv",header=TRUE)

# trasport RESULTS and add colum name "M1","M2, ecc,find the  numb. of simulations
RESULTS<-as.data.frame(t(RESULTS))
nSim=length(RESULTS[1,])/2
names<-c()
for (i in 1:nSim) {
  names<-c(names,"M1")
  names<-c(names,"M2")
}
colnames(RESULTS)<-names


#create the prediction table
PREDICTION=RESULTS
treshold<-0.5
PREDICTION[PREDICTION<treshold]=0
PREDICTION[PREDICTION>=treshold]=1


#create table for good/wrong predictions  
GW_glob = data.frame()
for(i in 1:nSim){
  if(i==1){
    GW_glob <- rbind(GW_glob,GoodWrong(TEST["TARGET"],PREDICTION[2*i-1],PREDICTION[2*i]))
  }else{
    GW_glob <- cbind(GW_glob,GoodWrong(TEST["TARGET"],PREDICTION[2*i-1],PREDICTION[2*i]))
  }
  
}

#fake Trainfeat and infoSimof 2 simulation
simdata<- read.csv("simdata.csv", header=TRUE)
simdata<-as.data.frame(simdata)
#fake Trainfeat and infoSimof 2 simulation
trainFeat=data.frame("Level Of Noise"=c(simdata[1]),
                     "% Data Used"=c(simdata[2]*100),
                     "%Var Used"=c(simdata[3]*100))

for (i in 1:nSim) {
  r1<-simdata[i,4:5]
  prec<-GoodWrong(TEST[length(TEST)],data.frame(PREDICTION[,2*i-1]),data.frame(PREDICTION[,2*i]))
  r2<-c(prec[2]+prec[3],prec[2]+prec[4])
  simi<-rbindlist(list(r1,r2))
  if(i==1){
    precTime<-simi
  }else{
    precTime<-cbind(precTime,simi)
  }
}
precTime=data.frame(precTime, row.names = c("Time","Percentage of good predicitions"))
col_names<-c()
for (i in 1:nSim) {
  col_names<-c(col_names,"Model 1")
  col_names<-c(col_names,"Model 2")
}
colnames(precTime)=col_names

cmat<- read.csv("simnn.csv",header=TRUE)
gmat<- read.csv("simlr.csv", header=TRUE)
ccmat<- read.csv("csimnn.csv",header=TRUE)
cgmat<- read.csv("csimlr.csv", header=TRUE)
cmat <- as.matrix(cmat)
gmat <- as.matrix(gmat)
ccmat <- as.matrix(ccmat)
cgmat <- as.matrix(cgmat)
Train <- read.csv("sample_train.csv", header=TRUE)
Test <- read.csv("sample_test.csv", header=TRUE)
var1 <- 2
var2 <- 80
cvar1 <- 113
cvar2 <- 58
grid <- 100
NData <- rbind(Train,Test)
mi1 <- min(NData[var1])
ma1 <- max(NData[var1])
mi2 <- min(NData[var2])
ma2 <- max(NData[var2])
xaxis<-seq(mi1, ma1, length.out = (grid+1))
yaxis<-seq(mi2, ma2, length.out = (grid+1))
cxaxis <- c(sort(unique(NData[cvar1])[,1]))
cyaxis <- c(sort(unique(NData[cvar2])[,1]))
morCorrIndex=order(abs(cor(TEST[,-length(TEST)],TEST[,"TARGET"])),decreasing = TRUE)
