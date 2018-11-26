library(fBasics)
library(tidyverse)
library(caret)

rm(list=ls())

source('C:/Users/user/Desktop/Progetto Machine Learning/R scripts/R functions.R')

#import data
Data <- read.csv("C:/Users/user/Desktop/Progetto Machine Learning/data/clean_data.csv", header=TRUE)

#split y and x
yname="TARGET"
yind=match(yname,colnames(Data))
y=Data[yind]
x=Data[-yind]


#create an info table that will be usefull for other parts of the Tool
variableInfo=infoVar(x,y)

# split the data in train and test: test=30%
# here we can chose another way to select the subset
# or we can import directly csv file already splitted
testIndices=sample(sample(nrow(x),0.3*nrow(x)))
xTest=x[testIndices,]
xTrain=x[-testIndices,]
yTest=data.frame("TARGET"= y[testIndices,1])
yTrain=data.frame("TARGET"= y[-testIndices,1])

#rm("Data","x","y","testIndices","yind")


#---------HERE WILL START OUR FIRST PAGE, WITH DATA PREPARED FOR TEST AND TRAIN


# 1) ------- SELECT FEATURES CODE--------------------------


# 1.1) SELECT A PERC OF MOST CORRELATED VARIABLES( es, only the 20% more correlated variables)
# we have 2 choise: 1) look at correlation only for the train data
#                   2) look at correlation for all the -> i did this one

perc=0.5
ind=indMostCorrelatedPerc(x,y,perc)
newXtrain=xTrain[ind]
nweXtest=xTest[ind]


# 1.2) SELECT A PERC OF OBSERVATION TO USE FOR THE TRAIN
# 

percOfObs=0.5
newXtrain=randomSampleOfObs(newXtrain,percOfObs)

# 1.3) ADD NOISE


#now we have the data prepared for the test and the train of the 2 models



#--------2) TRAIN AND TEST OF THE 2 MODELS CODE-------------------------

#time1=
#time2=
#results= xTest+ytest+PredictionModel1,predictionModel2
#I need the results in this way to then select a specific subset


#--------3) GENERAL INFO-----------------
# features used for the train
# time of the train of the models
# precision of the models


#--------4) VISUALIZE INFO AND GRAPH CODE--------------------------------------------------------------------------#

# 3.1) select a specific subset of observation, default= all data
# 
# subsetOfresults=selectSpecificSubsets(results,info)
# infoOfSubset=subsetOfresults[[2]]
# Subset=subsetOfresults[[1]]
#
#
# 3.2) infoTable1=function(Subset)
#      infoTable2=function(Subset)
#      infoTable3=function(Subset)
#      ...

# 3.3) graph1=function(Subset)
#      graph2=function(Subset)
#      graph3=function(Subset)
#      ....







