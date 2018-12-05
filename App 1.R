library(shiny)
library(BBmisc)
library(fBasics)
library(tidyverse)
library(caret)

randomSample <- function(x,perc){
  nobs=nrow(x)
  n=as.integer(perc*nobs)
  toRet=x[sample(nobs,n),]
  return(toRet)
}
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


ui <- fluidPage(
  numericInput("noise", "noise",0,min=0,max=3,step=1),
  numericInput("dataPerc", "dataPerc",50,min=25,max=100,step=25),
  actionButton("go","go"),
  
  textOutput("t")
)

server <- function(input, output){
  path <- "/Users/user/Desktop/Pi2"
  setwd(path)
  Train <- read.csv("sample_train.csv", header=TRUE)
  Test <- read.csv("sample_test.csv", header=TRUE)
  NData <- rbind(Train,Test)
  NData <- as.data.frame(lapply(NData, normalize))
  rangetrain<-1:nrow(Train)
  NTrain <- NData[rangetrain,]
  NTest <- NData[-rangetrain,]
  
  
  v<- reactiveValues(noise = NULL,dataPerc=NULL, NTrain=NULL, NTest=NULL)
  

  observeEvent(input$go, {
    v$noise <- input$noise
    v$dataPerc<- input$dataPerc/100
    v$NTrain<-  NTrain
    v$NTest<-  NTest
    
    if (v$noise==0) {
      v$NTrain <- NTrain
    } else if (v$noise==1) {
      r <- replicate(ncol(v$NTrain),rnorm(nrow(v$NTrain),0,0.02))
      v$NTrain <- v$NTrain+r;
      v$NTrain[length(v$NTrain)]<-Train[length(Train)]
    } else if (v$noise==2) {
      r <- replicate(ncol(v$NTrain),rnorm(nrow(v$NTrain),0,0.11))
      v$NTrain <- v$NTrain+r;
      v$NTrain[length(v$NTrain)]<-Train[length(Train)]
    } else if (v$noise==3) {
      r = replicate(ncol(v$NTrain),rnorm(nrow(v$NTrain),0,0.20))
      v$NTrain = v$NTrain+r;
      v$NTrain[length(v$NTrain)]<-Train[length(Train)]
    } else if (v$noise==4) {
      for (i in 1:ncol(v$NTrain)) {
        r = replicate(ncol(v$NTrain),rbernoulli(nrow(v$NTrain),0.05))
        if (nrow(as.data.frame(unique(v$NTrain[i])))==2){
          v$NTrain[,i]<-(v$NTrain[,i]+r[,i])%%2
        } else {
          for (j in 1:length(r[i])){
            if (r[j,j]){
              v$NTrain[j,i]<-sample(v$NTrain[,i],1)
            }
          }
        }
      }
      v$NTrain[length(v$NTrain)]<-Train[length(Train)]
    }
    
    v$NTrain <- randomSample(v$NTrain,v$dataPerc)
    write.csv(v$NTrain[1:length(v$NTrain[,1]),],file="train_features.csv", row.names = FALSE)
    write.csv(v$NTest[1:length(v$NTest[,1]),],file="test_features.csv", row.names = FALSE)
    
    
    
    
  })
  
  
  output$t <- renderText({
    print(paste(v$noise,v$dataPerc))
  })
}

shinyApp(ui, server)
