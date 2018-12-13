library(shiny)
library(BBmisc)
library(fBasics)
library(tidyverse)
library(caret)
library(reticulate)

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
  numericInput("varPerc", "varPerc", 50, min=10, max=100,step=10),
  actionButton("go","go"),
  
  textOutput("t")
)

server <- function(input, output){
  path <- "/Users/alessandrobusato/Desktop/ESILV/Project/Pi2-Transparence-des-algorithmes-master"
  setwd(path)
  Train <- read.csv("sample_train.csv", header=TRUE)
  Test <- read.csv("sample_test.csv", header=TRUE)
  NData <- rbind(Train,Test)
  NData <- as.data.frame(lapply(NData, normalize))
  cor=abs(cor(NData[,1:(length(Train)-1)],NData[,length(Train)]))
  ordCor=order(cor,decreasing=TRUE)
  rangetrain<-1:nrow(Train)
  NTrain <- NData[rangetrain,]
  NTest <- NData[-rangetrain,]
  if (file.exists("results_history.csv")){
    file.remove("results_history.csv")
  }
  file.create("results_history.csv")
  
  
 v<- reactiveValues(noise = NULL,dataPerc=NULL,varPerc=NULL, NTrain=NULL, NTest=NULL, count = 0, text=NULL)
  

  observeEvent(input$go, {
    v$noise <- input$noise
    v$dataPerc<- input$dataPerc/100
    v$varPerc<- input$varPerc/100
    v$NTrain<-  NTrain
    v$NTest<-  NTest
    v$count <- v$count + 1
    v$text <- paste("The results of simulation ",v$count, "were saved in results.csv. For more results you may check results_history.csv.")
    
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
    
    nvar <- as.integer((length(NTrain)-1)*v$varPerc)
    tar <- c(ordCor[1:nvar],length(NTrain))
    v$NTrain <- v$NTrain[tar]
    v$NTest <- v$NTest[tar]
    
    write.csv(v$NTrain[1:length(v$NTrain[,1]),],file="train_features.csv", row.names = FALSE)
    write.csv(v$NTest[1:length(v$NTest[,1]),],file="test_features.csv", row.names = FALSE)
    py_run_file("NN.py")
    nn_pred <- py$y_pred_nn
    py_run_file("LR-code.py")
    lr_pred <- py$y_pred_lr
    results <- rbind(nn_pred,lr_pred)
    write.csv(results[1:length(results[,1]),],file="results.csv", row.names = FALSE)
    write.table(results, file = "results_history.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
  })
  
  
  output$t <- renderText({
    print(v$text)
  })
}

shinyApp(ui, server)
