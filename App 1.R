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
  titlePanel("App1: Choice of 'a priori' features and running of the algorithms"),
  numericInput("noise", "Select a level of noise",0,min=0,max=4,step=1),
  numericInput("dataPerc","Select the percentage of observations to be used", 100,min=20,max=100,step=20),
  numericInput("varPerc","Select the percentage of variables to be used",  100, min=20, max=100,step=20),
  actionButton("go","Run algorithms"),
  
  textOutput("t")
)

server <- function(input, output){
  path <- "C:/Users/Marina/Documents/Semestre 9 (2018-2019)/Pi²/Pi2-Transparence-des-algorithmes-AlessandroBusato-patch-1"
  setwd(path)
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
  NData <- as.data.frame(lapply(NData, normalize))
  nxaxis <- c(sort(unique(NData[cvar1])[,1]))
  nyaxis <- c(sort(unique(NData[cvar2])[,1]))
  xgrid <- length(nxaxis)
  ygrid <- length(nyaxis)
  cor=abs(cor(NData[,1:(length(Train)-1)],NData[,length(Train)]))
  ordCor=order(cor,decreasing=TRUE)
  nvar1 <- match(var1,ordCor)
  nvar2 <- match(var2,ordCor)
  cnvar1 <- match(cvar1,ordCor)
  cnvar2 <- match(cvar2,ordCor)
  rangetrain<-1:nrow(Train)
  NTrain <- NData[rangetrain,]
  NTest <- NData[-rangetrain,]
  if (file.exists("simnn.csv")){
    file.remove("simnn.csv")
  }
  file.create("simnn.csv")
  if (file.exists("simlr.csv")){
    file.remove("simlr.csv")
  }
  file.create("simlr.csv")
  if (file.exists("results.csv")){
    file.remove("results.csv")
  }
  file.create("results.csv")
  if (file.exists("simdata.csv")){
    file.remove("simdata.csv")
  }
  if (file.exists("csimnn.csv")){
    file.remove("csimnn.csv")
  }
  file.create("csimnn.csv")
  if (file.exists("csimlr.csv")){
    file.remove("csimlr.csv")
  }
  file.create("csimlr.csv")
  a<-data.frame(t(c("Noise","% of Obs","% of Var","X4","X5")))
  write.table(a, file = "simdata.csv", sep = ",", append = TRUE, quote = TRUE, col.names = FALSE, row.names = FALSE)
  v<- reactiveValues(noise = NULL,dataPerc=NULL,varPerc=NULL, NTrain=NULL, NTest=NULL)
  

  observeEvent(input$go, {
    v$noise <- input$noise
    v$dataPerc<- input$dataPerc/100
    v$varPerc<- input$varPerc/100
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

    nvar <- as.integer((length(NTrain)-1)*v$varPerc)
    tar <- c(ordCor[1:nvar],length(NTrain))
    
    v$NTrain <- v$NTrain[tar]
    v$NTest <- v$NTest[tar]
    var1 <- nvar1
    var2 <- nvar2
    cvar1 <- cnvar1
    cvar2 <- cnvar2
    x<-data.frame(colMeans(v$NTest))
    y<-data.frame(t(data.frame(rep(x,each=(grid*(grid+1))))))
    ny<-data.frame(t(data.frame(rep(x,each=(xgrid*(ygrid))))))
    s1mi <- min(v$NTest[var1])
    s1ma <- max(v$NTest[var1])
    s2mi <- min(v$NTest[var2])
    s2ma <- max(v$NTest[var2])
    simul <- data.frame()
    s1 <- data.frame()
    s2 <- data.frame()
    for (i in 0:grid) {
      c1 <- data.frame(rep(s1mi+(s1ma-s1mi)*i/grid,grid))
      s1 <- rbind(s1,c1)
    }
    for (i in 0:grid) {
      c2 <- data.frame(s2mi+(s2ma-s2mi)*i/grid)
      s2 <- rbind(s2,c2)
    }
    s2<-data.frame((rep(t(s2),grid)))
    y[var1]<-s1
    y[var2]<-s2
    nsimul <- data.frame()
    ns1 <- data.frame()
    ns2 <- data.frame()
    for (i in nxaxis) {
      nc1 <- data.frame(rep(i,ygrid))
      ns1 <- rbind(ns1,nc1)
    }
    for (i in 1:xgrid) {
      ns2 <- rbind(ns2,data.frame(nyaxis))
    }
    ny[var1]<-ns1
    ny[var2]<-ns2
    write.csv(y[1:length(y[,1]),],file="simulated.csv", row.names = FALSE)
    write.csv(y[1:length(y[,1]),],file="nsimulated.csv", row.names = FALSE)
    write.csv(v$NTrain[1:length(v$NTrain[,1]),],file="train_features.csv", row.names = FALSE)
    write.csv(v$NTest[1:length(v$NTest[,1]),],file="test_features.csv", row.names = FALSE)
    timenn <- proc.time()
    py_run_file("NN.py")
    #end measure time
    proc.time() - timenn
    nn_simu <- py$simulation
    nn_csimu <- py$nsimulation
    timenn<-as.numeric(timenn[3])
    nn_pred <- py$y_pred_keras
    timelr <- proc.time()
    py_run_file("LR-code.py")
    #end measure time
    proc.time() - timelr
    timelr<-as.numeric(timelr[3])
    lr_simu <- py$lrsimulation
    lr_csimu <- py$nlrsimulation
    res_sim_nn <- data.frame(nn_simu[1:(grid+1)])
    res_sim_lr <- data.frame(lr_simu[1:(grid+1)])
    s<-grid+2
    for (i in 1:grid) {
      q <- s+grid
      rin <- nn_simu[s:q]
      ril <- lr_simu[s:q]
      res_sim_nn <- cbind(res_sim_nn,rin)
      res_sim_lr <- cbind(res_sim_lr,ril)
      s<-q+1
    }
    cmat <- as.matrix(res_sim_nn)
    gmat <- as.matrix(res_sim_lr)
    cres_sim_nn <- data.frame(nn_csimu[1:ygrid])
    cres_sim_lr <- data.frame(lr_csimu[1:ygrid])
    s<-ygrid+1
    for (i in 1:(xgrid-1)) {
      q <- s+ygrid-1
      rin <- nn_csimu[s:q]
      ril <- lr_csimu[s:q]
      cres_sim_nn <- cbind(cres_sim_nn,rin)
      cres_sim_lr <- cbind(cres_sim_lr,ril)
      s<-q+1
    }
    ccmat <- as.matrix(cres_sim_nn)
    cgmat <- as.matrix(cres_sim_lr)
    lr_pred <- py$y_pred_lr
    results <- rbind(nn_pred,lr_pred)
    simdata <- data.frame(t(c(v$noise,v$dataPerc,v$varPerc,timenn,timelr)))[1,]
    if (file.info("results.csv")$size == 0){
      write.csv(results[1:length(results[,1]),],file="results.csv", row.names = FALSE)
      write.table(simdata, file = "simdata.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
      write.csv(cmat[1:length(cmat[,1]),],file="simnn.csv", row.names = FALSE)
      write.csv(gmat[1:length(gmat[,1]),],file="simlr.csv", row.names = FALSE)
      write.csv(ccmat[1:length(ccmat[,1]),],file="csimnn.csv", row.names = FALSE)
      write.csv(cgmat[1:length(cgmat[,1]),],file="csimlr.csv", row.names = FALSE)
    }else{
      write.table(simdata, file = "simdata.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
      write.table(results, file = "results.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
      write.table(cmat, file = "simnn.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
      write.table(gmat, file = "simlr.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
      write.table(ccmat, file = "csimnn.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
      write.table(cgmat, file = "csimlr.csv", sep = ",", append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
      }
  })
  
  
  output$t <- renderText({
    print(paste(v$noise,v$dataPerc,v$varPerc))
  })
}

shinyApp(ui, server)
