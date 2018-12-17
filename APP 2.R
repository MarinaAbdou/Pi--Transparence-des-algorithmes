library(shiny)
library(BBmisc)
library(fBasics)
library(tidyverse)
library(caret)

#
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
  
  dt=data.frame("%number obs"=c(l),
                "%GOOD PREDICTION BOTH MODELS"=c(g),
                "%GOOD PREDICTION ONLY 1"=c(g1),
                "%GOOD PREDICTION ONLY 2"=c(g2),
                "%BOTH WRONG PRED"=c(ng)
                )
  return(dt)
}

ui <- fluidPage(
  
  
# output varariables that represent generic info: n if simulation, train features, time/prec 
  textOutput("N_Sim"),
  tableOutput("InfoSim"),  
  tableOutput("PrecTime"),
  tableOutput("GW"),
  
  
#buttons for next and prev simulation
  actionButton("Next","Next Simulation"),
  actionButton("Prev","Prev Simulation"),
  
  
#buttons to change the main graph:  
  actionButton("graph1","histogram1"),
  actionButton("graph2","histogram2"),
  actionButton("pieChart1", "Neural Network : True Predictions VS Wrong Predictions"),
  actionButton("pieChart2", "Logistic Regression : True Predictions VS Wrong Predictions"),

 
#output variable that rapresent the main graph:
  plotOutput("graph"),
  
#text in wich we represent nof obs, n of good prediction for both models, only one of them, no one
  textOutput("SubsetInfo"),
  
 
 # button to apply or not to apply the filter
  actionButton("nf","No filter"),
  actionButton("af","Apply filter"),

#  input to select the subset when we press apply filter
  numericInput("ind_var30", "ind_var30: select a value",0,min=0,max=1,step=1),
  numericInput("num_meses_var5_ult3", "num_meses_var5_ult3: select a value",0,min=0,max=3,step=1),
  sliderInput("num_var30", "num_var30: select a range", min = 0, 
              max = 33, value = c(0, 33)),
  sliderInput("num_var42", "num_var42: select a range", min = 0, 
              max = 18, value = c(0, 18)),
  numericInput("ind_var5", "ind_var5: select a value",0,min=0,max=1,step=1),
  
  tableOutput("GWsubset")
)

server <- function(input, output){
  # setting of directory
  path <- "C:/Users/Marina/Documents/Semestre 9 (2018-2019)/Pi²/Pi2-Transparence-des-algorithmes-master"
  setwd(path)
  
  # import the data: sample_test(the same that we use for app1)
  #                  result( fake data)
  RESULTS<- read.csv("results.csv", header=TRUE)
  TEST<- read.csv("sample_test.csv",header=TRUE)
 
  # trasport RESULTS and add colum name "M1","M2, ecc,find the  numb. of simulations
  RESULTS<-as.data.frame(t(RESULTS))
  colnames(RESULTS)=c("M1","M2","M1","M2")
  nSim=length(RESULTS)/2
  
  #create the prediction table
  PREDICTION=RESULTS
  PREDICTION[PREDICTION<0.5]=0
  PREDICTION[PREDICTION>=0.5]=1
  
  
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
  trainFeat=data.frame("Level Of Noise"=c(1,0),
                       "% Data Used"=c(50,75),
                       "%Var Used"=c(100,20)
                       )
  precTime=data.frame(c(10,0.90),c(3,0.88),c(15,0.95),c(5,0.9), row.names = c("time","prec"))
  colnames(precTime)=c("NN","LR","NN","LR")
  
  
  
  
  v<- reactiveValues(simSelected=data.frame(TEST,RESULTS[1:2]),
                     idSim=1, 
                     indSubset=c(1:length(TEST[,1])),  
                     v1 = 0,v2=0, v3=c(0,33),v4=c(0,18),v5=0
                     )

  
  output$N_Sim = renderText(
    print(paste("Sim. N.",v$idSim))
  )
  
  output$InfoSim=renderTable(
    trainFeat[v$idSim,]
 )
  
  output$PrecTime=renderTable(
    precTime[(v$idSim*2-1):(v$idSim*2)], rownames = TRUE
  )
  
  output$GW=renderTable(
    #GoodWrong(TEST["TARGET"],PREDICTION[v$idSim*2-1],PREDICTION[v$idSim*2])
    GW_glob[(v$idSim*5-4):(v$idSim*5)]
  )
  
  output$GWsubset=renderTable(
    GoodWrong(data.frame(TEST[v$indSubset,"TARGET"]),
              data.frame(PREDICTION[v$indSubset,v$idSim*2-1]),
              data.frame(PREDICTION[v$indSubset,v$idSim*2])
              )
  )
  

  
  
  

  observeEvent(input$Next, {
    a=v$idSim
    if(a<nSim){
      v$simSelected["M1"]=RESULTS[,(v$idSim+1)*2-1]
      v$simSelected["M2"]=RESULTS[,(v$idSim+1)*2]
      v$idSim=v$idSim+1
    }
  })
  observeEvent(input$Prev, {
    if(v$idSim>1){
      v$simSelected["M1"]=RESULTS[,(v$idSim-1)*2-1]
      v$simSelected["M2"]=RESULTS[,(v$idSim-1)*2]
      v$idSim=v$idSim-1
    }
  })
  
  observeEvent(input$nf, {
    v$indSubset=c(1:length(TEST[,1]))
  })
  observeEvent(input$af, {
    v$var1=input$ind_var30
    v$var2=input$num_meses_var5_ult3
    v$var3=input$num_var30
    v$var4=input$num_var42
    v$var5=input$ind_var5
    
    #filter variable
    v$indSubset=which(TEST["ind_var30"]==v$var1 
                      & TEST["num_meses_var5_ult3"]==v$var2
                      & TEST["num_var30"]>v$var3[1]
                      & TEST["num_var30"]<v$var3[2]
                      & TEST["num_var42"]>v$var4[1]
                      & TEST["num_var42"]<v$var4[2]
                      & TEST["ind_var5"]==v$var5)

    })
  
  
  output$SubsetInfo=renderText(
    print(paste("SUBSET INFO: n. of obs= ",length(v$indSubset)))
  )
  observeEvent(input$graph1, {
    output$graph=renderPlot(
      hist(v$simSelected[v$indSubset,"M1"])
    )
  })
  observeEvent(input$graph2, {
    output$graph=renderPlot(
      hist(v$simSelected[v$indSubset,"M2"])
    )
  })
  observeEvent(input$pieChart1, {
    output$graph=renderPlot(
      pie(as.numeric(c(GW_glob[v$idSim*5-2], 1.00 - GW_glob[v$idSim*5-2])), 
          labels =c(paste("True Predictions for Neural Networks : ",round(GW_glob[v$idSim*5-2]*100,1),"%"),paste("Wrong Predictions for Neural Networks : ", round((1.00 - GW_glob[v$idSim*5-2])*100,1),"%")),
          main = "Neural Network : True Predictions VS Wrong Predictions", 
          col = c("#336699","#FFFFFF"))
      )
  })
  observeEvent(input$pieChart2, {
    output$graph=renderPlot(
      pie(as.numeric(c(GW_glob[v$idSim*5-1], 1.00 - GW_glob[v$idSim*5-1])), 
          labels =c(paste("True Predictions for Logistic Regression : ",round(GW_glob[v$idSim*5-1]*100,1),"%"),paste("Wrong Predictions for Logistic Regression : ", round((1.00 - GW_glob[v$idSim*5-1])*100,1),"%")),
          main = "Logistic Regression : True Predictions VS Wrong Predictions", 
          col = c("#336699","#FFFFFF"))
    )
  })
  
}


shinyApp(ui, server)


