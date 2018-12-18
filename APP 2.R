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
  
  # App title ----
  titlePanel("Title ?"),
  
  hr(),
  
  fluidRow(
    column(1,
           #n if simulation
           h4(textOutput("N_Sim"))),
    
    #buttons for next and prev simulation
    column(1,
           actionButton("Prev","Previous Simulation")),
    
    column(1,
           actionButton("Next","Next Simulation"))
  ),
  
  hr(),
  
  # output varariables that represent generic info: train features, time/prec 
  h4("Generic Info :"),
  
  fluidRow(
    column(3,
           tableOutput("InfoSim")),
    
    column(2,
           tableOutput("PrecTime")),
    
    column(6,
           tableOutput("GW"))
  ),
  
  hr(),
  
  h4("Select your subset :"),
  
  fluidRow(
    column(2,
           numericInput("ind_var30", "ind_var30: select a value",0,min=0,max=1,step=1)),
    
    column(2,
           numericInput("num_meses_var5_ult3", "num_meses_var5_ult3: select a value",0,min=0,max=3,step=1)),
    
    column(2,
           sliderInput("num_var30", "num_var30: select a range", min = 0, 
                       max = 33, value = c(0, 33))),
    
    column(2,
           sliderInput("num_var42", "num_var42: select a range", min = 0, 
                       max = 18, value = c(0, 18))),
    
    column(2,
           numericInput("ind_var5", "ind_var5: select a value",0,min=0,max=1,step=1)),
    
    # button to apply or not to apply the filter
    column(1,
           actionButton("nf","No filter")),
    
    column(1,
           actionButton("af","Apply filter"))
  ),
  
  hr(),
  
  fluidRow(
    column(5,
           h4("Select a graph :"),
           selectInput("N", "", c("Default","histo1", "histo2", "Classification Pie Chart", "ROC Curve")),
           plotOutput("graph")),
    
    column(6,
           #text in wich we represent nof obs, n of good prediction for both models, only one of them, no one
           h4(textOutput("SubsetInfo")),
           
           tableOutput("GWsubset"))
  )
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
  
  
  graphName <- reactive(as.character(input$N))
  
  v<- reactiveValues(simSelected=data.frame(TEST,RESULTS[1:2]),
                     idSim=1, 
                     indSubset=c(1:length(TEST[,1])),  
                     v1 = 0,v2=0, v3=c(0,33),v4=c(0,18),v5=0
                     )

  
  output$N_Sim = renderText(
    print(paste("Simulation ",v$idSim))
  )
  
  output$InfoSim=renderTable(
    trainFeat[v$idSim,]
 )
  
  output$PrecTime=renderTable(
    precTime[(v$idSim*2-1):(v$idSim*2)], rownames = TRUE
  )
  
  output$GW=renderTable(
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
    print(paste("Subset Info : Number of observations = ",length(v$indSubset)))
  )
  
  
  output$graph <- renderPlot({
    
    if(graphName()=="histo1"){
      hist(v$simSelected[v$indSubset,"M1"])
      
    }else if(graphName()=="histo2"){
      hist(v$simSelected[v$indSubset,"M2"])
      
    }else if(graphName()=="Classification Pie Chart"){

      pie(as.numeric(c(GW_glob[v$idSim*5-3],
                       GW_glob[v$idSim*5-2], 
                       GW_glob[v$idSim*5],
                       GW_glob[v$idSim*5-1])), 
          labels =c(paste("Classified correctly by BOTH \n Neural Networks AND Logistic Regression : \n",round(GW_glob[v$idSim*5-3]*100,0),"%"),
                    paste("Classified correctly by \n Neural Networks ONLY : \n",round(GW_glob[v$idSim*5-2]*100,0),"%"),
                    paste("Wrongly classified by BOTH \n Neural Networks AND Logistic Regression : \n",round(GW_glob[v$idSim*5]*100,0),"%"),
                    paste("Classified correctly by \n Logistic Regression ONLY : \n", round(GW_glob[v$idSim*5-1]*100,0),"%")),
          main = "Classification Pie Chart",
          col = c("#A9EAFE","#CECECE","#FDF1B8","#CECECE"))
      
    }else if(graphName()=="ROC Curve"){
      
    }
  })
  
}


shinyApp(ui, server)


