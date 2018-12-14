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


ui <- fluidPage(

# output varariables that represent generic info: n if simulation, train features, time/prec 
  textOutput("N_Sim"),
  tableOutput("InfoSim"),  

  
#buttons for next and prev simulation
  actionButton("Next","Next Simulation"),
  actionButton("Prev","Prev Simulation"),
  
  
#buttons to change the main graph:  
  actionButton("graph1","histogram1"),
  actionButton("graph2","histogram2"),
  actionButton("circle", "Circle graph"),

 
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
  numericInput("ind_var5", "ind_var5: select a value",0,min=0,max=1,step=1)
  
 
)

server <- function(input, output, session){
  # setting of directory
  path <- "/Users/user/Desktop/Pi2"
  setwd(path)
  
  # import the data: sample_test(the same that we use for app1)
  #                  result( fake data)
  RESULTS<- read.csv("results.csv", header=TRUE)
  TEST<- read.csv("sample_test.csv",header=TRUE)
  # trasport RESULTS and add colum name "M1","M2, ecc,find the  numb. of simulations
  RESULTS<-as.data.frame(t(RESULTS))
  colnames(RESULTS)=c("M1","M2","M1","M2")
  nSim=length(RESULTS)/2
  
  #fake Trainfeat and infoSimof 2 simulation
  trainFeat=data.frame("Level Of Noise"=c(1,0),
                       "% Data Used"=c(50,75),
                       "%Var Used"=c(100,20),
                       "prec NN"=c(100,20) , 
                       "PrecLR"=c(100,20),
                       "TIme NN"=c(100,20),
                       "TIME LR"=c(100,20))
  
  

  
  v<- reactiveValues(simSelected=data.frame(TEST,RESULTS[1:2]),
                     idSim=1, 
                     indSubset=c(1:length(TEST[,1])),  #we will use this vector to use only the data of the subset selected
                     v1 = 0,v2=0, v3=c(0,33),v4=c(0,18) ,v5=0)
                     infoTable=data.frame("NN"=c(10,0.90),"LR"=c(3,0.88),row.names = c("time","prec"))
  
  output$N_Sim = renderText(
    print(paste("Sim. N.",v$idSim))
   
  )
  
  output$InfoSim=renderTable(
    trainFeat[v$idSim,]
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
  
}


shinyApp(ui, server)

