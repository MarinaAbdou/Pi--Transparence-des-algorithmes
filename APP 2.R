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
  
  numericInput("var1", "var1: select a value",0,min=0,max=1,step=1),
  numericInput("var2", "var2: select a value",1,min=1,max=7,step=1),
  sliderInput("var3", "var3: select a range", min = 0, 
              max = 1000, value = c(0, 1000)),
  
  actionButton("nf","no filter"),
  actionButton("af","apply filter"),
  tableOutput("t"),
  textOutput("SubsetInfo"),
  
  actionButton("graph1","histogram1"),
  actionButton("graph2","histogram2"),
  
  plotOutput("graph")
  
  
)

server <- function(input, output){
  path <- "/Users/user/Desktop/Pi2"
  setwd(path)
  RESULTS<- read.csv("results.csv", header=TRUE)
  INFO <- read.csv("Info.csv", header=TRUE)
  row.names(INFO)=c("Precision","Time")
  output$t=renderTable(INFO,rownames = TRUE)
    
  v<- reactiveValues(var1 = 0,var2=1, var3=c(0,1000),specificSubset=RESULTS,leng=NULL)
  
  observeEvent(input$nf, {
    v$specificSubset=RESULTS
    v$leng=length( v$specificSubset[,1])
  }
  )
  
  observeEvent(input$af, {
    v$var1=input$var1
    v$var2=input$var2
    v$var3=input$var3
    v$specificSubset=RESULTS
    
    #filter 1 variable
    ind=which(v$specificSubset[,1]==v$var1)
    v$specificSubset=v$specificSubset[ind,]
    
    #filter 2 variable
    ind=which(v$specificSubset[,2]==v$var2)
    v$specificSubset=v$specificSubset[ind,]   
    
    #filter 3 variable
    ind=which(v$specificSubset[,3]>=v$var3[1] & v$specificSubset[,3]<=v$var3[2])
    v$specificSubset=v$specificSubset[ind,]  
    

    }
    )
  
  output$SubsetInfo <- renderText({
  print(paste("Number of observation in the subset=  ", length(v$specificSubset[,1])))
  })
  
  
  observeEvent(input$graph1, {
    output$graph=renderPlot(
      hist(v$specificSubset[,5])
    )
  })
  
  observeEvent(input$graph2, {
    output$graph=renderPlot(
      hist(v$specificSubset[,6])
    )
  })
  
  
  
  
  
}

shinyApp(ui, server)

