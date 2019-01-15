path <- "C:/Users/Marina/Documents/Semestre 9 (2018-2019)/Pi²/Pi2-Transparence-des-algorithmes-AlessandroBusato-patch-1"
setwd(path)

source("App2_Settings.R")

ui <- fluidPage(
  # App title ----
  titlePanel("App2: Compatison between the results"),
  hr(),
  fluidRow(
    column(1,
           #n if simulation
           h4(textOutput("N_Sim"))),
    #buttons for next and prev simulation
    column(1,
           actionButton("Prev","Previous")),
    
    column(1,
           actionButton("Next","Next"))
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
  
  fluidRow(
    column(6,
           plotOutput("ROCGlob")),
    
    column(6,
           plotOutput("PieGlob"))
  ),
  
  hr(),
  
  h4("Select your subset :"),
  
  fluidRow(
    #column(2,
    #       numericInput("ind_var30", "ind_var30: select a value",0,min=0,max=1,step=1)),
    column(2,
           selectInput("ind_var30", "ind_var30: select a value",c("No Filter","0","1"))),
    
    #column(2,
    #       numericInput("num_meses_var5_ult3", "num_meses_var5_ult3: select a value",0,min=0,max=3,step=1)),
    
    column(2,
           selectInput("num_meses_var5_ult3", "num_meses_var5_ult3: select a value",c("No Filter","0","1","2","3"))),
    
    column(2,
           sliderInput("num_var30", "num_var30: select a range", min = 0, 
                       max = 33, value = c(0, 33))),
    
    column(2,
           sliderInput("num_var42", "num_var42: select a range", min = 0, 
                       max = 18, value = c(0, 18))),
    
    #↨column(2,
    #       numericInput("ind_var5", "ind_var5: select a value",0,min=0,max=1,step=1)),
    
    column(2,
           selectInput("ind_var5", "ind_var5: select a value",c("No Filter","0","1"))),
    
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
  ),
  
  hr(),
  
  fluidRow(
    column(5,
           h4("Distribution of the results for a variable:"),
           selectInput("VarForResultsDistr","Select a variable to visualize the distribution of the results",c(colnames(TEST[morCorrIndex]))),
           tableOutput("ResDisTab"),
           h4("Distribution of misclassified samples compared to test distribution"),
           tableOutput("Misobs")
  )
  ),
 
  hr(),
  fluidRow(
    column(5,
           h4(""),
           plotlyOutput("heatmap", width = "100%", height="500px")),
    column(5,
           h4(" "),
           plotlyOutput("heatmaplr", width = "100%", height="500px")),
    column(5,
           h4(""),
           plotlyOutput("cheatmap", width = "100%", height="500px")),
    column(5,
           h4(""),
           plotlyOutput("cheatmaplr", width = "100%", height="500px")
    )
  )
  
)

server <- function(input, output){
  
  graphName <- reactive(as.character(input$N))
  varForDisTab <- reactive(as.character(input$VarForResultsDistr))
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
    if(input$ind_var30 == "No Filter" && input$num_meses_var5_ult3 == "No Filter" && input$ind_var5 == "No Filter"){
      v$var3=input$num_var30
      v$var4=input$num_var42
      
      #filter variable
      v$indSubset=which(TEST["num_var30"]>v$var3[1]
                        & TEST["num_var30"]<v$var3[2]
                        & TEST["num_var42"]>v$var4[1]
                        & TEST["num_var42"]<v$var4[2])
    }else if(input$ind_var30 == "No Filter" && input$num_meses_var5_ult3 == "No Filter" && input$ind_var5 != "No Filter"){
      v$var3=input$num_var30
      v$var4=input$num_var42
      v$var5=as.numeric(input$ind_var5)
      
      #filter variable
      v$indSubset=which(TEST["num_var30"]>v$var3[1]
                        & TEST["num_var30"]<v$var3[2]
                        & TEST["num_var42"]>v$var4[1]
                        & TEST["num_var42"]<v$var4[2]
                        & TEST["ind_var5"]==v$var5)
    }else if(input$ind_var30 == "No Filter" && input$num_meses_var5_ult3 != "No Filter" && input$ind_var5 == "No Filter"){
      v$var2=as.numeric(input$num_meses_var5_ult3)
      v$var3=input$num_var30
      v$var4=input$num_var42
      
      #filter variable
      v$indSubset=which(TEST["num_meses_var5_ult3"]==v$var2
                        & TEST["num_var30"]>v$var3[1]
                        & TEST["num_var30"]<v$var3[2]
                        & TEST["num_var42"]>v$var4[1]
                        & TEST["num_var42"]<v$var4[2])
    }else if(input$ind_var30 != "No Filter" && input$num_meses_var5_ult3 == "No Filter" && input$ind_var5 == "No Filter"){
      v$var1=as.numeric(input$ind_var30)
      v$var3=input$num_var30
      v$var4=input$num_var42
      
      #filter variable
      v$indSubset=which(TEST["ind_var30"]==v$var1 
                        & TEST["num_var30"]>v$var3[1]
                        & TEST["num_var30"]<v$var3[2]
                        & TEST["num_var42"]>v$var4[1]
                        & TEST["num_var42"]<v$var4[2])
    }else if(input$ind_var30 != "No Filter" && input$num_meses_var5_ult3 != "No Filter" && input$ind_var5 == "No Filter"){
      v$var1=as.numeric(input$ind_var30)
      v$var2=as.numeric(input$num_meses_var5_ult3)
      v$var3=input$num_var30
      v$var4=input$num_var42
      
      #filter variable
      v$indSubset=which(TEST["ind_var30"]==v$var1 
                        & TEST["num_meses_var5_ult3"]==v$var2
                        & TEST["num_var30"]>v$var3[1]
                        & TEST["num_var30"]<v$var3[2]
                        & TEST["num_var42"]>v$var4[1]
                        & TEST["num_var42"]<v$var4[2])
    }else if(input$ind_var30 == "No Filter" && input$num_meses_var5_ult3 != "No Filter" && input$ind_var5 != "No Filter"){
      v$var2=as.numeric(input$num_meses_var5_ult3)
      v$var3=input$num_var30
      v$var4=input$num_var42
      v$var5=as.numeric(input$ind_var5)
      
      #filter variable
      v$indSubset=which(TEST["num_meses_var5_ult3"]==v$var2
                        & TEST["num_var30"]>v$var3[1]
                        & TEST["num_var30"]<v$var3[2]
                        & TEST["num_var42"]>v$var4[1]
                        & TEST["num_var42"]<v$var4[2]
                        & TEST["ind_var5"]==v$var5)
    }else if(input$ind_var30 != "No Filter" && input$num_meses_var5_ult3 == "No Filter" && input$ind_var5 != "No Filter"){
      v$var1=as.numeric(input$ind_var30)
      v$var3=input$num_var30
      v$var4=input$num_var42
      v$var5=as.numeric(input$ind_var5)
      
      #filter variable
      v$indSubset=which(TEST["ind_var30"]==v$var1 
                        & TEST["num_var30"]>v$var3[1]
                        & TEST["num_var30"]<v$var3[2]
                        & TEST["num_var42"]>v$var4[1]
                        & TEST["num_var42"]<v$var4[2]
                        & TEST["ind_var5"]==v$var5)
    }else if(input$ind_var30 != "No Filter" && input$num_meses_var5_ult3 != "No Filter" && input$ind_var5 != "No Filter"){
      v$var1=as.numeric(input$ind_var30)
      v$var2=as.numeric(input$num_meses_var5_ult3)
      v$var3=input$num_var30
      v$var4=input$num_var42
      v$var5=as.numeric(input$ind_var5)
      
      #filter variable
      v$indSubset=which(TEST["ind_var30"]==v$var1 
                        & TEST["num_meses_var5_ult3"]==v$var2
                        & TEST["num_var30"]>v$var3[1]
                        & TEST["num_var30"]<v$var3[2]
                        & TEST["num_var42"]>v$var4[1]
                        & TEST["num_var42"]<v$var4[2]
                        & TEST["ind_var5"]==v$var5)
    }
    
    #v$var1=input$ind_var30
    #v$var2=input$num_meses_var5_ult3
    #v$var3=input$num_var30
    #v$var4=input$num_var42
    #v$var5=input$ind_var5
    
    ##filter variable
    #v$indSubset=which(TEST["ind_var30"]==v$var1 
    #                  & TEST["num_meses_var5_ult3"]==v$var2
    #                  & TEST["num_var30"]>v$var3[1]
    #                  & TEST["num_var30"]<v$var3[2]
    #                  & TEST["num_var42"]>v$var4[1]
    #                  & TEST["num_var42"]<v$var4[2]
    #                  & TEST["ind_var5"]==v$var5)
    
  })
  
  
  output$SubsetInfo=renderText(
    print(paste("Subset Info : Number of observations = ",length(v$indSubset)))
  )
  
  
  output$ROCGlob <- renderPlot({
    predLR <- prediction(RESULTS[nSim*v$idSim],TEST["TARGET"])
    predNN <- prediction(RESULTS[nSim*v$idSim-1],TEST["TARGET"])
    perfLR <- performance(predLR,'tpr','fpr')
    perfNN <- performance(predNN,'tpr','fpr')
    ROCgraph <- plot(perfNN,main="ROC curve",col="red")
    par(new=TRUE)
    plot(perfLR,col="blue")
    abline(0,1)
    legend("topleft", legend=c("Neural Networks", "Logistic Regression"),
           col=c("red", "blue"), lty=1:1, cex=0.8)
  })
  
  output$PieGlob <- renderPlot({
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
  })
  
  output$graph <- renderPlot({
    
    if(graphName()=="histo1"){
      hist(v$simSelected[v$indSubset,"M1"])
      
    }else if(graphName()=="histo2"){
      hist(v$simSelected[v$indSubset,"M2"])
      
    }else if(graphName()=="Classification Pie Chart"){
      
      GWSub <-  GoodWrong(data.frame(TEST[v$indSubset,"TARGET"]),
                          data.frame(PREDICTION[v$indSubset,v$idSim*2-1]),
                          data.frame(PREDICTION[v$indSubset,v$idSim*2]))
      
      pie(
        as.numeric(c(GWSub[v$idSim*5-3],
                     GWSub[v$idSim*5-2], 
                     GWSub[v$idSim*5],
                     GWSub[v$idSim*5-1])), 
        
        labels =c(paste("Classified correctly by BOTH \n Neural Networks AND Logistic Regression : \n",round(GWSub[v$idSim*5-3]*100,0),"%"),
                  paste("Classified correctly by \n Neural Networks ONLY : \n",round(GWSub[v$idSim*5-2]*100,0),"%"),
                  paste("Wrongly classified by BOTH \n Neural Networks AND Logistic Regression : \n",round(GWSub[v$idSim*5]*100,0),"%"),
                  paste("Classified correctly by \n Logistic Regression ONLY : \n", round(GWSub[v$idSim*5-1]*100,0),"%")),
        
          main = "Classification Pie Chart",
          col = c("#A9EAFE","#CECECE","#FDF1B8","#CECECE"))
      
    }else if(graphName()=="ROC Curve"){
      predLR <- prediction(RESULTS[v$indSubset,nSim*v$idSim],TEST[v$indSubset,"TARGET"])
      predNN <- prediction(RESULTS[v$indSubset,nSim*v$idSim-1],TEST[v$indSubset,"TARGET"])
      perfLR <- performance(predLR,'tpr','fpr')
      perfNN <- performance(predNN,'tpr','fpr')
      ROCgraph <- plot(perfNN,main="ROC curve",col="red")
      par(new=TRUE)
      plot(perfLR,col="blue")
      abline(0,1)
      legend("topleft", legend=c("Neural Networks", "Logistic Regression"),
             col=c("red", "blue"), lty=1:1, cex=0.8)
    }
  })
  output$ResDisTab=renderTable(
    clusterResults(varForDisTab(),TEST,PREDICTION[,v$idSim*2-1],PREDICTION[,v$idSim*2]),rownames = TRUE
  )
  output$Misobs=renderTable(
    DistVarPred(varForDisTab(),Test[Misclassified(TEST[,length(TEST[1,])],PREDICTION[,v$idSim*2-1])])
  )
  output$heatmap <- renderPlotly({
    simicmat <- cmat[((v$idSim-1)*(grid+1)+1):((v$idSim)*(grid+1)),]
    plot_ly(z = simicmat, type = "heatmap",x = xaxis, y = yaxis , height = 400, width = 400)})
  output$heatmaplr <- renderPlotly({
    simigmat <- gmat[((v$idSim-1)*(grid+1)+1):((v$idSim)*(grid+1)),]
    plot_ly(z = simigmat, type = "heatmap",x = xaxis, y = yaxis, height = 400, width = 400)})
  output$cheatmap <- renderPlotly({
    st<-((v$idSim-1)*((length(cyaxis))+1))
    fi<-((v$idSim)*(length(cyaxis)))
    simiccmat <- ccmat[st:fi,]
    plot_ly(z = simiccmat, type = "heatmap",x = cxaxis, y = cyaxis , height = 400, width = 400)})
  output$cheatmaplr <- renderPlotly({
    st<-((v$idSim-1)*((length(cyaxis))+1))
    fi<-((v$idSim)*(length(cyaxis)))
    simicgmat <- cgmat[st:fi,]
    plot_ly(z = simicgmat, type = "heatmap",x = cxaxis, y = cyaxis, height = 400, width = 400)})
}
shinyApp(ui, server)
