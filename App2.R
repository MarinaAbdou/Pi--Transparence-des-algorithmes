#path <- "C:/Users/Marina/Documents/Semestre 9 (2018-2019)/Pi²/Pi2-Transparence-des-algorithmes-master"
#setwd(path)

#install.packages("shinythemes")
library(shinythemes)

source("App2_Settings.R")

ui <- fluidPage(
  
  includeCSS("styles.css"),
  
  theme = shinytheme("spacelab"),
  
  fluidRow(
    column(6, align = "left",
           actionButton("Prev","Previous")),
    
    column(6, align = "right",
           actionButton("Next","Next"))
  ),
  
  # App title ----
  fluidRow(headerPanel(textOutput("Title"))), 
  hr(),
  
  # output varariables that represent generic info: train features, time/prec 
  h3("Generic Informations :"),
  
  fluidRow(align = "center",
           column(3, align = "center",
                  tableOutput("InfoSim"),
                  tableOutput("PrecTime")),
           
           column(6, align = "center",
                  tableOutput("GW"))
  ),
  
  hr(),
  
  fluidRow(align = "center",
           column(6, align = "center",
                  plotOutput("ROCGlob")),
           
           column(6, align = "center",
                  plotOutput("PieGlob"))
  ),
  
  hr(),
  
  h3("Subset Informations : "),
  
  fluidRow(
    
    sidebarLayout(
      sidebarPanel(h5("Select your subset :"),
                   selectInput("ind_var30", "Select a value for ind_var30 : ",c("No Filter","0","1")),
                   selectInput("num_meses_var5_ult3", "Select a value for num_meses_var5_ult3 : ",c("No Filter","0","1","2","3")),
                   sliderInput("num_var30", "Select a range for num_var30 : ", min = 0, max = 33, value = c(0, 33)),
                   sliderInput("num_var42", "Select a range for num_var42 : ", min = 0, max = 18, value = c(0, 18)),
                   selectInput("ind_var5", "Select a value ind_var5 : ",c("No Filter","0","1")),
                   hr(),
                   fluidRow(align = "center",
                            column(6, align = "center",
                                   actionButton("nf","No filter")),
                            column(6, align = "center",
                                   actionButton("af","Apply filter")))),
      mainPanel(#text in wich we represent nof obs, n of good prediction for both models, only one of them, no one
        h5(textOutput("SubsetInfo")),
        tableOutput("GWsubset"),
        hr(),
        h5("Select a graph :"),
        selectInput("N", "", c("Default","histo1", "histo2", "Classification Pie Chart", "ROC Curve")),
        plotOutput("graph"))
    )
  ),
  
  hr(),
  
  h3("Distribution of the results for a variable :"),
  
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        selectInput("VarForResultsDistr","Select a variable to visualize the distribution of the results",c(colnames(TEST[morCorrIndex])))
      ),
      mainPanel(align = "center",
                tableOutput("ResDisTab")
      )
    )),
  
  hr(),
  
  h3(" Distribution of misclassified samples compared to test distribution"),
  
  fluidRow(tableOutput("Misobs")),
  
  hr(),
  
  h3("Changing of the prediction with the change of two variables"),
  
  hr(),
  
  fluidRow(align = "center",
           
           column(5,
                  h5("Neural Network, Numeric Var"),
                  plotlyOutput("heatmap", width = "100%", height="500px")),
           column(5,
                  h5("Logistic Regression, Numeric Var"),
                  plotlyOutput("heatmaplr", width = "100%", height="500px")),
           column(5,
                  h5("Neural Network, Categorical Var"),
                  plotlyOutput("cheatmap", width = "100%", height="500px")),
           column(5,
                  h5("Logistic Regression, Categorical Var"),
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
  
  output$Title = renderText(
    print(paste("Comparison between the results for simulation ",v$idSim))
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
    v$indSubset=indexSubset(TEST,v$var1,v$var2,v$var3,v$var4,v$var5)
    
  })
  
  
  output$SubsetInfo=renderText(
    print(paste("Number of observations = ",length(v$indSubset)))
  )
  
  
  output$ROCGlob <- renderPlot({
    predLR <- prediction(RESULTS[2*v$idSim],TEST["TARGET"])
    predNN <- prediction(RESULTS[2*v$idSim-1],TEST["TARGET"])
    perfLR <- performance(predLR,'tpr','fpr')
    perfNN <- performance(predNN,'tpr','fpr')
    ROCgraph <- plot(perfNN,main="Generic ROC curve",col="red")
    par(new=TRUE)
    plot(perfLR,col="blue")
    abline(0,1)
    legend("topleft", legend=c("Model 1", "Model 2"),
           col=c("red", "blue"), lty=1:1, cex=0.8)
  })
  
  output$PieGlob <- renderPlot({
    pie(as.numeric(c(GW_glob[v$idSim*5-3],
                     GW_glob[v$idSim*5-2], 
                     GW_glob[v$idSim*5],
                     GW_glob[v$idSim*5-1])), 
        
        labels =c(paste("Correctly classified : \n",round(GW_glob[v$idSim*5-3],2),"%"),
                  paste("Classified correctly by \n Model 1 ONLY : \n",round(GW_glob[v$idSim*5-2],2),"%"),
                  paste("Wrongly classified : \n",round(GW_glob[v$idSim*5],2),"%"),
                  paste("Classified correctly by \n Model 2 ONLY : \n", round(GW_glob[v$idSim*5-1],2),"%")),
        
        main = "Generic Classification Pie Chart",
        col = c("#87CEFA","#BAB1B1","#AD1D28","#BAB1B1"))
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
        as.numeric(c(GWSub[2],
                     GWSub[3], 
                     GWSub[5],
                     GWSub[4])), 
        
        labels =c(paste("Correctly classified : \n",round(GWSub[2],2),"%"),
                  paste("Classified correctly by \n Model 1 ONLY : \n",round(GWSub[3],2),"%"),
                  paste("Wrongly classified : \n",round(GWSub[5],2),"%"),
                  paste("Classified correctly by \n Model 2 ONLY : \n", round(GWSub[4],2),"%")),
        
        main = "Subset Classification Pie Chart",
        col = c("#87CEFA","#BAB1B1","#AD1D28","#BAB1B1"))
      
    }else if(graphName()=="ROC Curve"){
      predLR <- prediction(RESULTS[v$indSubset,2*v$idSim],TEST[v$indSubset,"TARGET"])
      predNN <- prediction(RESULTS[v$indSubset,2*v$idSim-1],TEST[v$indSubset,"TARGET"])
      perfLR <- performance(predLR,'tpr','fpr')
      perfNN <- performance(predNN,'tpr','fpr')
      ROCgraph <- plot(perfNN,main="Subset ROC curve",col="red")
      par(new=TRUE)
      plot(perfLR,col="blue")
      abline(0,1)
      legend("topleft", legend=c("Model 1", "Model 2"),
             col=c("red", "blue"), lty=1:1, cex=0.8)
    }
  })
  output$ResDisTab=renderTable(
    clusterResults(varForDisTab(),TEST,PREDICTION[,v$idSim*2-1],PREDICTION[,v$idSim*2]),rownames = TRUE
  )
  output$Misobs=renderTable(
    table1<-Uni_Table(varForDisTab(),TEST[Misclassified(c(TEST[,length(TEST[1,])]),PREDICTION[,v$idSim*2-1]),],TEST[Misclassified(c(TEST[,length(TEST[1,])]),PREDICTION[,v$idSim*2]),],TEST),
    rownames = TRUE
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
