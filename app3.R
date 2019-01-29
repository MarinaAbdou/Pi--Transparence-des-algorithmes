
library(shinyjs)
library(shiny)
library(BBmisc)
library(fBasics)
library(tidyverse)
library(caret) 
path <- "C:/Users/Marina/Documents/Semestre 9 (2018-2019)/Pi²/Pi2-Transparence-des-algorithmes-master"
setwd(path)
library(shinythemes)

source('App3/App3-code.R')


ui <- fluidPage(
  
  includeCSS("styles.css"),
  
  theme = shinytheme("spacelab"),
  
  fluidRow(headerPanel("Behaviour of the models")), 
  
  hr(),
  
  h3("Test informations : "),
  
  tableOutput("info"),
  
  hr(),
  
  h3("Behaviour of the two models given a noise level : "),
  
  hr(),
 
 sidebarLayout(
   sidebarPanel(
     selectInput("noise","Noise level",c("No noise"=1,"lOW"=2,"medium"=3,"high"=4,"NoiseG"=5),selected = 1),
     selectInput("ni", "Feature to visualize",
                 c("% good predictions"=1,
                   "AUC"=2,
                   "Time"=3,
                   "AUC/Time"=4,
                   "% good predictions/Time"=5
                 ), selected=1)
   ),
   
   mainPanel(
     fluidRow(
       
       column(6, align = "center",
              h5("Model 1"),
              tableOutput("tn1")),
              
       column(6, align = "center",
              h5("Model 2"),
              tableOutput("tn2"))
     )
   )
 ),
  
  
  hr(), 
 
 h3("Behaviour of the two models given an observations percentage : "),
  
 hr(), 
 
 sidebarLayout(
   sidebarPanel(
     selectInput("pdata","Observations percentage",c("20%"=1,"40%"=2,"60%"=3,"80%"=4,"100%"=5),selected=5),
     selectInput("di", "Feature to visualize",
                 c("% good predictions"=1,
                   "AUC"=2,
                   "Time"=3,
                   "AUC/Time"=4,
                   "% good predictions/Time"=5
                 ),selected=1)
   ),
   
   mainPanel(
     column(6, align = "center",
            h5("Model 1"),
            tableOutput("td1")
     ),
     
     column(6, align = "center",
            h5("Model 2"),
            tableOutput("td2")
     )
   )
 ),
 
 hr(),
 
 h3("Behaviour of the two models given a variables percentage : "),
 
 hr(),
 
 sidebarLayout(
   sidebarPanel(
     selectInput("pvar","Variables percentage",c("20%"=1,"40%"=2,"60%"=3,"80%"=4,"100%"=5),selected = 5),
     selectInput("vi", "Feature to visualize",
                 c("% good predictions"=1,
                   "AUC"=2,
                   "Time"=3,
                   "AUC/Time"=4,
                   "% good predictions/Time"=5
                 ),selected=1)
   ),
   
   mainPanel(
     fluidRow(
       column(6, align = "center",
              h5("Model 1"),
              tableOutput("tv1")),
       
       column(6, align = "center",
              h5("Model 2"),
              tableOutput("tv2"))
     )
   )
 ),
 
 h3("Behaviour of the two models in relation to the noise level used for the train : "),
   
   sidebarLayout(
     sidebarPanel(
       selectInput("nd","Observations percentage",c("20%"=1,"40%"=2,"60%"=3,"80%"=4,"100%"=5),selected=5),
       selectInput("nv","Variables percentage",c("20%"=1,"40%"=2,"60%"=3,"80%"=4,"100%"=5),selected=5),
       selectInput("ni2", "Feature to visualize",
                   c("% good predictions"=1,
                     "AUC"=2,
                     "Time"=3,
                     "AUC/Time"=4,
                     "% good predictions/Time"=5
                   ),selected=1)
     ),
     
     mainPanel(
       
       plotOutput("pn")
       
     )
   ),
 
 h3("Behaviour of the two models in relation to the observations percentage used for the train : "),
 
 sidebarLayout(
   sidebarPanel(
     selectInput("dn","Noise level",c("No noise"=1,"0"=2,"2"=3,"3"=4,"NoiseG"=5),selected = 1),
     selectInput("dv","Variables percentage",c("20%"=1,"40%"=2,"60%"=3,"80%"=4,"100%"=5),selected=5),
     selectInput("di2", "Feature to visualize",
                 c("% good predictions"=1,
                   "AUC"=2,
                   "Time"=3,
                   "AUC/Time"=4,
                   "% good predictions/Time"=5
                 ),selected=1)
   ),
   
   mainPanel(
     
     plotOutput("pd")
     
   )
 ),
 
 h3("Behaviour of the two models in relation to the variables percentage used for the train : "),
 
 sidebarLayout(
   sidebarPanel(
     selectInput("vn","Noise level",c("No noise"=1,"0"=2,"2"=3,"3"=4,"NoiseG"=5),selected = 1),
     selectInput("vd","Observations percentage",c("20%"=1,"40%"=2,"60%"=3,"80%"=4,"100%"=5),selected=5),
     selectInput("vi2", "Feature to visualize",
                 c("% good predictions"=1,
                   "AUC"=2,
                   "Time"=3,
                   "AUC/Time"=4,
                   "% good predictions/Time"=5
                 ),selected=1)
   ),
   
   mainPanel(
     
     plotOutput("pv")
     
   )
 )
   

 )




  
server <- function(input, output){
  
  output$info=renderTable({
    infoTab},rownames = FALSE
    )
  
  n=reactive(as.integer(input$noise))
  ni=reactive(as.integer(input$ni))
  
  d=reactive(as.integer(input$pdata))
  di=reactive(as.integer(input$di))
  
  v=reactive(as.integer(input$pvar))
  vi=reactive(as.integer(input$vi))
  
  nd=reactive(as.integer(input$nd))
  nv=reactive(as.integer(input$nv))
  ni2=reactive(as.integer(input$ni2))
  
  dn=reactive(as.integer(input$dn))
  dv=reactive(as.integer(input$dv))
  di2=reactive(as.integer(input$di2))
  
  vn=reactive(as.integer(input$vn))
  vd=reactive(as.integer(input$vd))
  vi2=reactive(as.integer(input$vi2))

  
  output$tn1=renderTable({
    tabToPlotN(n(),totData[[ni()]][[1]])}, rownames = TRUE,
    caption = "% of observations used (vertical) , % of variables used (horizontal)",
    caption.placement = getOption("xtable.caption.placement", "bottom"), 
    caption.width = getOption("xtable.caption.width", NULL)
  )
  
output$tn2=renderTable({
   tabToPlotN(n(),totData[[ni()]][[2]])}, rownames = TRUE,
  caption = "% of observations used (vertical) , % of variables used (horizontal)",
 caption.placement = getOption("xtable.caption.placement", "bottom"), 
caption.width = getOption("xtable.caption.width", NULL)
)
  
  output$td1=renderTable({
    tabToPlotD(d(),totData[[di()]][[1]])}, rownames = TRUE
  )
  output$td2=renderTable({
    tabToPlotD(d(),totData[[di()]][[2]])}, rownames = TRUE
    )
  output$tv1=renderTable({
    tabToPlotV(v(),totData[[vi()]][[1]])}, rownames = TRUE
    )
  output$tv2=renderTable({
    tabToPlotV(v(),totData[[vi()]][[2]])}, rownames = TRUE
  )
  
  
  
  output$pn=renderPlot({
    noise_level=c(0,1,2,3,4)
    y=totData[[ni2()]][[1]][,nd(),nv()]
    y2=totData[[ni2()]][[2]][,nd(),nv()]
    d=rbind(y,y2)
    mi=min(min(y),min(y2))
    ma=max(max(y),max(y2))
    barplot(d, beside=T,ylab=findYlabel(input$ni2), 
            names.arg = c("No","Low","Medium","High","Bernoulli"),
            cex.names=1, las=0, ylim=c(mi-0.02,ma),
            xpd = FALSE, col=c("darkblue","red"))
    legend("topright", 
           legend = c("Model 1", "Model 2"), 
           fill = c("darkblue", "red"))
    }
  )
  
  output$pd=renderPlot({
    Perc_Of_Obs=c(20,40,60,80,100)
    y=totData[[di2()]][[1]][dn(),,dv()]
    y2=totData[[di2()]][[2]][dn(),,dv()]
    d=rbind(y,y2)
    mi=min(min(y),min(y2))
    ma=max(max(y),max(y2))
    barplot(d, beside=T,ylab=findYlabel(input$di2), 
            names.arg = Perc_Of_Obs,
            cex.names=1, las=0, ylim=c(mi-0.02,ma),
            xpd = FALSE, col=c("darkblue","red"))
    legend("topright", 
           legend = c("Model 1", "Model 2"), 
           fill = c("darkblue", "red"))
    
  }
  )
  output$pv=renderPlot({
    Perc_Of_Var=c(20,40,60,80,100)
    y=totData[[vi2()]][[1]][vn(),vd(),]
    y2=totData[[vi2()]][[2]][vn(),vd(),]
    d=rbind(y,y2)
    mi=min(min(y),min(y2))
    ma=max(max(y),max(y2))
    barplot(d, beside=T,ylab=findYlabel(input$vi2), 
            names.arg = Perc_Of_Var,
            cex.names=1, las=0, ylim=c(mi-0.02,ma),
            xpd = FALSE, col=c("darkblue","red"))
    legend("topright", 
           legend = c("Model 1", "Model 2"), 
           fill = c("darkblue", "red"))
    
  }
  )
  

  
  


}
  
shinyApp(ui, server)
  
  
