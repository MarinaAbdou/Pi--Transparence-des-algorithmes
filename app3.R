#install.packages("shinyjs")
library(shinyjs)
library(shiny)
library(BBmisc)
library(fBasics)
library(tidyverse)
library(caret) 
#path <- "C:/Users/user/Desktop/Pi2"
path <- "C:/Users/Marina/Documents/Semestre 9 (2018-2019)/Pi²/Pi2-Transparence-des-algorithmes-AlessandroBusato-patch-1"
setwd(path)
#install.packages("shinythemes")
library(shinythemes)

source('App3-code.R')


ui <- fluidPage(
  
  includeCSS("styles.css"),
  
  theme = shinytheme("spacelab"),
  
  fluidRow(headerPanel("Behaviour of the models")), 
  
  hr(),
  
  h3("Behaviour of the two models given a noise level : "),
  
  hr(),
 
 sidebarLayout(
   sidebarPanel(
     selectInput("noise","Noise level",c("No noise"=1,"0"=2,"2"=3,"3"=4,"NoiseG"=5),selected = 1),
     selectInput("ni", "Feature to visualize",
                 c("% good predictions"=1,
                   "ROC"=2,
                   "Time"=3,
                   "ROC/Time"=4,
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
                   "ROC"=2,
                   "Time"=3,
                   "ROC/Time"=4,
                   "% good predictions/Time"=5
                 ),selected=0)
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
                   "ROC"=2,
                   "Time"=3,
                   "ROC/Time"=4,
                   "% good predictions/Time"=5
                 ),selected=0)
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
 )
)


  
server <- function(input, output){
  
  n=reactive(as.integer(input$noise))
  ni=reactive(as.integer(input$ni))
  d=reactive(as.integer(input$pdata))
  di=reactive(as.integer(input$di))
  v=reactive(as.integer(input$pvar))
  vi=reactive(as.integer(input$vi))
  

  
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
  
  
  
  
  

  
  

  

}
  
shinyApp(ui, server)
  
  