library(shinyjs)
library(shiny)
library(BBmisc)
library(fBasics)
library(tidyverse)
library(caret) 
path <- "C:/Users/user/Desktop/Pi2/App3"
setwd(path)
library(shinythemes)

source('App3 code.R')


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
                   "ROC"=2,
                   "Time"=3,
                   "ROC/Time"=4,
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
 
 hr("behaviour of the two models in relation to the noise level"),
   
   sidebarLayout(
     sidebarPanel(
       selectInput("nd","% of obs.",c("20%"=1,"40%"=2,"60%"=3,"80%"=4,"100%"=5),selected=5),
       selectInput("nv","% of var.",c("20%"=1,"40%"=2,"60%"=3,"80%"=4,"100%"=5),selected=5),
       selectInput("ni2", "Feature to visualize",
                   c("% good predictions"=1,
                     "ROC"=2,
                     "Time"=3,
                     "ROC/Time"=4,
                     "% good predictions/Time"=5
                   ),selected=1)
     ),
     
     mainPanel(
       
       hr("black= model 1, red= model2"),
       plotOutput("pn")
       
     )
   ),
 
 hr("behaviour of the two models in relation to the percentage of 
    the observations used for the train"),
 
 sidebarLayout(
   sidebarPanel(
     selectInput("dn","noise level.",c("No noise"=1,"0"=2,"2"=3,"3"=4,"NoiseG"=5),selected = 1),
     selectInput("dv","% of var.",c("20%"=1,"40%"=2,"60%"=3,"80%"=4,"100%"=5),selected=5),
     selectInput("di2", "Feature to visualize",
                 c("% good predictions"=1,
                   "ROC"=2,
                   "Time"=3,
                   "ROC/Time"=4,
                   "% good predictions/Time"=5
                 ),selected=1)
   ),
   
   mainPanel(
     
     hr("black= model 1, red= model2"),
     plotOutput("pd")
     
   )
 ),
 
 hr("behaviour of the two models in relation to the percentage of 
    the variables used for the train"),
 
 sidebarLayout(
   sidebarPanel(
     selectInput("vn","noise level.",c("No noise"=1,"0"=2,"2"=3,"3"=4,"NoiseG"=5),selected = 1),
     selectInput("vd","% of var.",c("20%"=1,"40%"=2,"60%"=3,"80%"=4,"100%"=5),selected=5),
     selectInput("vi2", "Feature to visualize",
                 c("% good predictions"=1,
                   "ROC"=2,
                   "Time"=3,
                   "ROC/Time"=4,
                   "% good predictions/Time"=5
                 ),selected=1)
   ),
   
   mainPanel(
     
     hr("black= model 1, red= model2"),
     plotOutput("pv")
     
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
    mi=min(min(y),min(y2))
    ma=max(max(y),max(y2))
    plot(noise_level,y,type="l",ylim=c(mi,ma))
    lines(noise_level,y2,col="red")
    
    }
  )
  output$pd=renderPlot({
    Perc_Of_Obs=c(20,40,60,80,100)
    y=totData[[di2()]][[1]][dn(),,dv()]
    y2=totData[[di2()]][[2]][dn(),,dv()]
    mi=min(min(y),min(y2))
    ma=max(max(y),max(y2))
    plot(Perc_Of_Obs,y,type="l",ylim=c(mi,ma))
    lines(Perc_Of_Obs,y2,col="red")
    
  }
  )
  output$pv=renderPlot({
    Perc_Of_Var=c(20,40,60,80,100)
    y=totData[[ni2()]][[1]][vn(),vd(),]
    y2=totData[[ni2()]][[2]][vn(),vd(),]
    mi=min(min(y),min(y2))
    ma=max(max(y),max(y2))
    plot(Perc_Of_Var,y,type="l",ylim=c(mi,ma))
    lines(Perc_Of_Var,y2,col="red")
    
  }
  )
  

  
  


}
  
shinyApp(ui, server)
  
  
