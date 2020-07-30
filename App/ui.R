#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(tidyverse)
library(shiny)

fluidPage(
  theme="bootstrap.css",
  tabsetPanel(
    
    tabPanel(title="LEVITT METRIC",
      br(),
      h1("LEVITT METRIC MODEL ON DISTRICT WISE INDIAN DATA"),
      br(),
      br(),
      br(),
      sidebarLayout(
      sidebarPanel(
        selectInput(inputId="state1","Select State:",states,selected="Maharashtra"),
        selectInput(inputId="district1","Select District:",districts1,selected="Thane"),
        sliderInput(inputId="lowlim_calc1","Select Base Point:",min=1,max=40,value=10),
        #actionButton(inputId="refresh","Refresh Data")
        #submitButton(text="Submit")
        ),
      mainPanel(
        plotOutput(outputId="LevMet")
      )
    )
  ),
  
  tabPanel(title="CUMULATIVE GRAPH",
           br(),
           h1("CUMULATIVE CASE AND DEATH GRAPH OF DISTRICT WISE INDIAN DATA"),
           br(),
           br(),
           br(),
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId="state2","Select State:",states,selected="Maharashtra"),
               selectInput(inputId="district2","Select District:",districts2,selected="Thane"),
               sliderInput(inputId="order2","Order of Curve Approximation:",min=1,max=8,value=3),
               #actionButton(inputId="refresh","Refresh Data")
               #submitButton(text="Submit")
             ),
             mainPanel(
               plotOutput(outputId="Cumul")
             )
           )
  )
  
)
)