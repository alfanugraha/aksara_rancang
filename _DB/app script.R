library(shiny)
library(gridExtra)
library(shinyBS)
library(rhandsontable)
library(shinyBS)
library(DT)
library(stringr)
library(shinyWidgets)
library(dplyr)
library(openxlsx)
library(plotly)


source("_DB/global.R")
source("_DB/lcd-scenario.R")
source("_DB/module.R")


ui <- fluidPage(
  titlePanel("SEKTOR"),
  tabsetPanel(
    tabPanel(
      h3("energi & transportasi"),
      tags$br(),
      tags$br(),
      buttonUI("forEnergy"),
    ),
    tabPanel(h3("limbah"),
             tags$br(),
             tags$br(),
             buttonUI("forWaste")
    ),
    tabPanel(h3("pertanian"),
             tags$br(),
             tags$br(),
             buttonUI("forAgri")
    ),
    tabPanel(h3("lahan"),
             tags$br(),
             tags$br(),
             buttonUI("forLand")
    )
  )
)



server <- function(input,output,session,data){
  callModule(buttonModule, "forEnergy", energyData)
  callModule(buttonModule, "forWaste", wasteData)
  callModule(buttonModule, "forAgri", agriData)
  callModule(buttonModule, "forLand", landData)
}

app <- shinyApp(ui,server)
runApp(app)
