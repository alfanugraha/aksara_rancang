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

source("_DB/debug_TIN.R")
#source("_DB/lcd-scenario.R")
source("_DB/global.R")
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
  callModule(buttonModule, "forEnergy", energyData, type="energy")
  callModule(buttonModule, "forWaste", wasteData, type="waste")
  callModule(buttonModule, "forAgri", agriData, type="agriculture")
  callModule(buttonModule, "forLand", landData, type="land")
}

app <- shinyApp(ui,server)
runApp(app)
