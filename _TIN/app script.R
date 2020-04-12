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


# source("_TIN/global.R")
# source("_TIN/lcd-scenario.R")
source("_TIN/module_TIN.R")


ui <- fluidPage(
  titlePanel("SEKTOR"),
  tabsetPanel(
    tabPanel(
      h3("energi & transportasi"),
      tags$br(),
      tags$br(),
      defineScenarioModuleUI("forEnergy"), 
    ),
    tabPanel(h3("limbah"),
             tags$br(),
             tags$br(),
             defineScenarioModuleUI("forWaste"),
    ),
    tabPanel(h3("pertanian"),
             tags$br(),
             tags$br(),
             defineScenarioModuleUI("forAgri"),
    ),
    tabPanel(h3("lahan"),
             tags$br(),
             tags$br(),
             defineScenarioModuleUI("forLand"),
             # listOfScenarioModule("for Land"),
             # dataTableOutput("tes"),
             # uiOutput('LDMTableTampilUI'),
             # uiOutput('modalLDMUI')
    )
    )
  )




server <- function(input,output,session,data){
  


  callModule(defineScenarioModuleServer, "forEnergy")
  callModule(defineScenarioModuleServer, "forWaste")
  callModule(defineScenarioModuleServer, "forAgri")
  callModule(defineScenarioModuleServer, "forLand")
  # callModule(editSatelliteServer, "forEnergy")
  # callModule(editSatelliteServer, "forWaste")
  # callModule(editSatelliteServer, "forAgri")
  # callModule(editSatelliteLandServer, "forLand")

  
  
}


app <- shinyApp(ui,server)
runApp(app)
