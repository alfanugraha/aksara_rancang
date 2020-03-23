library(shiny)
library(shinydashboard)
library(gridExtra)
library(shinyBS)
library(rhandsontable)
library(shinyBS)
library(DT)
library(stringr)
library(shinyWidgets)

### nama 52 sector
sector<-readRDS("data/JaBar/sector")
sector<-sector[,1]
sector<-as.character(sector)


### variable untuk aksi energy
FD_his <- read.table("_DB/17_final_demand_proyeksi_sken1_d.csv",header = T,sep = ",")
row.names(FD_his)<-sector
FDIntervensiSken1 <- FD_his[c(23,27,30,34),]

### variable untuk aksi satelit energi


##variable for LDM Prop###
LDMProp_his<-readRDS("data/JaBar/LDMProp")
LDMProp_his <- LDMProp_his[,-1]
colnamesLDM<-colnames(LDMProp_his)
row.names(LDMProp_his)<-sector
LDMProp_his<-as.matrix(LDMProp_his)
selectedProv<-"Prov"
username<-"dw"

###variable for aksi lahan###
LU_tahun<-readRDS("data/JaBar/LU_tahun")
user.intName <-list.files("user", pattern="\\user.int")
# user.scen<-readRDS("user/user.scen")




### data lahan,limbah,transportasi,pertanian
dataDummyLahan <- readRDS("_DB/LDMProp")
dataDummyLimbah <- readRDS("_DB/waste")
dataDummyTransportasi <-readRDS("_DB/waste") 
dataDummyPertanian <- readRDS("_DB/waste")


ui <- fluidPage(
  tabsetPanel(
    tabPanel("ENERGI",
      fluidRow(
        box(title = " ", width = 3, solidHeader = TRUE, status = "primary",
             uiOutput("defineButton")
        ),
        box(title = " ", width = 3, solidHeader = TRUE, status = "primary",
             uiOutput("constructEconButton")
        ),
        box(title = " ", width = 3, solidHeader = TRUE, status = "primary",
             uiOutput("constructSatelitButton")
          
        ),
        box(title = " ", width = 3, solidHeader = TRUE, status = "primary",
             uiOutput("RUNbutton")
        )
      ),
      dataTableOutput("dummyEnergy")
    ),
    #tab panel for aksi lahan
    tabPanel("LAHAN",
       # fluidRow(
       #    box(title = " ", width = 3, solidHeader = TRUE, status = "primary",
       #         uiOutput("defineButton2")
       #    ),
       #    box(title = " ", width = 3, solidHeader = TRUE, status = "primary",
       #         uiOutput("constructEconButton2")
       #    ),
       #    box(title = " ", width = 3, solidHeader = TRUE, status = "primary",
       #         uiOutput("constructSatelitButton2")
       # 
       #    ),
       #    box(title = " ", width = 3, solidHeader = TRUE, status = "primary",
       #         uiOutput("RUNbutton2")
       #    )
       #  ),
        dataTableOutput("dummyLahan")
       ),
    tabPanel("LIMBAH",
      #        fluidRow(
      #   box(title = " ", width = 3, solidHeader = TRUE, status = "primary",
      #        uiOutput("defineButton")
      #   ),
      #   box(title = " ", width = 3, solidHeader = TRUE, status = "primary",
      #        uiOutput("constructEconButton")
      #   ),
      #   box(title = " ", width = 3, solidHeader = TRUE, status = "primary",
      #        uiOutput("constructSatelitButton")
      #     
      #   ),
      #   box(title = " ", width = 3, solidHeader = TRUE, status = "primary",
      #        uiOutput("RUNbutton")
      #   )
      # ),
             dataTableOutput("dummyLimbah")
      ),
    tabPanel("TRANSPORTASI",
      #        fluidRow(
      #   box(title = " ", width = 3, solidHeader = TRUE, status = "primary",
      #        uiOutput("defineButton")
      #   ),
      #   box(title = " ", width = 3, solidHeader = TRUE, status = "primary",
      #        uiOutput("constructEconButton")
      #   ),
      #   box(title = " ", width = 3, solidHeader = TRUE, status = "primary",
      #        uiOutput("constructSatelitButton")
      #     
      #   ),
      #   box(title = " ", width = 3, solidHeader = TRUE, status = "primary",
      #        uiOutput("RUNbutton")
      #   )
      # ),
             dataTableOutput("dummyTrans")
      ),
    tabPanel("PERTANIAN",
      #        fluidRow(
      #   box(title = " ", width = 3, solidHeader = TRUE, status = "primary",
      #        uiOutput("defineButton")
      #   ),
      #   box(title = " ", width = 3, solidHeader = TRUE, status = "primary",
      #        uiOutput("constructEconButton")
      #   ),
      #   box(title = " ", width = 3, solidHeader = TRUE, status = "primary",
      #        uiOutput("constructSatelitButton")
      #     
      #   ),
      #   box(title = " ", width = 3, solidHeader = TRUE, status = "primary",
      #        uiOutput("RUNbutton")
      #   )
      # ),
             dataTableOutput("dummyPertanian")
      )
    
  )
)


server <- function(input,output){
  
  econ_new<-reactiveValues(
    tablo = NULL,
    coba= NULL
  )
  
  output$defineButton<-renderUI({
    actionButton('modalDefineButton', 'Deskripsi Skenario')
  })
  
  output$constructEconButton<-renderUI({
    actionButton('modalEconButton', 'Konstruksi Ekonomi')
  })
  
  output$constructSatelitButton<-renderUI({
    actionButton('modalSatelitButton', 'Konstruksi Satelit Akun')
  })
  
  output$RUNbutton<-renderUI({
    actionButton('modalRUNButton', 'Jalankan Aksi Skenario')
  })
  
  ################################################################################
  #                                                                              #
  #                          BUTTON KONSTRUKSI EKONOMI                           #
  #                                                                              #
  ################################################################################
  observeEvent(input$modalEconButton,{
    showModal(modalDialog(sidebarLayout(sidebarPanel(
      fluidRow(
        selectInput("intervensiEcon",
                    label="pilih intervensi",
                    choices=c("Final Demand","AV","Input-Output")),
        pickerInput("sektor",
                    label="pilih sektor",
                    choices=sector,options = list(`actions-box` = TRUE),multiple = T)),
      tags$br(),
      actionButton("econHit","tampilkan"),
      width=5
    ),
    mainPanel(
      tags$div(id = 'FDPlaceholder'),
      width=7)
    ),
    title="Sunting Intervensi Ekonomi",
    footer= tagList(
      actionButton("closeModalFD", "simpan tabel")
    ),
    size="l",
    easyClose = FALSE
    ))
  })
  
  ### tutup modal dialog ###
  observeEvent(input$closeModalFD,{
    removeModal()
  })
  

  observeEvent(input$econHit, {
    insertUI(selector='#FDPlaceholder',
             where='afterEnd',
             ui= uiOutput('FDMUIManual')
    )

  })
  
  output$FDMUIManual<- renderUI({
    tagList(tags$b('Sunting secara manual'),
            tags$br(),
            tags$br(),
            rHandsontableOutput('editFD'),
            
    )
  })
  
  ### tutup modal dialog ###
  observeEvent(input$closeModalEcon,{
    removeModal()
  })

  valFD<- reactive({
    table_show <- FDIntervensiSken1
    table_show
  })
  
  output$editFD <- renderRHandsontable({
    rhandsontable(valFD(),
                  rowHeaderWidth = 160,
    )%>%hot_cols(format=3)
  })
  

  ################################################################################
  #                                                                              #
  #                        BUTTON KONSTRUKSI SATELIT AKUN                        #
  #                                                                              #
  ################################################################################
  
  observeEvent(input$modalSatelitButton,{
    showModal(modalDialog(sidebarLayout(sidebarPanel(
      fluidRow(
        selectInput("intervensiEcon",
                    label="pilih intervensi",
                    choices=c("Final Demand","AV","Input-Output")),
        pickerInput("sektor",
                    label="pilih sektor",
                    choices=sector,options = list(`actions-box` = TRUE),multiple = T)),
      tags$br(),
      actionButton("econHit","tampilkan"),
      width=5
    ),
    mainPanel(
      tags$div(id = 'FDPlaceholder'),
      width=7)
    ),
    title="Sunting Intervensi Ekonomi",
    footer= tagList(
      actionButton("closeModalFD", "simpan tabel")
    ),
    size="l",
    easyClose = FALSE
    ))
  })
  
  ### tutup modal dialog ###
  observeEvent(input$closeModalFD,{
    removeModal()
  })
  
  
  observeEvent(input$econHit, {
    insertUI(selector='#FDPlaceholder',
             where='afterEnd',
             ui= uiOutput('FDMUIManual')
    )
    
  })
  
  output$FDMUIManual<- renderUI({
    tagList(tags$b('Sunting secara manual'),
            tags$br(),
            tags$br(),
            rHandsontableOutput('editFD')
            
    )
  })
  
  ### tutup modal dialog ###
  observeEvent(input$closeModalEcon,{
    removeModal()
  })
  
  valFD<- reactive({
    table_show <- FDIntervensiSken1
    table_show
  })
  
  output$editFD <- renderRHandsontable({
    rhandsontable(valFD(),
                  rowHeaderWidth = 160,
    )%>%hot_cols(format=3)
  })

}

app <- shinyApp(ui,server)
runApp(app)