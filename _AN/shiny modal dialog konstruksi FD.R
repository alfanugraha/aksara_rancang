library(shiny)
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
user.scen<-readRDS("user/user.scen")




### data lahan,limbah,transportasi,pertanian
dataDummyLahan <- readRDS("_DB/LDMProp")
dataDummyLimbah <- readRDS("_DB/waste")
dataDummyTransportasi <-readRDS("_DB/waste") 
dataDummyPertanian <- readRDS("_DB/waste")


ui <- fluidPage(
  tabsetPanel(
    tabPanel("ENERGI",
             tags$br(),
             tags$br(),
             uiOutput("defineButton"),
             tags$br(),
             tags$br(),
             uiOutput("constructEconButton"),
             tags$br(),
             tags$br(),
             uiOutput("constructSatelitButton"),
             tags$br(),
             tags$br(),
             uiOutput("RUNbutton")
    ),
    #tab panel for aksi lahan
    tabPanel("LAHAN",
             dataTableOutput("dummyLahan")),
    tabPanel("LIMBAH",
             dataTableOutput("dummyLimbah")),
    tabPanel("TRANSPORTASI",
             dataTableOutput("dummyTrans")),
    tabPanel("PERTANIAN",
             dataTableOutput("dummyPertanian"))
    
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
  
  # observeEvent(input$saveFDTable,{
  #   waktuFD<-Sys.time()
  #   simpanFD<-gsub(" ","_",waktuFD,fixed = TRUE)
  #   simpanFD<-gsub(":","-",simpanFD,fixed = TRUE)
  #   tanggalFD<-Sys.Date()
  #   namafileFD<-paste0(username,"_",selectedProv,"_",simpanFD)
  #   saveRDS(econ_new$coba, file = paste0('_DB/FDData/',selectedProv,'/',namafileFD))
  #   FDRV$FDListFile<-list.files(paste0("_DB/FDData/",selectedProv))
  #   FDRV$FDTotFile<-length(list.files("_DB/FDData/", selectedProv))
  #   removeUI(selector='#pesanHitungFDNo')
  #   removeUI(selector='#pesanHitungFDYes')
  #   removeUI(selector='#FDMUIManual')
  # })
  
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

  
  FDRV<-reactiveValues(
    FDListFile = unique(list.files(paste0("_DB/FDData/",selectedProv))),
    FDTotFile= unique(length(list.files("_DB/FDData/", selectedProv)))
  )
  
}

app <- shinyApp(ui,server)
runApp(app)
