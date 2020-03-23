library(shiny)
library(gridExtra)
library(shinyBS)
library(rhandsontable)
library(shinyBS)
library(DT)
library(stringr)
library(shinyWidgets)
library(plotly)

### nama 52 sector
sector<-readRDS("data/JaBar/sector")
sector<-sector[,1]
sector<-as.character(sector)


### variable untuk aksi energy
FD_his <- read.table("_DB/17_final_demand_proyeksi_sken1_d.csv",header = T,sep = ",")
row.names(FD_his)<-sector
FDIntervensiSken1 <- FD_his[c(23,27,30,34),]

### variable untuk aksi satelit energi
sat_his <- read.table("_DB/konsumsi 2016.csv",header = T,sep = ",")
row.names(sat_his)<-sector
satIntervensiSken1 <- sat_his[,c(1,5)]


### nama 26 bahan bakar 
bahanBakar <- colnames(sat_his)

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
             actionButton('modalDefineButton', 'Deskripsi Skenario'),
    actionButton('modalEconButton', 'Konstruksi Ekonomi'),
    actionButton('modalSatelitButton', 'Konstruksi Satelit Akun'),
    actionButton('modalRUNButton', 'Jalankan Aksi Skenario'),
             # tags$br(),
             # tags$br(),
             # uiOutput("defineButton"),
             # tags$br(),
             # tags$br(),
             # uiOutput("constructEconButton"),
             # tags$br(),
             # tags$br(),
             # uiOutput("constructSatelitButton"),
             # tags$br(),
             # tags$br(),
             # uiOutput("RUNbutton"),
             plotlyOutput("plotlyResults1"),
             plotlyOutput("plotlyResults2"),
             plotlyOutput("plotlyResults3")
    ),
    #tab panel for aksi lahan
    tabPanel("LAHAN",
             tags$br(),
             actionButton('modalDefineButton2', 'Deskripsi Skenario'),
    actionButton('modalEconButton2', 'Konstruksi Ekonomi'),
    actionButton('modalSatelitButton2', 'Konstruksi Satelit Akun'),
    actionButton('modalRUNButton2', 'Jalankan Aksi Skenario'),
             dataTableOutput("dummyLahan")),
    tabPanel("LIMBAH",
             tags$br(),
             actionButton('modalDefineButton3', 'Deskripsi Skenario'),
    actionButton('modalEconButton3', 'Konstruksi Ekonomi'),
    actionButton('modalSatelitButton3', 'Konstruksi Satelit Akun'),
    actionButton('modalRUNButton3', 'Jalankan Aksi Skenario'),
             dataTableOutput("dummyLimbah")),
    tabPanel("TRANSPORTASI",
             tags$br(),
             actionButton('modalDefineButton4', 'Deskripsi Skenario'),
    actionButton('modalEconButton4', 'Konstruksi Ekonomi'),
    actionButton('modalSatelitButton4', 'Konstruksi Satelit Akun'),
    actionButton('modalRUNButton4', 'Jalankan Aksi Skenario'),
             dataTableOutput("dummyTrans")),
    tabPanel("PERTANIAN",
             tags$br(),
             actionButton('modalDefineButton5', 'Deskripsi Skenario'),
    actionButton('modalEconButton5', 'Konstruksi Ekonomi'),
    actionButton('modalSatelitButton5', 'Konstruksi Satelit Akun'),
    actionButton('modalRUNButton5', 'Jalankan Aksi Skenario'),
             dataTableOutput("dummyPertanian"))
    
  )
)


server <- function(input,output){
  
  econ_new<-reactiveValues(
    tablo = NULL,
    coba= NULL
  )
  
  # output$defineButton<-renderUI({
  #   actionButton('modalDefineButton', 'Deskripsi Skenario')
  # })
  # 
  # output$constructEconButton<-renderUI({
  #   actionButton('modalEconButton', 'Konstruksi Ekonomi')
  # })
  # 
  # output$constructSatelitButton<-renderUI({
  #   actionButton('modalSatelitButton', 'Konstruksi Satelit Akun')
  # })
  # 
  # output$RUNbutton<-renderUI({
  #   actionButton('modalRUNButton', 'Jalankan Aksi Skenario')
  # })
  
  tes <- eventReactive(input$modalRUNButton, {
    list_data <- list(tblgdp=tblgdp, tblemisi=tblemisi, tblie=tblie)
    list_data
  })
  
  output$plotlyResults1 <- renderPlotly({
    tblgdp <- tes()
    ggplot(tblgdp$tblgdp, aes(x=Year, y=TotalGDP, group=Scenario)) +
             geom_line(aes(color=Scenario))+
             geom_point(aes(color=Scenario))+
             labs(x = "Tahun", y = "PDRB")+
             ggtitle("Grafik Proyeksi PDRB")
    # ggplot(data=tes(), aes(x=Year, y=resultTotalGDP, group=1)) + geom_line() + geom_point()
  })
  
  output$plotlyResults2 <- renderPlotly({
    tblemisi <- tes()
    ggplot(tblemisi$tblemisi, aes(x=Year, y=CummulativeEmission, group=Scenario)) +
            geom_line(aes(color=Scenario))+
             geom_point(aes(color=Scenario))+
             labs(x = "Tahun", y = "Emisi Kumulatif")+
             ggtitle("Grafik Emisi Kumulatif")
    # ggplot(data=tes(), aes(x=Year, y=CummulativeEmission, group=1)) + geom_line() + geom_point()
  })
  
  output$plotlyResults3 <- renderPlotly({
    tblie <- tes()
    ggplot(tblie$tblie, aes(x=Year, y=EmissionIntensity, group=Scenario)) +
             geom_line(aes(color=Scenario))+
             geom_point(aes(color=Scenario))+
             labs(x = "Tahun", y = "Intensitas Emisi")+
             ggtitle("Grafik Intensitas Emisi")
    # ggplot(data=tes(), aes(x=Year, y=EmissionIntensity, group=1)) + geom_line() + geom_point()
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
    tagList(pickerInput("pilihtahun",
                        label="pilih tahun",
                        choices=c(2016:2030),options = list(`actions-box` = TRUE),multiple = T),
            tags$b('Sunting secara manual'),
            tags$br(),
            tags$br(),
            rHandsontableOutput('editFD')
            
    )
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
        selectInput("intervensiSat",
                    label="pilih intervensi",
                    choices=c("konsumsi energi","faktor emisi")),
        pickerInput("sektor",
                    label="pilih sektor",
                    choices=sector,options = list(`actions-box` = TRUE),multiple = T)),
      tags$br(),
      actionButton("satHit","tampilkan"),
      width=5
    ),
    mainPanel(
      tags$div(id = 'satPlaceholder'),
      width=7)
    ),
    title="Sunting Intervensi Ekonomi",
    footer= tagList(
      actionButton("closeModalSat", "simpan tabel")
    ),
    size="l",
    easyClose = FALSE
    ))
  })
  
  ### tutup modal dialog ###
  observeEvent(input$closeModalSat,{
    removeModal()
  })
  
  
  observeEvent(input$satHit, {
    insertUI(selector='#satPlaceholder',
             where='afterEnd',
             ui= uiOutput('satMUIManual')
    )
    
  })
  
  output$satMUIManual<- renderUI({
    tagList(pickerInput("pilihtahun",
                        label="pilih tahun",
                        choices=c(2016:2030),options = list(`actions-box` = TRUE),multiple = T),
            pickerInput("pilihBahanBakar",
                        label="pilih bahan bakar",
                        choices=bahanBakar,options = list(`actions-box` = TRUE),multiple = T),
            tags$br(),
            tags$br(),
            tags$b('Sunting secara manual'),
            tags$br(),
            tags$br(),
            rHandsontableOutput('editSat')
            
    )
  })
  

  
  valSat<- reactive({
    table_show <- satIntervensiSken1
    table_show
  })
  
  output$editSat <- renderRHandsontable({
    rhandsontable(valSat(),
                  rowHeaderWidth = 160,
    )%>%hot_cols(format=3)
  })
  
  
  ################################################################################
  #                                                                              #
  #                        BUTTON DEFINE                                         #
  #                                                                              #
  ################################################################################
  
  observeEvent(input$modalDefineButton,{
    showModal(modalDialog(sidebarLayout(sidebarPanel(
      fluidRow(
        selectInput("intervensiDef",
                    label="nama skenario",
                    choices=c("PLTM on grid","Substitusi bahan bakar fosil ke biogas","Efisiensi Energi dengan lampu LED (untuk Sistem PJU)")),
        selectInput("tahunAwal",
                    label="tahun awal",
                    choices=c(2016:2030)),
        selectInput("tahunAkhir",
                    label="tahun akhir",
                    choices=c(2016:2030)),
        textAreaInput("deskripsi",
                      label = "Deskripsi Skenario",
                      width = "330px")
        ),
      tags$br(),
      actionButton("defHit","tampilkan"),
      width=5
    ),
    mainPanel(
      # tags$div(id = 'defPlaceholder'),
      # width=7
      )
    ),
    title="Deskripsi Skenario",
    footer= tagList(
      actionButton("closeModalDef", "simpan tabel")
    ),
    size="l",
    easyClose = FALSE
    ))
  })
  
  ### tutup modal dialog ###
  observeEvent(input$closeModalDef,{
    removeModal()
  })
  
  

}

app <- shinyApp(ui,server)
runApp(app)
