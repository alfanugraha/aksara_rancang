library(shiny)
library(gridExtra)
library(shinyBS)
library(rhandsontable)
library(shinyBS)
library(DT)
library(stringr)
library(shinyWidgets)
library(dplyr)


### DATA MASTER
# namaSken_his1 <- "PLTM on grid"
# fdsken_his1 <- readRDS("C:/dw/ICRAF/aksara_rancang/_DB/rds energy/fdAllYearSken1.rds")
# satSken_his1 <- readRDS("C:/dw/ICRAF/aksara_rancang/_DB/rds energy/proyTabelKonsEnergiSken1.rds")
# tahunSkenario_his1 <- names(satSken_his1)
# skenEnergy1 <- list(namaSken=namaSken_his1,
#                     fdsken=fdsken_his1,
#                     satSken=satSken_his1,
#                     tahunSkenario=tahunSkenario_his1)
# 
# namaSken_his2 <- "Substitusi bahan bakar fosil ke biogas"
# fdsken_his2 <- readRDS("C:/dw/ICRAF/aksara_rancang/_DB/rds energy/fdAllYearSken1.rds")
# satSken_his2 <- readRDS("C:/dw/ICRAF/aksara_rancang/_DB/rds energy/proyTabelKonsEnergiSken1.rds")
# tahunSkenario_his2 <- names(satSken_his2)
# skenEnergy2 <- list(namaSken=namaSken_his2,
#                     fdsken=fdsken_his2,
#                     satSken=satSken_his2,
#                     tahunSkenario=tahunSkenario_his2)


#ID
selectedProv<-"Prov"
selectedSektor <- "energi"
username<-"dw"

### nama 52 sector
sector<-readRDS("data/JaBar/sector")
sector<-sector[,1]
sector<-as.character(sector)


## alternatif data master
fdGab <- read.table("_AN/final_demand_gabungan.csv",header = T,sep = "," , stringsAsFactors = F) #kolom skenario utk id skenario
energyData <- filter(fdGab, sektor == 'energi')
wasteData <- filter(fdGab, sektor=="limbah")

# ### variable untuk aksi energy
# FD_his <- read.table("_DB/17_final_demand_proyeksi_sken1_d.csv",header = T,sep = ",")
# row.names(FD_his)<-sector
# FDIntervensiSken1 <- FD_his[c(23,27,30,34),]

### variable untuk aksi satelit energi
sat_his <- read.table("_AN/konsumsi 2016.csv",header = T,sep = ",")
row.names(sat_his)<-sector
satIntervensiSken1 <- sat_his[,c(1,5)]

### nama 26 bahan bakar 
bahanBakar <- colnames(sat_his)

buttonUI <- function(id) {
  ns <- NS(id)

  tagList(
    actionButton(ns("modalDefineButton"),'Deskripsi Skenario'),
    actionButton(ns("modalEconButton"),'Konstruksi Ekonomi dan Satelit Akun'),
    actionButton(ns("modalRUNButton"),'Run')
  )
}



buttonModule <- function(input, output, session, data) {

  dataDef <- reactive({
    namaSken <- unique(data$skenario)
    namaSken
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
                    choices=c(dataDef()
                      # "PLTM on grid","Substitusi bahan bakar fosil ke biogas",
                      # "Efisiensi Energi dengan lampu LED (untuk Sistem PJU)"
                      )),
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
      tags$div(id = 'defPlaceholder'),
      width=7
    )),
    title="Deskripsi Skenario",
    footer= tagList(
      actionButton("closeModalDef", "tutup")
    ),
    size="l",
    easyClose = FALSE
    ))
  })

  # observe({
  #   data
  #   print(data)
  # })

  

  
  ################################################################################
  #                                                                              #
  #                          BUTTON KONSTRUKSI EKONOMI                           #
  #                                                                              #
  ################################################################################
  observeEvent(input$modalEconButton,{
    showModal(
      modalDialog( 
        footer=tagList(
        actionButton("closeModalFD", "Tutup")
        ),
      tabsetPanel(
        tabPanel(
        h2("Ekonomi"),
        sidebarLayout(
        sidebarPanel(
          fluidRow(
            selectInput("intervensiEcon",
                        label="pilih intervensi",
                        choices=c("Final Demand","AV","Input-Output")),
            pickerInput("sektorEcon",
                        label="pilih sektor", selected = sector[1],
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
        size="l",
        easyClose = FALSE
        ),
       
       
  ################################################################################
  #                                                                              #
  #                        BUTTON KONSTRUKSI SATELIT AKUN                        #
  #                                                                              #
  ################################################################################
  tabPanel(
    h2("Satelit akun"),
    sidebarLayout(sidebarPanel(
      fluidRow(
        selectInput("intervensiSat",
                    label="pilih intervensi",
                    choices=c("konsumsi energi","faktor emisi")),
        pickerInput("sektorSat",
                    label="pilih sektor",selected = sector[1],
                    choices=sector,options = list(`actions-box` = TRUE),multiple = T)),
      tags$br(),
      actionButton("satHit","tampilkan"),
      width=5
    ),
    mainPanel(
      tags$div(id = 'satPlaceholder'),
      width=7)
    ),
    title="Sunting Intervensi Satelit Akun",
    size="l",
    easyClose = FALSE
    ))
  )
)
})
}



ui <- fluidPage(
  titlePanel("SEKTOR"),
  tabsetPanel(
              tabPanel(
                      h3("energi"),
                       tags$br(),
                       tags$br(),
                       buttonUI("forEnergy")
                       ),
              tabPanel(h3("limbah"),
                       tags$br(),
                       tags$br(),
                       buttonUI("forWaste")
              )
  )
)



server <- function(input,output,session,data){
  callModule(buttonModule, "forEnergy", energyData)
  callModule(buttonModule, "forWaste", wasteData)

  # observeEvent(input$defHit, {
  #   callModule(buttonModule2,"forEnergy")
  #   print("jalan")
  # })
  
  observeEvent(input$defHit, {
    insertUI(selector='#defPlaceholder',
             where='afterEnd',
             ui= uiOutput('defUIManual')
    )
  })

  output$defUIManual<- renderUI({
    tagList(rHandsontableOutput('editDefine'),
            tags$br(),
            actionButton('saveModalDef', 'simpan tabel')
    )
  })


  valDef<- reactive({
    namaSken <- input$intervensiDef
    tahunAwal <- input$tahunAwal
    tahunAkhir <- input$tahunAkhir
    deskrip <- input$deskripsi
    gabung <- rbind(namaSken,tahunAwal,tahunAkhir, deskrip)

    tableDef <- data.frame(gabung)
    colnames(tableDef) <- "Keterangan"
    rownames(tableDef) <- c("nama skenario",
                            "tahun awal",
                            "tahun akhir",
                            "deskripsi")
    tableDef
  })


  output$editDefine <- renderRHandsontable({
    rhandsontable(valDef(),
                  rowHeaderWidth = 160,
    )%>%hot_cols(format=3)
  })
  
  ##### simpan tabel define ke dalam folder ####
  observeEvent(input$saveModalDef,{
    waktuDefine<-Sys.time()
    simpanDefine<-gsub(" ","_",waktuDefine,fixed = TRUE)
    simpanDefine<-gsub(":","-",simpanDefine,fixed = TRUE)
    namafileDefine<-paste0(username,"_",selectedProv,"_",simpanDefine)
    saveRDS(valDef(), file = paste0('_AN/skenarioData/',selectedSektor,'/',selectedProv,'/',namafileDefine))
  })
 
  
  ################################################################################
  #                                                                              #
  #                          BUTTON KONSTRUKSI EKONOMI                           #
  #                                                                              #
  ################################################################################
  observeEvent(input$econHit, {
    insertUI(selector='#FDPlaceholder',
             where='afterEnd',
             ui= uiOutput('FDUIManual')
    )
  })  
  
  output$FDUIManual<- renderUI({
    tagList(pickerInput("pilihtahunFD",
                        label="pilih tahun", selected = input$tahunAwal,
                        choices=c(input$tahunAwal:input$tahunAkhir),options = list(`actions-box` = TRUE),multiple = T),
            tags$b('Sunting secara manual'),
            tags$br(),
            tags$br(),
            rHandsontableOutput('editFD'),
            tags$br(),
            actionButton('saveModalFD', 'simpan tabel')
    )
  })
  
  FDdata <- reactive({
    # browser()
    a <- energyData
    a <- filter(a, skenario == input$intervensiDef)
    sector <- as.character(sector)
    a <- cbind(sector,a)
    a <- a[,c(-2,-3)]
    a
  })
  
  
  # FDdataTahun <- reactive({
  #   a <- energyData
  #   a <- data.frame(a[,grep(pattern =input$pilihtahunFD,colnames(a))])
  #   a
  # })
  
  valFD<- reactive({
    #browser()
    tableDef <- valDef()
    hmm <- FDdata()
    tableFD <- filter(hmm, hmm$sector %in% c(input$sektorEcon))
    tableFD
  })
  
  valFD2 <- reactive({
    tableFD2 <- valFD()
    # tableFD2 <- data.frame(tableFD2[,c(1,grep(pattern = c(input$pilihtahunFD),colnames(tableFD2), fixed = T))])
    tableFD2 <- tableFD2[, c('sector', paste0("y", input$pilihtahunFD))]
    #masih baru bisa 1 tahun, belum bs beberpaa tahun
    tableFD2
  })
  
  output$editFD <- renderRHandsontable({
    rhandsontable(valFD2(),
                  rowHeaderWidth = 160,
    )%>%hot_cols(format=3)
  })

  ################################################################################
  #                                                                              #
  #                        BUTTON KONSTRUKSI SATELIT AKUN                        #
  #                                                                              #
  ################################################################################
  
  observeEvent(input$satHit, {
    insertUI(selector='#satPlaceholder',
             where='afterEnd',
             ui= uiOutput('satMUIManual')
    )
    
  })
  
  output$satMUIManual<- renderUI({
    tagList(pickerInput("pilihtahun",
                        label="pilih tahun", selected = input$tahunAwal,
                        choices=c(input$tahunAwal:input$tahunAkhir),options = list(`actions-box` = TRUE),multiple = T),
            pickerInput("pilihBahanBakar",
                        label="pilih bahan bakar",
                        choices=bahanBakar,options = list(`actions-box` = TRUE),multiple = T),
            tags$br(),
            tags$br(),
            tags$b('Sunting secara manual'),
            tags$br(),
            tags$br(),
            rHandsontableOutput('editSat'),
            
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
  
  ### tutup modal dialog define ###
  observeEvent(input$closeModalDef,{
    removeModal()
  })

  ### tutup modal dialog satelit akun###
  observeEvent(input$closeModalSat,{
    removeModal()
  })
  
  ### tutup modal dialog Econ ###
  observeEvent(input$closeModalFD,{
    removeModal()
  })

    
  
}

app <- shinyApp(ui,server)
runApp(app)
