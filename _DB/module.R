
buttonUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    actionButton(ns("modalDefineButton"),'Deskripsi Skenario'),
    tags$br(),
    tags$br(),
    dataTableOutput(ns("ListTable")),
    uiOutput(ns("daftarDefineShow")),
    plotlyOutput(ns('hasilRun')),
    plotlyOutput(ns('plot2')),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }")
  )
}



buttonModule <- function(input, output, session, data) {
  
  
  # load the namespace
  ns <- session$ns
  
  ################################################################################
  #                                                                              #
  #                        BUTTON DEFINE                                         #
  #                                                                              #
  ################################################################################
  observeEvent(input$modalDefineButton,{
    showModal(modalDialog(sidebarLayout(sidebarPanel(
      fluidRow(
        textInput(ns("intervensiDef"),
                  label="nama skenario"
        ),
        selectInput(ns("tahunAwal"),
                    label="tahun awal",
                    choices=c(2016:2030)),
        selectInput(ns("tahunAkhir"),
                    label="tahun akhir",
                    choices=c(2016:2030)),
        textAreaInput(ns("deskripsi"),
                      label = "Deskripsi Skenario",
                      width = "330px")
      ),
      tags$br(),
      actionButton(ns("defHit"),"tampilkan"),
      width=5
    ),
    mainPanel(
      tags$div(id = 'defPlaceholder'),
      width=7
    )),
    title="Deskripsi Skenario",
    footer= tagList(
      actionButton(ns("saveModalDef"), "simpan skenario"),
      actionButton(ns("cancelModalDef"), "tutup")
    ),
    size="l",
    easyClose = FALSE
    ))
  })
  
  
  observeEvent(input$defHit, {
    insertUI(selector='#defPlaceholder',
             where='afterEnd',
             ui= uiOutput(ns('defUIManual'))
    )
  })
  
  output$defUIManual<- renderUI({
    tagList(rHandsontableOutput(ns('editDefine')),
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
  
  listValDef <- reactive({
    newTableDef <- as.data.frame(hot_to_r(input$editDefine))

    namaSken <- as.character(newTableDef[1,]) #2
    tahunAwal <- as.numeric(trimws(newTableDef[2,])) #3
    tahunAkhir <- as.numeric(trimws(newTableDef[3,])) #4
    deskrip <- as.character(newTableDef[4,]) #5
    fdSelisih <- NULL #6
    satSelisih <- NULL #7
    proyPdrb <- NULL #8
    proyEmisi <- NULL #9
    
    
    combineDef <- list(namaSken=namaSken,tahunAwal=tahunAwal,tahunAkhir=tahunAkhir, deskrip=deskrip,
                       fdSelisih=fdSelisih, satSelisih=satSelisih, proyPdrb = proyPdrb,proyEmisi=proyEmisi)

    combineDef
  })
  
  ##### simpan tabel define ke dalam folder ####
  observeEvent(input$saveModalDef,{
    waktuDefine<-Sys.time()
    simpanDefine<-gsub(" ","_",waktuDefine,fixed = TRUE)
    simpanDefine<-gsub(":","-",simpanDefine,fixed = TRUE)
    namaSken <- gsub(" ","",input$intervensiDef, fixed = TRUE)
    namafileDefine<-paste0(username,"_",selectedProv,"_",simpanDefine,"_",namaSken)
    saveRDS(listValDef(), file = paste0(data$alamatFile,"/",namafileDefine))
    shinyjs::js$refresh()
  })
  
  
  observeEvent(input$cancelModalDef,{
    removeModal()
  })

  
  ListButton_fun <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id,i), ...))
    }
    inputs
  }
  
  loadRDSAll <- reactive({
    nameFiles <- list.files(path = data$alamatFile,
                            pattern = paste0("^", username))
    dirFile <- paste0(data$alamatFile, "/",
                      nameFiles)
    
    if(identical(nameFiles,character(0))){
      r <- data.frame(
        Nama.Skenario =  "file tidak tersedia",
        Tahun.Awal = "file tidak tersedia", 
        Tahun.Akhir = "file tidak tersedia",
        Deskripsi.Skenario = "file tidak tersedia",
        fdSelisih = "file tidak tersedia",
        satSelisih = "file tidak tersedia",
        Nama.File = "file tidak tersedia", 
        Sunting.Skenario = "file tidak tersedia",
        Jalankan.analisis = "file tidak tersedia"
      )
      o <- list(r)
    } else {
      funcFile <- function(x){
        a <- readRDS(x)
        b <- c(x,a)
        b
      }
      
      r <- lapply(dirFile, funcFile)
      r
    }
    
  })
  
  
  ### buat tabel daftar nama file reaktif ###
  ListTableReact <- reactive({
    data.frame(
          Nama.Skenario =  unlist(lapply(loadRDSAll(), function(x)x[[2]])),
          Tahun.Awal = unlist(lapply(loadRDSAll(), function(x)x[[3]])), 
          Tahun.Akhir = unlist(lapply(loadRDSAll(), function(x)x[[4]])),
          Deskripsi.Skenario = unlist(lapply(loadRDSAll(), function(x)x[[5]])),
          Nama.File = unlist(lapply(loadRDSAll(), function(x)x[[1]])), #nama file dr listValDef ada di index terakhir = 6
          Sunting.Skenario = ListButton_fun(actionButton,
                                              length(loadRDSAll()),
                                     'button_',
                                     label = "Sunting Konstruksi Ekonomi dan Satelit Akun",
                                     onclick = sprintf('Shiny.onInputChange("%s",this.id)', ns("select_button"))
                              #paste0('Shiny.onInputChange(\"' , ns("select_button"), '\", this.id)')
                                      ),
          Jalankan.analisis = ListButton_fun(actionButton,
                                         length(loadRDSAll()),
                              'buttonRun_',
                              label = "Jalankan Analisis",
                              onclick = sprintf('Shiny.onInputChange("%s",this.id)', ns("run_button")))
      )  
  })

  ###tampilkan tabel list ###
  output$ListTable <- renderDataTable({
    ListTableReact()
  }, escape = FALSE)
  
  
  
  observeEvent(input$select_button,{
    #browser()
    showModal(
      modalDialog( 
        footer=tagList(
          actionButton(ns("closeModalFD"), "Tutup")
        ),
        tabsetPanel(
          tabPanel(
            h2("Ekonomi"),
            sidebarLayout(
              sidebarPanel(
                fluidRow(
                  selectInput(ns("intervensiEcon"),
                              label="pilih intervensi",
                              choices=c("Final Demand","AV","Input-Output")),
                  pickerInput(ns("sektorEcon"),
                              label="pilih sektor", selected = Sector[1],
                              choices=Sector,options = list(`actions-box` = TRUE),multiple = T)),
                tags$br(),
                actionButton(ns("econHit"),"tentukan tahun intervensi"),
                width=5
              ),
              mainPanel(
                tags$div(id = ns('FDPlaceholder')),
                width=7)
            ),
            title="Sunting Intervensi Ekonomi"
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
                selectInput(ns("intervensiSat"),
                            label="pilih intervensi",
                            choices=c("konsumsi energi","faktor emisi")),
                pickerInput(ns("sektorSat"),
                            label="pilih sektor",selected = Sector[1],
                            choices=Sector,options = list(`actions-box` = TRUE),multiple = T)),
              tags$br(),
              actionButton(ns("satHit"),"tentukan tahun intervensi"),
              width=5
            ),
            mainPanel(
              tags$div(id = ns('satPlaceholder')),
              width=7)
            ),
            title="Sunting Intervensi Satelit Akun"
          ))
        ,
        size="l",
        easyClose = FALSE)
    )

  })
  
  
  
  
  
  ################################################################################
  #                                                                              #
  #                     ALUR BUTTON KONSTRUKSI EKONOMI                           #
  #                                                                              #
  ################################################################################
  loadFileRDS <- reactive({
    selectedRow <- as.numeric(strsplit(input$select_button,"_")[[1]][2])
    fileName<- as.character(ListTableReact()[selectedRow,5]) #nama file dari ListTableReact ada di col=5
    selectedFile<-readRDS(fileName)
    
    selectedFile
  })
  
  
  observeEvent(input$econHit, {
    insertUI(selector=paste0("#", ns("FDPlaceholder")),
             where='afterEnd',
             ui= uiOutput(ns('FDUIManual'))
    )
  })  
  
  output$FDUIManual<- renderUI({
    tagList(pickerInput(ns("pilihtahunFD"),
                        label="pilih tahun", selected = loadFileRDS()$tahunAwal,
                        choices=c(loadFileRDS()$tahunAwal : loadFileRDS()$tahunAkhir),options = list(`actions-box` = TRUE),multiple = T),
            tags$br(),
            actionButton(ns('showYearEco'), 'tampilkan tabel'),
            tags$br(),
            tags$br(),
            tags$div(id = 'SuntingPlaceHolder')
    )
  })
  
  observeEvent(input$showYearEco, {
    insertUI(selector='#SuntingPlaceHolder',
             where='afterEnd',
             ui= uiOutput(ns('SuntingUITable'))
    )
  }) 
  
  output$SuntingUITable<- renderUI({
    tagList(
      tags$b('Sunting secara manual'),
      tags$br(),
      rHandsontableOutput(ns('editFD')),
      tags$br(),
      actionButton(ns('saveModalFD'),' simpan tabel '),
      tags$div(id = 'objDownloadFD')
    )
  })
  
  finalDemand <- reactiveValues(
    table1 = NULL
  )
  
  fdBauReact <- reactiveValues(
    isi = fdZero
  )
  
  FDdata <- reactive({
    if (is.null(loadFileRDS()$fdSelisih)) {
      finalDemand$table1 = fdZero
    }else{
      finalDemand$table1 =  loadFileRDS()$fdSelisih
    }
  })
  
  valFD<- eventReactive(c(input$showYearEco),{
    #browser()
    finalDemand$table1 <- FDdata()
    finalDemand$table1 <- filter(finalDemand$table1, finalDemand$table1$Sektor %in% c(input$sektorEcon))
    rownames(finalDemand$table1) <- c(input$sektorEcon)
    finalDemand$table1 <- finalDemand$table1[,c("Sektor",paste0("y",input$pilihtahunFD))]
    finalDemand$table1
  })
  
  output$editFD <- renderRHandsontable({
    rhandsontable(valFD(),
                  rowHeaderWidth = 160,
    )%>%hot_cols(format=3)
    
  })
  
  #### masukkan nilai sel baru ke dalam kolom fdNew 
  FDSave<-eventReactive(input$saveModalFD,{
    finalDemand$table1 <- as.data.frame(hot_to_r(input$editFD))
    inputSektor<-input$sektorEcon #"tanaman pangan"
    inputTahun<-paste0("y",input$pilihtahunFD)
    indexSektor <- as.numeric(which(sapply(Sector,function(x) any(x==c(inputSektor)))))
    
    fdBauReact$isi <- fdBau
    fdBauReact$isi[c(indexSektor), c(inputTahun)] <- fdBauReact$isi[c(indexSektor), c(inputTahun)] + finalDemand$table1[,-1]
    
    fdSelisih <- fdZero
    fdSelisih[c(indexSektor), c(inputTahun)] <- fdSelisih[c(indexSektor), c(inputTahun)] + finalDemand$table1[,-1]
    
    
    fdNew_list<-list(fdBauReal = fdBau,
                     fdNew = fdBauReact$isi,
                     fdEditNew = finalDemand$table1,
                     fdSelisih = fdSelisih
    )
    fdNew_list

  })
  
  ### efek FD yang diubah ke tabel proyeksi konsumsi energi
  ## bagian Output
  outputTable <- reactive({
    fdTable <- FDSave()$fdNew
    out <- ioLeontiefInverse %*% as.matrix(fdTable[,-1]) #leontief jadi ioLeontiefInverse 
    out
  })
  
  ## bagian PDRB
  #observeEvent(input$saveModalFD,{
  proyPdrbTable <- reactive({
    #browser()
    proyPdrbScen <- outputTable()*data.frame(proportionGDP)[,1]
    colProyPdrbScen <- data.frame(colSums(proyPdrbScen))
    colnames(colProyPdrbScen) <- c("totalPDRB")
    colProyPdrbScen$year <- rownames(colProyPdrbScen)
    colProyPdrbScen <- colProyPdrbScen[1:length(loadFileRDS()$tahunAwal:loadFileRDS()$tahunAkhir), ]
    colProyPdrbScen$scenario <- c("SKENARIO")
    
    #BAU
    colProyPdrbBAU <- colProyPdrbDF
    colProyPdrbBAU <- colProyPdrbBAU[1:length(loadFileRDS()$tahunAwal:loadFileRDS()$tahunAkhir), ]
    
    #gabung BAU dan skenario
    colProyPdrbScen <- rbind(colProyPdrbScen,colProyPdrbBAU)
    
    colProyPdrbScen
  })
  
  ##### simpan tabel FD baru ke dalam folder ####
  observeEvent(input$saveModalFD,{
    dataDefine <-  loadFileRDS()
    dataDefine$fdSelisih <- FDSave()$fdSelisih
    dataDefine$proyPdrb <- proyPdrbTable()
    selectedRow <- as.numeric(strsplit(input$select_button,"_")[[1]][2])
    fileName<- as.character(ListTableReact()[selectedRow,5]) #nama file
    saveRDS(dataDefine, file = fileName)
  
    insertUI(selector='#objDownloadFD',
             where='afterEnd',
             ui= uiOutput(ns('downButtonFD'))
    )
  })

  output$downButtonFD<- renderUI({
    tagList(tags$br(),
            actionButton(ns('downloadFD'),'download tabel')
    )
  })


  observeEvent(input$downloadFD,{
    selectedRow <- as.numeric(strsplit(input$select_button,"_")[[1]][2])
    fileName<- as.character(ListTableReact()[selectedRow,5]) #nama file
    namaSheet <- names(FDSave())
    
    dirFile <- paste0(fileName,".xlsx")
    write.xlsx(FDSave(),
               file = dirFile,
               sheetName = namaSheet)
  })
  
  
  ################################################################################
  #                                                                              #
  #                   ALUR BUTTON KONSTRUKSI SATELIT AKUN                        #
  #                                                                              #
  ################################################################################
  ## proyeksi konsumsi energi
  #koefisien energi dari sheet energi
  #tabel konsumsi energi dari sheet proyeksi
  proyKonsumsiEnergiTable <- reactive({
    proyKons <- outputTable() * data$koefisien
    proyKons
  })
  

  #terbentuk 15 tabel konsumsi energi
  proyTabelKonsEnergiTable <- reactive({
    proyTabelKonsEnergiTable<-list()
    for (i in 1:ncol(proyKonsumsiEnergiTable())) {
      proyTabelKonsEnergiTable[[i]]<-proyKonsumsiEnergiTable()[,i]*data$proportionConsumption
    }
    names(proyTabelKonsEnergiTable)<-paste0("y",initialYear:finalYear)
    proyTabelKonsEnergiTable
  })
  
  
  observeEvent(input$satHit, {
    insertUI(selector= paste0("#", ns("satPlaceholder")),
             where='afterEnd',
             ui= uiOutput(ns('satMUIManual'))
    )
  })
  
  output$satMUIManual<- renderUI({
    tagList(selectInput(ns("pilihtahunSat"),
                        label="pilih tahun", selected = loadFileRDS()$tahunAwal,
                        choices=c(loadFileRDS()$tahunAwal:loadFileRDS()$tahunAkhir)),
            pickerInput(ns("pilihBahanBakar"),
                        label="pilih faktor emisi",selected = data$faktorEmisi[1],
                        choices=data$faktorEmisi,options = list(`actions-box` = TRUE),multiple = T),
            tags$br(),
            actionButton(ns('showYearSat'), 'tampilkan tabel'),
            tags$br(),
            tags$br(),
            tags$div(id = 'SuntingSatPlaceHolder')
    )
  })
  
  observeEvent(input$showYearSat, {
    insertUI(selector='#SuntingSatPlaceHolder',
             where='afterEnd',
             ui= uiOutput(ns('SuntingSatUITable'))
    )
  }) 
  
  output$SuntingSatUITable<- renderUI({
    tagList(
      tags$b('Sunting secara manual'),
      tags$br(),
      tags$br(),
      rHandsontableOutput(ns('editSat')),
      tags$br(),
      actionButton(ns('saveModalSat'), 'simpan tabel'),
      tags$br(),
      tags$br(),
      tags$div(id='teksSatSave')
    )
  })
  
  satAkun <- reactiveValues(
    table1 = NULL
  )
  
  satBauReact <- reactiveValues(
    isi = data$listConsumZero
  )
  
  SatData <- reactive({
    if (is.null(loadFileRDS()$satSelisih)) {
      satAkun$table1 = satBauReact$isi
    }else{
      satAkun$table1 =  loadFileRDS()$satSelisih
    }
  })
  
  #observeEvent(input$showYearSat,{
  valSat<- eventReactive(c(input$showYearSat),{
    #browser()
    #bentuk list
    indexAwal <- paste0("y",input$pilihtahunSat)
    satAkun$table1 <- SatData()[indexAwal]
    satAkun$table1 <- data.frame(satAkun$table1)
    satAkun$table1 <- satAkun$table1[,c(-1,-2,-3)]
    satAkun$table1 <- cbind(Sector,satAkun$table1)
    satAkun$table1 <- filter(satAkun$table1, satAkun$table1$Sector %in% c(input$sektorSat))
    satAkun$table1 <- satAkun$table1[,c("Sector",paste0("y",input$pilihtahunSat,".",input$pilihBahanBakar))]
    rownames(satAkun$table1) <- c(input$sektorSat)
    satAkun$table1
  })
  
  output$editSat <- renderRHandsontable({
    rhandsontable(valSat(),
                  rowHeaderWidth = 160,
    )%>%hot_cols(format=3)
  })
  
  #### masukkan nilai sel baru ke dalam kolom satNew 
  satSave<-eventReactive(input$saveModalSat,{
    satAkun$table1<-as.data.frame(hot_to_r(input$editSat))
    inputSektor<-input$sektorSat 
    indexSektor <- as.numeric(which(sapply(Sector,function(x) any(x==c(inputSektor)))))
    inputTahun<-paste0("y",input$pilihtahunSat)
    inputBahanBakar <- input$pilihBahanBakar
    
    satBauReact$isi <- data$listConsumBAU
    satBauReact$isi[[inputTahun]][indexSektor,inputBahanBakar]<-satBauReact$isi[[inputTahun]][indexSektor,inputBahanBakar] + satAkun$table1[-1]
    
    satSelisih <- data$listConsumZero
    satSelisih[[inputTahun]][indexSektor,inputBahanBakar]<-satSelisih[[inputTahun]][indexSektor,inputBahanBakar] + satAkun$table1[-1]
      
    satNew_list<-list(satBauReal = data$listConsumBAU, 
                      satNew = satBauReact$isi, #hasil penjumlahan BAU dengan selisih
                      satEditNew=satAkun$table1, # 1 data.frame = tabel yang diedit (partial) yang tampilkan di modal dialog UI
                      satSelisih = satSelisih  #list selisih (15 tabel)
    )
    satNew_list
  })
  
  # terbentuk 15 tabel proyeksi emisi
  ########################ini yang di simpen di file rds utk di baca plot
  #observeEvent(input$closeModalFD,{
  proyEmisiTabel <-  eventReactive(input$saveModalSat,{
    #browser()
    proyEmisiSken <- list()
    for (i in 1:(iteration+1)) {
      proyEmisiSken[[i]]<-as.matrix(satSave()$satNew[[i]][-c(1,2,3)]) %*% data$matEfBau
    }
    names(proyEmisiSken)<-paste0("y",initialYear:finalYear)
    
    for (i in 1:(iteration+1)) {
      if(i==1){
        rowsumProyEmisiSken <- rowSums(proyEmisiSken[[i]])
      }else{
        rowsumProyEmisiSken<- cbind(rowsumProyEmisiSken,rowSums(proyEmisiSken[[i]]))
      }
    }
    colnames(rowsumProyEmisiSken) <- paste0("y",initialYear:finalYear)
    
    #COLSUM proyeksi energi
    colsumProyEmisiSken <- colSums(rowsumProyEmisiSken)
    cumProyEmisiSken <- data.frame(cumsum(colsumProyEmisiSken))
    colnames(cumProyEmisiSken) <- c("cumEmisi")
    cumProyEmisiSken$year <- rownames(cumProyEmisiSken)
    cumProyEmisiSken <- cumProyEmisiSken[1:length(loadFileRDS()$tahunAwal:loadFileRDS()$tahunAkhir), ]
    cumProyEmisiSken$scenario <- c("SKENARIO")
    
    #BAU
    totalEmisi <- data$tabelEmisi %>% 
      filter(between(year, initialYear, finalYear)) %>% 
      group_by(year) %>% 
      summarise(totalEmisi = sum(Temission))
    
    cumsumTotalEmisi <- cumsum(totalEmisi[,2])
    colnames(cumsumTotalEmisi) <- "cumEmisi"
    
    cumProyEmisi <- cbind(totalEmisi[,1],cumsumTotalEmisi)
    
    
    cumProyEmisiBAU <- cumProyEmisi
    cumProyEmisiBAU <- cumProyEmisiBAU[1:length(loadFileRDS()$tahunAwal:loadFileRDS()$tahunAkhir), ]
    cumProyEmisiBAU$year <- paste0("y",cumProyEmisiBAU$year)
    cumProyEmisiBAU$scenario <- c("BAU")
    
    #gabung BAU dan skenario
    cumProyEmisiSken <- rbind(cumProyEmisiSken,cumProyEmisiBAU)
    
    cumProyEmisiSken 
  })
  
  ##### simpan tabel Sat baru ke dalam folder ####
  observeEvent(input$saveModalSat,{
    #browser()
    dataDefine <-  loadFileRDS()
    dataDefine$fdSelisih <- FDSave()$fdSelisih
    dataDefine$satSelisih <- satSave()$satSelisih
    dataDefine$proyPdrb <- proyPdrbTable()
    dataDefine$proyEmisi <- proyEmisiTabel()
    selectedRow <- as.numeric(strsplit(input$select_button,"_")[[1]][2])
    fileName<- as.character(ListTableReact()[selectedRow,5]) #nama file
    saveRDS(dataDefine, file = fileName)
    
    #tampilan keterangan setiap save tabel konsumsi
    inputSektorTampil<-capture.output(cat(input$sektorSat , sep=", ")) #"tanaman pangan"
    inputTahun<-paste0("y",input$pilihtahunSat)
    inputBahanBakarTampil <- capture.output(cat(input$pilihBahanBakar , sep=", "))
    textTampil <- paste0("Satelit akun yang diedit adalah ","sektor: ",inputSektorTampil," - ","tahun: ",
                         inputTahun," - ","bahan bakar: ",inputBahanBakarTampil)
    insertUI(selector='#teksSatSave',
             where = 'afterEnd',
             ui = tags$div (textTampil))
  })
  
  ### tutup modal dialog Econ ###
  observeEvent(input$closeModalFD,{
    removeModal()
    shinyjs::js$refresh()
  })
  
  

  ################################################################################
  #                                                                              #
  #                                  BUTTON RUN                                  #
  #                                                                              #
  ################################################################################
  loadFileRDSButtonRun <- reactive({
    selectedRow <- as.numeric(strsplit(input$run_button,"_")[[1]][2])
    fileName<- as.character(ListTableReact()[selectedRow,5]) #nama file dari ListTableReact ada di col=5
    selectedFile<-readRDS(fileName)
    
    selectedFile
  })
  

  plotPDRB <- eventReactive(input$run_button,{
    #browser()
    checkValue <- loadFileRDSButtonRun()$proyPdrb
    
    if (is.null(checkValue)) {
      #BAU
      colProyPdrbBAU <- colProyPdrbDF
      colProyPdrbBAU <- colProyPdrbBAU[1:length(loadFileRDSButtonRun()$tahunAwal:loadFileRDSButtonRun()$tahunAkhir), ]
      
      ggplot(colProyPdrbBAU, aes(x=year, y=totalPDRB, group=scenario))+
        geom_line(aes(color=scenario))+
        geom_point(aes(color=scenario))+
        labs(x="Tahun", y="PDRB")+
        ggtitle("Grafik Proyeksi PDRB Data Historis (BAU)")+
        theme(plot.title = element_text(hjust = 0.5))
    }else{
      dataDefine <-  data.frame(checkValue)
      ggplot(dataDefine, aes(x=year, y=totalPDRB, group=scenario))+
        geom_line(aes(color=scenario))+
        geom_point(aes(color=scenario))+
        labs(x="Tahun", y="PDRB")+
        ggtitle("Grafik Proyeksi PDRB")+
        theme(plot.title = element_text(hjust = 0.5))
      
    }
  })

  output$hasilRun <- renderPlotly({
    plotPDRB()

  })
  
  
  plotEmisi <- eventReactive(input$run_button,{
  
    checkValue <- loadFileRDSButtonRun()$proyEmisi
    
    if (is.null(checkValue)) {
      #BAU
      totalEmisi <- data$tabelEmisi %>% 
        filter(between(year, initialYear, finalYear)) %>% 
        group_by(year) %>% 
        summarise(totalEmisi = sum(Temission))
      
      cumsumTotalEmisi <- cumsum(totalEmisi[,2])
      colnames(cumsumTotalEmisi) <- "cumEmisi"
      
      cumProyEmisi <- cbind(totalEmisi[,1],cumsumTotalEmisi)
      
      
      cumProyEmisiBAU <- cumProyEmisi
      cumProyEmisiBAU <- cumProyEmisiBAU[1:length(loadFileRDSButtonRun()$tahunAwal:loadFileRDSButtonRun()$tahunAkhir), ]
      cumProyEmisiBAU$year <- paste0("y",cumProyEmisiBAU$year)
      cumProyEmisiBAU$scenario <- c("BAU")
      
      ggplot(cumProyEmisiBAU, aes(x=year, y=cumEmisi, group=scenario))+
        geom_line(aes(color=scenario))+
        geom_point(aes(color=scenario))+
        labs(x="Tahun", y="Emisi Kumulatif")+
        ggtitle("Grafik Emisi Kumulatif Data Historis (BAU)")+
        theme(plot.title = element_text(hjust = 0.5))
    }else{
      dataDefine <-  data.frame(loadFileRDSButtonRun()$proyEmisi)
      ggplot(dataDefine, aes(x=year, y=cumEmisi, group=scenario))+
          geom_line(aes(color=scenario))+
          geom_point(aes(color=scenario))+
          labs(x="Tahun", y="Emisi Kumulatif")+
          ggtitle("Grafik Emisi Kumulatif")+
          theme(plot.title = element_text(hjust = 0.5))
      
    }
  })

  output$plot2 <- renderPlotly({
    plotEmisi()
  })
  
  
  
  
}