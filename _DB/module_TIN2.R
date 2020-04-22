
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



buttonModule <- function(input, output, session, data, type) {
  
  
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
      actionButton(ns("cancelModalDef"), "batal")
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
    # proyPdrb <- NULL #8
    # proyEmisi <- NULL #9
    
    #tambahkan faktor emisi
    emissionFactor <- NULL
    
    
    combineDef <- list(namaSken=namaSken,tahunAwal=tahunAwal,tahunAkhir=tahunAkhir, deskrip=deskrip,
                       fdSelisih=fdSelisih, satSelisih=satSelisih, 
                       #proyPdrb = proyPdrb,proyEmisi=proyEmisi,
                       emissionFactor=emissionFactor)

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
                              onclick = sprintf('Shiny.onInputChange("%s",this.id)', ns("run_button"))),
          Hapus.skenario = ListButton_fun(actionButton,
                                             length(loadRDSAll()),
                                             'buttonDelete_',
                                             label = "Hapus Skenario",
                                             onclick = sprintf('Shiny.onInputChange("%s",this.id)', ns("delete_button")))
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
  
  # fdBauReact <- reactiveValues(
  #   isi = fdZero
  # )
  
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
    
    # fdBauReact$isi <- fdBau
    # fdBauReact$isi[c(indexSektor), c(inputTahun)] <- fdBauReact$isi[c(indexSektor), c(inputTahun)] + finalDemand$table1[,-1]
     
    #fdSelisih <- fdZero
    if (is.null(loadFileRDS()$fdSelisih)) {
      fdSelisih = fdZero
    }else{
      fdSelisih =  loadFileRDS()$fdSelisih
    }
    fdSelisih[c(indexSektor), c(inputTahun)] <- fdSelisih[c(indexSektor), c(inputTahun)] + finalDemand$table1[,-1]
    
    
    fdNew_list<-list(fdBauReal = fdBau,
                     #fdNew = fdBauReact$isi,
                     fdEditNew = finalDemand$table1,
                     fdSelisih = fdSelisih
    )
    fdNew_list

  })
  
 
  
  ##### simpan tabel FD baru ke dalam folder ####
  observeEvent(input$saveModalFD,{
    dataDefine <-  loadFileRDS()
    dataDefine$fdSelisih <- FDSave()$fdSelisih
    #dataDefine$proyPdrb <- proyPdrbTable()
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
    #browser()
    selectedRow <- as.numeric(strsplit(input$select_button,"_")[[1]][2])
    fileName <- as.character(ListTableReact()[selectedRow,5]) #nama file
    fileName <- strsplit(fileName,"/")[[1]][5]
    namaSheet <- names(FDSave())
    
    dirFile <- paste0(data$alamatFile,"/","Excel_",fileName,".xlsx")
    write.xlsx(FDSave(),
               file = dirFile,
               sheetName = namaSheet)
  })
  
  
  ################################################################################
  #                                                                              #
  #                   ALUR BUTTON KONSTRUKSI SATELIT AKUN                        #
  #                                                                              #
  ################################################################################
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
  
  
  
  SatData <- reactive({
    if (is.null(loadFileRDS()$satSelisih)) {
      satAkun$table1 = data$listConsumZero
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
    
    
    if (is.null(loadFileRDS()$satSelisih)) {
      satSelisih = data$listConsumZero
    }else{
      satSelisih =  loadFileRDS()$satSelisih
    }
    
    satSelisih[[inputTahun]][indexSektor,inputBahanBakar]<-satSelisih[[inputTahun]][indexSektor,inputBahanBakar] + satAkun$table1[-1]
      
    satNew_list<-list(satBauReal = data$listConsumBAU, 
                      satEditNew=satAkun$table1, # 1 data.frame = tabel yang diedit (partial) yang tampilkan di modal dialog UI
                      satSelisih = satSelisih  #list selisih (15 tabel)
    )
    satNew_list
  })
  
  
  
  ##### simpan tabel Sat baru ke dalam folder ####
  observeEvent(input$saveModalSat,{
    #browser()
    dataDefine <-  loadFileRDS()
    dataDefine$fdSelisih <- FDSave()$fdSelisih
    dataDefine$satSelisih <- satSave()$satSelisih
    #dataDefine$proyPdrb <- proyPdrbTable()
    #dataDefine$proyEmisi <- proyEmisiTabel()
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
  })
  
  

  ################################################################################
  #                                                                              #
  #                                  BUTTON RUN                                  #
  #                                                                              #
  ################################################################################
  #loadFileRDSButtonRun <- eventReactive(input$run_button,{
  runSimulation<-eventReactive(input$run_button, {
    selectedRow <- as.numeric(strsplit(input$run_button,"_")[[1]][2])
    fileName<- as.character(ListTableReact()[selectedRow,5]) #nama file dari ListTableReact ada di col=5
    selectedFile<-readRDS(fileName)
    
    initialYear<-selectedFile$tahunAwal
    finalYear <- selectedFile$tahunAkhir
    iteration <- finalYear - initialYear
    ioPeriod <- 2015
    
    if (type == "energy") {
      energyScen= selectedFile
      scenarioFD=energyScen[["fdSelisih"]]
      scenarioFD=scenarioFD[,2:ncol(scenarioFD)]
      scenarioInputLandCover= NULL
      additionalSatelliteEnergy=energyScen[["satSelisih"]]  
      additionalSatelliteWaste=NULL
      additionalSatelliteAgriculture=NULL
      additionalEmissionFactorEnergy=NULL
      additionalEmissionFactorWaste=NULL
      additionalEmissionFactorAgriculture=NULL
    } 
    
    # Series of GPD & Output projection
    scenarioSeriesOfGDP <- data.frame(Sektor = ioSector[,1], stringsAsFactors = FALSE)
    scenarioSeriesOfFinalDemand <- rowSumsMatrixIoFinalDemand
    scenarioSeriesOfOutput <- ioTotalOutput
    
    # Series of Intervention Point
    scenarioSeriesOfIntermediateDemand <- list()
    scenarioSeriesOfAddedValue <- list()
    scenarioSeriesOfFinalDemandComponent <- list()
    scenarioSeriesOfImpactLabour <- list()
    scenarioSeriesOfImpactEnergy <- list()
    scenarioSeriesOfImpactWaste <- list()
    scenarioSeriesOfImpactAgriculture <- list()
    scenarioSeriesOfImpactLand1<-list()
    scenarioSeriesOfImpactLand2<-list()
    scenarioSeriesOfImpactLand3<-list()
    
    
    # Historical consumption and emission data
    eval(parse(text=paste0("scenarioSeriesOfGDP$y",ioPeriod,"<- analysisGDP")))
    eval(parse(text=paste0("scenarioSeriesOfIntermediateDemand$y",ioPeriod," <- matrixIoIntermediateDemand")))
    eval(parse(text=paste0("scenarioSeriesOfAddedValue$y",ioPeriod," <- matrixIoAddedValue")))
    eval(parse(text=paste0("scenarioSeriesOfFinalDemandComponent$y",ioPeriod," <- matrixIoFinalDemand")))
    eval(parse(text=paste0("scenarioSeriesOfImpactLabour$y",ioPeriod," <- functionSatelliteImpact('labour', satellite = satelliteLabour, matrix_output = as.matrix(ioTotalOutput))")))
    eval(parse(text=paste0("scenarioSeriesOfImpactEnergy$y",ioPeriod," <- functionSatelliteImpact('energy', satellite = satelliteEnergy, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorEnergy)")))
    eval(parse(text=paste0("scenarioSeriesOfImpactWaste$y",ioPeriod," <- functionSatelliteImpact('waste', satellite = satelliteWaste, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorWaste)")))
    eval(parse(text=paste0("scenarioSeriesOfImpactAgriculture$y",ioPeriod," <- functionSatelliteImpact('agriculture', satellite = satelliteAgriculture, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorAgriculture)")))
    
    
    # historical LRC, land requirement, & land cover 
    eval(parse(text=paste0("scenarioSeriesOfImpactLand1$y",ioPeriod,"<-functionSatelliteLand1(type= 'historis', matrix_output= as.matrix(ioTotalOutput))")))
    # LSEI
    eval(parse(text=paste0("scenarioSeriesOfImpactLand2$y",ioPeriod,"<- functionSatelliteLand2(type='historis',carbonStock=carbonStock_his, GDP= as.matrix(bauSeriesOfGDP$y",ioPeriod,"))")))
    
    #projection data
    projectionYear <- initialYear
    listYear <- paste0("y", ioPeriod)
    
    # economic & impact (energy, waste, & agriculture projection 
    for(step in 1:(iteration+1)){
      
      # notes on the year
      timeStep <- paste0("y", projectionYear)
      
      projectionFinalDemand <- bauSeriesOfFinalDemand[,timeStep] + scenarioFD[,timeStep]  # input final demand ditambahkan di sini
      
      scenarioSeriesOfFinalDemand <- cbind(scenarioSeriesOfFinalDemand, projectionFinalDemand)
      projectionOutput <- ioLeontiefInverse %*% projectionFinalDemand 
      scenarioSeriesOfOutput <- cbind(scenarioSeriesOfOutput, projectionOutput)
      
      # add additional values to the list
      eval(parse(text=paste0("scenarioSeriesOfFinalDemandComponent$", timeStep, " <- as.matrix(proportionFinalDemand*projectionFinalDemand)"))) # contains NaN
      eval(parse(text=paste0("scenarioSeriesOfIntermediateDemand$", timeStep, " <-  analysisCT %*% diag(as.vector(projectionOutput), ncol = ioDimention, nrow= ioDimention)")))
      eval(parse(text=paste0("scenarioSeriesOfAddedValue$", timeStep, " <-  analysisCPI %*% diag(as.vector(projectionOutput), ncol = ioDimention, nrow= ioDimention)")))
      
      # GDP projection 
      eval(parse(text = paste0("scenarioSeriesOfGDP$", timeStep, "<- colSums(scenarioSeriesOfAddedValue$", timeStep, "[setdiff(1:nrow(matrixIoAddedValue), rowImport),])")))
      
      # Impact projection
      eval(parse(text= paste0("scenarioSeriesOfImpactLabour$", timeStep, " <- functionSatelliteImpact('labour', satellite = satelliteLabour, matrix_output = as.matrix(projectionOutput))")))
      eval(parse(text= paste0("scenarioSeriesOfImpactEnergy$", timeStep, " <- functionSatelliteImpact('energy', 
                                                                                                  satellite = satelliteEnergy, 
                                                                                                  matrix_output = as.matrix(projectionOutput), 
                                                                                                  emission_factor = emissionFactorEnergy,
                                                                                                  additional_satellite= additionalSatelliteEnergy[['",timeStep,"']],
                                                                                                  additional_emission_factor=additionalEmissionFactorEnergy[['",timeStep,"']])")))
      eval(parse(text= paste0("scenarioSeriesOfImpactWaste$", timeStep, " <- functionSatelliteImpact('waste', 
                                                                                                 satellite = satelliteWaste, 
                                                                                                 matrix_output = as.matrix(projectionOutput), 
                                                                                                 emission_factor = emissionFactorWaste,
                                                                                                 additional_satellite=additionalSatelliteWaste[['",timeStep,"']], 
                                                                                                 additional_emission_factor=additionalEmissionFactorWaste[['",timeStep,"']])")))
      eval(parse(text= paste0("scenarioSeriesOfImpactAgriculture$", timeStep, " <- functionSatelliteImpact('agriculture', 
                                                                                                        satellite = satelliteAgriculture, 
                                                                                                        matrix_output = as.matrix(projectionOutput), 
                                                                                                        emission_factor = emissionFactorAgriculture, 
                                                                                                        additional_satellite=additionalSatelliteAgriculture[['",timeStep,"']], 
                                                                                                        additional_emission_factor=additionalEmissionFactorAgriculture[['",timeStep,"']])")))
      
      listYear <- c(listYear, timeStep)
      projectionYear <- initialYear+step
      
    }
    
    colnames(scenarioSeriesOfOutput) <- as.character(listYear)
    
    scenarioSeriesOfFinalDemandTable <- cbind(data.frame(ioSector$V1), scenarioSeriesOfFinalDemand)
    colnames(scenarioSeriesOfFinalDemandTable) <- c("Sektor", as.character(listYear)) 
    
    # land cover projection 
    
    # non-advance mode
    for (i in 1:2){
      projectionYear <- initialYear
      listYear <- paste0("y", ioPeriod)
      for(step in 1:(iteration+1)){
        # notes on the year
        timeStep <- paste0("y", projectionYear)
        # projection
        eval(parse(text= paste0("scenarioSeriesOfImpactLand1$", timeStep, " <- functionSatelliteLand1(type= 'projection', 
                                                                                                      matrix_output= as.matrix(scenarioSeriesOfOutput[,'",timeStep,"']), 
                                                                                                      advanceMode = FALSE,
                                                                                                      currYear= projectionYear,
                                                                                                      runNum = ",i,", 
                                                                                                      LRCRate= NULL)")))
        listYear <- c(listYear, timeStep)
        projectionYear <- initialYear+step
      } 
      # jika tidak ada value landCover yang negatif, break loop
      if(any(unlist(sapply(scenarioSeriesOfImpactLand1,'[[', "landCover"))<0)==FALSE){  
        if(i==1){
          textDataLRCRate="historis"
        } else {
          textDataLRCRate="historis yang dimodifikasi" 
        }
        print(paste0("laju perubahan LRC yang digunakan untuk membangun proyeksi tutupan lahan adalah data laju LRC ", textDataLRCRate)) # use as UI textoutput 
        break
      } else {
        if(i==2){
          print("proyeksi luas tutupan lahan yang dihasilkan bernilai negatif. Silakan masukkan data laju perubahan LRC secara manual")
        }
      }
    }
    
    # jika masih ada value landCover yang negatif, force to enter advanceMode pada UI
    if(any(unlist(sapply(scenarioSeriesOfImpactLand1,'[[', "landCover"))<0)==TRUE){
      repeat{
        # insert UI here to request for new inputLRCRate 
        inputLRCRate<-LRCRate_2  
        projectionYear <- initialYear
        listYear <- paste0("y", ioPeriod)
        for(step in 1:(iteration+1)){
          # notes on the year
          timeStep <- paste0("y", projectionYear)
          eval(parse(text= paste0("scenarioSeriesOfImpactLand1$", timeStep, " <- functionSatelliteLand1(type= 'projection', 
                                                                                          matrix_output= as.matrix(scenarioSeriesOfOutput[,'",timeStep,"']), 
                                                                                          advanceMode = TRUE,
                                                                                          runNum = NULL,
                                                                                          LRCRate= inputLRCRate)")))
          listYear <- c(listYear, timeStep)
          projectionYear <- initialYear+step
        }  
        # jika tidak ada value landCover yang negatif, break loop
        if(any(unlist(sapply(scenarioSeriesOfImpactLand1,'[[', "landCover"))<0)==FALSE){ 
          print("laju perubahan LRC yang digunakan untuk membangun proyeksi tutupan lahan adalah data laju LRC yang telah Anda modifikasi") # use as UI textoutput 
          break
        } else {
          print("proyeksi tutupan lahan yang dihasilkan memiliki luasan negatif. Silakan menyunting ulang laju perubahan LRC dan atau kembali ke target permintaan akhir") # use as UI textoutput 
        }
      }
    }
    
    
    # LUTM Projection 
    projectionYear <- initialYear
    listYear <- paste0("y", ioPeriod)
    
    for(step in 1:(iteration+1)){
      timeStep <- paste0("y", projectionYear)
      for (i in 1:6){   # 6 tipe yg akan dirun otomatis
        eval(parse(text=paste0("scenarioSeriesOfImpactLand3$",timeStep,"<-functionSatelliteLand3(inputLandScen=scenarioInputLandCover,
                                                                                             timeScen='",timeStep,"')")))
        eval(parse(text=paste0("
        scenarioSeriesOfImpactLand2$",timeStep,"<-tryCatch({
        functionSatelliteLand2 (type ='projected',
                                landCoverProjection = as.matrix(scenarioSeriesOfImpactLand1[['",timeStep,"']][['landCover']][['luas.land.use']]),
                                landCoverProjectionMin = as.matrix(scenarioSeriesOfImpactLand1[[paste0('y',",projectionYear,"-1)]][['landCover']][['luas.land.use']]),
                                inputLandCover = scenarioSeriesOfImpactLand3[['",timeStep,"']][['landCover']], 
                                LUTMTemplate = scenarioSeriesOfImpactLand3[['",timeStep,"']][['LUTMTemplate']], 
                                advanceMode = FALSE,
                                runNum =",i," , 
                                GDP=as.matrix(scenarioSeriesOfGDP$",timeStep,",), 
                                additionalG = scenarioSeriesOfImpactLand3[['",timeStep,"']][['additionalG']], 
                                additionalH= scenarioSeriesOfImpactLand3[['",timeStep,"']][['additionalH']] 
                                )
        }, warning = function (a){NA}, error = function(b){NA})"
        )))
        if(any(is.na(scenarioSeriesOfImpactLand2[[timeStep]]))==FALSE){  
          print(paste0("use constraint ", i ," to make LUTM ",timeStep))
          break
        } else {
          if(i==6){
            print(paste0("tidak berhasil menghitung LUTM ",timeStep))
          } 
        }
      }    
      listYear <- c(listYear, timeStep)
      projectionYear <- initialYear+step
    }
    
    # jika tidak berhasil menghitung LUTM, force to enter advanceMode pada UI (spt pada land cover)
    
    #####END : intervention projection ####
    #####BEGIN : intervention visualization ####
    # 1. GDP (ind. 1)
    scenarioResultGDP <- data.frame(year = 0, sector = "", category="", GDP = 0, stringsAsFactors = FALSE)
    # scenarioResultGDP <- data.frame(year = 0, id.sector = 0, sector = "", GDP = 0, stringsAsFactors = FALSE)
    for(c in 2:ncol(scenarioSeriesOfGDP)){
      add.row <- cbind(ioSector, scenarioSeriesOfGDP[, c])
      names(add.row) <- c("sector", "category", "GDP")
      add.row$year <- initialYear + (c-3)
      add.row <- add.row[, colnames(scenarioResultGDP)]
      scenarioResultGDP <- data.frame(rbind(scenarioResultGDP, add.row), stringsAsFactors = FALSE)
    }
    scenarioResultGDP <- scenarioResultGDP[scenarioResultGDP$year != 0, ] # remove initial values
    
    # 2. Income per capita (ind. 9)
    scenarioResultIncomePerCapita <- data.frame(year = 0, Income.per.capita = 0)
    for(t in 0:iteration){
      t_curr <- initialYear + t
      pop_curr <- populationProjection[which(populationProjection[, 1] == t_curr), 2]
      inc_curr <- sum(scenarioSeriesOfAddedValue[[t+2]][rowIncome,])
      inc_capita <- inc_curr/pop_curr
      add.row <- data.frame(cbind(t_curr, inc_capita))
      names(add.row) <- names(scenarioResultIncomePerCapita)
      scenarioResultIncomePerCapita <- data.frame(rbind(scenarioResultIncomePerCapita, add.row), stringsAsFactors = FALSE)
    }
    scenarioResultIncomePerCapita <- scenarioResultIncomePerCapita[scenarioResultIncomePerCapita$year != 0, ]
    
    # 3. Wages or Income (ind. 7)
    scenarioResultIncome <- data.frame(year = 0, sector= "", income = 0, stringsAsFactors = FALSE)
    sc.name <- ioSector[,1]
    for(t in 0:iteration){
      t_curr <- initialYear + t
      inc_curr <- data.frame(scenarioSeriesOfAddedValue[[t+2]][rowIncome,])
      add.row <- data.frame(cbind(t_curr, sc.name, inc_curr), stringsAsFactors = FALSE)
      names(add.row) <- names(scenarioResultIncome)
      scenarioResultIncome <- data.frame(rbind(scenarioResultIncome, add.row), stringsAsFactors = FALSE)
    }
    scenarioResultIncome <- scenarioResultIncome[scenarioResultIncome$year != 0, ]
    
    # 4. Labour (ind. number 10)
    scenarioResultLabour <- data.frame(year = 0, id.sector = 0, sector= "", labour = 0, stringsAsFactors = FALSE)
    for(t in 0:iteration){
      t_curr <- initialYear + t
      add.row <- data.frame(scenarioSeriesOfImpactLabour[[t+2]][[1]])
      names(add.row) <- names(scenarioResultLabour)[2:4]
      add.row$year <- t_curr
      add.row <- add.row[, names(scenarioResultLabour)]
      scenarioResultLabour <- data.frame(rbind(scenarioResultLabour, add.row), stringsAsFactors = FALSE)
    }
    scenarioResultLabour <- scenarioResultLabour[scenarioResultLabour$year != 0, ]
    
    # 5. Energy cons (indicator number 2)
    scenarioResultEnergyConsumption <- scenarioSeriesOfImpactEnergy[[2]][[1]]
    scenarioResultEnergyConsumption$year <- initialYear
    scenarioResultEnergyConsumption <- scenarioResultEnergyConsumption[, c("year", names(scenarioSeriesOfImpactEnergy[[2]][[1]]))]
    for(t in 1:iteration){
      t_curr <- initialYear + t
      add.row <- data.frame(scenarioSeriesOfImpactEnergy[[t+2]][[1]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(scenarioResultEnergyConsumption)]
      scenarioResultEnergyConsumption <- data.frame(rbind(scenarioResultEnergyConsumption, add.row), stringsAsFactors = FALSE)
    }
    names(scenarioResultEnergyConsumption)[2:3] <- c("id.sector", "sector")
    
    # 6. Energy emission (indicator number 3)
    scenarioResultEnergyEmission <- scenarioSeriesOfImpactEnergy[[2]][[2]]
    scenarioResultEnergyEmission$year <- initialYear
    scenarioResultEnergyEmission <- scenarioResultEnergyEmission[, c("year", names(scenarioSeriesOfImpactEnergy[[2]][[2]]))]
    for(t in 1:iteration){
      t_curr <- initialYear + t
      add.row <- data.frame(scenarioSeriesOfImpactEnergy[[t+2]][[2]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(scenarioResultEnergyEmission)]
      scenarioResultEnergyEmission <- data.frame(rbind(scenarioResultEnergyEmission, add.row), stringsAsFactors = FALSE)
    }
    names(scenarioResultEnergyEmission)[2:3] <- c("id.sector", "sector")
    
    # 7. Waste cons (indicator number 2)
    scenarioResultWasteDisposal <- scenarioSeriesOfImpactWaste[[2]][[1]]
    scenarioResultWasteDisposal$year <- initialYear
    scenarioResultWasteDisposal <- scenarioResultWasteDisposal[, c("year", names(scenarioSeriesOfImpactWaste[[2]][[1]]))]
    for(t in 1:iteration){
      t_curr <- initialYear + t
      add.row <- data.frame(scenarioSeriesOfImpactWaste[[t+2]][[1]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(scenarioResultWasteDisposal)]
      scenarioResultWasteDisposal <- data.frame(rbind(scenarioResultWasteDisposal, add.row), stringsAsFactors = FALSE)
      
    }
    names(scenarioResultWasteDisposal)[2:3] <- c("id.sector", "sector")
    
    # 8. Waste emission (indicator number 3)
    scenarioResultWasteEmission <- scenarioSeriesOfImpactWaste[[2]][[2]]
    scenarioResultWasteEmission$year <- initialYear
    scenarioResultWasteEmission <- scenarioResultWasteEmission[, c("year", names(scenarioSeriesOfImpactWaste[[2]][[2]]))]
    for(t in 1:iteration){
      t_curr <- initialYear + t
      add.row <- data.frame(scenarioSeriesOfImpactWaste[[t+2]][[2]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(scenarioResultWasteEmission)]
      scenarioResultWasteEmission <- data.frame(rbind(scenarioResultWasteEmission, add.row), stringsAsFactors = FALSE)
    }
    names(scenarioResultWasteEmission)[2:3] <- c("id.sector", "sector")
    
    # 9. Fertilizer cons (indicator number 2)
    scenarioResultFertilizerUsed <- scenarioSeriesOfImpactAgriculture[[2]][[1]]
    scenarioResultFertilizerUsed$year <- initialYear
    scenarioResultFertilizerUsed <- scenarioResultFertilizerUsed[, c("year", names(scenarioSeriesOfImpactAgriculture[[2]][[1]]))]
    for(t in 1:iteration){
      t_curr <- initialYear + t
      add.row <- data.frame(scenarioSeriesOfImpactAgriculture[[t+2]][[1]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(scenarioResultFertilizerUsed)]
      scenarioResultFertilizerUsed <- data.frame(rbind(scenarioResultFertilizerUsed, add.row), stringsAsFactors = FALSE)
      
    }
    names(scenarioResultFertilizerUsed)[2:3] <- c("id.sector", "sector")
    
    # 10. Fertilizer emission (indicator number 3)
    scenarioResultFertilizerEmission <- scenarioSeriesOfImpactAgriculture[[2]][[2]]
    scenarioResultFertilizerEmission$year <- initialYear
    scenarioResultFertilizerEmission <- scenarioResultFertilizerEmission[, c("year", names(scenarioSeriesOfImpactAgriculture[[2]][[2]]))]
    for(t in 1:iteration){
      t_curr <- initialYear + t
      add.row <- data.frame(scenarioSeriesOfImpactAgriculture[[t+2]][[2]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(scenarioResultFertilizerEmission)]
      scenarioResultFertilizerEmission <- data.frame(rbind(scenarioResultFertilizerEmission, add.row), stringsAsFactors = FALSE)
    }
    names(scenarioResultFertilizerEmission)[2:3] <- c("id.sector", "sector")
    
    # 11. Land Requirement 
    scenarioResultLandReq <- scenarioSeriesOfImpactLand1[[2]][["landReq"]]
    scenarioResultLandReq$year <- initialYear
    scenarioResultLandReq <-scenarioResultLandReq[,c("year", names(scenarioSeriesOfImpactLand1[[2]][["landReq"]]))]
    for(t in 1:iteration){
      t_curr <- initialYear + t
      add.row <- data.frame(scenarioSeriesOfImpactLand1[[t+2]][["landReq"]])
      add.row$year <- t_curr
      add.row <- add.row[,names(scenarioResultLandReq)]
      scenarioResultLandReq <- data.frame(rbind(scenarioResultLandReq, add.row), stringsAsFactors = FALSE)
    }
    
    # 12. Land Cover
    scenarioResultLandCover <- scenarioSeriesOfImpactLand2[[2]][["landCover"]]
    scenarioResultLandCover$year <- initialYear
    scenarioResultLandCover <-scenarioResultLandCover[,c("year", names(scenarioSeriesOfImpactLand2[[2]][["landCover"]]))]
    for(t in 1:iteration){
      t_curr <- initialYear + t
      add.row <- data.frame(scenarioSeriesOfImpactLand2[[t+2]][["landCover"]])
      add.row$year <- t_curr
      add.row <- add.row[,names(scenarioResultLandCover)]
      scenarioResultLandCover <- data.frame(rbind(scenarioResultLandCover, add.row), stringsAsFactors = FALSE)
    }
    
    # 13. LUTM
    scenarioResultLUTM <- scenarioSeriesOfImpactLand2[[2]][["LUTM"]]
    scenarioResultLUTM$year <- initialYear
    scenarioResultLUTM <-scenarioResultLUTM[,c("year", names(scenarioSeriesOfImpactLand2[[2]][["LUTM"]]))]
    for(t in 1:iteration){
      t_curr <- initialYear + t
      add.row <- data.frame(scenarioSeriesOfImpactLand2[[t+2]][["LUTM"]])
      add.row$year <- t_curr
      add.row <- add.row[,names(scenarioResultLUTM)]
      scenarioResultLUTM <- data.frame(rbind(scenarioResultLUTM, add.row), stringsAsFactors = FALSE)
    }
    
    # 14. Land Emission by sector 
    
    scenarioResultLandEmission <- scenarioSeriesOfImpactLand2[[2]][["emission"]]
    scenarioResultLandEmission$year <- initialYear
    scenarioResultLandEmission <-scenarioResultLandEmission[,c("year", names(scenarioSeriesOfImpactLand2[[2]][["emission"]]))]
    for(t in 1:iteration){
      t_curr <- initialYear + t
      add.row <- data.frame(scenarioSeriesOfImpactLand2[[t+2]][["emission"]])
      add.row$year <- t_curr
      add.row <- add.row[,names(scenarioResultLandEmission)]
      scenarioResultLandEmission <- data.frame(rbind(scenarioResultLandEmission, add.row), stringsAsFactors = FALSE)
    } 
    
    # 15. Total Emission
    # scenarioResultTotalEmission <- baselineEmission[which(baselineEmission$Year>=initialYear & baselineEmission$Year<= finalYear),]
    scenarioResultTotalEmission <- data.frame(Year=initialYear:finalYear)
    emissionEnergyConsumption <- numeric()
    emissionWasteDisposal <- numeric()
    emissionFertilizer <- numeric()
    emissionLand <- numeric()
    for(t in 0:iteration){
      t_curr <- initialYear + t
      add_MEcons <- sum(scenarioResultEnergyEmission[scenarioResultEnergyEmission$year==t_curr, "Temission"])
      add_MWdisp <- sum(scenarioResultWasteEmission[scenarioResultWasteEmission$year==t_curr, "Temission"])
      add_MF <- sum(scenarioResultFertilizerEmission[scenarioResultFertilizerEmission$year==t_curr, "Temission"])
      add_MLand <-sum(scenarioResultLandEmission[scenarioResultLandEmission$year==t_curr, "emission"])
      emissionEnergyConsumption <- c(emissionEnergyConsumption, add_MEcons)
      emissionWasteDisposal <- c(emissionWasteDisposal, add_MWdisp)
      emissionFertilizer <- c(emissionFertilizer, add_MF)
      emissionLand<-c(emissionLand, add_MLand)
    }
    scenarioResultTotalEmission$emissionEnergyCons <- emissionEnergyConsumption
    scenarioResultTotalEmission$emissionWasteDisp <- emissionWasteDisposal
    scenarioResultTotalEmission$emissionFert <- emissionFertilizer
    scenarioResultTotalEmission$emissionLand <-emissionLand
    scenarioResultTotalEmission$TotalEmission <- rowSums(scenarioResultTotalEmission[, 2:ncol(scenarioResultTotalEmission)])
    scenarioResultTotalEmission$CummulativeEmission <- cumsum(scenarioResultTotalEmission$TotalEmission)
    
    # 16. intervention emission[economic sector, years]
    scenarioSeriesOfEmissionBySector <- data.frame(Sektor=ioSector[,1], Kategori=ioSector[,2])
    for(t in 0:iteration){
      t_curr <- initialYear + t
      add_MEcons <- scenarioResultEnergyEmission[scenarioResultEnergyEmission$year==t_curr, "Temission"]
      add_MWdisp <- scenarioResultWasteEmission[scenarioResultWasteEmission$year==t_curr, "Temission"]
      add_MF <- scenarioResultFertilizerEmission[scenarioResultFertilizerEmission$year==t_curr, "Temission"]
      add_MLand <- scenarioResultLandEmission[c(scenarioResultLandEmission$year==t_curr & scenarioResultLandEmission$sector != "lainnya (tidak menghasilkan output"), "emission"]
      eval(parse(text=paste0("scenarioSeriesOfEmissionBySector$y", t_curr, " <- add_MEcons + add_MWdisp + add_MF + add_MLand")))
    }
    
    # scenarioResultTotalGDP <- colSums(scenarioSeriesOfGDP[,2:(ncol(scenarioSeriesOfGDP)-1)])
    scenarioAllResult <- subset(scenarioResultTotalEmission, select=c(Year, TotalEmission, CummulativeEmission))
    # scenarioAllResult <- cbind(scenarioAllResult, scenarioResultTotalGDP)
    scenarioAllResult$ResultTotalGDP<-colSums(scenarioSeriesOfGDP[,2:(ncol(scenarioSeriesOfGDP)-1)])
    scenarioAllResult$CummulativeGDP <- cumsum(scenarioAllResult$ResultTotalGDP)
    scenarioAllResult$EmissionIntensity <- scenarioAllResult$TotalEmission / scenarioAllResult$ResultTotalGDP
    scenarioAllResult$CummulativeEmissionIntensity <-cumsum(scenarioAllResult$EmissionIntensity)
    
    # plot
    plotTotalEmission <- ggplot(data=scenarioAllResult, aes(x=Year, y=TotalEmission, group=1)) + geom_line() + geom_point()
    plotCummulativeEmission<- ggplot(data=scenarioAllResult, aes(x=Year, y=CummulativeEmission, group=1)) + geom_line() + geom_point()
    plotEmissionIntensity <-ggplot(data=scenarioAllResult, aes(x=Year, y=EmissionIntensity, group=1)) + geom_line() + geom_point()
    plotResultTotalGDP<-ggplot(data=scenarioAllResult, aes(x=Year, y=ResultTotalGDP, group=1)) + geom_line() + geom_point()
    plotCummulativeGDP<- ggplot(data=scenarioAllResult, aes(x=Year, y=CummulativeGDP, group=1)) + geom_line() + geom_point()
    plotCummulativeEmissionIntensity<-ggplot(data=scenarioAllResult, aes(x=Year, y=CummulativeEmissionIntensity, group=1)) + geom_line() + geom_point()
    
    
    #comparison with BAU
    scenarioAllResult$type <- "SCENARIO"
    bauAllResult$type<-"BAU"
    comparison<-rbind(scenarioAllResult,bauAllResult)
    for (i in as.character(colnames(comparison[,-c(1,8)]))){
      eval(parse(text=paste0('plotComparison',i,'<-ggplot(comparison, aes(x=Year, y=',i,', group=type))+
      geom_line(aes(color=type))+
      geom_point(aes(color=type))+
      labs(x="Tahun", y="',i,'")+
      ggtitle("Grafik ',i,'")+
      theme(plot.title = element_text(hjust = 0.5))'
      )))
    }
    
    list(scenarioResultGDP=scenarioResultGDP,
         scenarioResultIncome=scenarioResultIncome,
         scenarioResultLabour=scenarioResultLabour,
         scenarioResultEnergyConsumption=scenarioResultEnergyConsumption,
         scenarioResultEnergyEmission=scenarioResultEnergyEmission,
         scenarioResultWasteDisposal=scenarioResultWasteDisposal,
         scenarioResultWasteEmission=scenarioResultWasteEmission,
         scenarioResultFertilizerUsed=scenarioResultFertilizerUsed,
         scenarioResultFertilizerEmission=scenarioResultFertilizerEmission,
         scenarioResultLandReq=scenarioResultLandReq,
         scenarioResultLandCover=scenarioResultLandCover,
         scenarioResultLUTM=scenarioResultLUTM,
         scenarioResultLandEmission=scenarioResultLandEmission,
         scenarioResultTotalEmission=scenarioResultTotalEmission,
         scenarioAllResult=scenarioAllResult, 
         plotResultTotalGDP=plotResultTotalGDP,
         plotCummulativeGDP=plotCummulativeGDP,
         plotEmissionIntensity=plotEmissionIntensity,
         plotCummulativeEmissionIntensity=plotCummulativeEmissionIntensity,
         plotTotalEmission=plotTotalEmission,
         plotCummulativeEmission=plotCummulativeEmission,
         plotComparisonResultTotalGDP=plotResultTotalGDP,
         plotComparisonCummulativeGDP=plotCummulativeGDP,
         plotComparisonEmissionIntensity=plotEmissionIntensity,
         plotComparisonCummulativeEmissionIntensity=plotCummulativeEmissionIntensity,
         plotComparisonTotalEmission=plotTotalEmission,
         plotComparisonCummulativeEmission=plotCummulativeEmission
    )
  })
  
  
  
  # 
  # 
  # plotPDRB <- eventReactive(input$run_button,{
  #   #browser()
  #   checkValue <- loadFileRDSButtonRun()$proyPdrb
  #   
  #   if (is.null(checkValue)) {
  #     #BAU
  #     colProyPdrbBAU <- colProyPdrbDF
  #     colProyPdrbBAU <- colProyPdrbBAU[1:length(loadFileRDSButtonRun()$tahunAwal:loadFileRDSButtonRun()$tahunAkhir), ]
  #     
  #     ggplot(colProyPdrbBAU, aes(x=year, y=totalPDRB, group=scenario))+
  #       geom_line(aes(color=scenario))+
  #       geom_point(aes(color=scenario))+
  #       labs(x="Tahun", y="PDRB")+
  #       ggtitle("Grafik Proyeksi PDRB Data Historis (BAU)")+
  #       theme(plot.title = element_text(hjust = 0.5))
  #   }else{
  #     dataDefine <-  data.frame(checkValue)
  #     ggplot(dataDefine, aes(x=year, y=totalPDRB, group=scenario))+
  #       geom_line(aes(color=scenario))+
  #       geom_point(aes(color=scenario))+
  #       labs(x="Tahun", y="PDRB")+
  #       ggtitle("Grafik Proyeksi PDRB")+
  #       theme(plot.title = element_text(hjust = 0.5))
  #     
  #   }
  # })
  # 
  # output$hasilRun <- renderPlotly({
  #   plotPDRB()
  # 
  # })
  # 
  # 
  # plotEmisi <- eventReactive(input$run_button,{
  # 
  #   checkValue <- loadFileRDSButtonRun()$proyEmisi
  #   
  #   if (is.null(checkValue)) {
  #     #BAU
  #     totalEmisi <- data$tabelEmisi %>% 
  #       filter(between(year, initialYear, finalYear)) %>% 
  #       group_by(year) %>% 
  #       summarise(totalEmisi = sum(Temission))
  #     
  #     cumsumTotalEmisi <- cumsum(totalEmisi[,2])
  #     colnames(cumsumTotalEmisi) <- "cumEmisi"
  #     
  #     cumProyEmisi <- cbind(totalEmisi[,1],cumsumTotalEmisi)
  #     
  #     
  #     cumProyEmisiBAU <- cumProyEmisi
  #     cumProyEmisiBAU <- cumProyEmisiBAU[1:length(loadFileRDSButtonRun()$tahunAwal:loadFileRDSButtonRun()$tahunAkhir), ]
  #     cumProyEmisiBAU$year <- paste0("y",cumProyEmisiBAU$year)
  #     cumProyEmisiBAU$scenario <- c("BAU")
  #     
  #     ggplot(cumProyEmisiBAU, aes(x=year, y=cumEmisi, group=scenario))+
  #       geom_line(aes(color=scenario))+
  #       geom_point(aes(color=scenario))+
  #       labs(x="Tahun", y="Emisi Kumulatif")+
  #       ggtitle("Grafik Emisi Kumulatif Data Historis (BAU)")+
  #       theme(plot.title = element_text(hjust = 0.5))
  #   }else{
  #     dataDefine <-  data.frame(loadFileRDSButtonRun()$proyEmisi)
  #     ggplot(dataDefine, aes(x=year, y=cumEmisi, group=scenario))+
  #         geom_line(aes(color=scenario))+
  #         geom_point(aes(color=scenario))+
  #         labs(x="Tahun", y="Emisi Kumulatif")+
  #         ggtitle("Grafik Emisi Kumulatif")+
  #         theme(plot.title = element_text(hjust = 0.5))
  #     
  #   }
  # })
  # 
  # output$plot2 <- renderPlotly({
  #   plotEmisi()
  # })
  
  
  ### hapus file ###
  observeEvent(input$delete_button, {
    selectedRow <- as.numeric(strsplit(input$delete_button,"_")[[1]][2])
    fileName<- as.character(ListTableReact()[selectedRow,5]) #nama file dari ListTableReact ada di col=5
    file.remove(fileName)
    shinyjs::js$refresh()
  })
  
}