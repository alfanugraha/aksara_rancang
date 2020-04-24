
buttonUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    actionButton(ns("modalDefineButton"),'Deskripsi Skenario'),
    tags$br(),
    tags$br(),
    dataTableOutput(ns("ListTable")),
    uiOutput(ns("daftarDefineShow")),
    tags$div(id = ns('scenarioResultPlaceholder')),
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
                  readOnly = T,
                  rowHeaderWidth = 160,
    )%>%hot_cols(colWidths = 150)
  })
  
  listValDef <- reactive({
    newTableDef <- as.data.frame(hot_to_r(input$editDefine))

    namaSken <- as.character(newTableDef[1,]) #2
    tahunAwal <- as.numeric(trimws(newTableDef[2,])) #3
    tahunAkhir <- as.numeric(trimws(newTableDef[3,])) #4
    deskrip <- as.character(newTableDef[4,]) #5
    fdSelisih <- NULL #6
    satSelisih <- NULL #7
    
    emissionFactor <- NULL #8
    inputLRCRate <- NULL    #9
    inputPercentageDiagTPM <- NULL #10
    
    combineDef <- list(namaSken=namaSken,tahunAwal=tahunAwal,tahunAkhir=tahunAkhir, deskrip=deskrip,
                       fdSelisih=fdSelisih, satSelisih=satSelisih,
                       emissionFactor=emissionFactor,
                       inputLRCRate=inputLRCRate,
                       inputPercentageDiagTPM=inputPercentageDiagTPM)

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
    dirFile <- paste0(data$alamatFile, "/",nameFiles)
    funcFile <- function(x){
        a <- readRDS(x)
        b <- c(x,a)
        b}
      
    r <- lapply(dirFile, funcFile)
    r
  })
  
  
  ### buat tabel daftar nama file reaktif ###
  ListTableReact <- reactive({
    if(identical(list.files(path = data$alamatFile,pattern = paste0("^", username)),character(0))){
      data.frame(
        Nama.Skenario =  "file tidak tersedia",
        Tahun.Awal = "file tidak tersedia", 
        Tahun.Akhir = "file tidak tersedia",
        Deskripsi.Skenario = "file tidak tersedia",
        fdSelisih = "file tidak tersedia",
        satSelisih = "file tidak tersedia",
        Nama.File = "file tidak tersedia", 
        Sunting.Skenario = "file tidak tersedia",
        Jalankan.analisis = "file tidak tersedia",
        Hapus.skenario = "file tidak tersedia"
      )
    } else {
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
                                          onclick = sprintf('Shiny.onInputChange("%s", Math.random());Shiny.onInputChange("%s",this.id)',
                                                            ns("select_button_trigger"), ns("select_button"))),
        Jalankan.analisis = ListButton_fun(actionButton,
                                           length(loadRDSAll()),
                                           'buttonRun_',
                                           label = "Jalankan Analisis",
                                           onclick = sprintf('Shiny.onInputChange("%s", Math.random());Shiny.onInputChange("%s",this.id)',
                                                             ns("run_button_trigger"), ns("run_button"))),
        Hapus.skenario = ListButton_fun(actionButton,
                                        length(loadRDSAll()),
                                        'buttonDelete_',
                                        label = "Hapus Skenario",
                                        onclick = sprintf('Shiny.onInputChange("%s", Math.random());Shiny.onInputChange("%s",this.id)',
                                                          ns("delete_button_trigger"), ns("delete_button")))
      )
    }
    
      
  })

  ###tampilkan tabel list ###
  output$ListTable <- renderDataTable({
    ListTableReact()
  }, escape = FALSE)
  
  
  
  
  if(type=="land"){
    observeEvent(input$select_button_trigger,{
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
            #                        BUTTON KONSTRUKSI SATELIT AKUN LAHAN                  #
            #                                                                              #
            ################################################################################
            tabPanel(
              h2("Satelit akun lahan"),
              sidebarLayout(
                sidebarPanel(
                  fluidPage(
                    pickerInput(ns("pilihtahunSatLand"),
                                label="pilih tahun intervensi", selected = loadFileRDS()$tahunAwal,
                                choices=c(loadFileRDS()$tahunAwal : loadFileRDS()$tahunAkhir),
                                options = list(`actions-box` = TRUE),multiple = T),
                    selectInput(ns("banyakTupla"),
                                label="banyaknya perubahan tutupan lahan",
                                choices=c(1:10))
                    ),
                  tags$br(),
                  actionButton(ns("satLandHit"),"tampilkan tabel"),
                  width=5
                ),
                mainPanel(
                  tags$div(id = ns('satLandPlaceholder')),
                  width=12)
              ),
              title="Sunting Intervensi Satelit Akun"
            ))
          ,
          size="l",
          easyClose = FALSE)
      )
      
    })
  }else{
    observeEvent(input$select_button_trigger,{
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
                              choices=Sector,options = list(`actions-box` = TRUE),multiple = T)
                ),
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
  }
  
  
  
  
  
  
  
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
      tags$h6("Nilai yang diinputkan adalah selisih dari BAU"),
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

  
  valFD<- eventReactive(c(input$showYearEco),{
    selectedRow <- as.numeric(strsplit(input$select_button,"_")[[1]][2])
    fileName<- as.character(ListTableReact()[selectedRow,5]) #nama file
    dataDefine <-  readRDS(fileName)
    
      if (is.null(dataDefine$fdSelisih)) {
        finalDemand$table1 = fdZero
      }else{
        finalDemand$table1 =  dataDefine$fdSelisih
      }
    finalDemand$table1 <- filter(finalDemand$table1, finalDemand$table1$Sektor %in% c(input$sektorEcon))
    finalDemand$table1 <- finalDemand$table1[,c("Sektor",paste0("y",input$pilihtahunFD))]
    finalDemand$table1
  })
  
  output$editFD <- renderRHandsontable({
    rhandsontable(valFD(),
                  rowHeaderWidth = 50,
                  fixedColumnsLeft = 1
    )%>%hot_cols(colWidths = 100)
    
  })
  
  #### masukkan nilai sel baru ke dalam kolom fdNew
  observeEvent(input$saveModalFD,{
    selectedRow <- as.numeric(strsplit(input$select_button,"_")[[1]][2])
    fileName<- as.character(ListTableReact()[selectedRow,5]) #nama file
    dataDefine <-  readRDS(fileName)
    
    finalDemand$table1 <- as.data.frame(hot_to_r(input$editFD))
    inputSektor<-input$sektorEcon 
    inputTahun<-paste0("y",input$pilihtahunFD)
    indexSektor <- as.numeric(which(sapply(Sector,function(x) any(x==c(inputSektor)))))
    
    if (is.null(dataDefine$fdSelisih)) {
      fdSelisih = fdZero
      fdSelisih[c(indexSektor), c(inputTahun)] <- finalDemand$table1[,-1]
    }else{
      fdSelisih = dataDefine$fdSelisih
      fdSelisih[c(indexSektor), c(inputTahun)] <- finalDemand$table1[,-1]
    }
    
      dataDefine$fdSelisih <- fdSelisih
      saveRDS(dataDefine, file = fileName)

      insertUI(selector='#objDownloadFD',
               where='afterEnd',
               ui= uiOutput(ns('downButtonFD')))
      

  })

  output$downButtonFD<- renderUI({
    tagList(tags$br(),
            renderText("tabel diatas sudah tersimpan"),
            tags$br(),
            actionButton(ns('downloadFD'),'download tabel')
    )
  })


  observeEvent(input$downloadFD,{
    selectedRow <- as.numeric(strsplit(input$select_button,"_")[[1]][2])
    fileName<- as.character(ListTableReact()[selectedRow,5]) #nama file
    dataDefine <-  readRDS(fileName)
    
    fdNew_list<-list(fdBauReal = fdBau,
                     fdEditNew = finalDemand$table1,
                     fdSelisih = dataDefine$fdSelisih
    )
    fdNew_list
    
    fileNameDownload <- strsplit(fileName,"/")[[1]][5]
    namaSheet <- names(fdNew_list)
    
    
    
    dirFile <- paste0(data$alamatFile,"/","Excel_",fileNameDownload,".xlsx")
    write.xlsx(fdNew_list,
               file = dirFile,
               sheetName = namaSheet)
  })
  
  
  ################################################################################
  #                                                                              #
  #                   ALUR BUTTON KONSTRUKSI SATELIT AKUN                        #
  #                                                                              #
  ################################################################################
  satAkun <- reactiveValues(
    table1 = NULL
  )
  
  
  # START LAND ----------------------------------------------------------
  observeEvent(input$satLandHit, {
    insertUI(selector= paste0("#", ns("satLandPlaceholder")),
             where='afterEnd',
             ui= uiOutput(ns('satLandUIManual'))
    )
  })
  
  
  output$satLandUIManual<- renderUI({
    
    tagList(
      tags$b('Sunting secara manual'),
      tags$br(),
      tags$h6("Nilai yang diinputkan adalah selisih dari BAU"),
      tags$br(),
      rHandsontableOutput(ns('editSatLand')),
      tags$br(),
      tags$h6("untuk memilih tipe tutupan lahan disediakan menu dropdown pada kolom 1 dan 2"),
      tags$br(),
      actionButton(ns('saveModalSatLand'), 'simpan tabel'), 
      tags$br(), 
      tags$br(),
      tags$div(id='teksLandSatSave')
    )
  })
  
  valSatLand <- eventReactive(input$satLandHit,{
    selectedRow <- as.numeric(strsplit(input$select_button,"_")[[1]][2])
    fileName<- as.character(ListTableReact()[selectedRow,5]) #nama file
    dataDefine <-  readRDS(fileName)
    
    if (is.null(dataDefine$satSelisih)) {
      satAkun$table1 = data$listConsumZero
      satAkun$table1
    }else{
      selectedRow <- as.numeric(strsplit(input$select_button,"_")[[1]][2])
      fileName<- as.character(ListTableReact()[selectedRow,5]) #nama file
      dataDefine <-  readRDS(fileName)
      satAkun$table1 <- data$listConsumZero
      tupla <- ordered(factor(sort(colnames(LDMProp_his)[landCover_his!=0])))
      satAkun$table1[,1] <- tupla[1]
      satAkun$table1[,2] <- tupla[1]
      satAkun$table1 = dplyr::bind_rows(dataDefine$satSelisih,satAkun$table1)
    }
    
    indexCol <- c(colnames(satAkun$table1)[1],colnames(satAkun$table1)[2],
                  paste0("y",input$pilihtahunSatLand))
    indexRow <- input$banyakTupla
    satAkun$table1 <- satAkun$table1[1:indexRow,c(indexCol)]
    satAkun$table1
  })
  
  output$editSatLand <- renderRHandsontable({
    rhandsontable(valSatLand(),
                  rowHeaderWidth = 50,
                  fixedColumnsLeft = 2,
                  height = 200,
    ) %>% 
      hot_table(contextMenu = TRUE) %>% 
      hot_context_menu(customOpts = list(
                         search = list(name = "Search",
                                       callback = htmlwidgets::JS(
                                         "function (key, options) {
                         var srch = prompt('Search criteria');
                         this.search.query(srch);
                         this.render();
                       }")))) %>% 
      hot_col(col = c(colnames(satAkun$table1)[1],colnames(satAkun$table1)[2]),
              type = "dropdown", source = sort(colnames(LDMProp_his)[landCover_his!=0])) 
    
  })
  
  
  ##### simpan tabel Sat baru ke dalam folder ####
  observeEvent(input$saveModalSatLand,{
    removeUI(selector='#textInvalid')
    removeUI(selector='#textTampil')
    
    satEditNew<-as.data.frame(hot_to_r(input$editSatLand))
    satInvalid <- satEditNew[-2,]
    tupla <- ordered(factor(sort(colnames(LDMProp_his)[landCover_his!=0])))
    satZero = data$listConsumZero
    satZero[,1] <- tupla[1]
    satZero[,2] <- tupla[1]
    satSelisih <- dplyr::bind_rows(satZero,satEditNew)
    satSelisih <- satSelisih[-(1:nrow(data$listConsumZero)),]
    rownames(satSelisih) <- 1:nrow(satSelisih)
    satSelisih[is.na(satSelisih)] <- 0
    
    if (nlevels(satSelisih[,1])==length(colnames(LDMProp_his)[landCover_his!=0]) & nlevels(satSelisih[,2])==length(colnames(LDMProp_his)[landCover_his!=0]) ) {
        selectedRow <- as.numeric(strsplit(input$select_button,"_")[[1]][2])
        fileName<- as.character(ListTableReact()[selectedRow,5]) #nama file

        dataDefine <-  readRDS(fileName)
        dataDefine[5] <- list(dataDefine$fdSelisih)
        dataDefine$satSelisih <- satSelisih

        saveRDS(dataDefine, file = fileName)
        
        insertUI(selector='#teksLandSatSave',
                 where = 'afterEnd',
                 ui = tags$div(id="textTampil","tabel di atas sudah tersimpan"))
    }else{
      
      insertUI(selector='#teksLandSatSave',
               where = 'afterEnd',
               ui =tags$h4(id='textInvalid', 
                            "masih ada tutupan lahan yang belum dipilih, tabel tidak dapat disimpan"))
    }
  })
# ## END Land -------------------------------------------------------------
  
  observeEvent(input$satHit, {
    insertUI(selector= paste0("#", ns("satPlaceholder")),
             where='afterEnd',
             ui= uiOutput(ns('satUIManual'))
    )
  })
  
  output$satUIManual<- renderUI({
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
      tags$h6("Nilai yang diinputkan adalah selisih dari BAU"),
      tags$br(),
      rHandsontableOutput(ns('editSat')),
      tags$br(),
      actionButton(ns('saveModalSat'), 'simpan tabel'),
      tags$br(),
      tags$br(),
      tags$div(id='teksSatSave')
    )
  })
  
  
  valSat<- eventReactive(c(input$showYearSat),{
    selectedRow <- as.numeric(strsplit(input$select_button,"_")[[1]][2])
    fileName<- as.character(ListTableReact()[selectedRow,5]) #nama file
    dataDefine <-  readRDS(fileName)
    
    if (is.null(dataDefine$satSelisih)) {
      satAkun$table1 = data$listConsumZero
    }else{
      satAkun$table1 =  dataDefine$satSelisih
    }
    
    indexAwal <- paste0("y",input$pilihtahunSat)
    satAkun$table1 <- satAkun$table1[indexAwal]
    satAkun$table1 <- data.frame(satAkun$table1)
    satAkun$table1 <- satAkun$table1[,c(-1,-2,-3)]
    satAkun$table1 <- cbind(Sector,satAkun$table1)
    satAkun$table1 <- filter(satAkun$table1, satAkun$table1$Sector %in% c(input$sektorSat))
    satAkun$table1 <- satAkun$table1[,c("Sector",paste0("y",input$pilihtahunSat,".",input$pilihBahanBakar))]
    satAkun$table1
  })
  
  output$editSat <- renderRHandsontable({
    rhandsontable(valSat(),
                  rowHeaderWidth = 50,
                  fixedColumnsLeft = 1
    )%>%hot_cols(colWidths = 100)
  })
  
  #### masukkan nilai sel baru ke dalam kolom satNew 
  #satSave<-eventReactive(input$saveModalSat,{
  observeEvent(input$saveModalSat,{
    #browser()
    selectedRow <- as.numeric(strsplit(input$select_button,"_")[[1]][2])
    fileName<- as.character(ListTableReact()[selectedRow,5]) #nama file
    dataDefine <-  readRDS(fileName)
    
    satAkun$table1<-as.data.frame(hot_to_r(input$editSat))
    inputSektor<-input$sektorSat 
    indexSektor <- as.numeric(which(sapply(Sector,function(x) any(x==c(inputSektor)))))
    inputTahun<-paste0("y",input$pilihtahunSat)
    inputBahanBakar <- input$pilihBahanBakar
    
    if (is.null(dataDefine$satSelisih)) {
      satSelisih = data$listConsumZero
      satSelisih[[inputTahun]][indexSektor,inputBahanBakar]<-satAkun$table1[-1]
    }else{
      satSelisih =  dataDefine$satSelisih
      satSelisih[[inputTahun]][indexSektor,inputBahanBakar]<-satAkun$table1[-1]
    }
    
    satSelisih <- lapply(satSelisih, function(x){
      tconsum <- rowSums(x[4:ncol(x)]) 
      x[3] <- tconsum
      x
      })
    
      dataDefine[5] <- list(dataDefine$fdSelisih)
      dataDefine$satSelisih <- satSelisih
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
  scenarioSimulation<-reactiveValues(
    selectedFile=NULL,
    initialYear=NULL,
    finalYear=NULL,
    iteration=NULL,
    ioPeriod=NULL,
    scenarioSeriesOfGDP = NULL,
    scenarioSeriesOfFinalDemand = NULL,
    scenarioSeriesOfOutput = NULL,
    scenarioSeriesOfIntermediateDemand =NULL,
    scenarioSeriesOfAddedValue = NULL,
    scenarioSeriesOfFinalDemandComponent = NULL,
    scenarioSeriesOfImpactLabour = NULL,
    scenarioSeriesOfImpactEnergy = NULL,
    scenarioSeriesOfImpactWaste = NULL,
    scenarioSeriesOfImpactAgriculture = NULL,
    scenarioSeriesOfImpactLand1= NULL,
    scenarioSeriesOfImpactLand2= NULL,
    scenarioSeriesOfImpactLand3= NULL,
    inputLRCRate=NULL,
    inputPercentageDiagTPM=NULL,
    # tabel u/ visualisasi
    scenarioResultGDP=NULL,
    scenarioResultIncome=NULL,
    scenarioResultLabour=NULL,
    scenarioResultEnergyConsumption=NULL,
    scenarioResultEnergyEmission=NULL,
    scenarioResultWasteDisposal=NULL,
    scenarioResultWasteEmission=NULL,
    scenarioResultFertilizerUsed=NULL,
    scenarioResultFertilizerEmission=NULL,
    scenarioResultLandReq=NULL,
    scenarioResultLandCover=NULL,
    scenarioResultLUTM=NULL,
    scenarioResultLandEmission=NULL,
    scenarioResultTotalEmission=NULL,
    scenarioAllResult=NULL,
    # plot u/ visualisasi
    plotResultTotalGDP=NULL,
    plotCummulativeGDP=NULL,
    plotEmissionIntensity=NULL,
    plotCummulativeEmissionIntensity=NULL,
    plotTotalEmission=NULL,
    plotCummulativeEmission=NULL,
    plotComparisonResultTotalGDP=NULL,
    plotComparisonCummulativeGDP=NULL,
    plotComparisonEmissionIntensity=NULL,
    plotComparisonCummulativeEmissionIntensity=NULL,
    plotComparisonTotalEmission=NULL,
    plotComparisonCummulativeEmission=NULL
  )
  
  observeEvent(input$run_button_trigger, {
    
    removeUI(selector=paste0('#',ns('scenarioResultVisualization')))
    
    # reset reactiveValues to NULL
    scenarioSimulation$selectedFile <- NULL
    scenarioSimulation$initialYear <- NULL
    scenarioSimulation$finalYear <- NULL
    scenarioSimulation$iteration <- NULL
    scenarioSimulation$ioPeriod <- NULL
    scenarioSimulation$scenarioSeriesOfGDP <- NULL
    scenarioSimulation$scenarioSeriesOfFinalDemand <- NULL
    scenarioSimulation$scenarioSeriesOfOutput <- NULL
    scenarioSimulation$scenarioSeriesOfIntermediateDemand <- NULL
    scenarioSimulation$scenarioSeriesOfAddedValue <- NULL
    scenarioSimulation$scenarioSeriesOfFinalDemandComponent <- NULL
    scenarioSimulation$scenarioSeriesOfImpactLabour <- NULL
    scenarioSimulation$scenarioSeriesOfImpactEnergy <- NULL
    scenarioSimulation$scenarioSeriesOfImpactWaste <- NULL
    scenarioSimulation$scenarioSeriesOfImpactAgriculture <- NULL
    scenarioSimulation$scenarioSeriesOfImpactLand1 <- NULL
    scenarioSimulation$scenarioSeriesOfImpactLand2 <- NULL
    scenarioSimulation$scenarioSeriesOfImpactLand3 <- NULL
    scenarioSimulation$inputLRCRate=NULL
    scenarioSimulation$inputPercentageDiagTPM=NULL
    
    selectedRow <- as.numeric(strsplit(input$run_button,"_")[[1]][2])
    fileName<- as.character(ListTableReact()[selectedRow,5]) #nama file dari ListTableReact ada di col=5
    selectedFile<-readRDS(fileName)
    
    initialYear<-selectedFile$tahunAwal
    finalYear <- selectedFile$tahunAkhir
    iteration <- finalYear - initialYear
    ioPeriod <- ioPeriod
    scenarioFD=selectedFile[["fdSelisih"]]
    if(is.null(scenarioFD)){
      scenarioFD=NULL
    } else {
      scenarioFD=scenarioFD[,2:ncol(scenarioFD)]
    }   
    inputLRCRate=NULL
    inputPercentageDiagTPM=NULL
    # inputLRCRate=selectedFile[["inputLRCRate"]]
    # inputPercentageDiagTPM=selectedFile[["inputPercentageDiagTPM"]]
    
    
    if (type == "energy") {
      scenarioInputLandCover= NULL
      additionalSatelliteEnergy=selectedFile[["satSelisih"]]
      additionalSatelliteWaste=NULL
      additionalSatelliteAgriculture=NULL
      additionalEmissionFactorEnergy=NULL
      additionalEmissionFactorWaste=NULL
      additionalEmissionFactorAgriculture=NULL
    } else if(type=="waste"){
      scenarioInputLandCover= NULL
      additionalSatelliteEnergy=NULL
      additionalSatelliteWaste=selectedFile[["satSelisih"]]
      additionalSatelliteAgriculture=NULL
      additionalEmissionFactorEnergy=NULL
      additionalEmissionFactorWaste=NULL
      additionalEmissionFactorAgriculture=NULL
    } else if (type=="agriculture"){
      scenarioInputLandCover= NULL
      additionalSatelliteEnergy=NULL
      additionalSatelliteWaste=NULL
      additionalSatelliteAgriculture=selectedFile[["satSelisih"]]
      additionalEmissionFactorEnergy=NULL
      additionalEmissionFactorWaste=NULL
      additionalEmissionFactorAgriculture=NULL
    } else if (type=="land"){
      scenarioInputLandCover= selectedFile[["satSelisih"]]
      additionalSatelliteEnergy=NULL
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
    
    #projection data
    projectionYear <- initialYear
    listYear <- paste0("y", ioPeriod)
    
    # economic & impact (energy, waste, & agriculture projection
    for(step in 1:(iteration+1)){
      
      # notes on the year
      timeStep <- paste0("y", projectionYear)
      
      if(is.null(scenarioFD)){
        projectionFinalDemand <- bauSeriesOfFinalDemand[,timeStep]
      } else {
        projectionFinalDemand <- bauSeriesOfFinalDemand[,timeStep] + scenarioFD[,timeStep]  # input final demand ditambahkan di sini
      }     
      
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
    
    # masukkan semua variabel ke reactiveValues
    scenarioSimulation$selectedFile <- selectedFile
    scenarioSimulation$initialYear <-initialYear
    scenarioSimulation$finalYear <- finalYear
    scenarioSimulation$iteration <- iteration
    scenarioSimulation$ioPeriod <- ioPeriod
    scenarioSimulation$scenarioSeriesOfGDP <- scenarioSeriesOfGDP
    scenarioSimulation$scenarioSeriesOfFinalDemand <- scenarioSeriesOfFinalDemand
    scenarioSimulation$scenarioSeriesOfOutput <- scenarioSeriesOfOutput
    scenarioSimulation$scenarioSeriesOfIntermediateDemand <-scenarioSeriesOfIntermediateDemand
    scenarioSimulation$scenarioSeriesOfAddedValue <-scenarioSeriesOfAddedValue
    scenarioSimulation$scenarioSeriesOfFinalDemandComponent<-scenarioSeriesOfFinalDemandComponent
    scenarioSimulation$scenarioSeriesOfImpactLabour<-scenarioSeriesOfImpactLabour
    scenarioSimulation$scenarioSeriesOfImpactEnergy<-scenarioSeriesOfImpactEnergy
    scenarioSimulation$scenarioSeriesOfImpactWaste<-scenarioSeriesOfImpactWaste
    scenarioSimulation$scenarioSeriesOfImpactAgriculture<-scenarioSeriesOfImpactAgriculture
    scenarioSimulation$scenarioSeriesOfImpactLand1<-scenarioSeriesOfImpactLand1
    scenarioSimulation$inputLRCRate <- selectedFile[["inputLRCRate"]]
    scenarioSimulation$inputPercentageDiagTPM <-selectedFile[["inputPercentageDiagTPM"]]
    
    checkLandCover()
    
    ##### tutup####
    # # jika masih ada nilai landCover negatif, force to enter advance mode
    # if(any(unlist(sapply(scenarioSeriesOfImpactLand1,'[[', "landCover"))<0)==TRUE){
    #   if (is.null(inputLRCRate)){
    #     modalEditLRCRate()
    #     print("land cover gagal dihitung dan tidak ada input LRC")
    #   } else {
    #     projectionYear <- initialYear
    #     listYear <- paste0("y", ioPeriod)
    #     for(step in 1:(iteration+1)){
    #       # notes on the year
    #       timeStep <- paste0("y", projectionYear)
    #       eval(parse(text= paste0("scenarioSeriesOfImpactLand1$", timeStep, " <- functionSatelliteLand1(type= 'projection',
    #                                                                                       matrix_output= as.matrix(scenarioSeriesOfOutput[,'",timeStep,"']),
    #                                                                                       advanceMode = TRUE,
    #                                                                                       currYear=projectionYear,
    #                                                                                       runNum = NULL,
    #                                                                                       LRCRate= inputLRCRate)")))
    #       listYear <- c(listYear, timeStep)
    #       projectionYear <- initialYear+step
    #     }
    #     if(any(unlist(sapply(scenarioSeriesOfImpactLand1,'[[', "landCover"))<0)==TRUE){
    #       modalEditLRCRate()
    #       print("ada input rate LRC namun land cover tetap tidah berhasil dihitung")
    #     } else {
    #       print("ada input rate LRC dan land cover berhasil dihitung")
    #       runLUTM()
    #     }
    #   } 
    #   
    # # jika landCover bernilai positif, return variables ke reactive value  
    #   } else{ 
    #   scenarioSimulation$scenarioSeriesOfImpactLand1<-scenarioSeriesOfImpactLand1
    #   runLUTM()
    # }
    ######
    
  })
  
  checkLandCover<-reactive({
    
    
    # masukkan reactive values
    selectedFile=scenarioSimulation$selectedFile
    initialYear=scenarioSimulation$initialYear
    finalYear=scenarioSimulation$finalYear
    iteration=scenarioSimulation$iteration
    ioPeriod=scenarioSimulation$ioPeriod
    scenarioSeriesOfGDP = scenarioSimulation$scenarioSeriesOfGDP
    scenarioSeriesOfFinalDemand = scenarioSimulation$scenarioSeriesOfFinalDemand
    scenarioSeriesOfOutput = scenarioSimulation$scenarioSeriesOfOutput
    scenarioSeriesOfImpactLand1 = scenarioSimulation$scenarioSeriesOfImpactLand1
    inputLRCRate = scenarioSimulation$inputLRCRate
    
    # jika land Cover bernilai negatif
    if(any(unlist(sapply(scenarioSeriesOfImpactLand1,'[[', "landCover"))<0)==TRUE){
      if (is.null(inputLRCRate)){
        print("land cover gagal dihitung dan tidak ada input LRC")
        showModal(modalEditLRCRate()) 
      } else {
        projectionYear <- initialYear
        listYear <- paste0("y", ioPeriod)
        for(step in 1:(iteration+1)){
          # notes on the year
          timeStep <- paste0("y", projectionYear)
          eval(parse(text= paste0("scenarioSeriesOfImpactLand1$", timeStep, " <- functionSatelliteLand1(type= 'projection',
                                                                                            matrix_output= as.matrix(scenarioSeriesOfOutput[,'",timeStep,"']),
                                                                                            advanceMode = TRUE,
                                                                                            currYear=projectionYear,
                                                                                            runNum = NULL,
                                                                                            LRCRate= inputLRCRate)")))
          listYear <- c(listYear, timeStep)
          projectionYear <- initialYear+step
        }
        if(any(unlist(sapply(scenarioSeriesOfImpactLand1,'[[', "landCover"))<0)==TRUE){
          showModal(modalEditLRCRate()) 
          print("ada input rate LRC namun land cover tetap tidak berhasil dihitung")
        } else {
          # jika landCover bernilai positif jalankan perhitungan LUTM
          print("ada input rate LRC dan land cover berhasil dihitung")
          scenarioSimulation$scenarioSeriesOfImpactLand1<-scenarioSeriesOfImpactLand1
          runLUTM()
        }
      }
      # jika landCover bernilai positif jalankan perhitungan LUTM
    } else{
      scenarioSimulation$scenarioSeriesOfImpactLand1<-scenarioSeriesOfImpactLand1
      runLUTM()
    }
    
  })
  
  runLUTM<-reactive({
    
    selectedFile=scenarioSimulation$selectedFile
    initialYear=scenarioSimulation$initialYear
    finalYear=scenarioSimulation$finalYear
    iteration=scenarioSimulation$iteration
    ioPeriod=scenarioSimulation$ioPeriod
    scenarioSeriesOfGDP = scenarioSimulation$scenarioSeriesOfGDP
    scenarioSeriesOfFinalDemand = scenarioSimulation$scenarioSeriesOfFinalDemand
    scenarioSeriesOfOutput = scenarioSimulation$scenarioSeriesOfOutput
    scenarioSeriesOfImpactLand1= scenarioSimulation$scenarioSeriesOfImpactLand1
    
    #list of output
    scenarioSeriesOfImpactLand2<-list()
    scenarioSeriesOfImpactLand3<-list()
    
    if (type == "land") {
      scenarioInputLandCover= selectedFile[["satSelisih"]]
    } else {
      scenarioInputLandCover= NULL
    }
    
    # historical LUTM & Emission
    eval(parse(text=paste0("scenarioSeriesOfImpactLand2$y",ioPeriod,"<- functionSatelliteLand2(type='historis',carbonStock=carbonStock_his, GDP= as.matrix(bauSeriesOfGDP$y",ioPeriod,"))")))
    
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
    
    scenarioSimulation$scenarioSeriesOfImpactLand2<-scenarioSeriesOfImpactLand2
    scenarioSimulation$scenarioSeriesOfImpactLand3<-scenarioSeriesOfImpactLand3
    
    checkLUTM()
    
  })
  
  checkLUTM<-reactive({
    
    selectedFile=scenarioSimulation$selectedFile
    initialYear=scenarioSimulation$initialYear
    finalYear=scenarioSimulation$finalYear
    iteration=scenarioSimulation$iteration
    ioPeriod=scenarioSimulation$ioPeriod
    scenarioSeriesOfGDP = scenarioSimulation$scenarioSeriesOfGDP
    scenarioSeriesOfFinalDemand = scenarioSimulation$scenarioSeriesOfFinalDemand
    scenarioSeriesOfOutput = scenarioSimulation$scenarioSeriesOfOutput
    scenarioSeriesOfImpactLand1= scenarioSimulation$scenarioSeriesOfImpactLand1
    scenarioSeriesOfImpactLand2<-scenarioSimulation$scenarioSeriesOfImpactLand2
    scenarioSeriesOfImpactLand3<-scenarioSimulation$scenarioSeriesOfImpactLand3
    inputPercentageDiagTPM = scenarioSimulation$inputPercentageDiagTPM
    
    if(any(is.na(scenarioSeriesOfImpactLand2[[timeStep]]))==TRUE){
      if (is.null(inputPercentageDiagTPM)){
        showModal(modalEditPercentageDiagTPM())
      } else{
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
                                    advanceMode = TRUE,
                                    percentage = inputPercentageDiagTPM,
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
        if(any(is.na(scenarioSeriesOfImpactLand2[[timeStep]]))==TRUE){
          showModal(modalEditPercentageDiagTPM())
        } else{
          scenarioSimulation$scenarioSeriesOfImpactLand2<-scenarioSeriesOfImpactLand2
          scenarioSimulation$scenarioSeriesOfImpactLand3<-scenarioSeriesOfImpactLand3
          runScenarioVisualization()
        }
      }
      
    } else{
      print("hitung LUTM berhasil")
      scenarioSimulation$scenarioSeriesOfImpactLand2<-scenarioSeriesOfImpactLand2
      scenarioSimulation$scenarioSeriesOfImpactLand3<-scenarioSeriesOfImpactLand3
      runScenarioVisualization()
    }
  })
  
  runScenarioVisualization<-reactive({
    selectedFile = scenarioSimulation$selectedFile
    initialYear = scenarioSimulation$initialYear
    finalYear = scenarioSimulation$finalYear
    iteration = scenarioSimulation$iteration
    ioPeriod = scenarioSimulation$ioPeriod
    scenarioSeriesOfGDP = scenarioSimulation$scenarioSeriesOfGDP
    scenarioSeriesOfFinalDemand = scenarioSimulation$scenarioSeriesOfFinalDemand
    scenarioSeriesOfOutput = scenarioSimulation$scenarioSeriesOfOutput
    scenarioSeriesOfIntermediateDemand = scenarioSimulation$scenarioSeriesOfIntermediateDemand
    scenarioSeriesOfAddedValue = scenarioSimulation$scenarioSeriesOfAddedValue
    scenarioSeriesOfFinalDemandComponent = scenarioSimulation$scenarioSeriesOfFinalDemandComponent
    scenarioSeriesOfImpactLabour = scenarioSimulation$scenarioSeriesOfImpactLabour
    scenarioSeriesOfImpactEnergy = scenarioSimulation$scenarioSeriesOfImpactEnergy
    scenarioSeriesOfImpactWaste = scenarioSimulation$scenarioSeriesOfImpactWaste
    scenarioSeriesOfImpactAgriculture = scenarioSimulation$scenarioSeriesOfImpactAgriculture
    scenarioSeriesOfImpactLand1= scenarioSimulation$scenarioSeriesOfImpactLand1
    scenarioSeriesOfImpactLand2=scenarioSimulation$ scenarioSeriesOfImpactLand2
    scenarioSeriesOfImpactLand3= scenarioSimulation$scenarioSeriesOfImpactLand3
    
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
    comparison<-rbind(scenarioAllResult,bauAllResult[bauAllResult$Year==(initialYear:finalYear),])
    for (i in as.character(colnames(comparison[,-c(1,8)]))){
      eval(parse(text=paste0('plotComparison',i,'<-ggplot(comparison, aes(x=Year, y=',i,', group=type))+
      geom_line(aes(color=type))+
      geom_point(aes(color=type))+
      labs(x="Tahun", y="',i,'")+
      ggtitle("Grafik ',i,'")+
      theme(plot.title = element_text(hjust = 0.5))'
      )))
    }
    
    scenarioSimulation$scenarioResultGDP <- scenarioResultGDP
    scenarioSimulation$scenarioResultIncome <- scenarioResultIncome
    scenarioSimulation$scenarioResultLabour <- scenarioResultLabour
    scenarioSimulation$scenarioResultEnergyConsumption <- scenarioResultEnergyConsumption
    scenarioSimulation$scenarioResultEnergyEmission <- scenarioResultEnergyEmission
    scenarioSimulation$scenarioResultWasteDisposal <- scenarioResultWasteDisposal
    scenarioSimulation$scenarioResultWasteEmission <- scenarioResultWasteEmission
    scenarioSimulation$scenarioResultFertilizerUsed <- scenarioResultFertilizerUsed
    scenarioSimulation$scenarioResultFertilizerEmission <- scenarioResultFertilizerEmission
    scenarioSimulation$scenarioResultLandReq <- scenarioResultLandReq
    scenarioSimulation$scenarioResultLandCover <- scenarioResultLandCover
    scenarioSimulation$scenarioResultLUTM <- scenarioResultLUTM
    scenarioSimulation$scenarioResultLandEmission <- scenarioResultLandEmission
    scenarioSimulation$scenarioResultTotalEmission <- scenarioResultTotalEmission
    scenarioSimulation$scenarioAllResult <- scenarioAllResult
    scenarioSimulation$plotResultTotalGDP <- plotResultTotalGDP
    scenarioSimulation$plotCummulativeGDP <- plotCummulativeGDP
    scenarioSimulation$plotEmissionIntensity <- plotEmissionIntensity
    scenarioSimulation$plotCummulativeEmissionIntensity <- plotCummulativeEmissionIntensity
    scenarioSimulation$plotTotalEmission <- plotTotalEmission
    scenarioSimulation$plotCummulativeEmission <- plotCummulativeEmission
    scenarioSimulation$plotComparisonResultTotalGDP <- plotComparisonResultTotalGDP
    scenarioSimulation$plotComparisonCummulativeGDP <- plotComparisonCummulativeGDP
    scenarioSimulation$plotComparisonEmissionIntensity <- plotComparisonEmissionIntensity
    scenarioSimulation$plotComparisonCummulativeEmissionIntensity <- plotComparisonCummulativeEmissionIntensity
    scenarioSimulation$plotComparisonTotalEmission <- plotComparisonTotalEmission
    scenarioSimulation$plotComparisonCummulativeEmission <- plotComparisonCummulativeEmission
    
    insertUI(selector=paste0("#",ns("scenarioResultPlaceholder")),
             where='afterEnd',
             ui= uiOutput(ns('scenarioResultVisualization'))
    )
  })
  
  output$scenarioResultVisualization <- renderUI({
    tagList(selectInput(ns('selectScenarioResultPlot'), 
                        label="Pilih grafik yang akan ditampilkan",
                        choices=c("Proyeksi PDRB Kumulatif", 
                                  "Proyeksi Emisi Kumulatif",
                                  "Proyeksi Intensitas Emisi Kumulatif",
                                  "Proyeksi PDRB",
                                  "Proyeksi Emisi",
                                  "Proyeksi Intensitas Emisi")),
            plotlyOutput(ns('scenarioResultPlot')),
            selectInput(ns('selectScenarioResultTable'), 
                        label="Pilih tabel yang akan ditampilkan",
                        choices=c("PDRB",
                                  "Pendapatan", 
                                  "Tenaga Kerja",
                                  "Konsumsi Energi",
                                  "Buangan Limbah",
                                  "Penggunaan Pupuk",
                                  "Kebutuhan Lahan",
                                  "Emisi")),
            selectInput(ns('selectScenarioResultTableYear'),
                        label="Pilih tahun tabel",
                        choices=c(initialYear:finalYear)),
            dataTableOutput(ns('scenarioResultTable')))
  })
  
  modalEditLRCRate<-reactive({
    # print("show modal executed")
    # showModal(
    modalDialog(title="Sunting LRC", 
                "Perhitungan emisi tidak berhasil dilakukan karena luas lahan yang tersedia tidak dapat memenuhi perubahan permintaan akhir. 
                            Silakan menyunting kembali permintaan akhir, atau sunting laju land requirement coefficient (LRC) di bawah ini:", 
                tags$br(),
                tags$br(),
                rHandsontableOutput(ns('editLRCRate')),
                tags$br(),
                actionButton(ns('saveEditLRCRate'), "simpan tabel"),
                footer= tagList(
                  actionButton(ns("buttonRunCheckLandCover"), "Jalankan Aksi"),
                  actionButton(ns("buttonClose"), "Batal"),
                  size="l"
                )
    )
    # )
  })
  
  modalEditPercentageDiagTPM <- reactive  ({
    # showModal(
    modalDialog(title="Sunting Constraint LUTM", 
                "LUTM tidak berhasil dihitung. Silakan sunting persentase proporsi yang akan digunakan untuk menghitung luas tutupan
                          lahan yang tidak berubah:", 
                sliderInput(ns("sliderPercentageDiagTPM"), "tentukan persentase", min=0, max=1, step = 0.01, round = FALSE),
                actionButton(ns('saveEditPercentageDiagTPM'), "simpan tabel"),
                footer= tagList(
                  actionButton(ns("buttonRunCheckLUTM"), "Jalankan Aksi"),
                  actionButton(ns("buttonClose"), "Batal")
                )
                # )
    )
  })
  
  output$editLRCRate <- renderRHandsontable({
    rhandsontable(
      if(is.null(scenarioSimulation$inputLRCRate)){
        showTable<-as.matrix(LRCRate_his)
        colnames(showTable)<- "laju LRC"
        rownames(showTable)<- c(Sector, "Sektor yang tidak menghasilkan output")
        showTable
      } else {
        showTable<-scenarioSimulation$inputLRCRate
        colnames(showTable)<- "laju LRC"
        rownames(showTable)<- c(Sector, "Sektor yang tidak menghasilkan output")
        showTable
      },height=500, width=500,rowHeaderWidth = 400
    )%>%hot_cols(format=5,colWidths = 100)
  })
  
  observeEvent(input$saveEditLRCRate,{
    scenarioSimulation$inputLRCRate<-as.matrix(hot_to_r(input$editLRCRate))
    
    # coding u/save input$editLRCRate ke RDS
    selectedRow <- as.numeric(strsplit(input$select_button,"_")[[1]][2])
    fileName<- as.character(ListTableReact()[selectedRow,5]) #nama file
    
    dataDefine <-  readRDS(fileName)
    dataDefine$inputLRCRate <- as.matrix(hot_to_r(input$editLRCRate))
    saveRDS(dataDefine, file = fileName)
    
  })
  
  observeEvent(input$saveEditPercentageDiagTPM,{
    scenarioSimulation$sliderPercentageDiagTPM<-input$sliderPercentageDiagTPM
    
    # coding u/ save input$sliderPercentageDiagTPM ke RDDS
    selectedRow <- as.numeric(strsplit(input$select_button,"_")[[1]][2])
    fileName<- as.character(ListTableReact()[selectedRow,5]) #nama file
    
    dataDefine <-  readRDS(fileName)
    dataDefine$inputPercentageDiagTPM <- input$sliderPercentageDiagTPM
    saveRDS(dataDefine, file = fileName)
  })
  
  observeEvent(input$buttonRunCheckLandCover,{
    removeModal()
    checkLandCover()
  })
  
  observeEvent(input$buttonRunCheckLUTM,{
    removeModal()
    checkLUTM()
  })
  
  observeEvent(input$buttonClose,{
    removeModal()
  })
  
  output$scenarioResultPlot <- renderPlotly({
    if(input$selectScenarioResultPlot=="Proyeksi PDRB Kumulatif"){
      scenarioSimulation$plotComparisonCummulativeGDP
    } else if (input$selectScenarioResultPlot=="Proyeksi Emisi Kumulatif"){
      scenarioSimulation$plotComparisonCummulativeEmission
    } else if (input$selectScenarioResultPlot=="Proyeksi Intensitas Emisi Kumulatif"){
      scenarioSimulation$plotComparisonCummulativeEmissionIntensity
    } else if (input$selectScenarioResultPlot=="Proyeksi PDRB"){
      scenarioSimulation$plotComparisonResultTotalGDP
    } else if (input$selectScenarioResultPlot=="Proyeksi Emisi"){
      scenarioSimulation$plotComparisonTotalEmission
    } else if (input$selectScenarioResultPlot=="Proyeksi Intensitas Emisi"){
      scenarioSimulation$plotComparisonEmissionIntensity
    }
  })
  
  output$scenarioResultTable <- renderDataTable({
    
    scenarioResultGDP <- scenarioSimulation$scenarioResultGDP
    scenarioResultIncome <- scenarioSimulation$scenarioResultIncome
    scenarioResultLabour <- scenarioSimulation$scenarioResultLabour
    scenarioResultEnergyConsumption <- scenarioSimulation$scenarioResultEnergyConsumption
    scenarioResultWasteDisposal <- scenarioSimulation$scenarioResultWasteDisposal
    scenarioResultFertilizerUsed <- scenarioSimulation$scenarioResultFertilizerUsed
    scenarioResultLandReq <- scenarioSimulation$scenarioResultLandReq
    scenarioResultTotalEmission  <- scenarioSimulation$scenarioResultTotalEmission 
    
    if(input$selectScenarioResultTable=="PDRB"){
      scenarioResultGDP[scenarioResultGDP$year==input$selectScenarioResultTableYear,]
      
    } else if (input$selectScenarioResultTable=="Pendapatan"){
      scenarioResultIncome[scenarioResultIncome$year==input$selectScenarioResultTableYear,]
      
    } else if (input$selectScenarioResultTable=="Tenaga Kerja"){
      scenarioResultLabour[scenarioResultLabour$year==input$selectScenarioResultTableYear,] 
      
    } else if (input$selectScenarioResultTable=="Konsumsi Energi"){
      scenarioResultEnergyConsumption[scenarioResultEnergyConsumption$year==input$selectScenarioResultTableYear,] 
      
    } else if (input$selectScenarioResultTable=="Buangan Limbah"){
      scenarioResultWasteDisposal[scenarioResultWasteDisposal$year==input$selectScenarioResultTableYear,] 
      
    } else if (input$selectScenarioResultTable=="Penggunaan Pupuk"){
      scenarioResultFertilizerUsed[scenarioResultFertilizerUsed$year==input$selectScenarioResultTableYear,]
      
    } else if (input$selectScenarioResultTable=="Kebutuhan Lahan"){
      scenarioResultLandReq[scenarioResultLandReq$year==input$selectScenarioResultTableYear,] 
      
    } else if (input$selectScenarioResultTable=="Emisi"){
      scenarioResultTotalEmission[scenarioResultTotalEmission$Year==input$selectScenarioResultTableYear,] 
    } 
  })
  
  ### hapus file ###
  observeEvent(input$delete_button_trigger, {
    selectedRow <- as.numeric(strsplit(input$delete_button,"_")[[1]][2])
    fileName<- as.character(ListTableReact()[selectedRow,5]) #nama file
    file.remove(fileName)
    shinyjs::js$refresh()
  })
  
}