
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
      actionButton(ns("closeModalDef"), "simpan skenario"),
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
            # tags$br(),
            # actionButton('saveModalDef', 'simpan tabel')
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
    
    namaSken <- as.character(newTableDef[1,])
    tahunAwal <- as.numeric(newTableDef[2,])
    tahunAkhir <- as.numeric(newTableDef[3,])
    deskrip <- as.character(newTableDef[4,])
    combineDef <- list(namaSken=namaSken,tahunAwal=tahunAwal,tahunAkhir=tahunAkhir, deskrip=deskrip)
    
    combineDef
  })
  
  ##### simpan tabel define ke dalam folder ####
  ### tutup modal dialog define ###
  observeEvent(input$closeModalDef,{
    removeModal()
    
    waktuDefine<-Sys.time()
    simpanDefine<-gsub(" ","_",waktuDefine,fixed = TRUE)
    simpanDefine<-gsub(":","-",simpanDefine,fixed = TRUE)
    namaSken <- gsub(" ","",input$intervensiDef, fixed = TRUE) 
    namafileDefine<-paste0(username,"_",namaSken,"_",selectedProv,"_",simpanDefine)
    saveRDS(listValDef(), file = paste0(data$alamatFile,"/",namafileDefine))
    shinyjs::js$refresh()
  })
  
  observeEvent(input$cancelModalDef,{
    removeModal()
  })
  #### coba 
  
  daftarFile<-reactiveValues(
    ListFile = unique(list.files(data$alamatFile))
  )
  
  ListButton_fun <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id,i), ...))
    }
    inputs
  }
  
  
  ### buat tabel daftar nama file reaktif ###
  
  ListTableReact <- reactive({
    data.frame(
      Nama_File = c(daftarFile$ListFile),
      Edit = ListButton_fun(actionButton, length(daftarFile$ListFile),
                                     'button_',
                                     label = "Edit Konstruksi Ekonomi dan Satelit Akun"
                                     ,
                                     onclick = paste0('Shiny.onInputChange(\"' , ns("select_button"), '\", this.id)')
                                      ),
      Run = ListButton_fun(actionButton, length(daftarFile$ListFile),
                              'tombol_',
                              label = "Tampilkan Plot",
                              onclick = paste0('Shiny.onInputChange( \"run_button\" , this.id)'))
    )
  })

  ###tampilkan tabel list ###
  
  output$ListTable <- renderDataTable({
    ListTableReact()
  }, escape = FALSE)
  
  
  
  observeEvent(input$select_button,{
    #browser()
    selectedRow <- as.numeric(strsplit(input$select_button,"_")[[1]][2])
    fileName<- ListTableReact()[selectedRow,1]
    selectedFile<-readRDS(paste0(data$alamatFile,"/",fileName))
    listData<-list(fileName=fileName,
                 selectedFile=selectedFile)
    print("cek button")
    
    
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
                tags$div(id = 'FDPlaceholder'),
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
              tags$div(id = 'satPlaceholder'),
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
  #                          BUTTON KONSTRUKSI EKONOMI                           #
  #                                                                              #
  ################################################################################
  #observeEvent(input$modalEconButton,{
    # showModal(
    #   modalDialog( 
    #     footer=tagList(
    #       actionButton(ns("closeModalFD"), "Tutup")
    #     ),
    #     tabsetPanel(
    #       tabPanel(
    #         h2("Ekonomi"),
    #         sidebarLayout(
    #           sidebarPanel(
    #             fluidRow(
    #               selectInput(ns("intervensiEcon"),
    #                           label="pilih intervensi",
    #                           choices=c("Final Demand","AV","Input-Output")),
    #               pickerInput(ns("sektorEcon"),
    #                           label="pilih sektor", selected = Sector[1],
    #                           choices=Sector,options = list(`actions-box` = TRUE),multiple = T)),
    #             tags$br(),
    #             actionButton(ns("econHit"),"tentukan tahun intervensi"),
    #             width=5
    #           ),
    #           mainPanel(
    #             tags$div(id = 'FDPlaceholder'),
    #             width=7)
    #         ),
    #         title="Sunting Intervensi Ekonomi"
    #       ),
    #       
    #       
    #       ################################################################################
    #       #                                                                              #
    #       #                        BUTTON KONSTRUKSI SATELIT AKUN                        #
    #       #                                                                              #
    #       ################################################################################
    #       tabPanel(
    #         h2("Satelit akun"),
    #         sidebarLayout(sidebarPanel(
    #           fluidRow(
    #             selectInput(ns("intervensiSat"),
    #                         label="pilih intervensi",
    #                         choices=c("konsumsi energi","faktor emisi")),
    #             pickerInput(ns("sektorSat"),
    #                         label="pilih sektor",selected = Sector[1],
    #                         choices=Sector,options = list(`actions-box` = TRUE),multiple = T)),
    #           tags$br(),
    #           actionButton(ns("satHit"),"tentukan tahun intervensi"),
    #           width=5
    #         ),
    #         mainPanel(
    #           tags$div(id = 'satPlaceholder'),
    #           width=7)
    #         ),
    #         title="Sunting Intervensi Satelit Akun"
    #       ))
    #     ,
    #     size="l",
    #     easyClose = FALSE)
    # )
  #})
  
  
  
  ################################################################################
  #                                                                              #
  #                     ALUR BUTTON KONSTRUKSI EKONOMI                           #
  #                                                                              #
  ################################################################################
  observeEvent(input$econHit, {
    #browser()
    insertUI(selector="FDPlaceholder",
             where='afterEnd',
             ui= uiOutput(ns('FDUIManual'))
    )
  })  
  
  output$FDUIManual<- renderUI({
    tagList(pickerInput(ns("pilihtahunFD"),
                        label="pilih tahun", selected = listValDef()$tahunAwal,
                        choices=c(listValDef()$tahunAwal:listValDef()$tahunAkhir),options = list(`actions-box` = TRUE),multiple = T),
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
    isi = fdBau
  )
  
  FDdata <- reactive({
    finalDemand$table1 <- fdBauReact$isi
    finalDemand$table1
  })
  
  #observeEvent(input$econHit,
  valFD<- reactive({
    #browser()
    finalDemand$table1 <- FDdata()
    finalDemand$table1 <- filter(finalDemand$table1, finalDemand$table1$Sektor %in% c(input$sektorEcon))
    rownames(finalDemand$table1) <- c(input$sektorEcon)
    finalDemand$table1
  })
  
  valFD2 <- eventReactive(c(input$showYearEco),{
    finalDemand$table1 <- valFD()
    finalDemand$table1 <- finalDemand$table1[,c("Sektor",paste0("y",input$pilihtahunFD))]
    finalDemand$table1
  })
  
  output$editFD <- renderRHandsontable({
    rhandsontable(valFD2(),
                  #finalDemand$table1,
                  rowHeaderWidth = 160,
    )%>%hot_cols(format=3)
    
  })
  
  #### masukkan nilai sel baru ke dalam kolom fdBauNew 
  FDSave<-eventReactive(input$saveModalFD,{
    finalDemand$table1 <- as.data.frame(hot_to_r(input$editFD))
    inputSektor<-input$sektorEcon #"tanaman pangan"
    inputTahun<-paste0("y",input$pilihtahunFD)
    indexSektor <- as.numeric(which(sapply(Sector,function(x) any(x==c(inputSektor)))))
    fdBauReact$isi[c(indexSektor), c(inputTahun)] <- finalDemand$table1[,-1]
    fdNew_list<-list(fdBauNew = fdBauReact$isi,
                     fdNew=finalDemand$table1,
                     inputTahun=inputTahun
    )
    fdNew_list
  })
  
  ##### simpan tabel FD baru ke dalam folder ####
  observeEvent(input$saveModalFD,{
    waktuEcon<-Sys.time()
    simpanEcon<-gsub(" ","_",waktuEcon,fixed = TRUE)
    simpanEcon<-gsub(":","-",simpanEcon,fixed = TRUE)
    namaSken <- gsub(" ","",input$intervensiDef, fixed = TRUE)
    intEconomy <- gsub(" ","",input$intervensiEcon, fixed = TRUE)
    namafileDefine<-paste0(username,"_",namaSken,"_",selectedProv,"_",intEconomy,"_",simpanEcon)
    saveRDS(FDSave(), file = paste0('_DB/skenarioData/',selectedProv,'/',selectedSektor,'/',namafileDefine))
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
    waktuEcon<-Sys.time()
    simpanEcon<-gsub(" ","_",waktuEcon,fixed = TRUE)
    simpanEcon<-gsub(":","-",simpanEcon,fixed = TRUE)
    namaSheet <- names(FDSave())
    namaSken <- gsub(" ","",input$intervensiDef, fixed = TRUE)
    intEconomy <- gsub(" ","",input$intervensiEcon, fixed = TRUE)
    namafile<-paste0(username,"_",namaSken,"_",selectedProv,"_",intEconomy,"_",simpanEcon)
    dirFile <- paste0("_DB/download file/",namafile,".xlsx")
    write.xlsx(FDSave(), 
               file = dirFile, 
               sheetName = namaSheet)
  })
  
  
  ################################################################################
  #                                                                              #
  #                   ALUR BUTTON KONSTRUKSI SATELIT AKUN                        #
  #                                                                              #
  ################################################################################
  
  ### efek FD yang diubah ke tabel proyeksi konsumsi energi
  ## bagian Output
  outputTable <- reactive({
    fdTable <- FDSave()$fdBauNew
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
    colProyPdrbScen <- colProyPdrbScen[1:length(input$tahunAwal:input$tahunAkhir), ]
    colProyPdrbScen$scenario <- c("SKENARIO")
    
    #BAU
    colProyPdrbDF <- colProyPdrb
    colProyPdrbDF <- colProyPdrbDF[1:length(input$tahunAwal:input$tahunAkhir), ]
    colProyPdrbDF$year <- paste0("y",colProyPdrbDF$year)
    colProyPdrbDF$scenario <- c("BAU")
    
    #gabung BAU dan skenario
    colProyPdrbScen <- rbind(colProyPdrbScen,colProyPdrbDF)
    
    colProyPdrbScen
  })
  
  
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
    insertUI(selector='#satPlaceholder',
             where='afterEnd',
             ui= uiOutput(ns('satMUIManual'))
    )
  })
  
  output$satMUIManual<- renderUI({
    tagList(selectInput(ns("pilihtahunSat"),
                        label="pilih tahun", selected = input$tahunAwal,
                        choices=c(input$tahunAwal:input$tahunAkhir)),
            pickerInput(ns("pilihBahanBakar"),
                        label="pilih faktor emisi",selected = data$faktorEmisi[1],
                        choices=data$faktorEmisi,options = list(`actions-box` = TRUE),multiple = T),
            tags$br(),
            tags$br(),
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
    isi = NULL
  )
  
  observeEvent(input$saveModalFD,{
    satBauReact$isi <- proyTabelKonsEnergiTable()
  })
  
  SatData <- reactive({
    satAkun$table1 <- satBauReact$isi
    satAkun$table1
  })
  
  # observeEvent(input$saveModalSat,
  valSat<- reactive({
    indexAwal <- paste0("y",input$pilihtahunSat)
    satAkun$table1 <- SatData()[indexAwal]
    satAkun$table1 <- data.frame(satAkun$table1)
    satAkun$table1 <- cbind(Sector,satAkun$table1)
    satAkun$table1 <- filter(satAkun$table1, satAkun$table1$Sector %in% c(input$sektorSat))
    rownames(satAkun$table1) <- c(input$sektorSat)
    satAkun$table1
  })
  
  #valSat2 <- reactive(
  observeEvent(c(input$pilihtahunSat,input$pilihBahanBakar),{
    satAkun$table1 <- valSat()
    satAkun$table1 <- satAkun$table1[,c("Sector",paste0("y",input$pilihtahunSat,".",input$pilihBahanBakar))]
    satAkun$table1
  })
  
  output$editSat <- renderRHandsontable({
    rhandsontable(satAkun$table1,
                  rowHeaderWidth = 160,
    )%>%hot_cols(format=3)
  })
  
  #### masukkan nilai sel baru ke dalam kolom satBauNew 
  #satSave<-eventReactive(input$saveModalSat,
  observeEvent(input$saveModalSat,{
    #browser()
    satAkun$table1<-as.data.frame(hot_to_r(input$editSat))
    inputSektor<-input$sektorSat 
    indexSektor <- as.numeric(which(sapply(Sector,function(x) any(x==c(inputSektor)))))
    inputTahun<-paste0("y",input$pilihtahunSat)
    inputBahanBakar <- input$pilihBahanBakar
    
    satBauReact$isi[[inputTahun]][indexSektor,inputBahanBakar]<-satAkun$table1[,-1] 
    satNew_list<-list(satBauNew = satBauReact$isi,
                      satNew=satAkun$table1,
                      inputTahun=inputTahun
    )
    satNew_list
    
    ##### simpan tabel Sat baru ke dalam folder ####
    waktuSat<-Sys.time()
    simpanSat<-gsub(" ","_",waktuSat,fixed = TRUE)
    simpanSat<-gsub(":","-",simpanSat,fixed = TRUE)
    namaSken <- gsub(" ","",input$intervensiDef, fixed = TRUE)
    intEconomy <- gsub(" ","",input$intervensiEcon, fixed = TRUE)
    intSat <- gsub(" ","",input$intervensiSat, fixed = TRUE)
    namafileDefine<-paste0(username,"_",namaSken,"_",selectedProv,"_",intEconomy,"_",intSat,"_",simpanSat)
    saveRDS(satNew_list, file = paste0('_DB/skenarioData/',selectedProv,'/',selectedSektor,'/',namafileDefine))
    
    inputSektorTampil<-capture.output(cat(input$sektorSat , sep=", ")) #"tanaman pangan"
    inputBahanBakarTampil <- capture.output(cat(input$pilihBahanBakar , sep=", "))
    textTampil <- paste0("Satelit akun yang diedit adalah ","sektor: ",inputSektorTampil," - ","tahun: ",
                         inputTahun," - ","bahan bakar: ",inputBahanBakarTampil)
    insertUI(selector='#teksSatSave',
             where = 'afterEnd',
             ui = tags$div (textTampil))
  })
  
  
  # terbentuk 15 tabel proyeksi emisi
  proyEmisiTabel <- reactive({
    proyEmisiSken <- list()
    for (i in 1:(iteration+1)) {
      proyEmisiSken[[i]]<-as.matrix(proyTabelKonsEnergiTable()[[i]]) %*% data$matEfBau
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
    cumProyEmisiSken <- cumProyEmisiSken[1:length(input$tahunAwal:input$tahunAkhir), ]
    cumProyEmisiSken$scenario <- c("SKENARIO")

    #BAU
    totalEmisi <- data$tabelConsum %>% 
      filter(between(year, initialYear, finalYear)) %>% 
      group_by(year) %>% 
      summarise(totalEmisi = sum(Temission))
    
    cumsumTotalEmisi <- cumsum(totalEmisi[,2])
    colnames(cumsumTotalEmisi) <- "cumEmisi"
    
    cumProyEmisi <- cbind(totalEmisi[,1],cumsumTotalEmisi)
    
    
    cumProyEmisiDF <- cumProyEmisi
    cumProyEmisiDF <- cumProyEmisiDF[1:length(input$tahunAwal:input$tahunAkhir), ]
    cumProyEmisiDF$year <- paste0("y",cumProyEmisiDF$year)
    cumProyEmisiDF$scenario <- c("BAU")

    #gabung BAU dan skenario
    cumProyEmisiSken <- rbind(cumProyEmisiSken,cumProyEmisiDF)

    cumProyEmisiSken
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
  plotPDRB <- eventReactive(input$modalRUNButton,{
    ggplot(proyPdrbTable(), aes(x=year, y=totalPDRB, group=scenario))+
      geom_line(aes(color=scenario))+
      geom_point(aes(color=scenario))+
      labs(x="Tahun", y="PDRB")+
      ggtitle("Grafik Proyeksi PDRB")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$hasilRun <- renderPlotly({
    plotPDRB()
    
  })
  
  plotEmisi <- eventReactive(input$modalRUNButton,{
    ggplot(proyEmisiTabel(), aes(x=year, y=cumEmisi, group=scenario))+
      geom_line(aes(color=scenario))+
      geom_point(aes(color=scenario))+
      labs(x="Tahun", y="Emisi Kumulatif")+
      ggtitle("Grafik Emisi Kumulatif")+
      theme(plot.title = element_text(hjust = 0.5))
  })

  output$plot2 <- renderPlotly({
    plotEmisi()
  })
  
}