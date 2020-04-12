
defineScenarioModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    actionButton(ns("modalDefineButton"),'Buat Skenario Baru'),
    dataTableOutput(ns("listOfScenario")), 
  )
}

defineScenarioModuleServer<-function(input, output, session){
  
  ns <- session$ns
  
  
  # reactive value tabel 
  tabel <- reactiveValues(
    tableDefineScenario=NULL  # list skenario yang sudah didefine, kalau sudah ada foldernya, masukkan tabel sesuai nama
  )
  
  # function untuk membuat button
  buttonFunction <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  ##### BEGIN: modal dialog define skenario #####
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
      actionButton(ns("closeModalDef"), "tutup"),
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
            tags$br(),
            actionButton(ns('saveModalDef'), 'simpan tabel')
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
  

  

  showDefineScenario<-eventReactive(input$saveModalDef,{
    
    namaSken <- input$intervensiDef
    tahunAwal <- input$tahunAwal
    tahunAkhir <- input$tahunAkhir
    deskrip <- input$deskripsi
    gabung<-data.frame(cbind(namaSken, tahunAwal, tahunAkhir, deskrip))
    colnames(gabung)<- c("nama.skenario",
                        "tahun.awal",
                        "tahun.akhir",
                        "deskripsi")
    
    if(is.null(tabel$tableDefineScenario)){
      tabel$tableDefineScenario<-data.frame(gabung)
    } else {
      tabel$tableDefineScenario<-data.frame(rbind(tabel$tableDefineScenario,gabung))
    }
    showTableDefineScenario<-data.frame(cbind(tabel$tableDefineScenario,
                                              ekonomi = buttonFunction(actionButton, 
                                                                       nrow(as.data.frame(tabel$tableDefineScenario)),
                                                                       'buttonEcon_',
                                                                       label = "Sunting",
                                                                       # onclick = paste0("Shiny.onInputChange('",ns('buttonEcon'), "',this.id")
                                                                       # onclick = 'shiny.onInput'
                                                                       onclick = sprintf('Shiny.onInputChange("%s",this.id)', session$ns("buttonEcon"))
                                                                       ),
                                              # "Shiny.onInputChange('", ns('myLinkName'), "', '%s'
                                              # sprintf('Shiny.onInputChange("%s",  this.id)', session$ns("select_button"))
                                              akun_satelit = buttonFunction(actionButton, 
                                                                           nrow(as.data.frame(tabel$tableDefineScenario)),
                                                                           'buttonSatellite_',
                                                                           label = "Sunting",
                                                                           onclick = paste0("Shiny.onInputChange('",ns('buttonSatellite'), "',this.id")), 
                                              jalankan_analisis = buttonFunction(actionButton, 
                                                                            nrow(as.data.frame(tabel$tableDefineScenario)),
                                                                            'buttonRun_',
                                                                            label = "Jalankan Analisis",
                                                                            onclick = paste0("Shiny.onInputChange('",ns('buttonRun'), "',this.id"))
                                                                       
                                              
    )
    )
    # save tabel ke RDS (mau simpan di folder mana? strukturnya gimana?)
    showTableDefineScenario

  })
  
  output$listOfScenario <- renderDataTable({
    showDefineScenario()
  }, escape = FALSE)
  
  # tutup modal dialog define skenario 
  observeEvent(input$closeModalDef,{
    removeModal()
  })
  
  
  #### END: modal dialog define skenario #####
  
  
  #### BEGIN: modal dialog ekonomi #####
  # 
  # observeEvent(input$econHit, {
  #   insertUI(selector='#FDPlaceholder',
  #            where='afterEnd',
  #            ui= uiOutput(ns('FDUIManual'))
  #   )
  # })  
  # 
  # output$FDUIManual<- renderUI({
  #   tagList(pickerInput(ns("pilihtahunFD"),
  #                       label="pilih tahun", selected = input$tahunAwal,
  #                       choices=c(input$tahunAwal:input$tahunAkhir),options = list(`actions-box` = TRUE),multiple = T),
  #           tags$br(),
  #           actionButton(ns('showYearEco'), 'tampilkan tabel'),
  #           tags$br(),
  #           tags$br(),
  #           tags$div(id = 'SuntingPlaceHolder')
  #   )
  # })
  # 
  # observeEvent(input$showYearEco, {
  #   insertUI(selector='#SuntingPlaceHolder',
  #            where='afterEnd',
  #            ui= uiOutput(ns('SuntingUITable'))
  #   )
  # }) 
  # 
  # output$SuntingUITable<- renderUI({
  #   tagList(
  #     tags$b('Sunting secara manual'),
  #     tags$br(),
  #     rHandsontableOutput(ns('editFD')),
  #     tags$br(),
  #     actionButton(ns('saveModalFD'),' simpan tabel '),
  #     #actionButton('downloadFD','download tabel')
  #     tags$div(id = 'objDownloadFD')
  #   )
  # })
  # 
  # finalDemand <- reactiveValues(
  #   table1 = NULL
  # )
  # 
  # fdBauReact <- reactiveValues(
  #   isi = fdBau
  # )
  # 
  # FDdata <- reactive({
  #   finalDemand$table1 <- fdBauReact$isi
  #   finalDemand$table1
  # })
  # 
  # #observeEvent(input$econHit,
  # valFD<- reactive({
  #   #browser()
  #   finalDemand$table1 <- FDdata()
  #   finalDemand$table1 <- filter(finalDemand$table1, finalDemand$table1$sector %in% c(input$sektorEcon))
  #   rownames(finalDemand$table1) <- c(input$sektorEcon)
  #   finalDemand$table1
  # })
  # 
  # valFD2 <- eventReactive(c(input$showYearEco),{
  #   #observeEvent(input$pilihtahunFD,{
  #   #browser()
  #   finalDemand$table1 <- valFD()
  #   finalDemand$table1 <- finalDemand$table1[,c("sector",paste0("y",input$pilihtahunFD))]
  #   finalDemand$table1
  # })
  # 
  # output$editFD <- renderRHandsontable({
  #   rhandsontable(valFD2(),
  #                 #finalDemand$table1,
  #                 rowHeaderWidth = 160,
  #   )%>%hot_cols(format=3)
  #   
  # })
  # 
  # #### masukkan nilai sel baru ke dalam kolom fdBauNew 
  # FDSave<-eventReactive(input$saveModalFD,{
  #   finalDemand$table1 <- as.data.frame(hot_to_r(input$editFD))
  #   inputSektor<-input$sektorEcon #"tanaman pangan"
  #   inputTahun<-paste0("y",input$pilihtahunFD)
  #   indexSektor <- as.numeric(which(sapply(sector,function(x) any(x==c(inputSektor)))))
  #   fdBauReact$isi[c(indexSektor), c(inputTahun)] <- finalDemand$table1[,-1]
  #   #print(head(fdBauReact$isi))
  #   fdNew_list<-list(fdBauNew = fdBauReact$isi,
  #                    fdNew=finalDemand$table1,
  #                    inputTahun=inputTahun
  #   )
  #   fdNew_list
  # })
  # 
  # ##### simpan tabel FD baru ke dalam folder ####
  # observeEvent(input$saveModalFD,{
  #   waktuEcon<-Sys.time()
  #   simpanEcon<-gsub(" ","_",waktuEcon,fixed = TRUE)
  #   simpanEcon<-gsub(":","-",simpanEcon,fixed = TRUE)
  #   namaSken <- gsub(" ","",input$intervensiDef, fixed = TRUE)
  #   intEconomy <- gsub(" ","",input$intervensiEcon, fixed = TRUE)
  #   namafileDefine<-paste0(username,"_",namaSken,"_",selectedProv,"_",intEconomy,"_",simpanEcon)
  #   saveRDS(FDSave(), file = paste0('_DB/skenarioData/',selectedSektor,'/',selectedProv,'/',namafileDefine))
  #   insertUI(selector='#objDownloadFD',
  #            where='afterEnd',
  #            ui= uiOutput(ns('downButtonFD'))
  #   )
  # })
  # 
  # output$downButtonFD<- renderUI({
  #   tagList(tags$br(),
  #           actionButton(ns('downloadFD'),'download tabel')
  #   )
  # })
  # 
  # 
  # observeEvent(input$downloadFD,{
  #   #browser()
  #   waktuEcon<-Sys.time()
  #   simpanEcon<-gsub(" ","_",waktuEcon,fixed = TRUE)
  #   simpanEcon<-gsub(":","-",simpanEcon,fixed = TRUE)
  #   namaSheet <- names(FDSave())
  #   namaSken <- gsub(" ","",input$intervensiDef, fixed = TRUE)
  #   intEconomy <- gsub(" ","",input$intervensiEcon, fixed = TRUE)
  #   namafile<-paste0(username,"_",namaSken,"_",selectedProv,"_",intEconomy,"_",simpanEcon)
  #   dirFile <- paste0("_DB/download file/",namafile,".xlsx")
  #   write.xlsx(FDSave(), 
  #              file = dirFile, 
  #              sheetName = namaSheet)
  # })
  # 
  # 

  observeEvent(input$buttonEcon, {
    print("tes")
  })
  
  observeEvent(input$buttonSatellite, {
    print("tes")
  })
  
  observeEvent(input$buttonRun, {
    print("tes")
  })

}

# 
# editSatelliteServer<-function(input, output, session){
# 
#   ns <- session$ns
# 
#   observeEvent(input$buttonEcon,{
#     
#     print("outside")
#     
#     
#   })
# }


