library(shiny)
library(gridExtra)
library(shinyBS)
library(rhandsontable)
library(shinyBS)
library(DT)
library(stringr)

##variable for LDM Prop###
LDMProp_his<-readRDS("data/JaBar/LDMProp")
LDMProp_his <- LDMProp_his[,-1]
colnamesLDM<-colnames(LDMProp_his)
sector<-readRDS("data/JaBar/sector")
sector<-sector[,1]
sector<-as.character(sector)
row.names(LDMProp_his)<-sector
LDMProp_his<-as.matrix(LDMProp_his)
selectedProv<-"Prov"
username<-"dw"

###variable for aksi lahan###
LU_tahun<-readRDS("data/JaBar/LU_tahun")
user.intName <-list.files("user", pattern="\\user.int")
user.scen<-readRDS("user/user.scen")


ui <- fluidPage(
  tabsetPanel(
    #tab panel for LDM Prop
    tabPanel("Pilih LDM Prop",
             uiOutput("LDMFileOptions"),
             dataTableOutput("LDMListTable"),
             dataTableOutput("LDMTampil"),
             uiOutput('LDMTableTampilUI'),
             uiOutput('modalLDMUI')),
    #tab panel for aksi lahan
    tabPanel("Sunting Tabel Satelit Lahan",
             dataTableOutput("aksiLahanTable"))
)
)

server <- function(input,output){
  
######For LDM Prop--------
  LDMProp_new<-reactiveValues(
    tablo = NULL,
    coba= NULL
  )
  
  tabel<-reactiveValues(
    manualSave=NULL
  )
  
  ldmRV<-reactiveValues(
    LDMListFile = unique(list.files(paste0("LDMData/",selectedProv))),
    LDMTotFile= unique(length(list.files("LDMData/", selectedProv)))
  )
  
  
  ##### Tabel menampilkan LDM yang sudah dibuat######
  
  ###function untuk generate button_id###
  
  ListLDMButton_fun <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  
  ### buat tabel daftar nama file LDM reaktif ###
  
  LDMListTableReact <- reactive({
    data.frame(
      Nama_File = c("LDM historis", ldmRV$LDMListFile),
      Lihat_File = ListLDMButton_fun(actionButton, length(ldmRV$LDMListFile)+1,
                                  'button_',
                                  label = "Tampilkan",
                                  onclick = paste0('Shiny.onInputChange( \"select_button\" , this.id)')
      )
    )
  })
  
  ### buat list options ###
  output$LDMFileOptions<- renderUI({
    LDMListTableReact <- LDMListTableReact()
    namaFile<-as.character(LDMListTableReact$Nama_File)
    selectInput("LDMPropUse", "Pilih tabel LDM proporsi yang akan digunakan dalam perhitungan:", choices=namaFile)
  })
  
  ###tampilkan tabel list file LDM###
  
  output$LDMListTable <- renderDataTable({
    LDMListTableReact()
  }, escape = FALSE)
  
  
  
  ###pilh nama file dan file yang akan ditampilkan###
  
  LDMTableTampil<-eventReactive(input$select_button,{
    
    selectedRow <- as.numeric(strsplit(input$select_button,"_")[[1]][2])
    
    if(selectedRow==1){
      fileName = "LDM historis"
      selectedFile = LDMProp_his
      list<-list(fileName=fileName,
                 selectedFile=selectedFile)
      print("kondisi1")
    }
    else if(selectedRow != 1){
    fileName<- LDMListTableReact()[selectedRow,1]
    selectedFile<-readRDS(paste0("LDMData/Prov/",fileName))
    list<-list(fileName=fileName, 
               selectedFile=selectedFile)
    print("kondisi2")
    }
    list
  })
  
  
  ### tampilkan UI tabel LDM yang dipilih ###
  
  output$LDMTableTampilUI<-renderUI({
    
    LDMTableTampil<-LDMTableTampil()
    selectedFile<-LDMTableTampil$selectedFile
    fileName<-LDMTableTampil$fileName
    
    if(identical(fileName, "LDM historis")){
      tagList(tags$br(),
              tags$br(),
              tags$h3(paste0(fileName)),
              tags$br(),
              actionButton('modalLDMbutton', 'Sunting Tabel'),
              tags$br(),
              tags$br(),
              datatable(selectedFile))
    }
    else {
      tagList(tags$br(),
              tags$br(),
              tags$h3(paste0(fileName)),
              tags$br(),
              actionButton('modalLDMbutton', 'Sunting Tabel'),
              actionButton('deleteLDMTable','Hapus Tabel'),
              tags$br(),
              tags$br(),
              datatable(selectedFile))
    }
    
  })
  
  
  #### tampilkan modal dialog modalLDM ketika sunting tabel dipilih ####
  
  observeEvent(input$modalLDMbutton,{
    showModal(modalDialog(sidebarLayout(sidebarPanel(
                            fluidRow(
                              selectInput("tupla",
                                          label="jenis tutupan lahan",
                                          choices=colnamesLDM),
                              selectInput("sektor",
                                          label="sektor",
                                          choices=sector)),
                            rHandsontableOutput('editLDM'),
                            tags$br(),
                            uiOutput('LDMButton'),
                            textOutput("sunting"),
                            width=5
                          ),
                          mainPanel(
                            tags$div(id = 'LDMPlaceholder'),
                            width=7)
                          ),
                          title="sunting LDM",
                          footer= tagList(
                            actionButton("saveLDMTable", "simpan tabel"),
                            actionButton("closeModalLDM", "tutup")
                          ),
                          size="l",
                          easyClose = FALSE
    ))
  })
  
  ### tutup modal dialog ###
  observeEvent(input$closeModalLDM,{
    removeModal()
  })
  
  
  ### hapus file ###
  observeEvent(input$deleteLDMTable, {
    LDMTableTampil<-LDMTableTampil()
    fileName<-LDMTableTampil$fileName
    file.remove(paste0("LDMData/", selectedProv, "/", fileName))
    
  })
  ###### modal dialog######
  
  # hapus tampilan kolom jika memasukkan value baru
  
  observeEvent(c(input$modalLDMbutton,input$tupla,input$sektor), {
    
    LDMTableTampil<-LDMTableTampil()
    selectedFile<-LDMTableTampil$selectedFile
    fileName<-LDMTableTampil$fileName
    
    removeUI(selector='#LDMUIManual')
    removeUI(selector='#LDMUINormal')
    
    
    if(is.null(LDMProp_new$tablo) & is.null(LDMProp_new$coba)){
      LDMProp_new$tablo <- selectedFile
    }
    else if(!is.null(LDMProp_new$tablo) & is.null(LDMProp_new$coba)){
      LDMProp_new$tablo<- selectedFile
    }
    else if (!is.null(LDMProp_new$coba)){
      LDMProp_new$tablo<-LDMProp_new$coba
    }
    
  })
  
  
  
  
  teksLDM<-reactiveValues(unedited=" ",
                          edited="Total nilai kolom tutupan lahan tidak sama dengan 1")
  
  valLDM<- reactive({
    table_show <- as.matrix(subset(LDMProp_new$tablo, rownames(LDMProp_new$tablo) %in% input$sektor, 
                                   colnames(LDMProp_new$tablo) %in% input$tupla))
    table_show
  })
  
  output$editLDM <- renderRHandsontable({
    rhandsontable(valLDM(),
                  rowHeaderWidth = 160,
    )%>%hot_cols(format=3)
  })
  
  output$LDMButton<-renderUI({
    if (is.null(valLDM())){
      return(NULL)}
    else if (!is.null(valLDM)){
      tagList(actionButton('LDMButtonEdit','simpan hasil sunting'),
      tags$div(id='NormOrMan'))
    }
  })
  
  
  #### masukkan nilai sel baru ke dalam kolom tupla 
  
  LDM_fun<-eventReactive(input$LDMButtonEdit,{
    tablo = LDMProp_new$tablo
    LDM_sel<-as.data.frame(hot_to_r(input$editLDM))
    LDM_sel<-as.numeric(LDM_sel[1,1])
    # inputSektor<-as.character(input$sektor)
    # inputTupla<-as.character(input$tupla)
    inputSektor<-input$sektor #"tanaman pangan
    inputTupla<-input$tupla
    tablo[inputSektor, inputTupla]<-LDM_sel #tablo["tanaman pangan","Hutan lahan"]
    tablo<-as.data.frame(tablo)
    print(LDM_sel)
    #print(LDM_sel)
    totLDMProp<-as.data.frame(colSums(tablo))
    totLDMProp_sel<-totLDMProp[inputTupla,]
    LDM_list<-list(LDM_sel=LDM_sel,
                   totLDMProp_sel=totLDMProp_sel,
                   totLDMProp=totLDMProp,
                   tablo = tablo,
                   inputTupla=inputTupla
    )
    LDM_list
  })
  
  
  
observe({ 
    tes <- LDM_fun()
    inputTupla<-tes$inputTupla
    
        if(tes$totLDMProp_sel==1){
          insertUI(selector='#NormOrMan',
                   where= 'afterEnd',
                   ui= tags$div(id='pesanHitungLDMNo', "tidak ada perubahan")
                   )
        } else {
          insertUI(selector='#NormOrMan',
                   where = 'afterEnd',
                   ui= tags$div(id='pesanHitungLDMYes',
                                tagList(tags$br(),
                                        paste0("Jumlah total kolom ", inputTupla," tidak sama dengan 1"),
                                        tags$br(),
                                        tags$br(),
                                        radioButtons('LDMhit',
                                                     'Pilih perhitungan yang akan dilakukan',
                                                     choiceNames = c('normalisasi','hitung manual'),
                                                     choiceValues = c('normal','manual'),
                                                     selected = character(0))
                                        )

                  )
          )
        }
})

  
  ### visualisasi kolom baru yang sudah diisi nilai sel yang diganti 
  
  LDMPropKol_0 <- reactive({
    tes<-LDM_fun()
    inputTupla<-tes$inputTupla
    tablo<-tes$tablo
    # tablo<-LDMProp_new$tablo
    LDMProp_kol<-as.matrix(tablo[,inputTupla])
    kolsum<-as.matrix(colSums(LDMProp_kol))
    LDMProp_kol<-rbind(LDMProp_kol,kolsum)
    row.names(LDMProp_kol)<-c(row.names(tablo),"total")
    colnames(LDMProp_kol)<-inputTupla
    LDMProp_kol
  })
  
  
  observeEvent(input$LDMButtonEdit, {
    tabel$manualSave<-LDMPropKol_0()
    removeUI(selector='#LDMUIManual')
    removeUI(selector='#LDMUINormal')
  })
  
  
  observeEvent(input$LDMhit, {
    if(input$LDMhit=='normal'){
      removeUI(selector='#LDMUIManual')
      insertUI(selector='#LDMPlaceholder',
               where='afterEnd',
               ui= uiOutput('LDMUINormal')
      )
    }
    else if (input$LDMhit=='manual'){
      removeUI(selector='#LDMUINormal')
      insertUI(selector='#LDMPlaceholder',
               where='afterEnd',
               ui= uiOutput('LDMUIManual')
      )
    }
  })
  
  
  
  
  
  
  ##### untuk edit manual satu kolom tupla #####
  
  output$LDMUIManual<- renderUI({
    tagList(tags$b('Sunting secara manual'),
            tags$br(),
            tags$br(),
            rHandsontableOutput('LDMKolManualTable'),
            tags$br(),
            actionButton('saveLDMPropManual', 'Simpan kolom'),
            tags$br(),
            tags$div(id='teksLDMManual')
    )
  })
  
  
  observeEvent(input$LDMKolManualTable$changes$changes,{
    
    tes<-LDM_fun()
    inputTupla<-tes$inputTupla
    tablo<-tes$tablo
    
    LDMPropKol_1 <- as.data.frame(hot_to_r(input$LDMKolManualTable))
    row.names(LDMPropKol_1)<-c(row.names(tablo),"total")
    colnames(LDMPropKol_1)<-inputTupla
    LDMPropKol_1[nrow(LDMPropKol_1),1]<- sum(LDMPropKol_1[1:nrow(LDMPropKol_1)-1,1])
    tabel$manualSave<-LDMPropKol_1
    
    removeUI(selector='#teksManual')
    
  })
  
  
  
  output$LDMKolManualTable<-renderRHandsontable({
    rhandsontable(tabel$manualSave,
                  rowHeaderWidth = 220,
                  height=420,
                  fixedRowsBottom=1
    )
  })
  
  observeEvent(input$saveLDMPropManual,{
    tes<-LDM_fun()
    tablo = tes$tablo
    inputTupla = tes$inputTupla
    kolom<-as.data.frame(hot_to_r(input$LDMKolManualTable))
    total = kolom[nrow(kolom),1]
    
    if(total != 1){
      insertUI(selector='#teksLDMManual', 
               where = 'afterEnd',
               ui = tags$div (id ='teksManual', "Kolom tidak dapat disimpan. Nilai total tidak sama dengan 1."))
    }
    else {
      kolomMinSum <- kolom[1:nrow(kolom)-1,]
      tablo[,inputTupla]<-kolomMinSum
      LDMProp_new$coba<-tablo
      insertUI(selector='#teksLDMManual',
               where='afterEnd',
               ui= tags$div (id='teksManual',"Kolom berhasil disimpan. Silakan melanjutkan penyuntingan kolom lain.")
      )
      removeUI(selector='#pesanHitungLDMNo')
      removeUI(selector='#pesanHitungLDMYes')
    }
  })
  
  
  
  ##### untuk isi satu kolom dengan normalisasi #####
  
  output$LDMUINormal<- renderUI({
    tagList(tags$b ('Hasil perhitungan normalisasi'),
            tags$br(),
            tags$br(),
            rHandsontableOutput('LDMKolNormalTable'),
            tags$br(),
            actionButton('saveLDMPropNormal', 'Simpan kolom'),
            tags$br(),
            tags$div(id='teksLDMNormal')
    )
  })
  
  normal_fun<-reactive({
    LDMPropKol_0<-LDMPropKol_0()
    before<-as.matrix(LDMPropKol_0[1:nrow(LDMPropKol_0)-1,])
    sum<-matrix(data=LDMPropKol_0[nrow(LDMPropKol_0),], nrow=nrow(before), ncol=1)
    normal<- before/sum
    normal_sum<-sum(normal)
    normal<-rbind(normal,normal_sum)
    rownames(normal)<-rownames(LDMPropKol_0)
    colnames(normal)<-colnames(LDMPropKol_0)
    normal
    
  })
  
  output$LDMKolNormalTable<-renderRHandsontable({
    rhandsontable(normal_fun(), 
                  rowHeaderWidth = 220,
                  height=420, 
                  readOnly = TRUE, 
                  fixedRowsBottom=1
    )
  })
  
  observeEvent(input$saveLDMPropNormal,{
    tes<-LDM_fun()
    tablo = tes$tablo
    inputTupla = tes$inputTupla
    kolom <- as.data.frame(hot_to_r(input$LDMKolNormalTable))
    kolomMinSum <- kolom[1:nrow(kolom)-1,]
    tablo[,inputTupla]<-kolomMinSum
    LDMProp_new$coba<-tablo
    insertUI(selector='#teksLDMNormal',
             where='afterEnd',
             ui= tags$div (id='teksNormal',"Kolom berhasil disimpan. Silakan melanjutkan penyuntingan kolom lain.")
    )
    removeUI(selector='#pesanHitungLDMNo')
    removeUI(selector='#pesanHitungLDMYes')
    
  })
  
  
  ##### simpan tabel LDM ke dalam folder ####
  
  observeEvent(input$saveLDMTable,{
    waktuLDM<-Sys.time()
    simpanLDM<-gsub(" ","_",waktuLDM,fixed = TRUE)
    simpanLDM<-gsub(":","-",simpanLDM,fixed = TRUE)
    tanggalLDM<-Sys.Date()
    namafileLDM<-paste0(username,"_",selectedProv,"_",simpanLDM)
    saveRDS(LDMProp_new$coba, file = paste0('LDMData/',selectedProv,'/',namafileLDM))
    ldmRV$LDMListFile<-list.files(paste0("LDMData/",selectedProv))
    ldmRV$LDMTotFile<-length(list.files("LDMData/", selectedProv))
    removeUI(selector='#pesanHitungLDMNo')
    removeUI(selector='#pesanHitungLDMYes')
    removeUI(selector='#LDMUIManual')
    removeUI(selector='#LDMUINormal')
  })
  
  
  
######For aksi lahan --------
  
  
  RV<- reactiveValues(
    user.intList = list(NULL),
    aksi_lahan = user.scen,
    landReqList=list(NULL),
    LUTahun=list(NULL)
  )
  
  ListLDMButton_fun <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  ### buat tabel daftar yang memiliki intervensi di tabel satelit lahan ###
  aksiLahan_Fun<-reactive({
    user.intList<-RV$user.intList
    aksi_lahan=RV$aksi_lahan
    
    for (i in 1:length(user.intName)){
      user.intList[[i]]<- as.data.frame(readRDS(paste0("user/",user.intName[i])))
      if(any(user.intList[[i]]=="scensa.lahan")){
        aksi_lahan[i,]<-user.scen[i,]
      } else {
        aksi_lahan[i,]<-NA
      }
    }
    
    tabel<-data.frame(na.omit(aksi_lahan))
    rownames(tabel)<-1:nrow(tabel)
    tabel
    
  }) 
  
  #### tampilkan tabel aksi lahan dan tambahkan button untuk menyunting landreq ###
  AksiLahanTableReact <- reactive({
    data.frame(cbind(
      aksiLahan_Fun(),
      Satelit_Lahan = ListLDMButton_fun(actionButton, nrow(aksiLahan_Fun()),
                                     'buttonSaLahan_',
                                     label = "Sunting",
                                     onclick = paste0('Shiny.onInputChange( \"select_buttonSaLahan\" , this.id)')
      )
    )
    )
  })
  
  output$aksiLahanTable<-renderDataTable({AksiLahanTableReact()}, escape=FALSE)
  
  
  #### edit LU_tahun ###
  
  observeEvent(input$LDMPropUse,{
    for (i in 1:ncol(AksiLahanTableReact())){
    RV$LUTahun[[i]]<-LU_tahun
    print("observe done")
  }
  })
  
  tampilLUTahun<-eventReactive(input$select_buttonSaLahan,{
    
    selectedRow <- as.numeric(strsplit(input$select_buttonSaLahan,"_")[[1]][2])
    selectedFile<-as.data.frame(RV$LUTahun[[selectedRow]], row.names=colnames(LDMProp_his))
    selectedFile
  })
  
  
  #### perhitungan untuk memunculkan landreq
  
  #### hitung LPC dari LU_tahun dan LDMProp yang dipilih ####
  
  
  
landReq_fun <- reactive({

  print(input$LDMPropUse)

  if (input$LDMPropUse=="LDM historis"){
    LDMProp=LDMProp_his
  }
  else {
  LDMProp = readRDS(paste0("LDMData/Prov/",input$LDMPropUse))
  }

  tahun<-as.vector(str_extract_all(colnames(LU_tahun), '[0-9]+'))
  tahun<-as.data.frame(tahun)
  tahun<-t(tahun)
  print(tahun)
  LU_tahun<-as.data.frame(LU_tahun)
  LU_tahun<-as.matrix(LU_tahun)
  LDMdimcol<-ncol(LDMProp)
  LDMdimrow<-nrow(LDMProp)
  LDMProp<-as.matrix(LDMProp)
  # GDPAll<-as.data.frame(GDPAll)

  diagLU <- list()
  landTable<-list()
  landReq<-matrix(nrow=nrow(LDMProp),ncol=ncol(LU_tahun))

  for (i in 1:ncol(LU_tahun)){
    diagLU[[i]]<-as.matrix(diag(LU_tahun[,i]))
    landTable[[i]]<-LDMProp%*%diagLU[[i]]
    landReq[,i]<-as.matrix(rowSums(landTable[[i]]))
  }

  colnames(landReq)<-tahun
  landReq=as.data.frame(landReq, row.names=sector)
  landReq
})

landReqList_fun<-reactive(
  if (is.null(RV$landReqList)){
    for (i in 1:nrow(AksiLahanTableReact())){
      landReqList[[i]]<-landReq
    }
  }
)


  
  
  
  
  
  # LDMTableTampil<-eventReactive(input$select_button,{
  #   
  #   selectedRow <- as.numeric(strsplit(input$select_button,"_")[[1]][2])
  #   
  #   if(selectedRow==1){
  #     fileName = "LDM historis"
  #     selectedFile = LDMProp_his
  #     list<-list(fileName=fileName,
  #                selectedFile=selectedFile)
  #     print("kondisi1")
  #   }
  #   else if(selectedRow != 1){
  #     fileName<- LDMListTableReact()[selectedRow,1]
  #     selectedFile<-readRDS(paste0("LDMData/Prov/",fileName))
  #     list<-list(fileName=fileName, 
  #                selectedFile=selectedFile)
  #     print("kondisi2")
  #   }
  #   list
  # })
  # 
  
  
  
  
  
  
  #### modal dialog edit satelit lahan ###
  observeEvent(input$select_buttonSaLahan,{
    showModal(modalDialog(
      tabsetPanel(
        tabPanel("edit tabel land demand", 
                 rHandsontableOutput("LUTahunTable"),
                 actionButton('saveLUTahunTable','simpan tabel')),
        tabPanel("edit tabel land requirement", 
                 rHandsontableOutput("landReqTable"),
                 actionButton('saveLandReqTable', 'simpan tabel'))
      ),
      title="sunting luas kebutuhan lahan tiap tahun",
      footer= actionButton("closeModalAksiLahan", "tutup"),
      size="l",
      easyClose = FALSE
    ))
  })
  
 output$landReqTable<-renderRHandsontable(rhandsontable(landReq_fun(),height=500,rowHeaderWidth = 210))
 
 output$LUTahunTable<-renderRHandsontable(rhandsontable(tampilLUTahun(), height=500, rowHeaderWidth=210)) 
  
  
  
  
}

app <- shinyApp(ui,server)
runApp(app)
