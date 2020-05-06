library(shiny)
library(shinyBS)
library(dplyr)
library(plotly)
library(gtools)
library(plyr)
library(stringr)
library(scales)
library(tidyr)
library(shinyWidgets)

#source("_DB/debug_tradeoff_db.R")
source("_DB/debug_TIN.R")
username <- "dw"

ui <- fluidPage( 
  titlePanel("ANALISIS TRADE-OFF"),
  h4("Pastikan seluruh skenario aksi yang di definisikan pada modul Skenario & Simulasi (pada tab menu Skenario Intervensi) 
     sudah dilakukan analisis dengan menekan tombol Jalankan Analisis."),
  h4("Tekan tombol dibawah ini untuk menjalankan Analisis Trade-Off:"),
  actionButton("tradeOffRunButton","Jalankan Analisis Trade-Off"),
  tags$div(id = 'tradeOffResultPlaceholder')
)



server <- function(input,output,session){
  observeEvent(input$tradeOffRunButton, {
    insertUI(selector="#tradeOffResultPlaceholder",
             where='afterEnd',
             ui= uiOutput('tradeOffResultVisualization')
    )
  })
  
  
  
  output$tradeOffResultVisualization <- renderUI({
    tagList(  
      #dataTableOutput("ListTable"),
      #plotlyOutput("sunPlotTradeOff")
      fluidRow(
        column(8,
               br(),
               h4("Daftar skenario yang sudah dianalisis pada modul Skenario & Simulasi"),
               dataTableOutput("ListTable"),
               hr(),
               verbatimTextOutput("textBestScenario"),
               hr()
        ),
        column(4,
               plotlyOutput("sunPlotTradeOff")
        )
      ), 
      selectInput('selectTradeOffCombination', 
                  label = "pilih kombinasi skenario yang akan ditampilkan",
                  choices= c("seluruh kombinasi skenario", 
                             "kombinasi skenario terbaik", 
                             "kombinasi skenario emisi naik & PDRB naik",
                             "kombinasi skenario emisi naik & PDRB turun", 
                             "kombinasi skenario emisi turun & PDRB turun", 
                             "kombinasi skenario emisi turun & PDRB naik"
                  )), 
      plotlyOutput('tradeOffPlotEmission'), 
      plotlyOutput('tradeOffPlotGDP'),
      plotlyOutput('tradeOffPlotEmissionIntensity'),
    )
  })
  
  
  tradeOffPlot <-reactive({
    
    calculateTradeOff <- calculateTradeOff()
    tradeOffResultCombined <- calculateTradeOff$tradeOffResultCombined
    tradeOffSummary<- calculateTradeOff$tradeOffSummary
    
    #what to plot
    if(input$selectTradeOffCombination=="seluruh kombinasi skenario"){
      table<-tradeOffResultCombined
    } else if (input$selectTradeOffCombination== "kombinasi skenario terbaik"){
      tradeOffSummaryQ3<-filter(tradeOffSummary,tradeOffSummary$penurunan.emisi>0 & tradeOffSummary$peningkatan.PDRB>0 )
      bestScenID<-tradeOffSummaryQ3$ID[tradeOffSummaryQ3$penurunan.intensitasEmisi == min(tradeOffSummaryQ3$penurunan.intensitasEmisi)]
      table<-filter(tradeOffResultCombined, Category=="Q3" & ID==bestScenID)
      table<-rbind(table, filter(tradeOffResultCombined, ID=="BAU"))
    } else if (input$selectTradeOffCombination== "kombinasi skenario emisi naik & PDRB naik"){
      table<-filter(tradeOffResultCombined, Category=="Q1")
      table<-rbind(table, filter(tradeOffResultCombined, ID=="BAU"))
    } else if (input$selectTradeOffCombination== "kombinasi skenario emisi naik & PDRB turun"){
      table<-filter(tradeOffResultCombined, Category=="Q2")
      table<-rbind(table, filter(tradeOffResultCombined, ID=="BAU"))
    } else if (input$selectTradeOffCombination== "kombinasi skenario emisi turun & PDRB turun"){
      table<-filter(tradeOffResultCombined, Category=="Q4")
      table<-rbind(table, filter(tradeOffResultCombined, ID=="BAU"))
    } else if (input$selectTradeOffCombination== "kombinasi skenario emisi turun & PDRB naik"){
      table<-filter(tradeOffResultCombined, Category=="Q3")
      table<-rbind(table, filter(tradeOffResultCombined, ID=="BAU"))
    }
    
    table
    
  })
  
  output$tradeOffPlotEmission<-renderPlotly({
    ggplot(tradeOffPlot(), aes(x=Year, y=CummulativeEmission, group=ID))+
      geom_line(aes(color=ID))+
      geom_point(aes(color=ID))+
      labs(x="Tahun", y="Emisi")+
      ggtitle("Grafik Proyeksi Emisi")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$tradeOffPlotGDP<-renderPlotly({
    ggplot(tradeOffPlot(), aes(x=Year, y=ResultTotalGDP, group=ID))+
      geom_line(aes(color=ID))+
      geom_point(aes(color=ID))+
      labs(x="Tahun", y="Emisi")+
      ggtitle("Grafik Proyeksi PDRB")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$tradeOffPlotEmissionIntensity<-renderPlotly({
    ggplot(tradeOffPlot(), aes(x=Year, y=EmissionIntensity, group=ID))+
      geom_line(aes(color=ID))+
      geom_point(aes(color=ID))+
      labs(x="Tahun", y="Emisi")+
      ggtitle("Grafik Proyeksi Intensitas Emisi")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  
  loadRDSAll <- reactive({
    alamatFileEnergy <- paste0("_DB/skenarioData/", selectedProv, "/", "energi")
    alamatFileLand <- paste0("_DB/skenarioData/", selectedProv, "/", "lahan")
    alamatFileWaste <- paste0("_DB/skenarioData/", selectedProv, "/", "limbah")
    alamatFileAgri <- paste0("_DB/skenarioData/", selectedProv, "/", "pertanian")
    
    nameFilesEnergy <- list.files(path = alamatFileEnergy,
                                  pattern = paste0("^", username))
    dirFileEnergy <- paste0(alamatFileEnergy, "/",nameFilesEnergy)
    
    nameFilesLand <- list.files(path = alamatFileLand,
                                pattern = paste0("^", username))
    dirFileLand <- paste0(alamatFileLand, "/",nameFilesLand)
    
    nameFilesWaste <- list.files(path = alamatFileWaste,
                                 pattern = paste0("^", username))
    dirFileWaste <- paste0(alamatFileWaste, "/",nameFilesWaste)
    
    nameFilesAgri <- list.files(path = alamatFileAgri,
                                pattern = paste0("^", username))
    dirFileAgri <- paste0(alamatFileAgri, "/",nameFilesAgri)
    
    dirFileAll <- c(dirFileEnergy,dirFileLand,dirFileWaste,dirFileAgri)
    
    funcFile <- function(x){
      a <- readRDS(x)
      b <- c(x,a)
      b}
    
    fileAll <- lapply(dirFileAll, funcFile)
    #fileAll
    #menghapus list yang belum dijalankan run button pada modul skenario aksi
    newFileAll <- list.clean(fileAll, function(x) length(x) == 10)
    newFileAll
  })
  
  ### buat tabel daftar nama file reaktif ###
  ListTableReact <- reactive({
    #browser()
    data.frame(
      Nama.Skenario =  unlist(lapply(loadRDSAll(), function(x)x[[2]])),
      Tahun.Awal = unlist(lapply(loadRDSAll(), function(x)x[[3]])), 
      Tahun.Akhir = unlist(lapply(loadRDSAll(), function(x)x[[4]])),
      Deskripsi.Skenario = unlist(lapply(loadRDSAll(), function(x)x[[5]])),
      Nama.File = unlist(lapply(loadRDSAll(), function(x)x[[1]])) 
    )
  })
  
  ###tampilkan tabel list ###
  output$ListTable <- renderDataTable({
    DT::datatable(ListTableReact(), options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  }, escape = FALSE)
  
  calculateTradeOff <- eventReactive(input$tradeOffRunButton,{
  #observeEvent(input$tradeOffRunButton, {
    #browser()
    #nama dile yang msuk ke sini sudah hasil filter diloadRDSAll yang sudah di jalankan run buttonnya saja yang masuk 
    Nama.File = unlist(lapply(loadRDSAll(), function(x)x[[1]]))
    scenarioPath <- data.frame(path=Nama.File)
    
    for (i in 1:nrow(scenarioPath)){
      if(grepl("/energi/", scenarioPath$path[i])==TRUE){
        scenarioPath$type[i] <- "energi"
      } else if (grepl("/lahan/", scenarioPath$path[i])==TRUE){
        scenarioPath$type[i] <- "lahan"
      } else if (grepl("/pertanian/", scenarioPath$path[i])==TRUE){
        scenarioPath$type[i] <- "pertanian"
      } else if (grepl("/limbah/", scenarioPath$path[i])==TRUE){
        scenarioPath$type[i] <- "limbah"
      }
    }
    
    
    scenarioPath$ID<-paste0("scen",1:nrow(scenarioPath), ".", scenarioPath$type)
    
    #### harus sudah di run (Jalankan Analisis di tab Skenario Aksi)
    for (i in 1:ncol(scenarioPath)){
      eval(parse(text = paste0(scenarioPath$ID,"<-readRDS('",scenarioPath$path,"')")))
    }
    
    
    # calculate delta for each intervention
    for (a in scenarioPath$ID){
      eval(parse(text = paste0(a,"$scenarioResultGDP$GDP<-",a,"$scenarioResultGDP$GDP-resultGDP$GDP")))
      eval(parse(text = paste0(a,"$scenarioResultIncome$income<-",a,"$scenarioResultIncome$income-resultIncome$income")))
      eval(parse(text = paste0(a,"$scenarioResultLabour$labour<-",a,"$scenarioResultLabour$labour-resultLabour$labour")))
      eval(parse(text = paste0(a,"$scenarioResultEnergyConsumption[,4:ncol(",a,"$scenarioResultEnergyConsumption)]<-",a,"$scenarioResultEnergyConsumption[,4:ncol(",a,"$scenarioResultEnergyConsumption)]-resultEnergyConsumption[,4:ncol(resultEnergyConsumption)]")))
      eval(parse(text = paste0(a,"$scenarioResultEnergyEmission[,4:ncol(",a,"$scenarioResultEnergyEmission)]<-",a,"$scenarioResultEnergyEmission[,4:ncol(",a,"$scenarioResultEnergyEmission)]-resultEnergyEmission[,4:ncol(resultEnergyEmission)]")))
      eval(parse(text = paste0(a,"$scenarioResultWasteDisposal[,4:ncol(",a,"$scenarioResultWasteDisposal)]<-",a,"$scenarioResultWasteDisposal[,4:ncol(",a,"$scenarioResultWasteDisposal)]-resultWasteDisposal[,4:ncol(resultWasteDisposal)]")))
      eval(parse(text = paste0(a,"$scenarioResultWasteEmission[,4:ncol(",a,"$scenarioResultWasteEmission)]<-",a,"$scenarioResultWasteEmission[,4:ncol(",a,"$scenarioResultWasteEmission)]-resultWasteEmission[,4:ncol(resultWasteEmission)]")))
      eval(parse(text = paste0(a,"$scenarioResultFertilizerUsed[,4:ncol(",a,"$scenarioResultFertilizerUsed)]<-",a,"$scenarioResultFertilizerUsed[,4:ncol(",a,"$scenarioResultFertilizerUsed)]-resultFertilizerUsed[,4:ncol(resultFertilizerUsed)]")))
      eval(parse(text = paste0(a,"$scenarioResultFertilizerEmission[,4:ncol(",a,"$scenarioResultFertilizerEmission)]<-",a,"$scenarioResultFertilizerEmission[,4:ncol(",a,"$scenarioResultFertilizerEmission)]-resultFertilizerEmission[,4:ncol(resultFertilizerEmission)]")))
      eval(parse(text = paste0(a,"$scenarioResultLandReq$land.requirement<-",a,"$scenarioResultLandReq$land.requirement-resultLandReq$land.requirement")))
      eval(parse(text = paste0(a,"$scenarioResultLandCover$luas.land.use<-",a,"$scenarioResultLandCover$luas.land.use-resultLandCover$luas.land.use")))
      eval(parse(text = paste0(a,"$scenarioAllResult[,2:7]<-",a,"$scenarioAllResult[,2:7]-bauAllResult[,2:7]")))
    }
    
    
    # create all combinations
    scenarioCombination<-list()
    for (i in 4:nrow(scenarioPath)){
      scenarioCombination[[paste0(i)]]<-data.frame(combinations(nrow(scenarioPath),i,scenarioPath$ID), stringsAsFactors = FALSE)
    }
    # set rule for combination : each combination has to have >= 4 scens, with minimum 1 scen from each prk sectors
    for (i in 1:length(scenarioCombination)){
      for (a in 1:nrow(scenarioCombination[[i]])){
        if (any (c(sum(str_detect(scenarioCombination[[i]][a,], "lahan")), 
                   sum(str_detect(scenarioCombination[[i]][a,], "energi")),
                   sum(str_detect(scenarioCombination[[i]][a,], "limbah")),
                   sum(str_detect(scenarioCombination[[i]][a,], "pertanian"))) == 0)){
          scenarioCombination[[i]][a,]<-NA
        }
      }
      scenarioCombination[[i]]<-na.omit(scenarioCombination[[i]])
    }
    
    
    # create combination ID
    for (i in 1:length(scenarioCombination)){
      ID<-matrix(NA, nrow=nrow(scenarioCombination[[i]]), ncol=1)
      for (a in 1:nrow(scenarioCombination[[i]])){
        ID[a,]<-paste0("combination",i,".",a)
      }
      rownames(scenarioCombination[[i]])<-ID
    }
    
    # create tradeOffResult table based on BAU table
    tradeOffResult<-list()
    for (i in 1:length(scenarioCombination)){
      for (a in 1:nrow(scenarioCombination[[i]])){
        combinationName<-rownames(scenarioCombination[[i]])[a]
        # tradeOffResult[[i]][[combinationName]]<-list()
        print(combinationName)
        tradeOffResult[[combinationName]][['scenarioResultGDP']] <- resultGDP
        tradeOffResult[[combinationName]][['scenarioResultIncome']] <- resultIncome
        tradeOffResult[[combinationName]][['scenarioResultLabour']] <- resultLabour
        tradeOffResult[[combinationName]][['scenarioResultEnergyConsumption']] <- resultEnergyConsumption
        tradeOffResult[[combinationName]][['scenarioResultEnergyEmission']] <- resultEnergyEmission
        tradeOffResult[[combinationName]][['scenarioResultWasteDisposal']] <- resultWasteDisposal
        tradeOffResult[[combinationName]][['scenarioResultWasteEmission']] <- resultWasteEmission
        tradeOffResult[[combinationName]][['scenarioResultFertilizerUsed']] <- resultFertilizerUsed
        tradeOffResult[[combinationName]][['scenarioResultFertilizerEmission']] <- resultFertilizerEmission
        tradeOffResult[[combinationName]][['scenarioResultLandReq']] <- resultLandReq
        tradeOffResult[[combinationName]][['scenarioResultLandCover']] <- resultLandCover
        tradeOffResult[[combinationName]][['scenarioResultLandEmission']] <- resultLandEmission
        tradeOffResult[[combinationName]][['scenarioAllResult']] <- bauAllResult
        # tradeOffResult[[i]][[paste0(scenarioCombination[[i]][["ID"]][[a]])]]<-list()
        # eval(parse(text = paste0("tradeOffResult[[i]][['",scenarioCombination[[i]][["ID"]][[a]],"']][['resultGDP']] <- resultGDP")))
      }
    }
    # tradeOffResult = sum of all delta intervention + BAU 
    for (i in 1:length(scenarioCombination)){
      for (combinationName in rownames(scenarioCombination[[i]])){
        print(combinationName)
        for (x in 1:ncol(scenarioCombination[[i]])){
          scenName<-scenarioCombination[[i]][paste0(combinationName),x]
          print(scenName)
          eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioResultGDP']][['GDP']] <- tradeOffResult[['",combinationName,"']][['scenarioResultGDP']][['GDP']]  + ",scenName,"[['scenarioResultGDP']][['GDP']]")))
          eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioResultIncome']][['income']] <- tradeOffResult[['",combinationName,"']][['scenarioResultIncome']][['income']]  + ",scenName,"[['scenarioResultIncome']][['income']]")))
          eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioResultLabour']][['labour']] <- tradeOffResult[['",combinationName,"']][['scenarioResultLabour']][['labour']]  + ",scenName,"[['scenarioResultLabour']][['labour']]")))
          eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioResultEnergyConsumption']][,4:30] <- tradeOffResult[['",combinationName,"']][['scenarioResultEnergyConsumption']][,4:30] + ",scenName,"[['scenarioResultEnergyConsumption']][,4:30]")))
          eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioResultEnergyEmission']][,4:30] <- tradeOffResult[['",combinationName,"']][['scenarioResultEnergyEmission']][,4:30] + ",scenName,"[['scenarioResultEnergyEmission']][,4:30]")))
          eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioResultWasteDisposal']][,4:20] <- tradeOffResult[['",combinationName,"']][['scenarioResultWasteDisposal']][,4:20] + ",scenName,"[['scenarioResultWasteDisposal']][,4:20]")))
          eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioResultWasteEmission']][,4:20] <- tradeOffResult[['",combinationName,"']][['scenarioResultWasteEmission']][,4:20] + ",scenName,"[['scenarioResultWasteEmission']][,4:20]")))
          eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioResultFertilizerUsed']][,4:9] <- tradeOffResult[['",combinationName,"']][['scenarioResultFertilizerUsed']][,4:9] + ",scenName,"[['scenarioResultFertilizerUsed']][,4:9]")))
          eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioResultFertilizerEmission']][,4:9] <- tradeOffResult[['",combinationName,"']][['scenarioResultFertilizerEmission']][,4:9] + ",scenName,"[['scenarioResultFertilizerEmission']][,4:9]")))
          eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioResultLandReq']][['land.requirement']] <- tradeOffResult[['",combinationName,"']][['scenarioResultLandReq']][['land.requirement']]  + ",scenName,"[['scenarioResultLandReq']][['land.requirement']]")))
          eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioResultLandCover']][['luas.land.use']] <- tradeOffResult[['",combinationName,"']][['scenarioResultLandCover']][['luas.land.use']]  + ",scenName,"[['scenarioResultLandCover']][['luas.land.use']]")))
          eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioResultLandEmission']][['emission']] <- tradeOffResult[['",combinationName,"']][['scenarioResultLandEmission']][['emission']]  + ",scenName,"[['scenarioResultLandEmission']][['emission']]")))
          eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioAllResult']][,2:7] <- tradeOffResult[['",combinationName,"']][['scenarioAllResult']][,2:7] + ",scenName,"[['scenarioAllResult']][,2:7]")))
        }
        tradeOffResult[[paste0(combinationName)]][["scenarioAllResult"]]<-cbind(tradeOffResult[[paste0(combinationName)]][["scenarioAllResult"]], ID=paste0(combinationName))
      }
    }
    
    # check for any negative values in all tables
    if(any(unlist(tradeOffResult[[i]])<0)){
      tradeOffResult[[i]]<-NULL
    }
    any(is.null(tradeOffResult))
    
    
    # create trade off summary 
    tradeOffSummary<-data.frame(ID=NA, peningkatan.PDRB=NA,penurunan.emisi=NA, penurunan.intensitasEmisi=NA, stringsAsFactors = FALSE)
    for (i in 1:length(tradeOffResult)){
      scenarioAllResult<- tradeOffResult[[i]][["scenarioAllResult"]]
      scenarioAllResult.finalYear<-scenarioAllResult[scenarioAllResult$Year==finalYear,]
      bauAllResult.finalYear<-bauAllResult[bauAllResult$Year==finalYear,]
      
      # peningkatan PDRB <- mean ( GDP intervensi-GDP bau / GDP bau )
      peningkatan.PDRB <- mean((scenarioAllResult$ResultTotalGDP - bauAllResult$ResultTotalGDP) / bauAllResult$ResultTotalGDP) *100
      # penurunan emisi <- mean ( emisi intervensi-emisi bau / emisi bau )
      penurunan.emisi <- -mean((scenarioAllResult$TotalEmission - bauAllResult$TotalEmission) / bauAllResult$TotalEmission)*100
      # penurunan intensitas emisi <- - mean ( intensitas emisi intervensi-intensitas emisi bau / intensitas emisi bau )
      penurunan.intensitasEmisi <- -mean((scenarioAllResult$EmissionIntensity - bauAllResult$EmissionIntensity) / bauAllResult$EmissionIntensity)*100
      
      
      # penurunan emisi2 <- (emisi kumulatif intervensi tahun akhir - emisi kumulatif bau tahun akhir)/emisi kumulatif bau tahun akhir
      # penurunan.emisi2 <- -(scenarioAllResult.finalYear$CummulativeEmission - bauAllResult.finalYear$CummulativeEmission) / bauAllResult.finalYear$CummulativeEmission*100
      
      
      tradeOffSummary.addRow<-data.frame(ID=names(tradeOffResult)[[i]],
                                         peningkatan.PDRB=peningkatan.PDRB,
                                         penurunan.emisi=penurunan.emisi,
                                         # penurunan.emisi2=penurunan.emisi2,
                                         penurunan.intensitasEmisi=penurunan.intensitasEmisi, 
                                         stringsAsFactors = FALSE)
      tradeOffSummary<-rbind(tradeOffSummary,tradeOffSummary.addRow)
      
      
    }
    tradeOffSummary<-tradeOffSummary[-is.na(tradeOffSummary),]
    
   
    #### _DB
    ### belum buat logic ketika quadrantnya null
    tradeOffSummary[tradeOffSummary$penurunan.emisi<=0 & tradeOffSummary$peningkatan.PDRB>0,"quadrant"]<- "Q1" 
    tradeOffSummary[tradeOffSummary$penurunan.emisi<=0 & tradeOffSummary$peningkatan.PDRB<=0,"quadrant"]<- "Q2" 
    tradeOffSummary[tradeOffSummary$penurunan.emisi>0 & tradeOffSummary$peningkatan.PDRB>0,"quadrant"]<- "Q3" 
    tradeOffSummary[tradeOffSummary$penurunan.emisi>0 & tradeOffSummary$peningkatan.PDRB<=0,"quadrant"]<- "Q4" 
    
    id <- data.frame(
      ID=c("Q1","Q2","Q3","Q4")
    )
    tradeOffTablePlot <-  dplyr::bind_rows(id,tradeOffSummary)
    
    # Categorize tradeOffResultCombined by quadrant 
    tradeOffResultCombined <- cbind(bauAllResult, ID = "BAU", Category = "BAU")
    for ( i in 1:length (tradeOffResult)){
      if (any(str_detect(tradeOffSummaryQ1$ID, names(tradeOffResult)[[i]]))){
        quadrant <- "Q1"
      } else if (any(str_detect(tradeOffSummaryQ2$ID, names(tradeOffResult)[[i]]))){
        quadrant <- "Q2"
      } else if (any(str_detect(tradeOffSummaryQ3$ID, names(tradeOffResult)[[i]]))){
        quadrant <- "Q3"
      } else if (any(str_detect(tradeOffSummaryQ4$ID, names(tradeOffResult)[[i]]))){
        quadrant <- "Q4"
      }
      tradeOffResultCombined<-rbind(tradeOffResultCombined, cbind(tradeOffResult[[i]][["scenarioAllResult"]],
                                                                  Category = quadrant))
    }
    
    list(tradeOffTablePlot = tradeOffTablePlot,
         tradeOffResultCombined = tradeOffResultCombined,
         tradeOffSummary = tradeOffSummary)
  })
  
  
  output$sunPlotTradeOff <- renderPlotly({
    fig2 <- plot_ly(calculateTradeOff()$tradeOffTablePlot, ids = ~ID, labels = ~ID, parents = ~quadrant, 
                    width = 700, height = 700, type = 'sunburst')
    fig2
  })
  
  bestScenario <- eventReactive(input$tradeOffRunButton,{
    cleanNA <- drop_na(calculateTradeOff()$tradeOffTablePlot)
    best <- cleanNA %>% 
      filter(quadrant == "Q3") %>% 
      filter(penurunan.intensitasEmisi == min(penurunan.intensitasEmisi))
    
    best$ID
  })
  
  output$textBestScenario <- renderPrint({
    paste0("skenario terbaik adalah ", bestScenario(), 
           ", karena memiliki intensitas emisi terendah diantara kombinasi lainnya di kuadran 3" )
  })
  
}

app <- shinyApp(ui,server)
runApp(app)