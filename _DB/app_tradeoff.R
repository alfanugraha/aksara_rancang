library(shiny)
library(shinyBS)
library(dplyr)
library(plotly)
library(gtools)
library(plyr)
library(stringr)
library(scales)

source("_DB/debug_tradeoff_db.R")

ui <- fluidPage( 
  titlePanel("ANALISIS TRADE-OFF"),
  h4("Pastikan seluruh skenario aksi yang di definisikan pada modul Skenario & Simulasi (pada tab menu Skenario Intervensi) sudah dilakukan analisis dengan menekan tombol Jalankan Analisis"),
  
  fluidRow(
    column(8,
           br(),
           dataTableOutput("ListTable")
           ),
    column(4, 
           plotlyOutput("sunPlotTradeOff")
    )
  )
)



server <- function(input,output,session){
  
  output$sunPlotTradeOff <- renderPlotly({
    fig2 <- plot_ly(tradeOffTablePlot, ids = ~ID, labels = ~ID, parents = ~quadrant, 
                    width = 800, height = 800, type = 'sunburst')
    fig2
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
    fileAll
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
}

app <- shinyApp(ui,server)
runApp(app)