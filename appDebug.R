###*initiate library####
library(shiny)
library(shinydashboard)
library(shinyLP)
library(shinyjs)
# library(shinyBS)

library(digest)
library(rintrojs)
library(fmsb)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(formattable)
library(rtf)
library(rhandsontable)
library(stringr)
library(gridExtra)
#library(ggradar)
# library(RColorBrewer)

library(maditr)
library(limSolve)

# source("land.R")

###*setup dashboard page####
ui <- source('interfaceDebug.R')

###*define server#### 
server <- function(input, output, session) {
  ###BEGIN: initiate all variables & function####
  # debug mode
  debugMode <- 1
  notif_id <- NULL
  
  provList <- readRDS("data/provList")
  # usersList <- load("usersList")

  LDMProp_new<-reactiveValues(
    tablo = NULL,
    coba= NULL
  )

  tabel<-reactiveValues(
    manualSave=NULL
  )

  ldmRV<-reactiveValues(
    LDMListFile = unique(list.files(paste0("LDMData/Prov/"))),   # ganti mas alfa
    LDMTotFile= unique(length(list.files("LDMData/Prov/")))   # ganti mas alfa
  )

  editable<-reactiveValues(
    BAULahan_landCover=NULL
  )
  
  allDataProv <- reactiveValues(
    username = NULL,
    prov = NULL,
    ioSector = NULL,
    ioIntermediateDemand = NULL,
    ioFinalDemand = NULL,
    ioAddedValue = NULL,
    satelliteLabour = NULL,
    satelliteEnergy = NULL,
    satelliteWaste = NULL,
    satelliteAgriculture = NULL,
    emissionFactorEnergy = NULL,
    emissionFactorWaste = NULL,
    emissionFactorAgriculture = NULL,
    ioFinalDemandComponent = NULL,
    ioAddedValueComponent = NULL,
    populationProjection = NULL,
    baselineEmission = NULL,
    LU_tahun=NULL,
    LDMProp_his=NULL,
    LDMProp=NULL,
    # landDemand = NULL,
    # landDemand_prop = NULL,
    ioLeontif = NULL,
    ioLeontiefInverse = NULL,
    GDPAll = NULL,
    linkagesTable = NULL,
    multiplierAll = NULL,
    ioPeriod = NULL,
    rtffile = NULL,
    growthRate = NULL,
    prk_scenario = data.frame(time=NULL, action=NULL, year=NULL, username=NULL, provinsi=NULL, sector=NULL, fd_value=NULL),
    ### Start : Land Section ###
    LUTMDatabase = NULL,
    LUTMTemplate_his = NULL,
    LRCRate_his = NULL,
    LRCRate_2 = NULL,
    carbonStock_his = NULL
    ### End : Land Section ###
  )
  
  finalResults <- reactiveValues(table1=NULL, plot23=NULL, plot24=NULL, plot25=NULL)
  
  historicalResults <- reactiveValues()
  bauResults <- reactiveValues()
  interventionResults <- reactiveValues()
  
  userActivities <- reactiveValues(
    latestAct= NULL, message=NULL, dateTime=NULL, listOfActs = data.frame(latestAct=NULL, message=NULL, dateTime=NULL)
  )
  
  recordActivities <- function(latestAct, message, dateTime){
    userActivities$latestAct<-latestAct
    userActivities$message<-message
    userActivities$dateTime=dateTime
    userActivities$listOfActs<-rbind(userActivities$listOfActs,data.frame(latestAct=NULL, message=NULL, dateTime=NULL))
  }
  
  observeEvent(input$inputLogin, {
    fullname <- input$fullname
    username <- input$username
    password <- input$password
    selectedProv <- input$categoryProvince
    # print(selectedProv)
    
    # if(password %in% provList$Password){
    #
    # } else {
    #   return(NULL)
    # }
    # usersList <- data.frame(id=NULL, fullname=NULL, username=NULL, password=NULL, provinsi=NULL)
    
    datapath <- paste0("data/", selectedProv, "/")
    userFolder <- paste0(datapath, username)
    if(!dir.exists(userFolder)) dir.create(userFolder, mode = 777)
    # system(paste0("chmod -R 777 ", userFolder))
    
    ioSector <- readRDS(paste0(datapath, "sector"))
    ioIntermediateDemand <- readRDS(paste0(datapath, "indem"))
    ioFinalDemand <- readRDS(paste0(datapath, "findem"))
    ioAddedValue <- readRDS(paste0(datapath, "addval"))
    satelliteLabour <- readRDS(paste0(datapath, "labour"))
    satelliteEnergy <- readRDS(paste0(datapath, "energy"))
    satelliteWaste <- readRDS(paste0(datapath, "waste"))
    satelliteAgriculture <- readRDS(paste0(datapath, "agriculture"))
    emissionFactorEnergy <- readRDS(paste0(datapath, "ef_energy"))
    emissionFactorWaste <- readRDS(paste0(datapath, "ef_waste"))
    emissionFactorAgriculture <- readRDS(paste0(datapath, "ef_agriculture"))
    ioFinalDemandComponent <- readRDS(paste0(datapath, "findemcom"))
    ioAddedValueComponent <- readRDS(paste0(datapath, "addvalcom"))
    population <- readRDS(paste0(datapath, "currentPopulation"))
    populationProjection <- readRDS(paste0(datapath, "population"))
    baselineEmission <- readRDS(paste0(datapath, "otherEm"))
    LU_tahun<-readRDS(paste0(datapath,"LU_tahun"))
    # print(LU_tahun)
    LDMProp_his<-readRDS(paste0(datapath,"LDMProp"))
    row.names(LDMProp_his)<-ioSector[,1]
    # landDemand <- readRDS(paste0(datapath, "landDemand"))
    # landDemand_prop <- readRDS(paste0(datapath, "landDemand_prop"))
    # landtable <- readRDS(paste0(datapath, "landtable"))
    ioLeontif <- readRDS(paste0(datapath, "I_A"))
    ioLeontiefInverse <- readRDS(paste0(datapath, "leontief"))
    GDPAll <- readRDS(paste0(datapath, "GDPAll"))
    linkagesTable <- readRDS(paste0(datapath, "linkagesTable"))
    multiplierAll <- readRDS(paste0(datapath, "multiplierAll"))
    ioPeriod <- readRDS(paste0(datapath, "periodIO"))
    # print(ioPeriod)
    rtffile <- readRDS(paste0(datapath, "rtffile"))
    
    # growthRate <- read.table("_DB/jabar_in_redcluwe/growth5_1630.csv", header = T, sep = ",")
    
    ### Start : Land Section ###
    # datapath <- paste0("data/", selectedProv, "/")
    LUTMDatabase<-as.data.frame(read.csv("data/LUTMDatabaseID.csv"))
    LDMProp_his<-read.csv(paste0(datapath, "LDMProp.csv"))
    LUTMTemplate_his<-read.csv(paste0(datapath,"LUTM_template.csv"))
    LRCRate_his<-read.csv(paste0(datapath,"LRCRate.csv"),header = FALSE)
    LRCRate_2<-read.csv(paste0(datapath,"LRCRate_2.csv"),header=FALSE)
    # LRCRate_2<-read.csv(paste0(datapath,"LRCRate.csv"),header=FALSE)  #delete after use
    carbonStock_his<-data.matrix(read.csv(paste0(datapath,"carbonStock.csv")))
    carbonStock_his<-as.matrix(carbonStock_his[,3])
    ### End : Land Section ###
    
    allDataProv$username = username
    allDataProv$selectedProv = selectedProv
    allDataProv$ioSector = ioSector
    allDataProv$ioIntermediateDemand = ioIntermediateDemand
    allDataProv$ioFinalDemand = ioFinalDemand
    allDataProv$ioAddedValue = ioAddedValue
    allDataProv$satelliteLabour = satelliteLabour
    allDataProv$satelliteEnergy = satelliteEnergy
    allDataProv$satelliteWaste = satelliteWaste
    allDataProv$satelliteAgriculture = satelliteAgriculture
    allDataProv$emissionFactorEnergy = emissionFactorEnergy
    allDataProv$emissionFactorWaste = emissionFactorWaste
    allDataProv$emissionFactorAgriculture = emissionFactorAgriculture
    allDataProv$ioFinalDemandComponent = ioFinalDemandComponent
    allDataProv$ioAddedValueComponent = ioAddedValueComponent
    allDataProv$populationProjection = populationProjection
    allDataProv$population = population
    allDataProv$baselineEmission = baselineEmission
    allDataProv$LU_tahun = LU_tahun
    allDataProv$LDMProp_his = LDMProp_his
    allDataProv$LDMProp = data.frame()
    # allDataProv$landDemand = landDemand
    # allDataProv$landDemand_prop = landDemand_prop
    allDataProv$ioLeontif = ioLeontif
    allDataProv$ioLeontiefInverse = ioLeontiefInverse
    allDataProv$GDPAll = GDPAll
    allDataProv$linkagesTable = linkagesTable
    allDataProv$multiplierAll = multiplierAll
    allDataProv$ioPeriod = ioPeriod
    allDataProv$rtffile = rtffile
    allDataProv$growthRate = data.frame(Lapangan_usaha=as.character(ioSector[,1]))
    ### Start : Land Section ###
    allDataProv$LUTMDatabase = LUTMDatabase
    allDataProv$LUTMTemplate_his = LUTMTemplate_his
    allDataProv$LRCRate_his = LRCRate_his
    allDataProv$LRCRate_2 = LRCRate_2
    allDataProv$carbonStock_his = carbonStock_his
    ### End : Land Section ###
    
    recordActivities("Login redcluwe.id", "Berhasil", paste0(Sys.time()))
    
    notif_id <<- showNotification("Anda berhasil masuk", duration = 4, closeButton = TRUE, type = "warning")
    updateTabItems(session, "tabs", selected = "pageOne")
    
  })
  
  blackBoxInputs <- function(){
    # browser()
    ioSector <- allDataProv$ioSector
    ioIntermediateDemand <- allDataProv$ioIntermediateDemand
    ioFinalDemand <- allDataProv$ioFinalDemand
    ioAddedValue <- allDataProv$ioAddedValue
    satelliteLabour <- allDataProv$satelliteLabour
    satelliteEnergy <- allDataProv$satelliteEnergy
    satelliteWaste <- allDataProv$satelliteWaste
    satelliteAgriculture <- allDataProv$satelliteAgriculture
    emissionFactorEnergy <- allDataProv$emissionFactorEnergy
    emissionFactorWaste <- allDataProv$emissionFactorWaste
    emissionFactorAgriculture <- allDataProv$emissionFactorAgriculture
    ioFinalDemandComponent <- allDataProv$ioFinalDemandComponent
    ioAddedValueComponent <- allDataProv$ioAddedValueComponent
    populationProjection <- allDataProv$populationProjection
    population <- allDataProv$population
    baselineEmission <- allDataProv$baselineEmission
    # LU_tahun <- allDataProv$LU_tahun
    LDMProp_his <- allDataProv$LDMProp_his
    # row.names(LDMProp_his)<-ioSector[,1]
    LDMProp <- allDataProv$LDMProp
    # landDemand <- allDataProv$landDemand
    # landDemand_prop <- allDataProv$landDemand_prop
    # landtable <- allDataProv$landtable
    ioLeontif <- allDataProv$ioLeontif
    ioLeontiefInverse <- allDataProv$ioLeontiefInverse
    GDPAll <- allDataProv$GDPAll
    linkagesTable <- allDataProv$linkagesTable
    multiplierAll <- allDataProv$multiplierAll
    ioPeriod <- allDataProv$ioPeriod
    rtffile <- allDataProv$rtffile
    ### Start : Land Section ###
    LUTMDatabase <- allDataProv$LUTMDatabase
    LUTMTemplate_his <- allDataProv$LUTMTemplate_his
    LRCRate_his <- allDataProv$LRCRate_his
    LRCRate_2 <- allDataProv$LRCRate_2
    carbonStock_his <-allDataProv$carbonStock_his
    ### End : Land Section ###
    
    # Row explicit definition for Income (Wages & Salary)
    
    matrixIoIntermediateDemand <- as.matrix(ioIntermediateDemand) #indem_matrix
    matrixIoAddedValue <- as.matrix(ioAddedValue) #addval_matrix
    nrowMatrixIoAddedValue <- nrow(matrixIoAddedValue) #num_addval
    ioDimention <- ncol(ioIntermediateDemand) #dimensi

    matrixIoFinalDemand <- as.matrix(ioFinalDemand)
    rowSumsMatrixIoFinalDemand <- as.matrix(rowSums(matrixIoFinalDemand))
    proportionFinalDemand <- ioFinalDemand/rowSumsMatrixIoFinalDemand
    proportionFinalDemand[is.na(proportionFinalDemand)] <- 0

    colSumsMatrixIoIntermediateDemand <- colSums(matrixIoIntermediateDemand) #indem_colsum
    colSumsMatrixIoAddedValue <- colSums(matrixIoAddedValue) #addval_colsum
    ioTotalOutput <- colSumsMatrixIoIntermediateDemand + colSumsMatrixIoAddedValue # ioTotalInput (total_output <- indem_colsum+addval_colsum)
    ioTotalOutputInverse <- 1/ioTotalOutput #fin_con
    ioTotalOutputInverse[is.infinite(ioTotalOutputInverse)] <- 0
    ioTotalOutputInverse <- diag(ioTotalOutputInverse) #tinput_invers
    
    rowImport <- 1
    rowIncome <- 2 #income_row <- 2
    rowProfit <- 3
    
    # initialYear <- input$dateFrom
    # finalYear <- input$dateTo
    # iteration <- finalYear - initialYear
    
    functionSatelliteImpact <- function(type = "energy", satellite = data.frame(), matrix_output = matrix(), emission_factor = data.frame()) { 
      impact <- list()
      
      # impact$consumption
      impact$consumption <- satellite
      
      # calculate the proportion
      if(type != "labour"){
        proportionConsumption <- impact$consumption[, 4:ncol(impact$consumption)] / impact$consumption[, 3]
        impact$consumption[, 4:ncol(impact$consumption)] <- proportionConsumption
      }
      
      # calculate the coefficient & the new total satellite consumption 
      coefficientConsumption <- as.matrix(impact$consumption[,3]) / ioTotalOutput
      impact$consumption[,3] <- coefficientConsumption * matrix_output
      
      # calculate emission
      if(type != "labour"){
        
        colnames(impact$consumption)[3] <- "Tconsumption"
        
        # get the new satellite consumption for each sector
        # total consumption * proportion
        impact$consumption[,4:ncol(impact$consumption)] <- impact$consumption[,4:ncol(impact$consumption)] * impact$consumption[, 3]
        
        # checking the order of factor emission 
        orderEnergyType <- names(impact$consumption)[4:ncol(impact$consumption)]
        emissionFactor <- numeric()
        for(m in 1:length(orderEnergyType)){
          emissionFactor <- c(emissionFactor, emission_factor[which(emission_factor[,1]==orderEnergyType[m]), 2])
        }
        emissionFactor <- diag(emissionFactor, nrow = length(emissionFactor), ncol = length(emissionFactor))
        
        # impact$emission
        impact$emission <- impact$consumption
        impact$emission[,4:ncol(impact$emission)] <- as.matrix(impact$consumption[,4:ncol(impact$consumption)]) %*% emissionFactor
        impact$emission[,3] <- rowSums(impact$emission[,4: ncol(impact$emission)])
        impact$emission[is.na(impact$emission)] <- 0
        colnames(impact$emission)[3] <- "Temission"
      } 
      
      impact$consumption[is.na(impact$consumption)] <- 0
      return(impact)
      
    }
    ###END: initiate ####
    
    ###BEGIN: regional economic impact analysis & historical emission from satellite account####
    # Direct Backward Linkage
    analysisDBL <- colSums(ioLeontiefInverse) #DBL
    analysisBPD <- analysisDBL/(mean(analysisDBL))
    
    # Direct Forward Linkage
    analysisDFL <- rowSums(ioLeontiefInverse) #DFL
    analysisFPD <- analysisDFL/(mean(analysisDFL))
    
    # GDP
    analysisGDP <- colSums(matrixIoAddedValue[rowIncome:nrowMatrixIoAddedValue,]) #GDP
    analysisTotalGDP <- sum(analysisGDP)
    
    # Multiplier Output (MO)
    analysisMO <- colSums(ioLeontiefInverse) #multiplierOutput
    
    # Coefficient Income (CI) & Multiplier Income (MI)
    analysisCI <- as.matrix(matrixIoAddedValue[rowIncome,]) / ioTotalOutput #income_coef
    analysisMI <- ioLeontiefInverse %*% analysisCI #multiplierIncome
    analysisMI[is.na(analysisMI)] <- 0
    
    # Coefficient Labour (CL) & Multiplier Labour (ML)
    analysisCL <- as.matrix(satelliteLabour[,3]) / ioTotalOutput #labour_coef
    analysisML <- ioLeontiefInverse %*% analysisCL #multiplierLabour
    analysisML[is.na(analysisML)] <- 0
    
    # Coefficient Energy Used (CE) & Multiplier Energy (ME)
    analysisCE <- as.matrix(satelliteEnergy[,3]) / ioTotalOutput #energy_coef
    analysisME <- ioLeontiefInverse %*% analysisCE #multiplierEnergy
    analysisME[is.na(analysisME)] <- 0
    
    # Coefficient Waste Product (CW) & Multiplier Waste (MW) 
    analysisCW <- as.matrix(satelliteWaste[,3]) / ioTotalOutput #waste_coef
    analysisMW <- ioLeontiefInverse %*% analysisCW #multiplierWaste
    analysisMW[is.na(analysisMW)] <- 0
    
    # # Coefficient Agriculture-Fertilizer (CA) & Multiplier Agriculture-Fertilizer (MA)
    # analysisCA <- as.matrix(satelliteAgriculture[,3]) / ioTotalOutput
    # analysisMA <- ioLeontiefInverse %*% analysisCA
    # analysisMA[is.na(analysisMA)] <- 0
    
    # Ratio Wages / Business Surplus
    analysisRatioWS <- t(as.matrix(ioAddedValue[2,] / ioAddedValue[3,])) #ratio_ws
    analysisRatioWS[is.na(analysisRatioWS)] <- 0
    analysisRatioWS[analysisRatioWS == Inf] <- 0
    colnames(analysisRatioWS) <- "ratio_ws"
    
    # Satellite account by sectoral GDP
    analysisEnergyByGDP <- as.matrix(satelliteEnergy[,3]) / analysisTotalGDP #coef_energy
    analysisWasteByGDP <- as.matrix(satelliteWaste[,3]) / analysisTotalGDP #coef_waste
    # analysisAgricultureByGDP <- as.matrix(satelliteAgriculture[,3]) / analysisTotalGDP
    
    # Emission from energy
    emissionFactorEnergyDiagonal <- diag(emissionFactorEnergy[,2], ncol = nrow(emissionFactorEnergy), nrow = nrow(emissionFactorEnergy)) #f_energy_diag
    emissionEnergy <- as.matrix(satelliteEnergy[,4:ncol(satelliteEnergy)]) %*% emissionFactorEnergyDiagonal #em_energy
    emissionEnergyTotal <- rowSums(emissionEnergy) #em_energy_total
    
    # Emission from waste
    emissionFactorWasteDiagonal <- diag(emissionFactorWaste[,2], ncol = nrow(emissionFactorWaste), nrow = nrow(emissionFactorWaste)) #f_waste_diag
    emissionWaste <- as.matrix(satelliteWaste[,4:ncol(satelliteWaste)]) %*% emissionFactorWasteDiagonal #em_waste
    emissionWasteTotal <- rowSums(emissionWaste) #em_waste_total
    
    # Emission from agriculture-fertilizer
    emissionFactorAgricultureDiagonal <- diag(emissionFactorAgriculture[,2], ncol = nrow(emissionFactorAgriculture), nrow = nrow(emissionFactorAgriculture))
    emissionAgriculture <- as.matrix(satelliteAgriculture[,4:ncol(satelliteAgriculture)]) %*% emissionFactorAgricultureDiagonal
    emissionAgricultureTotal <- rowSums(emissionAgriculture)
    
    # Wages
    analysisWages <- as.matrix(t(ioAddedValue[2,])) #wages
    colnames(analysisWages) <- "wages"
    
    # Income per capita
    analysisIncomePerCapita <- sum(as.matrix(matrixIoAddedValue[rowIncome,])) / population #income_per_capita
    
    # Coefficient technology (intermediate demand) or A
    analysisCT <- t( t(matrixIoIntermediateDemand) / ioTotalOutput)
    
    # Coefficient primary input
    analysisCPI <- t(t(ioAddedValue) / ioTotalOutput)
    
    ###END: analysis ####
    
    # # for calculate landTable, LPC, LRC historis     # TIN CEK, variabel u/ proyeksi bau ekonomi berdasarkan tutupan lahan
    # LU_tahun<-as.data.frame(LU_tahun[,1:ncol(LU_tahun)])
    # LU_tahun<-as.matrix(LU_tahun)
    # LDMProp_his<-as.matrix(LDMProp_his[,2:ncol(LDMProp_his)])
    # GDPAll<-as.data.frame(GDPAll)
    # diagLU_his<-as.matrix(diag(LU_tahun[,1]))
    # landTable_his<-LDMProp_his %*% diagLU_his
    # landReq_his<-as.matrix(rowSums(landTable_his))
    # 
    # LPC_his<-GDPAll[,4]/landReq_his
    # LPC_his[is.infinite(LPC_his)]<-0
    # LRC_his<-1/LPC_his
    # LRC_his[is.infinite(LRC_his)]<-0
    # landTable_his<-cbind(ioSector, landTable_his, landReq_his, LPC_his, LRC_his)
    # # # colnames(landTable_his)<-c("Sektor", "Kategori", colnames(LDMProp_his),"Total Kebutuhan Lahan", "LPC", "LRC")
    # tahun<-as.vector(str_extract_all(colnames(LU_tahun), '[0-9]+'))
    # tahun<-as.data.frame(tahun)
    # tahun<-t(tahun)
    
    ### START : TIN INI DICEK ####
    # land use transition matrix (LUTM) historis
    LUTMDatabase<-LUTMDatabase[LUTMDatabase$Provinsi==paste0(input$categoryProvince),c("Count",paste0("PL", ioPeriod-1, "RCL"), paste0("PL",ioPeriod,"RCL"))]  # tidak perlu di result
    colnames(LUTMDatabase)<-c("COUNT","ID_LC1","ID_LC2")
    tuplaID<- cbind(as.matrix(cbind(matrix(0, nrow=23^2, ncol=1), as.matrix(expand.grid(1:23, 1:23)))))  # tidak perlu di result
    colnames(tuplaID)<-c("COUNT","ID_LC1","ID_LC2")
    LUTMDatabase<-rbind(LUTMDatabase,tuplaID)
    LUTMDatabase<-aggregate(LUTMDatabase, by=list(LUTMDatabase$ID_LC1,LUTMDatabase$ID_LC2), FUN=sum)
    LUTMDatabase<-LUTMDatabase[,1:3]
    colnames(LUTMDatabase)<-c("ID_LC1","ID_LC2","COUNT")
    LUTMDatabase<-LUTMDatabase[LUTMDatabase$ID_LC1>0,]
    LUTMDatabase<-LUTMDatabase[LUTMDatabase$ID_LC2>0,]
    LUTMDatabase <- melt(data = LUTMDatabase, id.vars=c('ID_LC1','ID_LC2'), measure.vars=c('COUNT'))
    LUTM_his <- dcast(data = LUTMDatabase, formula = ID_LC1 ~ ID_LC2, fun.aggregate = sum)
    LUTM_his<-as.matrix(LUTM_his[,-1])


    # # land cover historis
    landCover_his <- dcast(data = LUTMDatabase, formula = ID_LC2 ~ ., fun.aggregate = sum)
    landCover_his<-as.matrix(landCover_his[,2])
    landCover_his0<- dcast(data = LUTMDatabase, formula = ID_LC1 ~ ., fun.aggregate = sum)
    landCover_his0<-as.matrix(landCover_his0[,2])

    #TPM
    TPM<-matrix(nrow=nrow(LUTM_his), ncol=ncol(LUTM_his))
    for (i in 1:ncol(TPM)){
      TPM[,i]<-LUTM_his[,i]/landCover_his0[i,1]   #proporsi semua elemen LUTM dibagi tupla tahun kedua
    }
    TPM[is.nan(TPM)]<-0

    #land distribution matrix dalam luas (analysisLDMLuas)
    analysisLDMLuas<-as.matrix(LDMProp_his)%*%as.matrix(diag(landCover_his[,1]))

    #land requirement historis (analysisLR)
    landReq_his<-as.matrix(rowSums(analysisLDMLuas))

    # Land requirement coefficient (analysisLRC) & land productivity coefficient (analysisLPC)
    analysisLPC<-rbind(as.matrix(rowSums(cbind(ioIntermediateDemand, ioFinalDemand))), 0)/landReq_his    #rowSums(cbind(indem, findem))=output
    analysisLPC[is.infinite(analysisLPC)]<-0
    analysisLPC[is.nan(analysisLPC)]<-0
    analysisLRC<-1/analysisLPC
    analysisLRC[is.infinite(analysisLRC)]<-0
    analysisLRC[is.nan(analysisLPC)]<-0

    # land distribution matrix proportion (total sector = 1)
    LDMProp_sektor<-matrix(NA, nrow=ncol(analysisLDMLuas), ncol=nrow(analysisLDMLuas))
    for (i in 1:ncol(LDMProp_sektor)){
      LDMProp_sektor[,i]<-as.matrix(analysisLDMLuas[i,]/sum(analysisLDMLuas[i,]))
    }
    LDMProp_sektor[is.na(LDMProp_sektor)]<-0


    # LUTM Template
    LUTMTemplate_his<-as.matrix(LUTMTemplate_his)
    for (i in 1:nrow(landCover_his)){
      if (sum(landCover_his[i,])==0){
        LUTMTemplate_his[i,]<-matrix(0,ncol=ncol(LUTMTemplate_his))    #LUTMTemplate bisa diedit di interface
        LUTMTemplate_his[,i]<-matrix(0,nrow=ncol(LUTMTemplate_his))
      } else {}
    }
    LUTMTemplate_his[is.na(LUTMTemplate_his)]<-paste0("x",1:length(LUTMTemplate_his[is.na(LUTMTemplate_his)]))

    #### END : TIN INI DICEK ####
    
    result <- cbind(ioSector,
                    analysisBPD,
                    analysisFPD, 
                    analysisGDP, 
                    analysisMO, 
                    analysisMI,
                    analysisML,
                    analysisCL,
                    analysisME,
                    analysisMW,
                    analysisWages,
                    analysisRatioWS, 
                    analysisCE,
                    analysisCW,
                    emissionEnergyTotal,
                    emissionWasteTotal
    )
    colnames(result)[1] <- "Sektor"
    
    list_table <- list(result=result,
                       ioSector=ioSector, 
                       ioIntermediateDemand=ioIntermediateDemand, 
                       ioFinalDemand=ioFinalDemand, 
                       ioAddedValue=ioAddedValue, 
                       satelliteLabour=satelliteLabour, 
                       satelliteEnergy=satelliteEnergy, 
                       satelliteAgriculture=satelliteAgriculture,
                       ioFinalDemandComponent=ioFinalDemandComponent, 
                       ioAddedValueComponent=ioAddedValueComponent,
                       satelliteWaste=satelliteWaste,
                       emissionFactorEnergy=emissionFactorEnergy,
                       emissionFactorWaste=emissionFactorWaste,
                       emissionFactorAgriculture=emissionFactorAgriculture,
                       # landcover=allDataProv$LU_tahun,
                       analysisIncomePerCapita=analysisIncomePerCapita,
                       baselineEmission=baselineEmission,
                       populationProjection=populationProjection,
                       # LU_tahun=LU_tahun,
                       LDMProp=allDataProv$LDMProp_his, #bedanya 1
                       GDPAll=GDPAll,
                       # landTable_t0=landTable_t0,
                       # landReq=landReq,
                       # tahun=tahun, 
                       # landTable_his=landTable_his,
                       baselineEmission=baselineEmission, 
                       LDMProp_his=LDMProp_his, #bedanya 2
                       ### Function in BAU Scenario ###
                       analysisLRC=analysisLRC,
                       landReq_his=landReq_his,
                       landCover_his=landCover_his,
                       LDMProp_sektor=LDMProp_sektor,
                       LUTMTemplate_his=LUTMTemplate_his,
                       LUTM_his=LUTM_his,
                       TPM=TPM,
                       carbonStock_his=carbonStock_his,
                       ioDimention=ioDimention,
                       proportionFinalDemand=proportionFinalDemand,
                       analysisCT=analysisCT,
                       analysisCPI=analysisCPI,
                       LRCRate_his=LRCRate_his,
                       LRCRate_2=LRCRate_2,
                       ioPeriod=ioPeriod
                      
    ) 
    
    return(list_table)
  }
  
  ###*historical input####
  allInputs <- eventReactive(input$button, {
    inSector <- input$ioSector
    if(is.null(inSector))
      return(NULL)
    
    inIntermediateDemand <- input$ioIntermediateDemand
    if(is.null(inIntermediateDemand))
      return(NULL)
    
    inFinalDemand <- input$ioFinalDemand
    if(is.null(inFinalDemand))
      return(NULL)
    
    inAddedValue <- input$ioAddedValue
    if(is.null(inAddedValue))
      return(NULL)    
    
    inLabour <- input$satelliteLabour
    if(is.null(inLabour))
      return(NULL)
    
    inEnergy <- input$energyTable
    if(is.null(inEnergy))
      return(NULL) 
    
    inWaste <- input$wasteTable
    if(is.null(inWaste))
      return(NULL)
    
    inAgriculture <- input$agricultureTable
    if(is.null(inAgriculture))
      return(NULL)
    
    inEmissionFactorEnergiTable <- input$emissionFactorEnergiTable
    if(is.null(inEmissionFactorEnergiTable))
      return(NULL)
    
    inEmissionFactorLandWasteTable <- input$emissionFactorLandWasteTable
    if(is.null(inEmissionFactorLandWasteTable))
      return(NULL)
    
    inEmissionFactorAgricultureTable <- input$emissionFactorAgricultureTable
    if(is.null(inEmissionFactorLandWasteTable))
      return(NULL)
    
    inFinalDemandComp <- input$ioFinalDemandComponent
    if(is.null(inFinalDemandComp))
      return(NULL) 
    
    inAddedValueComp <- input$ioAddedValueComponent
    if(is.null(inAddedValueComp))
      return(NULL)  
    
    ioSector <- read.table(inSector$datapath, header=FALSE, sep=",")
    ioIntermediateDemand <- read.table(inIntermediateDemand$datapath, header=FALSE, sep=",")
    ioFinalDemand <- read.table(inFinalDemand$datapath, header=FALSE, sep=",")
    ioAddedValue <- read.table(inAddedValue$datapath, header=FALSE, sep=",")
    satelliteLabour <- read.table(inLabour$datapath, header=TRUE, sep=",")
    satelliteEnergy <- read.table(inEnergy$datapath, header=TRUE, sep=",")
    satelliteWaste <- read.table(inWaste$datapath, header=TRUE, sep=",")
    satelliteAgriculture <- read.table(inAgriculture$datapath, header=TRUE, sep=",")
    emissionFactorEnergy <- read.table(inEmissionFactorEnergiTable$datapath, header=TRUE, sep=",")
    emissionFactorWaste <- read.table(inEmissionFactorLandWasteTable$datapath, header=TRUE, sep=",")
    emissionFactorAgriculture <- read.table(inEmissionFactorAgricultureTable$datapath, header=TRUE, sep=",")
    ioFinalDemandComponent <- read.table(inFinalDemandComp$datapath, header=FALSE, sep=",")
    ioAddedValueComponent <- read.table(inAddedValueComp$datapath, header=FALSE, sep=",")
    
    # Row explicit definition
    rowImport <- 1
    rowIncome <- 2
    rowProfit <- 3
    
    # initialYear <- input$dateFrom
    # finalYear <- input$dateTo
    # iteration <- finalYear - initialYear
    
    matrixIoIntermediateDemand <- as.matrix(ioIntermediateDemand)
    matrixIoAddedValue <- as.matrix(ioAddedValue)
    nrowMatrixIoAddedValue <- nrow(matrixIoAddedValue)
    ioDimention <- ncol(ioIntermediateDemand)
    
    colSumsMatrixIoIntermediateDemand <- colSums(matrixIoIntermediateDemand)
    colSumsMatrixIoAddedValue <- colSums(matrixIoAddedValue)
    ioTotalOutput <- colSumsMatrixIoIntermediateDemand + colSumsMatrixIoAddedValue # ioTotalInput 
    ioTotalOutputInverse <- 1/ioTotalOutput
    ioTotalOutputInverse[is.infinite(ioTotalOutputInverse)] <- 0
    ioTotalOutputInverse <- diag(ioTotalOutputInverse)
    A <- matrixIoIntermediateDemand %*% ioTotalOutputInverse
    I <- as.matrix(diag(ioDimention))
    ioLeontif <- ioLeontif
    ioLeontiefInverse <- solve(ioLeontif)
    
    # Backward Linkage
    analysisDBL <- colSums(ioLeontiefInverse)
    analysisBPD <- analysisDBL/(mean(analysisDBL))
    # Forward Linkage
    analysisDFL <- rowSums(ioLeontiefInverse)
    analysisFPD <- analysisDFL/(mean(analysisDFL))
    # GDP
    analysisGDP <- colSums(matrixIoAddedValue[rowIncome:nrowMatrixIoAddedValue,])
    analysisTotalGDP <- sum(analysisGDP)
    # Multiplier Output (MO)
    analysisMO <- colSums(ioLeontiefInverse)
    # Coefficient Income (CI) & Multiplier Income (MI)
    analysisCI <- as.matrix(matrixIoAddedValue[rowIncome,]) / ioTotalOutput
    analysisMI <- ioLeontiefInverse %*% analysisCI
    analysisMI[is.na(analysisMI)] <- 0
    # Coefficient Labour (CL) & Multiplier Labour (ML)
    analysisCL <- as.matrix(satelliteLabour[,3]) / ioTotalOutput
    analysisML <- ioLeontiefInverse %*% analysisCL
    analysisML[is.na(analysisML)] <- 0
    # Coefficient Energy Used (CE) & Multiplier Energy (ME)
    analysisCE <- as.matrix(satelliteEnergy[,3]) / ioTotalOutput
    analysisME <- ioLeontiefInverse %*% analysisCE
    analysisME[is.na(analysisME)] <- 0
    # Coefficient Waste Product (CW) & Multiplier Waste (MW)
    analysisCW <- as.matrix(satelliteWaste[,3]) / ioTotalOutput
    analysisMW <- ioLeontiefInverse %*% analysisCW
    analysisMW[is.na(analysisMW)] <- 0
    # Ratio Wages / Business Surplus
    analysisRatioWS <- t(as.matrix(ioAddedValue[2,] / ioAddedValue[3,]))
    analysisRatioWS[is.na(analysisRatioWS)] <- 0
    analysisRatioWS[analysisRatioWS == Inf] <- 0
    colnames(analysisRatioWS) <- "ratio_ws"
    # Satellite account by sectoral GDP
    analysisEnergyByGDP <- as.matrix(satelliteEnergy[,3]) / analysisTotalGDP
    analysisWasteByGDP <- as.matrix(satelliteWaste[,3]) / analysisTotalGDP
    analysisAgricultureByGDP <- as.matrix(satelliteAgriculture[,3]) / analysisTotalGDP
    # Emission from energy
    emissionFactorEnergyDiagonal <- diag(emissionFactorEnergy[,2], ncol = nrow(emissionFactorEnergy), nrow = nrow(emissionFactorEnergy))
    emissionEnergy <- as.matrix(satelliteEnergy[,4:ncol(satelliteEnergy)]) %*% emissionFactorEnergyDiagonal
    emissionEnergyTotal <- rowSums(emissionEnergy)
    # Emission from waste
    emissionFactorWasteDiagonal <- diag(emissionFactorWaste[,2], ncol = nrow(emissionFactorWaste), nrow = nrow(emissionFactorWaste))
    emissionWaste <- as.matrix(satelliteWaste[,4:ncol(satelliteWaste)]) %*% emissionFactorWasteDiagonal
    emissionWasteTotal <- rowSums(emissionWaste)
    # Wages
    analysisWages <- as.matrix(t(ioAddedValue[2,]))
    colnames(analysisWages) <- "wages"
    # Income per capita
    analysisIncomePerCapita <- sum(as.matrix(matrixIoAddedValue[rowIncome,])) / population
    
    result <- cbind(analysisIncomePerCapita,
                    analysisBPD,
                    analysisFPD, 
                    analysisGDP, 
                    analysisMO, 
                    analysisMI,
                    analysisML,
                    analysisME,
                    analysisMW,
                    analysisWages,
                    analysisRatioWS, 
                    analysisCE,
                    analysisCW,
                    emissionEnergyTotal,
                    emissionWasteTotal
    )
    colnames(result)[1] <- "Sektor"
    
    list_table <- list(result=result, 
                       ioSector=ioSector, 
                       ioIntermediateDemand=ioIntermediateDemand, 
                       ioFinalDemand=ioFinalDemand, 
                       ioAddedValue=ioAddedValue, 
                       satelliteLabour=satelliteLabour, 
                       satelliteEnergy=satelliteEnergy, 
                       satelliteWaste=satelliteWaste,
                       satelliteAgriculture=satelliteAgriculture,
                       ioFinalDemandComponent=ioFinalDemandComponent, 
                       ioAddedValueComponent=ioAddedValueComponent,
                       emissionFactorAgriculture=emissionFactorAgriculture,
                       emissionFactorWaste=emissionFactorWaste,
                       emissionFactorEnergy=emissionFactorEnergy,
                       analysisIncomePerCapita=analysisIncomePerCapita
    ) 
    list_table
  })
  
  output$yearIO <- renderText({ paste0("Tahun Tabel IO: ", allDataProv$ioPeriod) })
  
  output$sectorSelection <- renderUI({
    if(debugMode){
      sec <- blackBoxInputs()
    } else {
      sec <- allInputs()
    }
    analysisResult <- sec$result
    selectInput("selectedSector", "Sektor", "Pilih sektor", choices=as.character(analysisResult$Sektor))
  })
  
  output$plotlyResults <- renderPlotly({
    if(debugMode){
      sec <- blackBoxInputs()
    } else {
      sec <- allInputs()
    }
    analysisResult <- sec$result
    analysisIncomePerCapita <- sec$analysisIncomePerCapita
    graph <- data.frame(Sektor="", Analysis="")
    landTable_his<-sec$landTable_his
    
    if(input$categorySector=="Ekonomi"){
      if(input$pprkResults == "PDRB"){
        graph <- subset(analysisResult, select = c(Sektor, analysisGDP))
        GDPvalues <- as.matrix(analysisResult$analysisGDP)
        GDPTotal <- colSums(GDPvalues)
        GDPTotal <- round(GDPTotal,digits = 2)
        #GDPTotalL <- formattable(GDPTotal, digits = 2, format = "f")
        insertUI(
          selector="#placeholder",
          ui = tags$div(
            valueBox(format(GDPTotal, nsmall = 2, big.mark = ".", decimal.mark = ","), "Juta Rupiah", icon = icon("credit-card"), width = 12),
            id='pdrb'
          )
        )
        removeUI(selector = '#capita')
      } else if(input$pprkResults == "Backward Linkage"){
        graph <- subset(analysisResult, select = c(Sektor, analysisBPD))
        removeUI(selector = '#pdrb')
        removeUI(selector = '#capita')
      } else if(input$pprkResults == "Forward Linkage"){
        graph <- subset(analysisResult, select = c(Sektor, analysisFPD))
        removeUI(selector = '#pdrb')
        removeUI(selector = '#capita')
      } else if(input$pprkResults == "Angka Pengganda Output"){
        graph <- subset(analysisResult, select = c(Sektor, analysisMO))
        removeUI(selector = '#pdrb')
        removeUI(selector = '#capita')
      } else if(input$pprkResults == "Angka Pengganda Pendapatan Rumah Tangga"){
        graph <- subset(analysisResult, select = c(Sektor, analysisMI))
        removeUI(selector = '#pdrb')
        removeUI(selector = '#capita')
      } else if(input$pprkResults == "Angka Pengganda Tenaga Kerja"){
        graph <- subset(analysisResult, select = c(Sektor, analysisML))
        removeUI(selector = '#pdrb')
        removeUI(selector = '#capita') 
      } else if(input$pprkResults == "Upah gaji"){
        graph <- subset(analysisResult, select = c(Sektor, wages)) #analysisWages
        removeUI(selector = '#pdrb')
        removeUI(selector = '#capita')
      } else if(input$pprkResults == "Rasio Upah gaji per Surplus Usaha"){
        graph <- subset(analysisResult, select = c(Sektor, ratio_ws)) #analysisRatioWS
        removeUI(selector = '#pdrb')
        removeUI(selector = '#capita')
      } else if(input$pprkResults == "Pendapatan per kapita"){
        removeUI(selector = '#pdrb')
        insertUI(
          selector="#placeholder",
          ui = tags$div(
            valueBox(format(analysisIncomePerCapita, nsmall = 2, big.mark = ".", decimal.mark = ","), "Juta Rupiah/Jiwa", icon = icon("credit-card"), width = 8),
            id='capita'
          )
        )
      } 
      
      if(input$pprkResults == "Perbandingan Angka Pengganda"){
        removeUI(selector = '#pdrb')
        removeUI(selector = '#capita')
        
        # browser()
        multiplierTable <- subset(analysisResult, select = c(Sektor, analysisMI, analysisMO, analysisML, analysisME, analysisMW))
        tabel_radarchart <- multiplierTable[multiplierTable$Sektor==input$selectedSector,]
        
        normalize<- function(x){
          return((x-min(x))/(max(x)-min(x)))
        }
        
        radarchartValue <- as.data.frame(tabel_radarchart[2:6])
        tabel_radar <- normalize(radarchartValue)
        nilai_temp <- t(tabel_radar)
        plot_ly(
          type='scatterpolar',
          r = c(nilai_temp),
          theta = c('analysisMI','analysisMO','analysisML','analysisME','analysisMW'),
          fill='toself'
        ) %>%
          layout(
            polar=list(
              radialaxis=list(
                visible=T,
                range=c(0,1)
              )
            ),
            showlegend=F
          )
        # tabel_radar <- tabel_radarchart
        # tabel_radar$Sektor <- NULL
        # tabel_radarmax <- data.frame(multiplierIncome=max(multiplierTable$multiplierIncome), 
        #                              multiplierOutput=max(multiplierTable$multiplierOutput), 
        #                              multiplierLabour=max(multiplierTable$multiplierLabour), 
        #                              multiplierEnergy=max(multiplierTable$multiplierEnergy),
        #                              multiplierWaste=max(multiplierTable$multiplierWaste) 
        #                              )
        # tabel_radarmin <- data.frame(multiplierIncome=min(multiplierTable$multiplierIncome),  
        #                              multiplierOutput=min(multiplierTable$multiplierOutput),  
        #                              multiplierLabour=min(multiplierTable$multiplierLabour),  
        #                              multiplierEnergy=min(multiplierTable$multiplierEnergy),
        #                              multiplierWaste=min(multiplierTable$multiplierWaste) 
        #                              )
        # tabel_radar <- rbind(tabel_radarmax, tabel_radarmin, tabel_radar)
        # radarchart(tabel_radar)
        
      } else {
        colnames(graph) <- c("Sektor", "Analisis")
        gplot<-ggplot(data=graph, aes(x=Sektor, y=Analisis, fill=Sektor)) +
          geom_bar(stat="identity", colour="black") + theme_void() +
          coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
        ggplotly(gplot)
        
        # plot_ly(data=graph, x = ~Analisis, y = ~Sektor, type = 'bar', orientation = 'h') %>% layout(xaxis = list(title = ""), yaxis = list(title = "", showticklabels=F))
        
        # plot_ly(graph, x=~Analisis, y=~Sektor, fill=~Sektor) %>%
        #   add_bars(orientation = 'h',name=~Sektor) %>%
        #   layout(barmode = 'stack',
        #          xaxis = list(title = "Nilai"),
        #          yaxis = list(title ="Sektor"))
      }
    } else if(input$categorySector=="Energi"){
      if(input$pprkEnergy == "Angka Pengganda Energi"){
        graph <- subset(analysisResult, select = c(Sektor, analysisME))
        removeUI(selector = '#pdrb')
        removeUI(selector = '#capita')
      } else if(input$pprkEnergy == "Koefisien Intensitas Energi"){
        graph <- subset(analysisResult, select = c(Sektor, analysisCE))
        removeUI(selector = '#pdrb')
        removeUI(selector = '#capita')
      } else if(input$pprkEnergy == "Emisi dari Penggunaan Energi"){
        graph <- subset(analysisResult, select = c(Sektor, emissionEnergyTotal))
        removeUI(selector = '#pdrb')
        removeUI(selector = '#capita')
      } 
      
      colnames(graph) <- c("Sektor", "Analisis")
      gplot1<-ggplot(data=graph, aes(x=Sektor, y=Analisis, fill=Sektor)) +
        geom_bar(colour="black", stat="identity") + theme_void() +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      ggplotly(gplot1)
      # plot_ly(graph, x=~Nilai, y=~Sektor, fill=~Sektor) %>%
      #   add_bars(orientation = 'h',name=~Sektor) %>%
      #   layout(barmode = 'stack',
      #          xaxis = list(title = "Nilai"),
      #          yaxis = list(title ="Sektor"))
    } else if(input$categorySector=="Lahan"){
      removeUI(selector = '#pdrb')
      removeUI(selector = '#capita')
      if(input$pprkLand == "Koefisien Kebutuhan Lahan") {
        graph <- subset(landTable_his, select=c(Sektor, Kategori, LRC))
        colnames(graph) <- c("Sektor", "Kategori", "LRC")
        gplot2<-ggplot(data=graph, aes(x=Sektor, y=LRC, fill=Kategori)) +
          geom_bar(colour="black", stat="identity")+ coord_flip() + theme_void() +
          guides(fill=FALSE) + xlab("Sectors") + ylab("Koefisien Kebutuhan Lahan")
        ggplotly(gplot2)
        # plot_ly(graph, x=~LRC, y=~Sektor, fill=~Kategori) %>%
        #   add_bars(orientation = 'h',name=~Kategori) %>%
        #   layout(barmode = 'stack',
        #          xaxis = list(title = "Koefisien Kebutuhan Lahan"),
        #          yaxis = list(title ="Sectors"))
      } else if(input$pprkLand == "Koefisien Produktivitas Lahan") {
        graph <- subset(landTable_his, select=c(Sektor, Kategori, LPC))
        colnames(graph) <- c("Sektor", "Kategori", "LPC")
        gplot2<-ggplot(data=graph, aes(x=Sektor, y=LPC, fill=Kategori)) +
          geom_bar(colour="black", stat="identity")+ coord_flip() + theme_void() +
          guides(fill=FALSE) + xlab("Sektor") + ylab("Koefisien Produktivitas Lahan")
        ggplotly(gplot2)
        # plot_ly(graph, x=~LPC, y=~Sektor, fill=~Kategori) %>%
        #   add_bars(orientation = 'h',name=~Kategori) %>%
        #   layout(barmode = 'stack',
        #          xaxis = list(title = "Koefisien Produktivitas Lahan"),
        #          yaxis = list(title ="Sektor"))
      }
    } else {
      if(input$pprkWaste == "Angka Pengganda Buangan Limbah"){
        graph <- subset(analysisResult, select = c(Sektor, analysisMW))
        removeUI(selector = '#pdrb')
        removeUI(selector = '#capita')
      } else if(input$pprkWaste == "Koefisien Produk Limbah"){
        graph <- subset(analysisResult, select = c(Sektor, analysisCW))
        removeUI(selector = '#pdrb')
        removeUI(selector = '#capita') 
      } else if(input$pprkWaste == "Emisi dari Limbah"){
        graph <- subset(analysisResult, select = c(Sektor, emissionWasteTotal))
        removeUI(selector = '#pdrb')
        removeUI(selector = '#capita')
      }
      
      colnames(graph) <- c("Sektor", "Analisis")
      gplot3<-ggplot(data=graph, aes(x=Sektor, y=Analisis, fill=Sektor)) +
        geom_bar(colour="black", stat="identity") + theme_void() +
        coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      ggplotly(gplot3)
      # plot_ly(graph, x=~Analisis, y=~Sektor, fill=~Sektor) %>%
      #   add_bars(orientation = 'h',name=~Sektor) %>%
      #   layout(barmode = 'stack',
      #          xaxis = list(title = "Nilai"),
      #          yaxis = list(title ="Sektor"))
    }
  })
  
  output$tableDesc <- renderText({
    if(input$categorySector=="Ekonomi"){
      if(input$pprkResults == "PDRB"){
        return(NULL)
      } else if(input$pprkResults == "Backward Linkage"){
        paste0("Direct Backward Linkage (DBL) menunjukkan tingkat keterkaitan kebelakang dari sebuah sektor ekonomi.
               Nilai DBL yang tinggi dari sebuah sektor menunjukkan bahwa sektor tersebut banyak menggunakan output yang dihasilkan oleh sektor lain dalam menghasilkan outputnya sendiri")
      } else if(input$pprkResults == "Forward Linkage"){
        paste0("Direct Forward Linkage (DFL) menunjukkan tingkat keterkaitan kedepan dari sebuah sektor ekonomi.
        Nilai DFL yang tinggi dari sebuah sektor menunjukkan bahwa output dari sektor tersebut banyak digunakan oleh sektor lain.")
      } else if(input$pprkResults == "Angka Pengganda Output"){
        paste0("Angka Pengganda Output menunjukkan dampak perubahan permintaan akhir sebuah sektor terhadap total output masing-masing sektor di sebuah daerah.
        Angka Pengganda Output yang tinggi menunjukkan seberapa besarnya pengaruh sebuah sektor terhadap kondisi perekonomian daerah")
      } else if(input$pprkResults == "Angka Pengganda Pendapatan Rumah Tangga"){
        paste0("Angka Pengganda Pendapatan Rumah Tangga menunjukkan dampak perubahan permintaan akhir sebuah sektor terhadap total income yang dihasilkan masing-masing sektor di sebuah daerah.")
      } else if(input$pprkResults == "Angka Pengganda Tenaga Kerja"){
        paste0("Angka Pengganda Tenaga Kerja menunjukkan dampak perubahan permintaan akhir sebuah sektor ekonomi terhadap penyerapan tenaga kerja suatu provinsi.")
      } else if(input$pprkResults == "Upah gaji"){
        return(NULL)
      } else if(input$pprkResults == "Rasio Upah gaji per Surplus Usaha"){
        return(NULL)
      } else if(input$pprkResults == "Pendapatan per kapita"){
        return(NULL)
      } else if(input$pprkResults == "Perbandingan Angka Pengganda"){
        
      }
    } else if(input$categorySector=="Energi"){
      
    } else if(input$categorySector=="Lahan"){
      
    } else {
      
    }
  })
  
  output$tableResults <- renderDataTable({
    if(debugMode){
      sec <- blackBoxInputs()
    } else {
      sec <- allInputs()
    }
    analysisResult <- sec$result
    landTable_his <- sec$landTable_his
    
    if(input$categorySector=="Ekonomi"){
      if(input$pprkResults == "PDRB"){
        tables <- subset(analysisResult, select = c(Sektor, analysisGDP))
        tables
      } else if(input$pprkResults == "Backward Linkage"){
        tables <- subset(analysisResult, select = c(Sektor, analysisBPD))
        tables
      } else if(input$pprkResults == "Forward Linkage"){
        tables <- subset(analysisResult, select = c(Sektor, analysisFPD))
        tables
      } else if(input$pprkResults == "Angka Pengganda Output"){
        tables <- subset(analysisResult, select = c(Sektor, analysisMO))
        tables
      } else if(input$pprkResults == "Angka Pengganda Pendapatan Rumah Tangga"){
        tables <- subset(analysisResult, select = c(Sektor, analysisMI))
        tables
      } else if(input$pprkResults == "Angka Pengganda Tenaga Kerja"){
        tables <- subset(analysisResult, select = c(Sektor, analysisML))
        tables
      } else if(input$pprkResults == "Upah gaji"){
        tables <- subset(analysisResult, select = c(Sektor, wages)) #analysisWages
        tables
      } else if(input$pprkResults == "Rasio Upah gaji per Surplus Usaha"){
        tables <- subset(analysisResult, select = c(Sektor, ratio_ws)) #analysisRatioWS
        tables
      } else if(input$pprkResults == "Pendapatan per kapita"){
        return(NULL)
      } else if(input$pprkResults == "Perbandingan Angka Pengganda"){
        tables <- multiplierTable <- subset(analysisResult, select = c(Sektor, analysisMI, analysisMO, analysisML, analysisME, analysisMW)) 
        tables
      }
    } else if(input$categorySector=="Energi"){
      if(input$pprkEnergy == "Angka Pengganda Energi"){
        tables <- subset(analysisResult, select = c(Sektor, analysisME))
        tables
      } else if(input$pprkEnergy == "Koefisien Intensitas Energi"){
        tables <- subset(analysisResult, select = c(Sektor, analysisCE))
        tables
      } else if(input$pprkEnergy == "Emisi dari Penggunaan Energi"){
        tables <- subset(analysisResult, select = c(Sektor, emissionEnergyTotal))
        tables
      }
    } else if (input$categorySector=="Lahan"){
      if(input$pprkLand == "Matriks Distribusi Lahan"){
        # removeUI(selector = '#plotlyResults') 
        tables <- subset(landTable_his <- sec$landTable_his, select=-Kategori)
        tables
      } else if(input$pprkLand == "Koefisien Kebutuhan Lahan") {
        tables <- subset(landTable_his <- sec$landTable_his, select=c(Sektor, LRC, Kategori))
        tables
      } else if(input$pprkLand == "Koefisien Produktivitas Lahan") {
        tables <- subset(landTable_his <- sec$landTable_his, select=c(Sektor, LPC, Kategori))
        tables
      } else {
        # removeUI(selector = '#plotlyResults')
        tables <- landTable_his <- sec$landTable_his[,c("Sektor", colnames(landTable_his <- sec$landTable_his)[ncol(landTable_his <- sec$landTable_his)-2])]
        tables
      }
    } else {
      if(input$pprkWaste == "Angka Pengganda Buangan Limbah"){
        tables <- subset(analysisResult, select = c(Sektor, analysisMW))
        tables
      }  else if(input$pprkWaste == "Koefisien Produk Limbah"){
        tables <- subset(analysisResult, select = c(Sektor, analysisCW))
        tables
      }  else if(input$pprkWaste == "Emisi dari Limbah"){
        tables <- subset(analysisResult, select = c(Sektor, emissionWasteTotal))
        tables
      } 
    }
    datatable(tables, extensions = "FixedColumns", options=list(pageLength=100, scrollX=TRUE, scrollY="70vh", fixedColumns=list(leftColumns=1)), rownames=FALSE, height=540) %>%
      formatRound(columns=c(2:length(tables)),2) %>%
      formatStyle(colnames(tables)[2], background = styleColorBar(tables[,2], 'lightblue'), backgroundSize = '98% 88%', backgroundRepeat = 'no-repeat', backgroundPosition = 'center')
  }) #extensions = "FixedColumns", options=list(pageLength=50,scrollX=TRUE, scrollY="600px", fixedColumns=list(leftColumns=1)), rownames=FALSE)
  
  output$downloadTable <- downloadHandler(
    filename = input$pprkResults,
    contentType = "text/csv",
    content = function(file) {
      if(debugMode){
        sec <- blackBoxInputs()
      } else {
        sec <- allInputs()
      }
      analysisResult <- sec$result
      landTable_his <- sec$landTable_his
      
      if(input$categorySector=="Ekonomi"){
        if(input$pprkResults == "PDRB"){
          tables <- subset(analysisResult, select = c(Sektor, analysisGDP))
        } else if(input$pprkResults == "Backward Linkage"){
          tables <- subset(analysisResult, select = c(Sektor, analysisBPD))
        } else if(input$pprkResults == "Forward Linkage"){
          tables <- subset(analysisResult, select = c(Sektor, analysisFPD))
        } else if(input$pprkResults == "Angka Pengganda Output"){
          tables <- subset(analysisResult, select = c(Sektor, analysisMO))
        } else if(input$pprkResults == "Angka Pengganda Pendapatan Rumah Tangga"){
          tables <- subset(analysisResult, select = c(Sektor, analysisMI))
        } else if(input$pprkResults == "Angka Pengganda Tenaga Kerja"){
          tables <- subset(analysisResult, select = c(Sektor, analysisML))
        } else if(input$pprkResults == "Upah gaji"){
          tables <- subset(analysisResult, select = c(Sektor, analysisWages))
        } else if(input$pprkResults == "Rasio Upah gaji per Surplus Usaha"){
          tables <- subset(analysisResult, select = c(Sektor, analysisRatioWS))
        } else if(input$pprkResults == "Pendapatan per kapita"){
          tables <- data.frame(NODATA="")
        } else if(input$pprkResults == "Perbandingan Angka Pengganda"){
          tables <- data.frame(NODATA="")
        }
      } else if(input$categorySector=="Energi"){
        if(input$pprkResults == "Angka Pengganda Energi"){
          tables <- subset(analysisResult, select = c(Sektor, analysisME))
        } else if(input$pprkResults == "Koefisien Intensitas Energi"){
          tables <- subset(analysisResult, select = c(Sektor, analysisCE))
        } else if(input$pprkResults == "Emisi dari Penggunaan Energi"){
          tables <- subset(analysisResult, select = c(Sektor, emissionEnergyTotal))
        } 
      } else if (input$categorySector== "Lahan"){
        if(input$pprkLand == "Matriks Distribusi Lahan"){
          tables <- subset(landTable_his, select=-Kategori)
        } else if(input$pprkLand == "Koefisien Kebutuhan Lahan") {
          tables <- subset(landTable_his, select=c(Sektor, LRC, Kategori))
        } else if(input$pprkLand == "Koefisien Produktivitas Lahan") {
          tables <- subset(landTable_his, select=c(Sektor, LPC, Kategori))
        } else {
          # removeUI(selector = '#plotlyResults')
          tables <- landTable_his[,c("Sektor", colnames(landTable_his)[ncol(landTable_his)-2])]
        }
      } else {
        if(input$pprkResults == "Angka Pengganda Buangan Limbah"){
          tables <- subset(analysisResult, select = c(Sektor, analysisMW))
        } else if(input$pprkResults == "Koefisien Produk Limbah"){
          tables <- subset(analysisResult, select = c(Sektor, analysisCW))
        } else if(input$pprkResults == "Emisi dari Limbah"){
          tables <- subset(analysisResult, select = c(Sektor, emissionWasteTotal))
        }
      }
      write.table(tables, file, quote=FALSE, row.names=FALSE, sep=",")
    }
  )
  
  output$downloadReport <- downloadHandler(
    filename = "report.doc",
    content = function(file){
      file.copy(paste0("data/", allDataProv$prov, "/", allDataProv$prov, "_analisa_deskriptif.doc"), file)
    }
  )
  
  output$tableIO <- renderDataTable({
    if(debugMode){
      sec <- blackBoxInputs()
    } else {
      sec <- allInputs()
    }
    ioSector <- sec$ioSector
    ioIntermediateDemand <- sec$ioIntermediateDemand
    ioFinalDemand <- sec$ioFinalDemand
    ioAddedValue <- sec$ioAddedValue
    ioFinalDemandComponent <- sec$ioFinalDemandComponent
    ioAddedValueComponent <- sec$ioAddedValueComponent
    
    io_table <- cbind(as.data.frame(ioSector[,1]), ioIntermediateDemand)
    colnames(io_table) <- c("Sektor", t(as.data.frame(ioSector[,1])))
    io_table$`Total Permintaan Antara` <- rowSums(ioIntermediateDemand)
    
    colnames(ioFinalDemand) <- c(t(ioFinalDemandComponent))
    ioFinalDemand$`Total Permintaan Akhir` <- rowSums(ioFinalDemand)
    io_table <- cbind(io_table, ioFinalDemand)
    
    total_indem <- colSums(ioIntermediateDemand)
    out_indem <- sum(total_indem)
    total_findem <- colSums(ioFinalDemand)
    out_findem <- sum(total_findem)
    total_all_indem <- as.data.frame(cbind("JUMLAH INPUT ANTARA", t(total_indem), out_indem, t(total_findem)))
    
    colnames(total_all_indem) <- colnames(io_table)
    io_table<-rbind(io_table, total_all_indem)
    
    totalrow_addval <- rowSums(ioAddedValue)
    totalcol_addval <- colSums(ioAddedValue)
    total_addval <- sum(totalrow_addval)
    addval_table <- cbind(ioAddedValueComponent, ioAddedValue, totalrow_addval)
    total_addval_table <- as.data.frame(cbind("JUMLAH INPUT", t(totalcol_addval), total_addval))
    
    remaining_col <- ncol(io_table) - ncol(total_addval_table) 
    for(i in 1:remaining_col){
      eval(parse(text=(paste("addval_table$new_col",  i, "<- ''", sep=""))))
      eval(parse(text=(paste("total_addval_table$new_col",  i, "<- ''", sep=""))))
    }
    colnames(addval_table) <- colnames(io_table)
    colnames(total_addval_table) <- colnames(io_table)
    io_table <- rbind(io_table, addval_table, total_addval_table)
    io_table
    
    datatable(io_table, extensions = "FixedColumns", options=list(paging=FALSE, scrollX=TRUE, scrollY='70vh', fixedColumns=list(leftColumns=1)), rownames=FALSE) %>%
      formatStyle('Sektor',target = "row", backgroundColor = styleEqual(c("JUMLAH INPUT ANTARA"), c('orange'))) %>%
      formatStyle(columns = "Total Permintaan Antara", target = "cell", backgroundColor = "#F7080880") %>%
      formatRound(columns=c(2:length(io_table)),2)
  })
  
  output$SatelitTenagaKerja <- renderDataTable({
    if(debugMode){
      sec <- blackBoxInputs()
    } else {
      sec <- allInputs()
    }
    satelliteLabour <- sec$satelliteLabour
  }, options=list(paging=FALSE, scrollY='70vh'))
  
  output$SatelitEnergi <- renderDataTable({
    if(debugMode){
      sec <- blackBoxInputs()
    } else {
      sec <- allInputs()
    }
    satelliteEnergy   <- sec$satelliteEnergy
  }, extensions = "FixedColumns", options=list(paging = FALSE, scrollY='70vh', scrollX=TRUE, fixedColumns=list(leftColumns=3)))  
  
  output$SatelitLimbah <- renderDataTable({
    if(debugMode){
      sec <- blackBoxInputs()
    } else {
      sec <- allInputs()
    }
    satelliteWaste <- sec$satelliteWaste
  }, extensions = "FixedColumns", options=list(paging = FALSE, scrollY='70vh', scrollX=TRUE, fixedColumns=list(leftColumns=3)))
  
  output$SatelitLahan <- renderDataTable({
    if(debugMode){
      sec <- blackBoxInputs()
    } else {
      sec <- allInputs()
    }
    LDM <- sec$LDM
  }, extensions = "FixedColumns", options=list(paging = FALSE, scrollY='70vh', scrollX=TRUE, fixedColumns=list(leftColumns=3)))
  
  output$TutupanLahan <- renderDataTable({
    if(debugMode){
      sec <- blackBoxInputs()
    } else {
      sec <- allInputs()
    }
    landcover <- sec$landcover
  }, extensions = "FixedColumns", options=list(paging = FALSE, scrollY='70vh', scrollX=TRUE, fixedColumns=list(leftColumns=3)))
  
  ###*bau input####
  generate_table<-function(table, first_year, second_year, value=0.00){
    n <- second_year-first_year
    eval(parse(text=(paste0("table$y", first_year, " <- value"))))
    for(i in 1:n){
      eval(parse(text=(paste0("table$y", first_year+i, " <- value"))))
    }
    table
  }
  observeEvent(input$generateBAUTable, {
    allDataProv$growthRate <- data.frame(Lapangan_usaha=as.character(allDataProv$ioSector[,1])) # reset table
    allDataProv$growthRate <- generate_table(allDataProv$growthRate, as.numeric(input$dateFrom), as.numeric(input$dateTo))
    recordActivities(paste0("Membuat tabel proyeksi BAU tahun ", input$dateFrom, "-", input$dateTo), "Berhasil", paste0(Sys.time()))
    notif_id <<- showNotification("Tabel berhasil dimuat", duration = 4, closeButton = TRUE, type = "warning")
  })
  output$tableBAUType <- renderRHandsontable({
    rhandsontable(allDataProv$growthRate, fixedColumnsLeft=1, height=640) %>% hot_cols(format="0%") # load table
  })
  observeEvent(input$saveTableBauType, {
    # if(input$typeIntervention=='Tipe 1'){
    #   allDataProv$growthRate <- generate_table(allDataProv$growthRate, as.numeric(input$dateFrom), as.numeric(input$dateTo), value=as.numeric(input$gdpRate/100))
    # } 
    if(input$typeIntervention=='Tipe 1'){
      column_year <- paste0("y", input$yearBAUInv)
      growthRate <- allDataProv$growthRate
      eval(parse(text=(paste0("growthRate$", column_year, "<-as.numeric(input$gdpRate/100)"))))
      
      allDataProv$growthRate<-growthRate
    } else {
      allDataProv$growthRate <- hot_to_r(input$tableBAUType)
    }
    # print(allDataProv$growthRate)
    recordActivities(paste0("Menyimpan tabel proyeksi BAU tahun ", input$dateFrom, "-", input$dateTo), "Berhasil", paste0(Sys.time()))
    notif_id <<- showNotification("Tabel berhasil disimpan", duration = 4, closeButton = TRUE, type = "warning")
    # 
    # print(allDataProv$growthRate)
    # 
  })
  
  ### start LAHAN #####
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
  
  ### tampilkan tabel list file LDM###
  output$LDMListTable <- renderDataTable({
    LDMListTableReact()
  }, escape = FALSE)
  
  ### pilh nama file dan file yang akan ditampilkan###
  LDMTableTampil <- eventReactive(input$select_button,{
    sec<-blackBoxInputs()
    LDMProp_his<-sec$LDMProp_his
    selectedRow <- as.numeric(strsplit(input$select_button,"_")[[1]][2])
    
    if(selectedRow==1){
      fileName = "LDM historis"
      selectedFile = LDMProp_his
      list<-list(fileName=fileName,
                 selectedFile=selectedFile)
      print("kondisi1")
    } else if(selectedRow != 1){
      fileName<-LDMListTableReact()[selectedRow,1]
      selectedFile<-readRDS(paste0("LDMData/Prov/",fileName))   # ganti mas alfa
      list<-list(fileName=fileName, selectedFile=selectedFile)
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
              datatable(selectedFile, extensions = "FixedColumns", options=list(pageLength=50, scrollX=TRUE, scrollY="500px", fixedColumns=list(leftColumns=1)), rownames=TRUE))
    } else {
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
    sec<-blackBoxInputs()
    LDMProp_his<-sec$LDMProp_his
    sector<-sec$sector[,1]
    colnamesLDM<-colnames(LDMProp_his)
    showModal(modalDialog(sidebarLayout(sidebarPanel(
      fluidRow(
        selectInput("tupla", label="jenis tutupan lahan", choices=colnamesLDM),
        selectInput("sektor", label="sektor", choices=sector)
      ),
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
    file.remove(paste0("LDMData/Prov/", fileName))   # ganti mas alfa
    
  })
  ###### modal dialog######
  
  # hapus tampilan kolom jika memasukkan 
  observeEvent(c(input$modalLDMbutton, input$tupla, input$sektor), {
    LDMTableTampil<-LDMTableTampil()
    selectedFile<-LDMTableTampil$selectedFile
    fileName<-LDMTableTampil$fileName
    
    removeUI(selector='#LDMUIManual')
    removeUI(selector='#LDMUINormal')
    
    if(is.null(LDMProp_new$tablo) & is.null(LDMProp_new$coba)){
      LDMProp_new$tablo <- selectedFile
    } else if(!is.null(LDMProp_new$tablo) & is.null(LDMProp_new$coba)){
      LDMProp_new$tablo<- selectedFile
    } else if (!is.null(LDMProp_new$coba)){
      LDMProp_new$tablo<-LDMProp_new$coba
    }
    
  })
  
  teksLDM<-reactiveValues(unedited=" ", edited="Total nilai kolom tutupan lahan tidak sama dengan 1")
  
  valLDM<- reactive({
    table_show <- as.matrix(subset(LDMProp_new$tablo, rownames(LDMProp_new$tablo) %in% input$sektor, colnames(LDMProp_new$tablo) %in% input$tupla))
    table_show
  })
  
  output$editLDM <- renderRHandsontable({
    rhandsontable(valLDM(), rowHeaderWidth = 160,) %>% hot_cols(format=3)
  })
  
  output$LDMButton<-renderUI({
    if (is.null(valLDM())){
      return(NULL)
    } else if (!is.null(valLDM)){
      tagList(actionButton('LDMButtonEdit','simpan hasil sunting'),
              tags$div(id='NormOrMan')
      )
    }
  })
  
  #### masukkan nilai sel baru ke dalam kolom tupla 
  LDM_fun<-eventReactive(input$LDMButtonEdit,{
    tablo = LDMProp_new$tablo
    LDM_sel<-as.data.frame(hot_to_r(input$editLDM))
    LDM_sel<-as.numeric(LDM_sel[1,1])
    # inputSektor<-as.character(input$sektor)
    # inputTupla<-as.character(input$tupla)
    inputSektor<-input$sektor
    inputTupla<-input$tupla
    tablo[inputSektor, inputTupla]<-LDM_sel
    tablo<-as.data.frame(tablo)
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
    
    if(tes$totLDMProp_sel==1) {
      insertUI(selector='#NormOrMan',
               where= 'afterEnd',
               ui= tags$div(id='pesanHitungLDMNo', "tidak ada perubahan")
      )
    } else {
      removeUI('#pesanHitungLDMNo')
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
    } else {
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
    namafileLDM<-paste0("username","_","Prov","_",simpanLDM) # ganti mas alfa
    saveRDS(LDMProp_new$coba, file = paste0('LDMData/Prov/',namafileLDM)) #ganti mas alfa
    ldmRV$LDMListFile<-list.files(paste0("LDMData/Prov")) #ganti mas alfa
    ldmRV$LDMTotFile<-length(list.files("LDMData/Prov")) # ganti mas alfa
    removeUI(selector='#pesanHitungLDMNo')
    removeUI(selector='#pesanHitungLDMYes')
    removeUI(selector='#LDMUIManual')
    removeUI(selector='#LDMUINormal')
  })
  
  ###### select tipe proyeksi BAU yang diinginkan 
  observeEvent(input$selectProjType,{
    if(input$selectProjType=="Proyeksi BAU berdasarkan pertumbuhan ekonomi"){
      removeUI(selector='#projTypeLandUI')
      insertUI(selector='#inputProjType',
               where='afterEnd',
               ui= uiOutput('projTypeEconomyUI'))
    }
    else {
      removeUI(selector='#projTypeEconomyUI')
      insertUI(selector='#inputProjType',
               where = 'afterEnd',
               ui=uiOutput('projTypeLandUI'))
      
    }
  })
  
  output$projTypeEconomyUI<-renderUI(
    tagList(menuSubItem("Input ", tabName = "pageFour"),
            selectInput("typeIntervention", "Tipe Intervensi", choices = c("Tipe 1", "Tipe 2")),
            selectInput("dateFrom", "Tahun awal:", choices = 1990:2100, selected=2015),
            selectInput("dateTo", "Tahun akhir:", choices = 1990:2100, selected=2030),
            # fileInput("populationTable", "Tabel Populasi per Tahun", buttonLabel="Browse...", placeholder="No file selected"),
            # fileInput("emissionSectorRADTable", "Tabel Emisi Sumber Lain", buttonLabel="Browse...", placeholder="No file selected"),
            actionButton("generateBAUTable", "Buat Tabel"),
            menuSubItem("Hasil analisis", tabName = "pageFive"),
            selectInput("bauResults",
                        label="Pilih output yang ingin ditampilkan",
                        choices=c("Proyeksi PDRB",
                                  "Proyeksi Upah per Kapita",
                                  "Proyeksi Upah Gaji",
                                  "Proyeksi Tenaga Kerja",
                                  "Proyeksi Konsumsi Energi",
                                  "Proyeksi Emisi Terkait Konsumsi Energi",
                                  "Proyeksi Buangan Limbah",
                                  "Proyeksi Emisi Terkait Buangan Limbah",
                                  "Proyeksi Total Emisi",
                                  "Proyeksi Intensitas Emisi", 
                                  "Proyeksi Tutupan Lahan",
                                  "Proyeksi Emisi Terkait Tutupan Lahan"
                        ))
    )
  )
  
  output$projTypeLandUI<-renderUI(
    tagList(menuSubItem("Input sektor lahan", tabName = "pageNine"),
            menuSubItem("Hasil analisis sektor lahan", tabName = "pageTen"),
            selectInput("lahanResults",
                        label="pilih output sektor lahan yang ingin ditampilkan",
                        choices=c("Proyeksi Output",
                                  "Proyeksi PDRB",
                                  "Proyeksi Income",
                                  "Proyeksi Profit",
                                  "Proyeksi Pajak",
                                  "Proyeksi Impor",
                                  "Proyeksi Ekspor",
                                  "Proyeksi Belanja Pemerintah",
                                  "Proyeksi Belanja Rumah Tangga",
                                  "Proyeksi Tenaga Kerja",
                                  "Proyeksi Neraca Perdagangan"
                        )
            )
    )
  )
  ### end LAHAN ####
  
  
  observeEvent(input$buttonBAU, {
    if(debugMode){
      sec <- blackBoxInputs()
      otherEm <- sec$baselineEmission
      population <- sec$populationProjection
    } else {
      sec <- allInputs()
      inPopTable <- input$populationTable
      if(is.null(inPopTable))
        return(NULL)
      
      inEmOtherTable <- input$emissionSectorRADTable
      if(is.null(inEmOtherTable))
        return(NULL)

      populationProjection <- read.table(inPopTable$datapath, header=TRUE, sep=",")
      baselineEmission <- read.table(inEmOtherTable$datapath, header=TRUE, sep=",")

    }
    ioSector <- sec$ioSector
    ioIntermediateDemand <- sec$ioIntermediateDemand
    ioFinalDemand <- sec$ioFinalDemand
    ioAddedValue <- sec$ioAddedValue
    ioFinalDemandComponent <- sec$ioFinalDemandComponent
    ioAddedValueComponent <- sec$ioAddedValueComponent
    satelliteLabour <- sec$satelliteLabour
    satelliteEnergy <- sec$satelliteEnergy
    emissionFactorEnergy <- sec$emissionFactorEnergy
    satelliteWaste <-sec$satelliteWaste
    emissionFactorWaste <- sec$emissionFactorWaste
    satelliteAgriculture <- sec$satelliteAgriculture
    emissionFactorAgriculture <- sec$emissionFactorAgriculture
    growthRate <- allDataProv$growthRate
    
    LU_tahun<-sec$LU_tahun
    GDPAll<-sec$GDPAll
    tahun<-sec$tahun

    
    # tin cek dulu LDMProp-nya !
    ###### gunakan LDMProp yang ditentukan di menu sebelumnya #####
    if (input$LDMPropUse=="LDM historis"){
      LDMProp=sec$LDMProp_his
    } else {
      LDMProp = readRDS(paste0("LDMData/Prov/",input$LDMPropUse))  #ganti mas alfa
    }

    ###BEGIN: BAU projection####
    
    # Series of GPD & Output projection
    # output$sectorSelection <- renderUI({
    #   if(debugMode){
    #     sec <- blackBoxInputs()
    #   } else {
    #     sec <- allInputs()
    #   }
    #   analysisResult <- sec$result
    #   selectInput("selectedSector", "Sektor", "Pilih sektor", choices=as.character(analysisResult$Sektor))
    # })
    # browser()
  
    ###BEGIN : Define function ####
    functionVar <- blackBoxInputs()
    #1 Function for ...
    functionSatelliteImpact <- function(type = "energy", satellite = data.frame(), matrix_output = matrix(), emission_factor = data.frame()) { 
      impact <- list()
      
      # impact$consumption
      impact$consumption <- satellite
      
      # calculate the proportion
      if(type != "labour"){
        proportionConsumption <- impact$consumption[, 4:ncol(impact$consumption)] / impact$consumption[, 3]
        impact$consumption[, 4:ncol(impact$consumption)] <- proportionConsumption
      }
      
      # calculate the coefficient & the new total satellite consumption 
      coefficientConsumption <- as.matrix(impact$consumption[,3]) / ioTotalOutput
      impact$consumption[,3] <- coefficientConsumption * matrix_output
      
      # calculate emission
      if(type != "labour"){
        
        colnames(impact$consumption)[3] <- "Tconsumption"
        
        # get the new satellite consumption for each sector
        # total consumption * proportion
        impact$consumption[,4:ncol(impact$consumption)] <- impact$consumption[,4:ncol(impact$consumption)] * impact$consumption[, 3]
        
        # checking the order of factor emission 
        orderEnergyType <- names(impact$consumption)[4:ncol(impact$consumption)]
        emissionFactor <- numeric()
        for(m in 1:length(orderEnergyType)){
          emissionFactor <- c(emissionFactor, emission_factor[which(emission_factor[,1]==orderEnergyType[m]), 2])
        }
        emissionFactor <- diag(emissionFactor, nrow = length(emissionFactor), ncol = length(emissionFactor))
        
        # impact$emission
        impact$emission <- impact$consumption
        impact$emission[,4:ncol(impact$emission)] <- as.matrix(impact$consumption[,4:ncol(impact$consumption)]) %*% emissionFactor
        impact$emission[,3] <- rowSums(impact$emission[,4: ncol(impact$emission)])
        impact$emission[is.na(impact$emission)] <- 0
        colnames(impact$emission)[3] <- "Temission"
      } 
      
      impact$consumption[is.na(impact$consumption)] <- 0
      return(impact)
      
    }
  
    #2 Function for calculating Land Requirement Coefficient, Land Requirement, & land Cover
    functionSatelliteLand1<-function(type=NULL, 
                                     matrix_output=NULL, 
                                     advanceMode = FALSE,
                                     currYear= NULL,
                                     runNum = NULL, # input for advanceMode = FALSE, runNUm 1 to 2
                                     LRCRate= NULL){ # input for advanceMode = TRUE, LRCRate sebagai reactive value yang by default diisi LRC historis 
      
      impact<-list()
      if(type=="historis"){
        impact$LRC <- functionVar$analysisLRC ###panggil dari blackbox input
        impact$landReq <- functionVar$landReq_his
        impact$landCover <- functionVar$landCover_his
      } else{
        # browser()
        if(advanceMode== TRUE){
          impact$LRC<-functionVar$analysisLRC*LRCRate^(currYear-functionVar$ioPeriod)
        } else{
          if (runNum == 1 ){
            impact$LRC<-functionVar$analysisLRC*(functionVar$LRCRate_his^(currYear-functionVar$ioPeriod))
          } else if (runNum ==2 ){
            impact$LRC<-functionVar$analysisLRC*(functionVar$LRCRate_2^(currYear-functionVar$ioPeriod))
          }
        }
        # Land Requirement
        impact$landReq<-diag(impact$LRC[,1]) %*% rbind(as.matrix(matrix_output[,1]),0)
        impact$landReq[nrow(as.matrix(impact$landReq)),]<-sum(functionVar$landCover_his[,1])-sum(as.matrix(impact$landReq[1:nrow(as.matrix(impact$landReq))-1,]))
        # Land Cover
        impact$landCover<-functionVar$LDMProp_sektor %*% as.matrix(impact$landReq)
        rownames(impact$landCover)<-colnames(functionVar$LDMProp_his)
        
      }
      
      # Rapikan
      impact$landReq <- data.frame(c(rownames(functionVar$ioSector), nrow(functionVar$ioSector)+1),
                                   c(as.character(functionVar$ioSector[,1]), "lainnya (tidak menghasilkan output)"),
                                   impact$landReq, stringsAsFactors = FALSE)
      
      colnames(impact$landReq)<-c("id.sector", "sector", "land.requirement")
      
      
      impact$landCover <- data.frame(as.character(1:23),
                                     colnames(functionVar$LDMProp_his),
                                     impact$landCover[,1],stringsAsFactors=FALSE)
      colnames(impact$landCover)<-c("id.land.use", "land.use", "luas.land.use")
      
      
      return(impact)
    }
    
    #3 Function for calculating LUTM
    # Land cover yang dihasilkan pada fungsi ini adalah land cover proyeksi + input land cover skenario
    functionSatelliteLand2<- function(type=NULL,
                                      landCoverProjection = NULL,  #proyeksi land cover BAU atau skenario
                                      landCoverProjectionMin=NULL,
                                      inputLandCover=NULL,  #perubahan land cover skenario aksi
                                      LUTMTemplate=NULL,
                                      advanceMode=FALSE, 
                                      percentage=NULL, # input parameter jika advanceMode=TRUE 
                                      runNum=NULL, # input parameter jika advanceMode=FALSE
                                      additionalG=NULL, 
                                      additionalH=NULL,
                                      additionalE=NULL, 
                                      additionalF=NULL,
                                      LUTMChange=NULL,
                                      GDP=NULL,
                                      carbonStock=carbonStock_his){
      
      impact<- list()
      
      if (type=="historis"){
        impact$landCover<-functionVar$landCover_his
        # impact$matrixE<-NULL
        # impact$matrixF<-NULL
        # impact$matrixG<-NULL
        # impact$matrixH<-NULL
        impact$LUTM<-functionVar$LUTM_his
        
      } else{
        
        # set multiiplier for making matrix H
        if(advanceMode==TRUE){
          multiplier <- matrix(percentage, nrow=ncol(functionVar$TPM), ncol=1)
        } else {
          if(runNum==1){ multiplier = 0.8
          } else if (runNum ==2) {multiplier <- 0.5
          } else if (runNum == 3) {multiplier <- 0.3
          } else if (runNum == 4) {multiplier <- 0.1
          } else if (runNum==5) {multiplier <- 0
          } else if (runNum==6) {
            multiplier <- 0.1
            LUTMTemplate <- matrix(NA, nrow=nrow(functionVar$LUTMTemplate_his),ncol=ncol(functionVar$LUTMTemplate_his))
            rownames(LUTMTemplate)<-rownames(functionVar$LUTMTemplate_his)
            colnames(LUTMTemplate)<-colnames(functionVar$LUTMTemplate_his)
            for (i in 1:nrow(sec$landCover_his)){
              if (sum(sec$landCover_his[i,])==0){
                LUTMTemplate[i,]<-matrix(0,ncol=ncol(functionVar$LUTMTemplate_his))    #LUTMTemplate bisa diedit di interface
                LUTMTemplate[,i]<-matrix(0,nrow=ncol(functionVar$LUTMTemplate_his))
              } else {}
            }
            # LUTMTemplate<-read.csv("_TIN/data/JaBar/LUTMTemplate_his2.csv", header=TRUE)
            LUTMTemplate[is.na(LUTMTemplate)]<-paste0("x",1:length(LUTMTemplate[is.na(LUTMTemplate)]))
          }
        }
        
        # landCover 
        if(!is.null(inputLandCover)){
          impact$landCover<-as.matrix(landCoverProjection)+as.matrix(inputLandCover)
        } else{
          impact$landCover<-as.matrix(landCoverProjection)
        }
        
        # LUTM Template
        jumlahVariabel<-length(LUTMTemplate[LUTMTemplate!=0])
        namaVariabel<-paste0("x",1:length(LUTMTemplate[LUTMTemplate!=0]))
        LUTMTemplate[LUTMTemplate!=0]<-namaVariabel
        
        # matrix E
        impact$matrixE<-matrix(NA,nrow = 46, ncol = jumlahVariabel)
        colnames(impact$matrixE)<-namaVariabel
        variabel_x<-list()
        variabel_y<-list()
        for (a in 1:nrow(LUTMTemplate)){  ## constraint 1
          variabel_x[[a]]<-t(LUTMTemplate[a,])[t(LUTMTemplate[a,])!= 0]
          eval(parse(text=paste0("variabel_x_",a,"<-NULL")))
          eval(parse(text=paste0("variabel_x_",a,"<-variabel_x[[",a,"]]")))
          for (i in 1:length(variabel_x[[a]])){
            if(!identical(variabel_x[[a]],c(numeric(0), character(0),integer(0)))){
              eval(parse(text=paste0("impact$matrixE[",a,",paste0(variabel_x_",a,"[",i,"])]<-1")))
              # impact$matrixE[a,paste0(variabel_n[i])]<-1
            } else {impact$matrixE[a,]<-0}
          }
        }
        for (a in 1:ncol(LUTMTemplate)){  ##constraint 2
          variabel_y[[a]]<-t(LUTMTemplate[,a])[t(LUTMTemplate[,a])!= 0]
          eval(parse(text=paste0("variabel_y_",a,"<-NULL")))
          eval(parse(text=paste0("variabel_y_",a,"<-variabel_y[[",a,"]]")))
          for (i in 1:length(variabel_y[[a]])){
            if(!identical(variabel_y[[a]],c(numeric(0), character(0), integer(0)))){
              eval(parse(text=paste0("impact$matrixE[(23+",a,"),paste0(variabel_y_",a,"[",i,"])]<-1")))
              # impact$matrixE[a,paste0(variabel_n[i])]<-1
            } else {impact$matrixE[(23+a),]<-0}
          }
        }
        impact$matrixE[is.na(impact$matrixE)]<-0
        impact$matrixE<-impact$matrixE[(!(rbind(as.matrix(impact$landCover[,1]),as.matrix(impact$landCover[,1]))) == 0),]  #hapus constraint untuk tupla yg jumlahnya 0 agar compatible saat perhitungan LSEI
        if (is.null(additionalE)){
          impact$matrixE<-impact$matrixE
        } else{
          impact$matrixE<- rbind(impact$matrixE, as.matrix(additionalE))
        }
        
        # matrix F
        impact$matrixF<-rbind(landCoverProjectionMin,as.matrix(impact$landCover))
        impact$matrixF<- as.matrix(impact$matrixF[!(rowSums(impact$matrixF) == 0),])
        if (is.null(additionalF)){
          impact$matrixF<-impact$matrixF
        } else{
          impact$matrixF<- rbind(impact$matrixF, as.matrix(additionalF))
        }
        
        # check all diagonal variable names
        diagVariable<-matrix(NA, ncol=1, nrow=ncol(LUTMTemplate))
        for (i in 1:ncol(LUTMTemplate)){
          diagVariable[i,1]<-LUTMTemplate[i,i]
        }
        diagVariable<-diagVariable[!(diagVariable==0),]
        
        # matrix G
        impact$matrixG<-rbind(diag(nrow=(jumlahVariabel)), matrix(0, nrow=length(diagVariable),ncol=jumlahVariabel))  ## buat matrix G constraint 1 & 2
        colnames(impact$matrixG)<-namaVariabel
        for (i in 1:length(diagVariable)){
          impact$matrixG[jumlahVariabel+i,diagVariable[i]]<-1   #assign 1 untuk semua variabel diagonal
        }
        if (is.null(additionalG)){
          impact$matrixG<-impact$matrixG
        } else{
          impact$matrixG<- rbind(impact$matrixG, as.matrix(additionalG))
        }
        
        # get TPM value for each diagonal variable
        diagTPM<-matrix(NA, ncol=1, nrow=ncol(sec$TPM))
        for (i in 1:ncol(sec$TPM)){
          diagTPM[i,1]<-sec$TPM[i,i]
        }
        diagTPM<-as.matrix(diagTPM[!(diagTPM==0),])
        
        # matrix H
        diagTPM <- diagTPM*multiplier 
        impact$matrixH<-rbind(matrix(0,nrow=jumlahVariabel,ncol=1),as.matrix(diagTPM*landCoverProjectionMin[sec$landCover_his!=0]))
        
        if (is.null(additionalH)){
          impact$matrixH<-impact$matrixH
        } else{
          impact$matrixH<- rbind(impact$matrixH, as.matrix(additionalH))
        }
        
        # LUTM dengan metode LSEI
        variabelLSEI<-lsei(E = impact$matrixE, F = impact$matrixF, G=impact$matrixG, H=impact$matrixH)
        variabelLSEI<-as.matrix(unlist(variabelLSEI[["X"]]))
        variabelLSEI<-as.matrix(as.numeric(variabelLSEI[1:jumlahVariabel,]))
        row.names(variabelLSEI)<-namaVariabel
        impact$LUTM<-as.matrix(LUTMTemplate)
        # impact$LUTM<-matrix(ncol=ncol(LUTMTemplate), nrow=nrow(LUTMTemplate))
        # colnames(impact$LUTM)<-colnames(LUTMTemplate)
        # colnames(impact$LUTM)<-rownames(LUTMTemplate)
        for (a in 1:nrow(impact$LUTM)){
          for(b in 1:ncol(impact$LUTM)){
            if (impact$LUTM[a,b]==0){
              impact$LUTM[a,b]<-as.numeric(0)
            } else {impact$LUTM[a,b]<-as.numeric(variabelLSEI[paste0(LUTMTemplate[a,b]),1])
            }
          }
        }
        class(impact$LUTM)<-"numeric"
        if (!is.null(LUTMChange)){
          impact$LUTM<- as.matrix(impact$LUTM)+as.matrix(LUTMChange)
        }
        
      }
      
      # emission
      impact$emission<-matrix(NA,nrow=nrow(as.matrix(impact$LUTM)), ncol=ncol(as.matrix(impact$LUTM)))
      for (a in 1:nrow(impact$LUTM)){
        for (b in 1:ncol(impact$LUTM)){
          impact$emission[a,b]<-as.numeric(impact$LUTM[a,b])*(functionVar$carbonStock_his[b,]-functionVar$carbonStock_his[a,])*3.67*(-1)
        }
      }
      
      impact$emission<-matrix(sum(impact$emission),nrow=nrow(GDP))
      impact$emission<-impact$emission *GDP/sum(GDP)
      
      
      # rapikan
      impact$landCover <- data.frame(as.character(1:23),
                                     colnames(functionVar$LDMProp_his),
                                     impact$landCover[,1],stringsAsFactors=FALSE)
      colnames(impact$landCover)<-c("id.land.use", "land.use", "luas.land.use")
      
      impact$LUTM <- data.frame(as.character(1:23),
                                colnames(functionVar$LDMProp_his),
                                impact$LUTM,stringsAsFactors=FALSE)
      colnames(impact$LUTM)<-c("id.land.use", "land.use", colnames(functionVar$LDMProp_his))
      
      impact$emission <- data.frame(rownames(ioSector),
                                    as.character(ioSector[,1]),
                                    impact$emission,stringsAsFactors=FALSE)
      colnames(impact$emission)<-c("id.sector", "sector", "emission")
      
      
      return(impact)
    }
    ###END : Define function ####
    
    sec <- blackBoxInputs()
    analysisResult <- sec$result
    # ioDimention <- ncol(allDataProv$ioIntermediateDemand)
    # ioPeriod <- allDataProv$ioPeriod
    
    rowImport <- 1
    rowIncome <- 2
    rowProfit <- 3
    
    # browser()
    initialYear <- as.numeric(input$dateFrom)
    finalYear <- as.numeric(input$dateTo)
    iteration <- finalYear - initialYear
    
    bauSeriesOfGDP <- data.frame(Sektor = ioSector[,1], stringsAsFactors = FALSE)
    bauSeriesOfGDP$y2015 <- analysisResult$analysisGDP
    
    # Final Demand
    matrixIoFinalDemand <- as.matrix(sec$ioFinalDemand)
    rowSumsMatrixIoFinalDemand <- as.matrix(rowSums(matrixIoFinalDemand))
    bauSeriesOfFinalDemand <- rowSumsMatrixIoFinalDemand
    
    # Total Output
    matrixIoIntermediateDemand <- as.matrix(sec$ioIntermediateDemand)
    colSumsMatrixIoIntermediateDemand <- colSums(matrixIoIntermediateDemand)
    matrixIoAddedValue <- as.matrix(sec$ioAddedValue)
    colSumsMatrixIoAddedValue <- colSums(matrixIoAddedValue)
    ioTotalOutput <- colSumsMatrixIoIntermediateDemand + colSumsMatrixIoAddedValue 
    bauSeriesOfOutput <- ioTotalOutput
    
    # Series of Intervention Point
    bauSeriesOfIntermediateDemand <- list()
    bauSeriesOfAddedValue <- list()
    bauSeriesOfFinalDemandComponent <- list()
    bauSeriesOfImpactLabour <- list()
    bauSeriesOfImpactEnergy <- list()
    bauSeriesOfImpactWaste <- list()
    bauSeriesOfImpactAgriculture <- list()
    bauSeriesOfImpactLand1<-list()
    bauSeriesOfImpactLand2<-list()
    
    # Historical consumption and emission data
    eval(parse(text=paste0("bauSeriesOfIntermediateDemand$y",sec$ioPeriod," <- matrixIoIntermediateDemand")))
    eval(parse(text=paste0("bauSeriesOfAddedValue$y",sec$ioPeriod," <- matrixIoAddedValue")))
    eval(parse(text=paste0("bauSeriesOfFinalDemandComponent$y",sec$ioPeriod," <- matrixIoFinalDemand")))
    eval(parse(text=paste0("bauSeriesOfImpactLabour$y",sec$ioPeriod," <- functionSatelliteImpact('labour', satellite = satelliteLabour, matrix_output = as.matrix(ioTotalOutput))")))
    eval(parse(text=paste0("bauSeriesOfImpactEnergy$y",sec$ioPeriod,"<- functionSatelliteImpact('energy', satellite = satelliteEnergy, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorEnergy)")))
    eval(parse(text=paste0("bauSeriesOfImpactWaste$y",sec$ioPeriod," <- functionSatelliteImpact('waste', satellite = satelliteWaste, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorWaste)")))
    eval(parse(text=paste0("bauSeriesOfImpactAgriculture$y",sec$ioPeriod,"<- functionSatelliteImpact('agriculture', satellite = satelliteAgriculture, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorAgriculture)")))
    
    # historical LRC, land requirement, & land cover 
    eval(parse(text=paste0("bauSeriesOfImpactLand1$y",sec$ioPeriod,"<-functionSatelliteLand1(type= 'historis', matrix_output= as.matrix(ioTotalOutput))")))
    # LSEI
    eval(parse(text=paste0("bauSeriesOfImpactLand2$y",sec$ioPeriod," <- functionSatelliteLand2(type='historis',carbonStock=carbonStock_his, GDP= as.matrix(bauSeriesOfGDP$y",sec$ioPeriod,") )")))
    
    growthRateSeries <- growthRate
    growthRateSeries$Lapangan_usaha <- NULL
    growthRateSeries <- as.matrix(1+growthRateSeries)
    
    projectionYear <- initialYear
    listYear <- paste0("y", sec$ioPeriod)
    
    # browser()
    # economic & impact (energy, waste, & agriculture projection 
    for(step in 1:(iteration+1)){
      projectionFinalDemand <- growthRateSeries[, step] * bauSeriesOfFinalDemand[, step]
      bauSeriesOfFinalDemand <- cbind(bauSeriesOfFinalDemand, projectionFinalDemand)
      projectionOutput <- allDataProv$ioLeontiefInverse %*% projectionFinalDemand 
      bauSeriesOfOutput <- cbind(bauSeriesOfOutput, projectionOutput)
      
      # notes on the year
      timeStep <- paste0("y", projectionYear)
      
      # add additional values to the list
      eval(parse(text=paste0("bauSeriesOfFinalDemandComponent$", timeStep, " <- as.matrix(sec$proportionFinalDemand*projectionFinalDemand)"))) # contains NaN
      eval(parse(text=paste0("bauSeriesOfIntermediateDemand$", timeStep, " <-  sec$analysisCT %*% diag(as.vector(projectionOutput), ncol = sec$ioDimention, nrow= sec$ioDimention)")))
      eval(parse(text=paste0("bauSeriesOfAddedValue$", timeStep, " <-  sec$analysisCPI %*% diag(as.vector(projectionOutput), ncol = sec$ioDimention, nrow= sec$ioDimention)")))
      
      # GDP projection 
      eval(parse(text = paste0("bauSeriesOfGDP$", timeStep, "<- colSums(bauSeriesOfAddedValue$", timeStep, "[setdiff(1:nrow(matrixIoAddedValue), rowImport),])")))
      
      # Impact projection
      eval(parse(text= paste0("bauSeriesOfImpactLabour$", timeStep, " <- functionSatelliteImpact('labour', satellite = satelliteLabour, matrix_output = as.matrix(projectionOutput))")))
      eval(parse(text= paste0("bauSeriesOfImpactEnergy$", timeStep, " <- functionSatelliteImpact('energy', satellite = satelliteEnergy, matrix_output = as.matrix(projectionOutput), emission_factor = emissionFactorEnergy)")))
      eval(parse(text= paste0("bauSeriesOfImpactWaste$", timeStep, " <- functionSatelliteImpact('waste', satellite = satelliteWaste, matrix_output = as.matrix(projectionOutput), emission_factor = emissionFactorWaste)")))
      eval(parse(text= paste0("bauSeriesOfImpactAgriculture$", timeStep, " <- functionSatelliteImpact('agriculture', satellite = satelliteAgriculture, matrix_output = as.matrix(projectionOutput), emission_factor = emissionFactorAgriculture)")))
      
      listYear <- c(listYear, timeStep)
      projectionYear <- initialYear+step
      
    }
    
    colnames(bauSeriesOfOutput) <- as.character(listYear)
    colnames(bauSeriesOfFinalDemand)<- as.character(listYear)
    
    bauSeriesOfFinalDemandTable <- cbind(data.frame(ioSector$V1), bauSeriesOfFinalDemand)
    colnames(bauSeriesOfFinalDemandTable) <- c("Sektor", as.character(listYear)) 
    
    # land cover projection 
    # non-advance mode
    for (i in 1:2){
      projectionYear <- initialYear
      listYear <- paste0("y", sec$ioPeriod)
      for(step in 1:(iteration+1)){
        # notes on the year
        timeStep <- paste0("y", projectionYear)
        # projection
        eval(parse(text= paste0("bauSeriesOfImpactLand1$", timeStep, " <- functionSatelliteLand1(type= 'projection', 
                                                                                          matrix_output= as.matrix(bauSeriesOfOutput[,'",timeStep,"']), 
                                                                                          advanceMode = FALSE,
                                                                                          currYear=projectionYear,
                                                                                          runNum = ",i,", # input for advanceMode = FALSE
                                                                                          LRCRate= NULL)")))
        listYear <- c(listYear, timeStep)
        projectionYear <- initialYear+step
      } 
      # jika tidak ada value landCover yang negatif, break loop
      if(any(unlist(sapply(bauSeriesOfImpactLand1,'[[', "landCover"))<0)==FALSE){  
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
    if(any(unlist(sapply(bauSeriesOfImpactLand1,'[[', "landCover"))<0)==TRUE){
      repeat{
        # insert UI here to request for new inputLRCRate 
        inputLRCRate<-LRCRate_2  
        projectionYear <- initialYear
        listYear <- paste0("y", ioPeriod)
        for(step in 1:(iteration+1)){
          # notes on the year
          timeStep <- paste0("y", projectionYear)
          eval(parse(text= paste0("bauSeriesOfImpactLand1$", timeStep, " <- functionSatelliteLand1(type= 'projection', 
                                                                                          matrix_output= as.matrix(bauSeriesOfOutput[,'",timeStep,"']), 
                                                                                          advanceMode = TRUE,
                                                                                          runNum = NULL,
                                                                                          LRCRate= inputLRCRate)")))
          listYear <- c(listYear, timeStep)
          projectionYear <- initialYear+step
        }  
        # jika tidak ada value landCover yang negatif, break loop
        if(any(unlist(sapply(bauSeriesOfImpactLand1,'[[', "landCover"))<0)==FALSE){ 
          print("laju perubahan LRC yang digunakan untuk membangun proyeksi tutupan lahan adalah data laju LRC yang telah Anda modifikasi") # use as UI textoutput 
          break
        } else {
          print("proyeksi tutupan lahan yang dihasilkan memiliki luasan negatif. Silakan menyunting ulang laju perubahan LRC dan atau kembali ke target permintaan akhir") # use as UI textoutput 
        }
      }
    }
    
    # LUTM Projection 
    projectionYear <- initialYear
    listYear <- paste0("y", sec$ioPeriod)
    
    for(step in 1:(iteration+1)){
      for (i in 1:6){   # 5 tipe yg akan dirun otomatis
        timeStep <- paste0("y", projectionYear)
  #       eval(parse(text=paste0(
  #       "bauSeriesOfImpactLand2$",timeStep,"<-tryCatch({
  #         functionSatelliteLand2 (type ='projected',
  #                           landCoverProjection = as.matrix(bauSeriesOfImpactLand1[['",timeStep,"']][['landCover']][['luas.land.use']]) ,
  #                           landCoverProjectionMin=  as.matrix(bauSeriesOfImpactLand1[[paste0('y',",projectionYear,"-1)]][['landCover']][['luas.land.use']]),
  #                           LUTMTemplate = LUTMTemplate_his, 
  #                           advanceMode = FALSE,
  #                           runNum =",i," , 
  #                           GDP=as.matrix(bauSeriesOfGDP$",timeStep,")
  #   )
  # }, warning = function (a){NA}, error = function(b){NA})"
  #       )))
        eval(parse(text=paste0(
          "bauSeriesOfImpactLand2$",timeStep,"<-
          functionSatelliteLand2 (type ='projected',
                            landCoverProjection = as.matrix(bauSeriesOfImpactLand1[['",timeStep,"']][['landCover']][['luas.land.use']]) ,
                            landCoverProjectionMin=  as.matrix(bauSeriesOfImpactLand1[[paste0('y',",projectionYear,"-1)]][['landCover']][['luas.land.use']]),
                            LUTMTemplate = sec$LUTMTemplate_his, 
                            advanceMode = FALSE,
                            runNum =",i," , 
                            GDP=as.matrix(bauSeriesOfGDP$",timeStep,")
    )"
        )))
        # if(any(is.na(bauSeriesOfImpactLand2[[timeStep]]))==FALSE){
        #   print(paste0("use constraint ", i ," to make LUTM ",timeStep))
        #   break
        # } else {
        #   if(i==6){
        #     print(paste0("tidak berhasil menghitung LUTM ",timeStep))
        #   } 
        # }
      }
      listYear <- c(listYear, timeStep)
      projectionYear <- initialYear+step
    }
    
    # jika tidak berhasil menghitung LUTM, force to enter advanceMode pada UI (spt pada land cover)
    
    #####END : BAU projection ####
    
    #####BEGIN : BAU projection visualization ####
    # 1. GDP (ind. 1)
    resultGDP <- data.frame(year = 0, sector = "", category="", GDP = 0, stringsAsFactors = FALSE)
    # resultGDP <- data.frame(year = 0, id.sector = 0, sector = "", GDP = 0, stringsAsFactors = FALSE)
    for(c in 2:ncol(bauSeriesOfGDP)){
      add.row <- cbind(ioSector, bauSeriesOfGDP[, c])
      names(add.row) <- c("sector", "category", "GDP")
      add.row$year <- initialYear + (c-3)
      add.row <- add.row[, colnames(resultGDP)]
      resultGDP <- data.frame(rbind(resultGDP, add.row), stringsAsFactors = FALSE)
    }
    resultGDP <- resultGDP[resultGDP$year != 0, ] # remove initial values
    
    # 2. Income per capita (ind. 9)
    resultIncomePerCapita <- data.frame(year = 0, Income.per.capita = 0)
    for(t in 0:iteration){
      t_curr <- initialYear + t
      pop_curr <- sec$populationProjection[which(sec$populationProjection[, 1] == t_curr), 2]
      inc_curr <- sum(bauSeriesOfAddedValue[[t+2]][rowIncome,])
      inc_capita <- inc_curr/pop_curr
      add.row <- data.frame(cbind(t_curr, inc_capita))
      names(add.row) <- names(resultIncomePerCapita)
      resultIncomePerCapita <- data.frame(rbind(resultIncomePerCapita, add.row), stringsAsFactors = FALSE)
    }
    resultIncomePerCapita <- resultIncomePerCapita[resultIncomePerCapita$year != 0, ]
    
    # 3. Wages or Income (ind. 7)
    resultIncome <- data.frame(year = 0, sector= "", income = 0, stringsAsFactors = FALSE)
    sc.name <- ioSector[,1]
    for(t in 0:iteration){
      t_curr <- initialYear + t
      inc_curr <- data.frame(bauSeriesOfAddedValue[[t+2]][rowIncome,])
      add.row <- data.frame(cbind(t_curr, sc.name, inc_curr), stringsAsFactors = FALSE)
      names(add.row) <- names(resultIncome)
      resultIncome <- data.frame(rbind(resultIncome, add.row), stringsAsFactors = FALSE)
    }
    resultIncome <- resultIncome[resultIncome$year != 0, ]
    
    # 4. Labour (ind. number 10)
    resultLabour <- data.frame(year = 0, id.sector = 0, sector= "", labour = 0, stringsAsFactors = FALSE)
    for(t in 0:iteration){
      t_curr <- initialYear + t
      add.row <- data.frame(bauSeriesOfImpactLabour[[t+2]][[1]])
      names(add.row) <- names(resultLabour)[2:4]
      add.row$year <- t_curr
      add.row <- add.row[, names(resultLabour)]
      resultLabour <- data.frame(rbind(resultLabour, add.row), stringsAsFactors = FALSE)
    }
    resultLabour <- resultLabour[resultLabour$year != 0, ]
    
    # 5. Energy cons (indicator number 2)
    resultEnergyConsumption <- bauSeriesOfImpactEnergy[[2]][[1]]
    resultEnergyConsumption$year <- initialYear
    resultEnergyConsumption <- resultEnergyConsumption[, c("year", names(bauSeriesOfImpactEnergy[[2]][[1]]))]
    for(t in 1:iteration){
      t_curr <- initialYear + t
      add.row <- data.frame(bauSeriesOfImpactEnergy[[t+2]][[1]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(resultEnergyConsumption)]
      resultEnergyConsumption <- data.frame(rbind(resultEnergyConsumption, add.row), stringsAsFactors = FALSE)
    }
    names(resultEnergyConsumption)[2:3] <- c("id.sector", "sector")
    
    # 6. Energy emission (indicator number 3)
    resultEnergyEmission <- bauSeriesOfImpactEnergy[[2]][[2]]
    resultEnergyEmission$year <- initialYear
    resultEnergyEmission <- resultEnergyEmission[, c("year", names(bauSeriesOfImpactEnergy[[2]][[2]]))]
    for(t in 1:iteration){
      t_curr <- initialYear + t
      add.row <- data.frame(bauSeriesOfImpactEnergy[[t+2]][[2]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(resultEnergyEmission)]
      resultEnergyEmission <- data.frame(rbind(resultEnergyEmission, add.row), stringsAsFactors = FALSE)
    }
    names(resultEnergyEmission)[2:3] <- c("id.sector", "sector")
    
    # 7. Waste cons (indicator number 2)
    resultWasteDisposal <- bauSeriesOfImpactWaste[[2]][[1]]
    resultWasteDisposal$year <- initialYear
    resultWasteDisposal <- resultWasteDisposal[, c("year", names(bauSeriesOfImpactWaste[[2]][[1]]))]
    for(t in 1:iteration){
      t_curr <- initialYear + t
      add.row <- data.frame(bauSeriesOfImpactWaste[[t+2]][[1]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(resultWasteDisposal)]
      resultWasteDisposal <- data.frame(rbind(resultWasteDisposal, add.row), stringsAsFactors = FALSE)
      
    }
    names(resultWasteDisposal)[2:3] <- c("id.sector", "sector")
    
    # 8. Waste emission (indicator number 3)
    resultWasteEmission <- bauSeriesOfImpactWaste[[2]][[2]]
    resultWasteEmission$year <- initialYear
    resultWasteEmission <- resultWasteEmission[, c("year", names(bauSeriesOfImpactWaste[[2]][[2]]))]
    for(t in 1:iteration){
      t_curr <- initialYear + t
      add.row <- data.frame(bauSeriesOfImpactWaste[[t+2]][[2]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(resultWasteEmission)]
      resultWasteEmission <- data.frame(rbind(resultWasteEmission, add.row), stringsAsFactors = FALSE)
    }
    names(resultWasteEmission)[2:3] <- c("id.sector", "sector")
    
    # 9. Fertilizer cons (indicator number 2)
    resultFertilizerUsed <- bauSeriesOfImpactAgriculture[[2]][[1]]
    resultFertilizerUsed$year <- initialYear
    resultFertilizerUsed <- resultFertilizerUsed[, c("year", names(bauSeriesOfImpactAgriculture[[2]][[1]]))]
    for(t in 1:iteration){
      t_curr <- initialYear + t
      add.row <- data.frame(bauSeriesOfImpactAgriculture[[t+2]][[1]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(resultFertilizerUsed)]
      resultFertilizerUsed <- data.frame(rbind(resultFertilizerUsed, add.row), stringsAsFactors = FALSE)
      
    }
    names(resultFertilizerUsed)[2:3] <- c("id.sector", "sector")
    
    # 10. Fertilizer emission (indicator number 3)
    resultFertilizerEmission <- bauSeriesOfImpactAgriculture[[2]][[2]]
    resultFertilizerEmission$year <- initialYear
    resultFertilizerEmission <- resultFertilizerEmission[, c("year", names(bauSeriesOfImpactAgriculture[[2]][[2]]))]
    for(t in 1:iteration){
      t_curr <- initialYear + t
      add.row <- data.frame(bauSeriesOfImpactAgriculture[[t+2]][[2]]) # [[2]] for emission
      add.row$year <- t_curr
      add.row <- add.row[, names(resultFertilizerEmission)]
      resultFertilizerEmission <- data.frame(rbind(resultFertilizerEmission, add.row), stringsAsFactors = FALSE)
    }
    names(resultFertilizerEmission)[2:3] <- c("id.sector", "sector")
    
    # browser()
    # 11. Land Requirement 
    resultLandReq <- bauSeriesOfImpactLand1[[2]][["landReq"]]
    resultLandReq$year <- initialYear
    resultLandReq <-resultLandReq[,c("year", names(bauSeriesOfImpactLand1[[2]][["landReq"]]))]
    for(t in 1:iteration){
      t_curr <- initialYear + t
      add.row <- data.frame(bauSeriesOfImpactLand1[[t+2]][["landReq"]])
      add.row$year <- t_curr
      add.row <- add.row[,names(resultLandReq)]
      resultLandReq <- data.frame(rbind(resultLandReq, add.row), stringsAsFactors = FALSE)
    }
    
    # 12. Land Cover
    resultLandCover <- bauSeriesOfImpactLand2[[2]][["landCover"]]
    resultLandCover$year <- initialYear
    resultLandCover <-resultLandCover[,c("year", names(bauSeriesOfImpactLand2[[2]][["landCover"]]))]
    for(t in 1:iteration){
      t_curr <- initialYear + t
      add.row <- data.frame(bauSeriesOfImpactLand2[[t+2]][["landCover"]])
      add.row$year <- t_curr
      add.row <- add.row[,names(resultLandCover)]
      resultLandCover <- data.frame(rbind(resultLandCover, add.row), stringsAsFactors = FALSE)
    }
    
    # 13. LUTM
    resultLUTM <- bauSeriesOfImpactLand2[[2]][["LUTM"]]
    resultLUTM$year <- initialYear
    resultLUTM <-resultLUTM[,c("year", names(bauSeriesOfImpactLand2[[2]][["LUTM"]]))]
    for(t in 1:iteration){
      t_curr <- initialYear + t
      add.row <- data.frame(bauSeriesOfImpactLand2[[t+2]][["LUTM"]])
      add.row$year <- t_curr
      add.row <- add.row[,names(resultLUTM)]
      resultLUTM <- data.frame(rbind(resultLUTM, add.row), stringsAsFactors = FALSE)
    }
    
    # 14. Land Emission by sector 
    resultLandEmission <- bauSeriesOfImpactLand2[[2]][["emission"]]
    resultLandEmission$year <- initialYear
    resultLandEmission <-resultLandEmission[,c("year", names(bauSeriesOfImpactLand2[[2]][["emission"]]))]
    for(t in 1:iteration){
      t_curr <- initialYear + t
      add.row <- data.frame(bauSeriesOfImpactLand2[[t+2]][["emission"]])
      add.row$year <- t_curr
      add.row <- add.row[,names(resultLandEmission)]
      resultLandEmission <- data.frame(rbind(resultLandEmission, add.row), stringsAsFactors = FALSE)
    } 
    
    # 15. Total Emission
    # resultTotalEmission <- baselineEmission[which(baselineEmission$Year>=initialYear & baselineEmission$Year<= finalYear),]
    resultTotalEmission <- data.frame(Year=initialYear:finalYear)
    emissionEnergyConsumption <- numeric()
    emissionWasteDisposal <- numeric()
    emissionFertilizer <- numeric()
    emissionLand <- numeric()
    for(t in 0:iteration){
      t_curr <- initialYear + t
      add_MEcons <- sum(resultEnergyEmission[resultEnergyEmission$year==t_curr, "Temission"])
      add_MWdisp <- sum(resultWasteEmission[resultWasteEmission$year==t_curr, "Temission"])
      add_MF <- sum(resultFertilizerEmission[resultFertilizerEmission$year==t_curr, "Temission"])
      add_MLand <-sum(resultLandEmission[resultLandEmission$year==t_curr, "emission"])
      emissionEnergyConsumption <- c(emissionEnergyConsumption, add_MEcons)
      emissionWasteDisposal <- c(emissionWasteDisposal, add_MWdisp)
      emissionFertilizer <- c(emissionFertilizer, add_MF)
      emissionLand<-c(emissionLand, add_MLand)
    }
    resultTotalEmission$emissionEnergyCons <- emissionEnergyConsumption
    resultTotalEmission$emissionWasteDisp <- emissionWasteDisposal
    resultTotalEmission$emissionFert <- emissionFertilizer
    resultTotalEmission$emissionLand <-emissionLand
    resultTotalEmission$TotalEmission <- rowSums(resultTotalEmission[, 2:ncol(resultTotalEmission)])
    resultTotalEmission$CummulativeEmission <- cumsum(resultTotalEmission$TotalEmission)
    
    # 16. BAU emission[economic sector, years]
    bauSeriesOfEmissionBySector <- data.frame(Sektor=ioSector[,1], Kategori=ioSector[,2])
    for(t in 0:iteration){
      t_curr <- initialYear + t
      add_MEcons <- resultEnergyEmission[resultEnergyEmission$year==t_curr, "Temission"]
      add_MWdisp <- resultWasteEmission[resultWasteEmission$year==t_curr, "Temission"]
      add_MF <- resultFertilizerEmission[resultFertilizerEmission$year==t_curr, "Temission"]
      add_MLand <- resultLandEmission[c(resultLandEmission$year==t_curr & resultLandEmission$sector != "lainnya (tidak menghasilkan output"), "emission"]
      eval(parse(text=paste0("bauSeriesOfEmissionBySector$y", t_curr, " <- add_MEcons + add_MWdisp + add_MF + add_MLand")))
    }
    
    # resultTotalGDP <- colSums(bauSeriesOfGDP[,2:(ncol(bauSeriesOfGDP)-1)])
    bauAllResult <- subset(resultTotalEmission, select=c(Year, TotalEmission, CummulativeEmission))
    # bauAllResult <- cbind(bauAllResult, resultTotalGDP)
    bauAllResult$ResultTotalGDP<-colSums(bauSeriesOfGDP[,2:(ncol(bauSeriesOfGDP)-1)])
    bauAllResult$CummulativeGDP <- cumsum(bauAllResult$ResultTotalGDP)
    bauAllResult$EmissionIntensity <- bauAllResult$TotalEmission / bauAllResult$ResultTotalGDP
    bauAllResult$CummulativeEmissionIntensity <-cumsum(bauAllResult$EmissionIntensity)
    
    
    ggplot(data=bauAllResult, aes(x=Year, y=TotalEmission, group=1)) + geom_line() + geom_point()
    ggplot(data=bauAllResult, aes(x=Year, y=CummulativeEmission, group=1)) + geom_line() + geom_point()
    ggplot(data=bauAllResult, aes(x=Year, y=EmissionIntensity, group=1)) + geom_line() + geom_point()
    ggplot(data=bauAllResult, aes(x=Year, y=ResultTotalGDP, group=1)) + geom_line() + geom_point()
    ggplot(data=bauAllResult, aes(x=Year, y=CummulativeGDP, group=1)) + geom_line() + geom_point()
    ggplot(data=bauAllResult, aes(x=Year, y=CummulativeEmissionIntensity, group=1)) + geom_line() + geom_point()
    
    #####END : BAU projection visualization #### 

    recordActivities("Simulasi skenario BAU", "Berhasil", paste0(Sys.time()))
    notif_id <<- showNotification("Simulasi skenario bisnis seperti biasa telah berhasil", duration = 4, closeButton = TRUE, type = "warning")
    
    bauResults$population = sec$populationProjection
    bauResults$otherEm = sec$baselineEmission
    bauResults$GDP_table = resultGDP
    bauResults$income_percapita_table = resultIncomePerCapita
    bauResults$income_table = resultIncome
    bauResults$labour_table = resultLabour
    bauResults$energy_consumption_table = resultEnergyConsumption
    bauResults$energy_emission_table = resultEnergyEmission
    bauResults$waste_disposal_table = resultWasteDisposal
    bauResults$waste_emission_table = resultWasteEmission
    bauResults$total_emission_table = resultTotalEmission
    bauResults$impactLabour = bauSeriesOfImpactLabour
    bauResults$impactEnergy = bauSeriesOfImpactEnergy
    bauResults$impactWaste = bauSeriesOfImpactWaste
    bauResults$GDPSeries = bauSeriesOfGDP
    bauResults$tOutputSeries = bauSeriesOfOutput
    bauResults$FDSeries = bauSeriesOfFinalDemandComponent
    bauResults$IDSeries = bauSeriesOfIntermediateDemand
    bauResults$AVSeries = bauSeriesOfAddedValue
    bauResults$GDP_rate = growthRateSeries
    bauResults$dateTo = finalYear
    bauResults$dateFrom = initialYear
    bauResults$resultLandCover = resultLandCover
    # bauResults$landCover_t1=landCover_t1
    # bauResults$landCover_t1_years=landCover_t1_years
    
    updateTabItems(session, "tabs", selected = "pageFive")
  })
  
  #### BEGIN : input BAU sektor lahan, edit tabel land cover ====
  landCoverTable_0 <- reactive({
    sec <- blackBoxInputs()
    LU_tahun<-sec$LU_tahun
    tahun<-sec$tahun
    colnames(LU_tahun)<-tahun
    sum<-colSums(LU_tahun)
    as.data.frame(rbind(LU_tahun, sum), row.names=c(colnames(allDataProv$LDMProp_his), "total luas"))
  })
  
  landCoverTable_fun <- reactive ({
    if(is.null(input$inputBAULahanLandCover)){return(landCoverTable_0())}
    else if (!identical(landCoverTable_0(), input$inputBAULahanLandCover)){
      landCoverTable_1 <- as.data.frame(hot_to_r(input$inputBAULahanLandCover))
      landCoverTable_1[nrow(landCoverTable_1),]<-colSums(landCoverTable_1[1:nrow(landCoverTable_1)-1,])
      landCoverTable_1
    }
  })
  
  
  output$inputBAULahanLandCover <- renderRHandsontable({
    rhandsontable(landCoverTable_fun(),
                  # fixedColumnsLeft=1,
                  fixedRowsBottom=1,
                  height=640,
                  rowHeaderWidth = 180
                  # )%>% hot_col("sektor", readOnly = TRUE,colWidths=180, worldWrap=TRUE)
    )
  })
  
  observeEvent(input$saveInputBAULahanLandCover,{
    tabLandCover<-hot_to_r(input$inputBAULahanLandCover)
    tabLandCoverMinSum<-tabLandCover[1:nrow(allDataProv$LU_tahun),1:ncol(allDataProv$LU_tahun)]
    editable$BAULahan_landCover<-tabLandCoverMinSum
    notif_id <<- showNotification("Tabel berhasil disimpan", duration = 4, closeButton = TRUE, type = "warning")
  })
  
  #### END : input BAU sektor lahan, edit tabel land cover ====
  
  #### BEGIN : all inputs BAU sektor lahan====
  allInputsBAULahan <- eventReactive(input$buttonBAULahan, {
    if(debugMode){
      sec <- blackBoxInputs()
    } else {
      sec <- allInputs()
    }
    
    GDPAll<-sec$GDPAll
    # landTable_t0<-sec$landTable_t0
    landReq<-sec$landReq
    analysisResult <- sec$result
    analysisML <- analysisResult$analysisML
    ioSector <- sec$ioSector
    kategori <- sec$kategori
    ioIntermediateDemand <- sec$ioIntermediateDemand
    ioFinalDemand <- sec$ioFinalDemand
    ioAddedValue <- sec$ioAddedValue
    ioFinalDemandComponent <- sec$ioFinalDemandComponent
    ioAddedValueComponent <- sec$ioAddedValueComponent
    satelliteLabour <- sec$satelliteLabour
    satelliteEnergy <- sec$satelliteEnergy
    emissionFactorEnergy <- sec$emissionFactorEnergy
    satelliteWaste <-sec$satelliteWaste
    emissionFactorWaste <- sec$emissionFactorWaste
    growthRate <- allDataProv$growthRate
    tahun<-sec$tahun
    # GDPAll<-sec$GDPAll
    # LDMProp<-allDataProv$LDMProp
    analysisCL<-analysisResult$analysisCL
    ioLeontiefInverse<-sec$ioLeontiefInverse
    
    ##### Gunakan input LDM yang sudah diedit 
    if (input$LDMPropUse=="LDM historis"){
      LDMProp=sec$LDMProp_his
    } else {
      LDMProp = readRDS(paste0("LDMData/Prov/",input$LDMPropUse))   # ganti mas alfa
    }
    
    
    ##### Gunakan input Land cover yang sudah diedit
    if(is.null(editable$BAULahan_landCover)){
      LU_tahun <- sec$LU_tahun
    }
    else {
      LU_tahun<-editable$BAULahan_landCover
    }
    
    ### Sektor Lahan: hitung LDM dalam satuan luas, LPC, LRC
    
    LU_tahun<-as.data.frame(LU_tahun)
    LU_tahun<-as.matrix(LU_tahun)
    LDMdimcol<-ncol(LDMProp)
    LDMdimrow<-nrow(LDMProp)
    LDMProp<-as.matrix(LDMProp)
    GDPAll<-as.data.frame(GDPAll)
    #proporis findem/output
    propFindemOutput<- as.matrix(rowSums(findem)/GDPAll[,4])
    
    diagLU <- list()
    landTable<-list()
    landReq<-matrix(nrow=nrow(LDMProp),ncol=ncol(LU_tahun))
    
    for (i in 1:ncol(LU_tahun)){
      diagLU[[i]]<-as.matrix(diag(LU_tahun[,i]))
      landTable[[i]]<-LDMProp%*%diagLU[[i]]
      landReq[,i]<-as.matrix(rowSums(landTable[[i]]))
    }
    
    LPC<-GDPAll[,4]/landReq[,1]
    LPC[is.infinite(LPC)]<-0
    LRC<-1/LPC
    LRC[is.infinite(LRC)]<-0
    landTable_t0<-cbind(sector,kategori, landTable[[1]],landReq[,1], LPC, LRC)
    colnames(landTable_t0)<-c("Sektor", "Kategori",colnames(LDMProp),"Total Kebutuhan Lahan", "LPC", "LRC")
    
    
    #### proyeksi output sektor lahan
    
    #generate tabel findem sektor u/ tiap tahun
    findemLahan_tahun<-matrix(nrow=nrow(landReq), ncol=ncol(landReq))
    for(x in 1:ncol(landReq)){
      findemLahan_tahun[,x]<-landReq[,x]*landTable_t0[,"LPC"]*propFindemOutput
    }
    
    findemLahan_tahun[is.na(findemLahan_tahun)]<-0
    
    # generate tabel output sektor u/ tiap tahun
    outputlahan_tahun<-matrix(nrow=nrow(findemLahan_tahun), ncol=ncol(findemLahan_tahun))
    for(x in 1:ncol(findemLahan_tahun)){
      outputlahan_tahun[,x]<-leontief %*% findemLahan_tahun[,x]
    }
    
    # generate tabel BAU output sektor lahan untuk ditampilkan di shiny
    colnames(outputlahan_tahun)<-tahun
    outputlahan<-data.frame(sector.id=1:nrow(outputlahan_tahun),sector=sector[,1], outputlahan_tahun)
    
    outputlahan_result<-data.frame(year=0, id.sector=0, sector="", Output=0)
    for (x in 3:ncol(outputlahan)){
      newtable<- outputlahan[,c(1,2,x)]
      names(newtable) <- c("id.sector", "sector", "Output")
      yearcol<-(str_extract_all(colnames(outputlahan), '[0-9]+'))
      newtable$year<-yearcol[x]
      newtable<-newtable[,colnames(outputlahan_result)]
      outputlahan_result<-data.frame(rbind(newtable, outputlahan_result))
    }
    
    outputlahan_table <- outputlahan_result[outputlahan_result$year != 0, ] # remove initial values
    
    ##### proyeksi lain2
    landProp<-as.data.frame(cbind(
      GDPAll$GDP/GDPAll$OUTPUT,
      t(addval[2,]/GDPAll$OUTPUT),
      t(addval[3,]/GDPAll$OUTPUT),
      t(addval[5,]/GDPAll$OUTPUT),
      t(addval[1,]/GDPAll$OUTPUT),
      findem[,5]/GDPAll$OUTPUT,
      findem[,2]/GDPAll$OUTPUT,
      findem[,1]/GDPAll$OUTPUT,
      labour_coef
    ))
    
    landProp[is.na(landProp)]<-0
    colnames(landProp)<-c("PDRB",
                          "income",
                          "profit",
                          "pajak",
                          "impor",
                          "ekspor", 
                          "belanja_pemerintah", 
                          "belanja_RT",
                          "labour"
    )
    landProp_name<-colnames(landProp)
    
    # generate tabel proyeksi BAU tiap indikator per tahun & sektor (landProp * outputlahan_tahun)
    for(b in 1:ncol(landProp)){
      eval(parse(text=(paste0("tabel_",b, "<- matrix(nrow=nrow(outputlahan_tahun), ncol=ncol(outputlahan_tahun))"))))
      eval(parse(text=(paste0("BAULahan_",b,"<- matrix(nrow=ncol(outputlahan_tahun), ncol=2)"))))
      for (c in 1:ncol(outputlahan_tahun)){
        #tabel indikator ekonomi per tahun & sektor
        eval(parse(text=paste0("tabel_",b,"[,c]<-landProp[,",b,"]*outputlahan_tahun[,c]")))
        eval(parse(text = paste0("tabel_",b,"[is.na(tabel_",b,")]<-0")))
        #tabel total indikator tiap tahun
        eval(parse(text= paste0("BAULahan_",b,"<-cbind(tahun,as.data.frame(colSums(tabel_",b,")))")))
        #tabel untuk ditampilkan di shiny
      }
      eval(parse(text=(paste0("colnames(BAULahan_",b,")=c('year',landProp_name[",b,"])"))))
      eval(parse(text=(paste0("colnames(tabel_",b,")<-tahun"))))
      eval(parse(text=(paste0("lahan_",b,"<-data.frame(sector.id=1:nrow(tabel_",b,"),sector=sector[,1], tabel_",b,")"))))
      eval(parse(text=(paste0('lahanResult_',b,'<-data.frame(year=0, id.sector=0, sector="",indikator=0)'))))
      eval(parse(text=(paste0("newtable_",b,"<-data.frame(id.sector=0,sector='', indikator=0, year=0)"))))
      
    }
    
    tabel_list <- list (tabel_1,
                        tabel_2,
                        tabel_3, 
                        tabel_4,
                        tabel_5,
                        tabel_6,
                        tabel_7,
                        tabel_8,
                        tabel_9
    )
    lahan_list <- list( lahan_1, 
                        lahan_2, 
                        lahan_3, 
                        lahan_4, 
                        lahan_5, 
                        lahan_6, 
                        lahan_7, 
                        lahan_8,
                        lahan_9
    )
    newtable_list<-list(newtable_1, 
                        newtable_2, 
                        newtable_3, 
                        newtable_4, 
                        newtable_5, 
                        newtable_6, 
                        newtable_7, 
                        newtable_8,
                        newtable_9
    )    
    lahanResult_list<-list(lahanResult_1,
                           lahanResult_2,
                           lahanResult_3,
                           lahanResult_4,
                           lahanResult_5,
                           lahanResult_6,
                           lahanResult_7,
                           lahanResult_8,
                           lahanResult_9
    )
    
    for (i in 1:length(tabel_list)){
      for(x in 3:ncol(lahan_list[[i]])){
        newtable_list[[i]]<- lahan_list[[i]][,c(1,2,x)]
        names(newtable_list[[i]]) <- c("id.sector", "sector", "indikator")
        yearcol<-(str_extract_all(colnames(outputlahan), '[0-9]+'))
        newtable_list[[i]]$year<-as.double(yearcol[x])
        newtable_list[[i]]<-newtable_list[[i]][,colnames(lahanResult_list[[i]])]
        lahanResult_list[[i]]<-data.frame(rbind(newtable_list[[i]], lahanResult_list[[i]]))
      }
      eval(parse(text=(paste0("lahanResult_",i,"<-matrix(unlist(lahanResult_list[[",i,"]]), byrow=TRUE)"))))
      eval(parse(text=(paste0("lahanResult_list[[",i,"]]<-as.matrix(lahanResult_list[[",i,"]], header=TRUE)"))))
      eval(parse(text=(paste0("lahanResult_",i,"<-matrix(unlist(lahanResult_list[[",i,"]]),ncol=4)"))))
      eval(parse(text=(paste0('colnames(lahanResult_',i,')<-c("year", "id.sector", "sector",landProp_name[',i,'])'))))
    }
    
    BAULahan_0<-as.data.frame(cbind(tahun,colSums(outputlahan_tahun)))
    colnames(BAULahan_0)<-c("year","output")
    
    listBAU_lahan<-list(BAULahan_0=BAULahan_0,
                        BAULahan_1=BAULahan_1,
                        BAULahan_2=BAULahan_2,
                        BAULahan_3=BAULahan_3,
                        BAULahan_4=BAULahan_4,
                        BAULahan_5=BAULahan_5,
                        BAULahan_6=BAULahan_6,
                        BAULahan_7=BAULahan_7,
                        BAULahan_8=BAULahan_8,
                        BAULahan_9=BAULahan_9,
                        lahanResult_0=outputlahan_result,
                        lahanResult_1=lahanResult_1,
                        lahanResult_2=lahanResult_2,
                        lahanResult_3=lahanResult_3,
                        lahanResult_4=lahanResult_4,
                        lahanResult_5=lahanResult_5,
                        lahanResult_6=lahanResult_6,
                        lahanResult_7=lahanResult_7,
                        lahanResult_8=lahanResult_8,
                        lahanResult_9=lahanResult_9,
                        tahun=tahun, 
                        landTable_t0=landTable_t0
    )
    listBAU_lahan
  })
  #### END : all inputs BAU sektor lahan ####
  
  output$yearSelection <- renderUI({
    selectInput("selectedYear", "Tahun", "Pilih tahun", choices=c(input$dateFrom:input$dateTo))
  })
  
  output$plotlyResultsBAU <- renderPlotly({
    GDP_table <- bauResults$GDP_table
    income_percapita_table <- bauResults$income_percapita_table  
    income_table <- bauResults$income_table
    labour_table <- bauResults$labour_table
    energy_consumption_table <- bauResults$energy_consumption_table 
    energy_emission_table <- bauResults$energy_emission_table 
    waste_disposal_table <- bauResults$waste_disposal_table  
    waste_emission_table <- bauResults$waste_emission_table 
    total_emission_table <- bauResults$total_emission_table
    # landCover_t1 <- bauResults$landCover_t1
    # landCover_t1_years <- bauResults$landCover_t1_years
    
    if(input$bauResults == "Proyeksi PDRB"){
      removeUI(selector = '#baupdrb')
      graph <- GDP_table[GDP_table$year==input$selectedYear,]
      GDPvalues <- as.matrix(graph$GDP)
      GDPTotal <- colSums(GDPvalues)
      insertUI(
        selector="#bauplaceholder",
        ui = tags$div(
          valueBox(format(GDPTotal, nsmall = 1, big.mark = ","), "Juta Rupiah", icon = icon("credit-card"), width = 8),
          id='baupdrb'
        )
      )
      # ggplot(data=graph, aes(x=sector, y=GDP)) + 
      #   geom_bar(colour="blue", stat="identity") + 
      #   coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      GDP_all <- aggregate(x = GDP_table$GDP, by = list(GDP_table$year), FUN = sum)
      colnames(GDP_all) = c("year", "PDRB")
      gplot4<-ggplot(data=GDP_all, aes(x=year, y=PDRB, group=1)) + geom_line() + geom_point()
      ggplotly(gplot4)
      # plot_ly(GDP_all, x = ~year, y = ~PDRB, type = 'scatter', mode = 'lines')
      
    } else if(input$bauResults == "Proyeksi Upah per Kapita"){
      removeUI(selector = '#baupdrb')
      gplot5<-ggplot(data=income_percapita_table, aes(x=year, y=Income.per.capita, group=1)) + geom_line() + geom_point()
      ggplotly(gplot5)
      
    } else if(input$bauResults == "Proyeksi Upah Gaji"){
      removeUI(selector = '#baupdrb')
      graph <- income_table[income_table$year==input$selectedYear,]
      # ggplot(data=graph, aes(x=sector, y=income)) +
      #   geom_bar(colour="blue", stat="identity") +
      #   coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      income_all <- aggregate(x = income_table$income, by = list(income_table$year), FUN = sum)
      colnames(income_all) = c("year", "income")
      gplot6<-ggplot(data=income_all, aes(x=year, y=income, group=1)) + geom_line() + geom_point()
      ggplotly(gplot6)
      
    } else if(input$bauResults == "Proyeksi Tenaga Kerja"){
      removeUI(selector = '#baupdrb')
      graph <- labour_table[labour_table$year==input$selectedYear,]
      # ggplot(data=graph, aes(x=sector, y=labour)) +
      #   geom_bar(colour="blue", stat="identity") +
      #   coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      labour_all <- aggregate(x = labour_table$labour, by = list(labour_table$year), FUN = sum)
      colnames(labour_all) = c("year", "Labour")
      gplot7<-ggplot(data=labour_all, aes(x=year, y=Labour, group=1)) + geom_line() + geom_point()
      ggplotly(gplot7)
      
      
    } else if(input$bauResults == "Proyeksi Konsumsi Energi"){
      removeUI(selector = '#baupdrb')
      graph <- energy_consumption_table[energy_consumption_table$year==input$selectedYear,]
      # ggplot(data=graph, aes(x=sector, y=Tconsumption)) +
      #   geom_bar(colour="blue", stat="identity") +
      #   coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      energy_all <- aggregate(x = energy_consumption_table$Tconsumption, by = list(energy_consumption_table$year), FUN = sum)
      colnames(energy_all) = c("year", "Energy")
      gplot8<-ggplot(data=energy_all, aes(x=year, y=Energy, group=1)) + geom_line() + geom_point()
      ggplotly(gplot8)
      
    } else if(input$bauResults == "Proyeksi Emisi Terkait Konsumsi Energi"){
      removeUI(selector = '#baupdrb')
      graph <- energy_emission_table[energy_emission_table$year==input$selectedYear,]
      # ggplot(data=graph, aes(x=sector, y=Temission)) +
      #   geom_bar(colour="blue", stat="identity") +
      #   coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      em_energy_all <- aggregate(x = energy_emission_table$Temission, by = list(energy_emission_table$year), FUN = sum)
      colnames(em_energy_all) = c("year", "EmEnergy")
      gplot9<-ggplot(data=em_energy_all, aes(x=year, y=EmEnergy, group=1)) + geom_line() + geom_point()
      ggplotly(gplot9)
      
    } else if(input$bauResults == "Proyeksi Buangan Limbah"){
      removeUI(selector = '#baupdrb')
      graph <- waste_disposal_table[waste_disposal_table$year==input$selectedYear,]
      # ggplot(data=graph, aes(x=sector, y=Tconsumption)) +
      #   geom_bar(colour="blue", stat="identity") +
      #   coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      waste_all <- aggregate(x = waste_disposal_table$Tconsumption, by = list(waste_disposal_table$year), FUN = sum)
      colnames(waste_all) = c("year", "Waste")
      gplot10<-ggplot(data=waste_all, aes(x=year, y=Waste, group=1)) + geom_line() + geom_point()
      ggplotly(gplot10)
      
    } else if(input$bauResults == "Proyeksi Emisi Terkait Buangan Limbah"){
      removeUI(selector = '#baupdrb')
      graph <- waste_emission_table[waste_emission_table$year==input$selectedYear,]
      # ggplot(data=graph, aes(x=sector, y=Temission)) +
      #   geom_bar(colour="blue", stat="identity") +
      #   coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
      em_waste_all <- aggregate(x = waste_emission_table$Temission, by = list(waste_emission_table$year), FUN = sum)
      colnames(em_waste_all) = c("year", "EmWaste")
      gplot11<-ggplot(data=em_waste_all, aes(x=year, y=EmWaste, group=1)) + geom_line() + geom_point()
      ggplotly(gplot11)
      
    } else if(input$bauResults == "Proyeksi Total Emisi"){
      removeUI(selector = '#baupdrb')
      gplot12<-ggplot(data=total_emission_table[total_emission_table$Year > input$dateFrom,], aes(x=Year, y=TotalEmission, group=1)) + geom_line() + geom_point()
      ggplotly(gplot12)
    } else if(input$bauResults == "Proyeksi Intensitas Emisi"){
      removeUI(selector = '#baupdrb')
      GDP_all <- aggregate(x = GDP_table$GDP, by = list(GDP_table$year), FUN = sum)
      colnames(GDP_all) = c("year", "PDRB")
      GDP_all$emisi <- total_emission_table$TotalEmission
      GDP_all$intensitas <-  GDP_all$emisi / GDP_all$PDRB 
      gplot13<-ggplot(data=GDP_all[GDP_all$year > input$dateFrom,], aes(x=year, y=intensitas, group=1)) + geom_line() + geom_point()
      ggplotly(gplot13)
    } else if(input$bauResults=="Proyeksi Tutupan Lahan"){
      removeUI(selector='#baupdrb')
      landCoverData<-cbind(landCover_t1_years,colSums(landCover_t1))
      colnames(landCoverData)<-c("year", "Tutupan_Lahan")
      landCover_plot<-ggplot(data=landCoverData, aes(x=year, y=Tutupan_Lahan, group=1)) + geom_line() + geom_point()
      ggplotly(landCover_plot)
    } 
    # else if(input$bauResults=="Proyeksi Emisi Terkait Tutupan Lahan"){
    #   removeUI(selector='#baupdrb')
    #   landCoverData<-cbind(landCover_t1_years,colSums(landCover_t1))
    #   colnames(landCoverData)<-c("year", "Tutupan_Lahan")
    #   landCover_plot<-ggplot(data=landCoverData, aes(x=year, y=Tutupan_Lahan, group=1)) + geom_line() + geom_point()
    #   ggplotly(landCover_plot)
    # }
    
    
  })
  
  output$tableResultsBAU <- renderDataTable({
    GDP_table <- bauResults$GDP_table
    income_percapita_table <- bauResults$income_percapita_table  
    income_table <- bauResults$income_table
    labour_table <- bauResults$labour_table
    energy_consumption_table <- bauResults$energy_consumption_table 
    energy_emission_table <- bauResults$energy_emission_table 
    waste_disposal_table <- bauResults$waste_disposal_table  
    waste_emission_table <- bauResults$waste_emission_table 
    total_emission_table <- bauResults$total_emission_table
    resultLandCover <- bauResults$resultLandCover
    
    if(input$bauResults == "Proyeksi PDRB"){
      tables <- GDP_table[GDP_table$year==input$selectedYear,]
      tables
    } else if(input$bauResults == "Proyeksi Upah per Kapita"){
      return(NULL)
    } else if(input$bauResults == "Proyeksi Upah Gaji"){
      tables <- income_table[income_table$year==input$selectedYear,]
      tables
    } else if(input$bauResults == "Proyeksi Tenaga Kerja"){
      tables <- labour_table[labour_table$year==input$selectedYear,]
      tables
    } else if(input$bauResults == "Proyeksi Konsumsi Energi"){
      tables <- energy_consumption_table[energy_consumption_table$year==input$selectedYear,]
      tables
    } else if(input$bauResults == "Proyeksi Emisi Terkait Konsumsi Energi"){
      tables <- energy_emission_table[energy_emission_table$year==input$selectedYear,]
      tables
    } else if(input$bauResults == "Proyeksi Buangan Limbah"){
      tables <- waste_disposal_table[waste_disposal_table$year==input$selectedYear,]
      tables
    } else if(input$bauResults == "Proyeksi Emisi Terkait Buangan Limbah"){
      tables <- waste_emission_table[waste_emission_table$year==input$selectedYear,]
      tables
    } else if(input$bauResults == "Proyeksi Total Emisi"){
      return(NULL)
    } else if(input$bauResults == "Proyeksi Intensitas Emisi"){
      return(NULL)
    } else if (input$bauResults=='Proyeksi Tutupan Lahan'){
      tables <- resultLandCover[resultLandCover$year==input$selectedYear,]
      tables 
    }
    datatable(tables, extensions = "FixedColumns", options=list(pageLength=100, scrollX=TRUE, scrollY="500px", fixedColumns=list(leftColumns=1)), rownames=FALSE)%>%
      formatRound(columns=c(3:length(tables)),2)
  }) #extensions = "FixedColumns", options=list(pageLength=50, scrollX=TRUE, scrollY="600px", fixedColumns=list(leftColumns=1)), rownames=FALSE)  
  
  output$downloadTableBAU <- downloadHandler(
    filename = input$bauResults,
    contentType = "text/csv",
    content = function(file) {
      GDP_table <- bauResults$GDP_table
      income_percapita_table <- bauResults$income_percapita_table  
      income_table <- bauResults$income_table
      labour_table <- bauResults$labour_table
      energy_consumption_table <- bauResults$energy_consumption_table 
      energy_emission_table <- bauResults$energy_emission_table 
      waste_disposal_table <- bauResults$waste_disposal_table  
      waste_emission_table <- bauResults$waste_emission_table 
      total_emission_table <- bauResults$total_emission_table
      
      if(input$bauResults == "Proyeksi PDRB"){
        tables <- GDP_table[GDP_table$year==input$selectedYear,]
      } else if(input$bauResults == "Proyeksi Upah per Kapita"){
        return(NULL)
      } else if(input$bauResults == "Proyeksi Upah Gaji"){
        tables <- income_table[income_table$year==input$selectedYear,]
      } else if(input$bauResults == "Proyeksi Tenaga Kerja"){
        tables <- labour_table[labour_table$year==input$selectedYear,]
      } else if(input$bauResults == "Proyeksi Konsumsi Energi"){
        tables <- energy_consumption_table[energy_consumption_table$year==input$selectedYear,]
      } else if(input$bauResults == "Proyeksi Emisi Terkait Konsumsi Energi"){
        tables <- energy_emission_table[energy_emission_table$year==input$selectedYear,]
      } else if(input$bauResults == "Proyeksi Buangan Limbah"){
        tables <- waste_disposal_table[waste_disposal_table$year==input$selectedYear,]
      } else if(input$bauResults == "Proyeksi Emisi Terkait Buangan Limbah"){
        tables <- waste_emission_table[waste_emission_table$year==input$selectedYear,]
      } else if(input$bauResults == "Proyeksi Total Emisi"){
        return(NULL)
      }
      write.table(tables, file, quote=FALSE, row.names=FALSE, sep=",")
    }
  )  
  
  #### BEGIN : tampilkan result BAU sektor lahan====
  output$yearSelection2 <- renderUI({
    sec<-blackBoxInputs()
    tahun<-sec$tahun
    selectInput("selectedYear2", "Tahun", "Pilih tahun", choices=c(tahun))
  }) 
  
  output$plotlyResultsBAU_lahan <- renderPlotly({
    results2 <- allInputsBAULahan()
    OutputLahan_plot<-results2$BAULahan_0
    PDRBLahan_plot <-results2$BAULahan_1
    incomeLahan_plot<-results2$BAULahan_2
    profitLahan_plot<-results2$BAULahan_3
    pajakLahan_plot<-results2$BAULahan_4
    imporLahan_plot<-as.data.frame(results2$BAULahan_5)
    eksporLahan_plot<-as.data.frame(results2$BAULahan_6)
    belpemLahan_plot<-results2$BAULahan_7
    belrtLahan_plot<-results2$BAULahan_8
    labourLahan_plot<-results2$BAULahan_9
    tahun<-as.data.frame(results2$tahun)
    ekspor_impor<-as.vector(eksporLahan_plot$ekspor) - as.vector(imporLahan_plot$impor)
    neracaLahan_plot <-as.data.frame(cbind(tahun,ekspor_impor))
    colnames(neracaLahan_plot)<-c("year", "ekspor_impor")
    
    if(input$lahanResults=="Proyeksi Output"){
      removeUI(selector = '#baupdrb')
      OutputLahan_gplot<-ggplot(data=OutputLahan_plot, aes(x=year, y=output, group=1)) + geom_line() + geom_point()
      ggplotly(OutputLahan_gplot)
    } 
    else if(input$lahanResults=="Proyeksi PDRB"){
      removeUI(selector = '#baupdrb')
      PDRBLahan_gplot<-ggplot(data=PDRBLahan_plot, aes(x=year, y=PDRB, group=1)) + geom_line() + geom_point()
      ggplotly(PDRBLahan_gplot)
    } 
    else if (input$lahanResults=="Proyeksi Income"){
      removeUI(selector = '#baupdrb')
      incomeLahan_gplot<-ggplot(data=incomeLahan_plot, aes(x=year, y=income, group=1)) + geom_line() + geom_point()
      ggplotly(incomeLahan_gplot)
    }
    else if (input$lahanResults=="Proyeksi Profit"){
      removeUI(selector = '#baupdrb')
      profitLahan_gplot<-ggplot(data=profitLahan_plot, aes(x=year, y=profit, group=1)) + geom_line() + geom_point()
      ggplotly(profitLahan_gplot)
    }
    else if (input$lahanResults=="Proyeksi Pajak"){
      removeUI(selector = '#baupdrb')
      pajakLahan_gplot<-ggplot(data=pajakLahan_plot, aes(x=year, y=pajak, group=1)) + geom_line() + geom_point()
      ggplotly(pajakLahan_gplot)
    }
    else if (input$lahanResults=="Proyeksi Impor"){
      removeUI(selector = '#baupdrb')
      imporLahan_gplot<-ggplot(data=imporLahan_plot, aes(x=year, y=impor, group=1)) + geom_line() + geom_point()
      ggplotly(imporLahan_gplot)
    }
    else if (input$lahanResults=="Proyeksi Ekspor"){
      removeUI(selector = '#baupdrb')
      eksporLahan_gplot<-ggplot(data=eksporLahan_plot, aes(x=year, y=ekspor, group=1)) + geom_line() + geom_point()
      ggplotly(eksporLahan_gplot)
    }
    else if (input$lahanResults=="Proyeksi Belanja Pemerintah"){
      removeUI(selector = '#baupdrb')
      belpemLahan_gplot<-ggplot(data=belpemLahan_plot, aes(x=year, y=belanja_pemerintah, group=1)) + geom_line() + geom_point()
      ggplotly(belpemLahan_gplot)
    }
    else if (input$lahanResults=="Proyeksi Belanja Rumah Tangga"){
      removeUI(selector = '#baupdrb')
      belrtLahan_gplot<-ggplot(data=belrtLahan_plot, aes(x=year, y=belanja_RT, group=1)) + geom_line() + geom_point()
      ggplotly(belrtLahan_gplot)
    }
    else if (input$lahanResults=="Proyeksi Tenaga Kerja"){
      removeUI(selector = '#baupdrb')
      labourLahan_gplot<-ggplot(data=labourLahan_plot, aes(x=year, y=labour, group=1)) + geom_line() + geom_point()
      ggplotly(labourLahan_gplot)
    }
    else if (input$lahanResults=="Proyeksi Neraca Perdagangan"){
      removeUI(selector = '#baupdrb')
      neracaLahan_gplot<-ggplot(data=neracaLahan_plot, aes(x=year, y=ekspor_impor, group=1)) + geom_line() + geom_point()
      ggplotly(neracaLahan_gplot)
    }
  })
  output$tableResultsBAU_lahan <- renderDataTable({
    results2 <- allInputsBAULahan()
    OutputLahan_table<-as.data.frame(results2$lahanResult_0)
    PDRBLahan_table <-as.data.frame(results2$lahanResult_1)
    incomeLahan_table<-as.data.frame(results2$lahanResult_2)
    profitLahan_table<-as.data.frame(results2$lahanResult_3)
    pajakLahan_table<-as.data.frame(results2$lahanResult_4)
    imporLahan_table<-as.data.frame(results2$lahanResult_5)
    eksporLahan_table<-as.data.frame(results2$lahanResult_6)
    belpemLahan_table<-as.data.frame(results2$lahanResult_7)
    belrtLahan_table<-as.data.frame(results2$lahanResult_8)
    labour_table<-as.data.frame(results2$lahanResult_9)
    
    if(input$lahanResults== "Proyeksi Output"){
      tables <- OutputLahan_table[OutputLahan_table$year==input$selectedYear2,]
      tables
    }
    else if(input$lahanResults == "Proyeksi PDRB"){
      tables <- PDRBLahan_table[PDRBLahan_table$year==input$selectedYear2,]
      tables
    } 
    else if(input$lahanResults == "Proyeksi Income"){
      tables <- incomeLahan_table[incomeLahan_table$year==input$selectedYear2,]
      tables} 
    else if(input$lahanResults == "Proyeksi Profit"){
      tables <- profitLahan_table[profitLahan_table$year==input$selectedYear2,]
      tables
    } 
    else if(input$lahanResults == "Proyeksi Pajak"){
      tables <- pajakLahan_table[pajakLahan_table$year==input$selectedYear2,]
      tables} 
    else if(input$lahanResults == "Proyeksi Impor"){
      tables <- imporLahan_table[imporLahan_table$year==input$selectedYear2,]
      tables} 
    else if(input$lahanResults == "Proyeksi Ekspor"){
      tables <- eksporLahan_table[eksporLahan_table$year==input$selectedYear2,]
      tables} 
    else if(input$lahanResults == "Proyeksi Belanja Pemerintah"){
      tables <- belpemLahan_table[belpemLahan_table$year==input$selectedYear2,]
      tables
    } 
    else if(input$lahanResults == "Proyeksi Belanja Rumah Tangga"){
      tables <- belrtLahan_table[belrtLahan_table$year==input$selectedYear2,]
      tables
    } 
    else if(input$lahanResults == "Proyeksi Tenaga Kerja"){
      tables <- labour_table[labour_table$year==input$selectedYear2,]
      tables
    }
  })
  
  ## download table BAU sektor lahan
  output$downloadTableBAU_lahan <- downloadHandler(
    filename = input$lahanResults,
    contentType = "text/csv",
    content = function(file) {
      results2 <- allInputsBAULahan()
      OutputLahan_table<-as.data.frame(results2lahanResult_0)
      PDRBLahan_table <-as.data.frame(results2$lahanResult_1)
      incomeLahan_table<-as.data.frame(results2$lahanResult_2)
      profitLahan_table<-as.data.frame(results2$lahanResult_3)
      pajakLahan_table<-as.data.frame(results2$lahanResult_4)
      imporLahan_table<-as.data.frame(results2$lahanResult_5)
      eksporLahan_table<-as.data.frame(results2$lahanResult_6)
      belpemLahan_table<-as.data.frame(results2$lahanResult_7)
      belrtLahan_table<-as.data.frame(results2$lahanResult_8)
      labour_table<-as.data.frame(results2$lahanResult_9)
      
      if(input$lahanResults== "Proyeksi Output"){
        tables <- OutputLahan_table[OutputLahan_table$year==input$selectedYear2,]
      }
      else if(input$lahanResults == "Proyeksi PDRB"){
        tables <- PDRBLahan_table[PDRBLahan_table$year==input$selectedYear2,]
      } 
      else if(input$lahanResults == "Proyeksi Income"){
        tables <- incomeLahan_table[incomeLahan_table$year==input$selectedYear2,]
      }
      else if(input$lahanResults == "Proyeksi Profit"){
        tables <- profitLahan_table[profitLahan_table$year==input$selectedYear2,]
      }
      else if(input$lahanResults == "Proyeksi Pajak"){
        tables <- pajakLahan_table[pajakLahan_table$year==input$selectedYear2,]
      }
      else if(input$lahanResults == "Proyeksi Impor"){
        tables <- imporLahan_table[imporLahan_table$year==input$selectedYear2,]
      }
      else if(input$lahanResults == "Proyeksi Ekspor"){
        tables <- eksporLahan_table[eksporLahan_table$year==input$selectedYear2,]
      }
      else if(input$lahanResults == "Proyeksi Belanja Pemerintah"){
        tables <- belpemLahan_table[belpemLahan_table$year==input$selectedYear2,]
      }
      else if(input$lahanResults == "Proyeksi Belanja Rumah Tangga"){
        tables <- belrtLahan_table[belrtLahan_table$year==input$selectedYear2,]
      } 
      else if(input$lahanResults == "Proyeksi Tenaga Kerja"){
        tables <- labour_table[labour_table$year==input$selectedYear2,]
      }
      else if(input$lahanResults == "Proyeksi Neraca Perdagangan"){
        return(NULL)
      }
      write.table(tables, file, quote=FALSE, row.names=FALSE, sep=",")
    }
  )  
  
  # observe({
  #   resultsBAU <- allInputsBAU()
  #   fd_table <- resultsBAU[[10]]
  #   output$interactiveFD <- renderDataTable({
  #     datatable(fd_table, selection='none', editable=TRUE, options=list(pageLength=50))
  #   })
  #   proxy = dataTableProxy('interactiveFD')
  #   observeEvent(input$x1_cell_edit, {
  #     info = input$x1_cell_edit
  #     str(info)
  #     i = info$row
  #     j = info$col + 1  # column index offset by 1
  #     v = info$value
  #     fd_table[i, j] <<- DT::coerceValue(v, fd_table[i, j])
  #     replaceData(proxy, fd_table, resetPaging = FALSE, rownames = FALSE)
  #   })
  # })
  
  output$selectizeSector <- renderUI({
    if(debugMode){
      sec <- blackBoxInputs()
    } else {
      sec <- allInputs()
    }
    analysisResult <- sec$result
    selectizeInput('selectMultiSector', 'Lapangan Usaha Terkait', choices=list(
      Sektor=as.character(analysisResult$Sektor)
    ), multiple=TRUE)
  })
  
  # observe({
  #   resultOfBAU <- allInputsBAU()
  #   finalDemandSeriesTable <- resultOfBAU$FDSeries
  #   
  #   if(input$interTableOutput=="Permintaan Akhir"){
  #     req(input$selectMultiSector)
  #     selectedSector <- input$selectMultiSector
  #     lenSelSector <- length(selectedSector)
  #     
  #     startCol <- grep(paste0("y", input$yearInter), colnames(finalDemandSeriesTable))
  #     
  #     lapply(1:lenSelSector, function(x){
  #       selectedSectorFinDem <- finalDemandSeriesTable[finalDemandSeriesTable$Sector==selectedSector[x],]
  #       output[[paste0("INV_", x)]] = renderUI({
  #         req(input$selectMultiSector)
  #         div(
  #           numericInput(inputId=paste0("num_inv_", x), label=paste0("Intervensi ", x), min=0, value=selectedSectorFinDem[, startCol]),
  #           sliderInput(inputId=paste0("slide_inv_", x), label=as.character(selectedSectorFinDem[,1]), min=0, max=100, post=" %", value=0, step=.5)
  #         )
  #       })
  #       
  #       observeEvent(input$paste0("num_inv_", x), {
  #         
  #       })
  #     })
  #     
  #     output$rowIntervention <- renderUI({
  #       lapply(1:lenSelSector, function(i){
  #         uiOutput(paste0("INV_", i))
  #       })
  #     })
  #   }
  # })
  
  output$yearSelectionInter <- renderUI({
    selectInput("selectedYearInter", "Tahun", "Pilih tahun", choices=c(input$dateFrom:input$dateTo))
  })  
  
  #### END : tampilkan result BAU sektor lahan====
  
  ###*intervention input####
  # observeEvent(input$buttonInter, {
  #   if(debugMode){
  #     sec <- blackBoxInputs()
  #   } else {
  #     sec <- allInputs()
  #   }
  #   sector <- sec$sector
  #   indem <- sec$indem
  #   findem <- sec$findem
  #   addval <- sec$addval
  #   findemcom <- sec$findemcom
  #   addvalcom <- sec$addvalcom
  #   labour <- sec$labour
  #   energy <- sec$energy
  #   ef_energy <- sec$ef_energy
  #   waste <-sec$waste
  #   ef_waste <- sec$ef_waste  
  #   growthRate <- allDataProv$growthRate
  #   
  #   importRow <- 1
  #   incomeRow <- 2
  #   profitRow <- 3
  #   
  #   indem_matrix <- as.matrix(indem)
  #   addval_matrix <- as.matrix(addval)
  #   dimensi <- ncol(indem_matrix)
  #   
  #   indem_colsum <- colSums(indem_matrix)
  #   addval_colsum <- colSums(addval_matrix)
  #   fin_con <- 1/(indem_colsum+addval_colsum)
  #   fin_con[is.infinite(fin_con)] <- 0
  #   tinput_invers <- diag(fin_con)
  #   A <- indem_matrix %*% tinput_invers
  #   I <- as.matrix(diag(dimensi))
  #   I_A <- I-A
  #   leontief <- solve(I_A)
  #   
  #   population <- bauResults$population
  #   otherEm <- bauResults$otherEm
  #   intDemandSeries <- bauResults$IDSeries
  #   addValueSeries <- bauResults$AVSeries
  #   tOutputSeries <- bauResults$tOutputSeries
  #   GDPseries <- bauResults$GDPSeries
  #   impactLabour <- bauResults$impactLabour
  #   impactEnergy <- bauResults$impactEnergy
  #   impactWaste <- bauResults$impactWaste
  #   endT <- bauResults$dateTo
  #   startT <- bauResults$dateFrom
  #   
  #   yearIntervention <- as.numeric(input$yearInter)
  #   mfinalDemandSeriesTable <- values$finalDemandSeriesTableInv
  #   
  #   mSatelliteImpact <- function(sat_type = "energy", tbl_sat = data.frame(), table_output_matrix = matrix(), emission_lookup = data.frame(), yearInt= 2010){ 
  #     # second arg is the total output matrix calculated earlier
  #     # third arg is compulsory only when sat_type is either "energy" or "waste"
  #     if(sat_type == "energy" | sat_type == "waste"){
  #       impact <- list() # impact$cons; impact$emission
  #       impact$cons <- tbl_sat
  #       prop <- impact$cons[, 4:ncol(impact$cons)]/impact$cons[, 3]
  #       impact$cons[, 4:ncol(impact$cons)] <- prop
  #       
  #       # calculate m.tinput_invers===
  #       m.indem_matrix <- eval(parse(text= paste0("intDemandSeries$y", yearInt)))
  #       m.addval_matrix <- eval(parse(text= paste0("addValueSeries$y", yearInt)))
  #       dimensi <- ncol(m.indem_matrix)
  #       
  #       m.indem_colsum <- colSums(m.indem_matrix)
  #       m.addval_colsum <- colSums(m.addval_matrix)
  #       m.fin_con <- 1 / (m.indem_colsum + m.addval_colsum)
  #       m.fin_con[is.infinite(m.fin_con)] <- 0
  #       m.tinput_invers <- diag(m.fin_con)
  #       # calculate m.m.tinput_invers\end==
  #       
  #       # calculate the new gross consumptions of fuel/waste types
  #       coeff_sat <- m.tinput_invers %*% as.matrix(impact$cons[,3])
  #       coeff_matrix <- diag(as.numeric(coeff_sat), ncol = nrow(impact$cons), nrow = nrow(impact$cons))
  #       impact$cons[,3] <- coeff_matrix %*% table_output_matrix
  #       colnames(impact$cons)[3] <- "Tconsumption"
  #       # distribute the newly calculated gross consumption
  #       impact$cons[,4:ncol(impact$cons)] <- impact$cons[,4:ncol(impact$cons)]*impact$cons[, 3]
  #       # on emission factor
  #       # arrange function to distribute the emission factor accordingly
  #       order_cname <- names(impact$cons)[4:ncol(impact$cons)]
  #       em_f <- numeric()
  #       for(m in 1: length(order_cname)){
  #         em_f <- c(em_f, emission_lookup[which(emission_lookup[,1]==order_cname[m]), 2])
  #       }
  #       em_f <- diag(em_f, nrow = length(em_f), ncol = length(em_f))
  #       # second part of the list: Emission
  #       impact$emission <- impact$cons
  #       impact$emission[,4:ncol(impact$emission)] <- as.matrix(impact$cons[,4:ncol(impact$cons)])%*%em_f
  #       impact$emission[,3] <- rowSums(impact$emission[,4: ncol(impact$emission)])
  #       colnames(impact$emission)[3] <- "Temission"
  #     } else { # for labour case
  #       impact <- list()
  #       impact$cons <- tbl_sat
  #       coeff_sat <- tinput_invers %*% as.matrix(impact$cons[,3])
  #       coeff_matrix <- diag(as.numeric(coeff_sat), ncol = nrow(impact$cons), nrow = nrow(impact$cons))
  #       impact$cons[,3] <- coeff_matrix %*% table_output_matrix
  #     }
  #     impact$cons[is.na(impact$cons)] <- 0
  #     impact$emission[is.na(impact$emission)] <- 0
  #     return(impact)
  #   }
  #   
  #   coef_primary_input <- addval_matrix %*% tinput_invers
  #   coef_grise <- 1+input$gdpRate
  #   growthRate$Lapangan_usaha <- NULL
  #   growthRate_matrix <- as.matrix(growthRate)
  #   growthRate_matrix <- 1+growthRate_matrix
  #   
  #   stepN <- endT - startT
  #   stepInv <- yearIntervention - startT
  #   
  #   mGDPseries <- GDPseries[, 1:which(colnames(GDPseries) == paste0("y", startT+(stepInv-1)))]
  #   
  #   # colnames(mtOutputseries) <- colnames(tOutputSeries)[1:which(colnames(tOutputSeries) == paste0("y", startT+(tu-1)))] # retain missing colnames
  #   mtOutputseries <- tOutputSeries[, colnames(tOutputSeries)[1:which(colnames(tOutputSeries) == paste0("y", startT+(stepInv-1)))]]
  #   
  #   mAddValueSeries <- addValueSeries[1:which(names(addValueSeries) == paste0("y", startT+(stepInv-1)))] # list can also be subsetted by using single square bracket
  #   
  #   mImpactLabour <- impactLabour[1:which(names(impactLabour) == paste0("y", startT+(stepInv-1)))] # list can also be subsetted by using single square bracket
  #   mImpactEnergy <- impactEnergy[1:which(names(impactEnergy) == paste0("y", startT+(stepInv-1)))]
  #   mImpactWaste <- impactWaste[1:which(names(impactWaste) == paste0("y", startT+(stepInv-1)))]
  #   
  #   for(tu in stepInv:stepN){
  #     # Time relevant colnames
  #     mProjT <- startT+tu
  #     mProjT <- paste0("y", mProjT)
  #     
  #     if(startT+tu == yearIntervention){
  #       mProjFinDem <- mfinalDemandSeriesTable[, mProjT]
  #     } else {
  #       # if(input$typeIntervention=='Tipe 1'){
  #       # mProjFinDem <- mfinalDemandSeriesTable[, mProjT] * coef_grise
  #       # } else {
  #       mProjFinDem <- growthRate_matrix[, mProjT] * mfinalDemandSeriesTable[, mProjT]
  #       # }
  #     }
  #     mProjOutput <- leontief %*% as.numeric(as.character(mProjFinDem))
  #     mtOutputseries <- cbind(mtOutputseries, mProjOutput)
  #     
  #     # calculation of mAddValueSeries
  #     eval(parse(text=paste0("mAddValueSeries$", mProjT, " <-  coef_primary_input %*% diag(as.vector(mProjOutput), ncol = dimensi, nrow= dimensi)")))
  #     # calculation of mGDPseries
  #     eval(parse(text = paste0("mGDPseries$", mProjT, "<- colSums(mAddValueSeries$", mProjT, "[setdiff(1:nrow(addval_matrix), importRow),])")))
  #     # calculation of mImpactLabour
  #     eval(parse(text= paste0("mImpactLabour$", mProjT, " <- mSatelliteImpact('labour', tbl_sat=labour, table_output_matrix = as.matrix(mProjOutput), yearInt=yearIntervention)")))
  #     # calculation of mImpactEnergy
  #     eval(parse(text= paste0("mImpactEnergy$", mProjT, " <- mSatelliteImpact('energy', tbl_sat=energy, table_output_matrix = as.matrix(mProjOutput), emission_lookup=ef_energy, yearInt=yearIntervention)")))
  #     # calculation of mImpactWaste
  #     eval(parse(text= paste0("mImpactWaste$", mProjT, " <- mSatelliteImpact('waste', tbl_sat=waste, table_output_matrix = as.matrix(mProjOutput), emission_lookup=ef_waste, yearInt=yearIntervention)")))
  #   }
  #   
  #   mGDPOutput <- data.frame(year = 0, id.sector = 0, sector = "", GDP = 0, stringsAsFactors = FALSE)
  #   for(c in 3:ncol(mGDPseries)){
  #     add.row <- mGDPseries[, c(1,2, c)]
  #     names(add.row) <- c("id.sector", "sector", "GDP")
  #     add.row$year <- startT + (c-3)
  #     add.row <- add.row[, colnames(mGDPOutput)]
  #     mGDPOutput <- data.frame(rbind(mGDPOutput, add.row), stringsAsFactors = FALSE)
  #   }
  #   mGDPOutput <- mGDPOutput[mGDPOutput$year != 0, ] # remove initial values
  #   # 2. Income per capita (ind. 9)
  #   
  #   mIncomePerCapitaOutput <- data.frame(year = 0, Income.per.capita = 0)
  #   for(t in 0: stepN){
  #     t_curr <- startT + t
  #     pop_curr <- population[which(population[, 1] == t_curr), 2]
  #     inc_curr <- sum(mAddValueSeries[[t+1]][incomeRow,])
  #     inc_capita <- inc_curr/pop_curr
  #     add.row <- data.frame(cbind(t_curr, inc_capita))
  #     names(add.row) <- names(mIncomePerCapitaOutput)
  #     mIncomePerCapitaOutput <- data.frame(rbind(mIncomePerCapitaOutput, add.row), stringsAsFactors = FALSE)
  #   }
  #   mIncomePerCapitaOutput <- mIncomePerCapitaOutput[mIncomePerCapitaOutput$year != 0, ]
  #   
  #   # 3. Wages or Income (ind. 7)
  #   mIncomeOutput <- data.frame(year = 0, id.sector = 0, sector= "", income = 0, stringsAsFactors = FALSE)
  #   id.sc <- 1:dimensi
  #   sc.name <- sector[,1]
  #   for(t in 0: stepN){
  #     t_curr <- startT + t
  #     inc_curr <- data.frame(mAddValueSeries[[t+1]][incomeRow,])
  #     add.row <- data.frame(cbind(t_curr, id.sc, sc.name, inc_curr), stringsAsFactors = FALSE)
  #     names(add.row) <- names(mIncomeOutput)
  #     mIncomeOutput <- data.frame(rbind(mIncomeOutput, add.row), stringsAsFactors = FALSE)
  #   }
  #   mIncomeOutput <- mIncomeOutput[mIncomeOutput$year != 0, ]
  #   
  #   # 4. Labour (ind. number 10)
  #   mLabourOutput <- data.frame(year = 0, id.sector = 0, sector= "", labour = 0, stringsAsFactors = FALSE)
  #   for(t in 0: stepN){
  #     t_curr <- startT + t
  #     add.row <- data.frame(mImpactLabour[[t+1]][[1]])
  #     names(add.row) <- names(mLabourOutput)[2:4]
  #     add.row$year <- t_curr
  #     add.row <- add.row[, names(mLabourOutput)]
  #     mLabourOutput <- data.frame(rbind(mLabourOutput, add.row), stringsAsFactors = FALSE)
  #   }
  #   mLabourOutput <- mLabourOutput[mLabourOutput$year != 0, ]
  #   
  #   # 5. Energy cons (indicator number 2)
  #   mEnergyConsOutput <- mImpactEnergy[[1]][[1]]
  #   mEnergyConsOutput$year <- startT
  #   mEnergyConsOutput <- mEnergyConsOutput[, c("year", names(mImpactEnergy[[1]][[1]]))]
  #   
  #   for(t in 1: stepN){
  #     t_curr <- startT + t
  #     add.row <- data.frame(mImpactEnergy[[t+1]][[1]]) # [[2]] for emission
  #     add.row$year <- t_curr
  #     add.row <- add.row[, names(mEnergyConsOutput)]
  #     mEnergyConsOutput <- data.frame(rbind(mEnergyConsOutput, add.row), stringsAsFactors = FALSE)
  #   }
  #   names(mEnergyConsOutput)[2:3] <- c("id.sector", "sector")
  #   # mEnergyConsOutput <- mEnergyConsOutput[mEnergyConsOutput$year != 0, ]
  #   
  #   # 6. Energy emission (indicator number 3)
  #   mEnergyEmissionOutput <- mImpactEnergy[[1]][[2]]
  #   mEnergyEmissionOutput$year <- startT
  #   mEnergyEmissionOutput <- mEnergyEmissionOutput[, c("year", names(mImpactEnergy[[1]][[2]]))]
  #   
  #   for(t in 1: stepN){
  #     t_curr <- startT + t
  #     add.row <- data.frame(mImpactEnergy[[t+1]][[2]]) # [[2]] for emission
  #     add.row$year <- t_curr
  #     add.row <- add.row[, names(mEnergyEmissionOutput)]
  #     mEnergyEmissionOutput <- data.frame(rbind(mEnergyEmissionOutput, add.row), stringsAsFactors = FALSE)
  #   }
  #   names(mEnergyEmissionOutput)[2:3] <- c("id.sector", "sector")
  #   # mEnergyEmissionOutput <- mEnergyEmissionOutput[mEnergyEmissionOutput$year != 0, ]
  #   
  #   # 7. Waste cons (indicator number 2)
  #   mWasteDispOutput <- mImpactWaste[[1]][[1]]
  #   mWasteDispOutput$year <- startT
  #   mWasteDispOutput <- mWasteDispOutput[, c("year", names(mImpactWaste[[1]][[1]]))]
  #   
  #   for(t in 1: stepN){
  #     t_curr <- startT + t
  #     add.row <- data.frame(mImpactWaste[[t+1]][[1]]) # [[2]] for emission
  #     add.row$year <- t_curr
  #     add.row <- add.row[, names(mWasteDispOutput)]
  #     mWasteDispOutput <- data.frame(rbind(mWasteDispOutput, add.row), stringsAsFactors = FALSE)
  #   }
  #   names(mWasteDispOutput)[2:3] <- c("id.sector", "sector")
  #   # mWasteDispOutput <- mWasteDispOutput[mWasteDispOutput$year != 0, ]
  #   
  #   # 8. Waste emission (indicator number 3)
  #   mWasteEmissionOutput <- mImpactWaste[[1]][[2]]
  #   mWasteEmissionOutput$year <- startT
  #   mWasteEmissionOutput <- mWasteEmissionOutput[, c("year", names(mImpactWaste[[1]][[2]]))]
  #   
  #   for(t in 1: stepN){
  #     t_curr <- startT + t
  #     add.row <- data.frame(mImpactWaste[[t+1]][[2]]) # [[2]] for emission
  #     add.row$year <- t_curr
  #     add.row <- add.row[, names(mWasteEmissionOutput)]
  #     mWasteEmissionOutput <- data.frame(rbind(mWasteEmissionOutput, add.row), stringsAsFactors = FALSE)
  #   }
  #   names(mWasteEmissionOutput)[2:3] <- c("id.sector", "sector")
  #   # mWasteEmissionOutput <- mWasteEmissionOutput[mWasteEmissionOutput$year != 0, ]
  #   
  #   # 9. Total Emission
  #   mTotalEmissionOutput <- otherEm[which(otherEm$Year>=startT & otherEm$Year<= endT),]
  #   mEmissionEnergyCons <- numeric()
  #   mEmissionIndWaste <- numeric()
  #   for(t in 0: stepN){
  #     t_curr <- startT + t
  #     add_MEcons <- sum(mEnergyEmissionOutput[mEnergyEmissionOutput$year==t_curr, "Temission"])
  #     add_MWdisp <- sum(mWasteEmissionOutput[mWasteEmissionOutput$year==t_curr, "Temission"])
  #     mEmissionEnergyCons <- c(mEmissionEnergyCons, add_MEcons)
  #     mEmissionIndWaste <- c(mEmissionIndWaste, add_MWdisp)
  #   }
  #   mTotalEmissionOutput$emissionEnergyCons <- mEmissionEnergyCons
  #   mTotalEmissionOutput$emissionWasteDisp <- mEmissionIndWaste
  #   mTotalEmissionOutput$TotalEmission <- rowSums(mTotalEmissionOutput[, 2:ncol(mTotalEmissionOutput)])
  #   mTotalEmissionOutput$CummulativeEmission <- cumsum(mTotalEmissionOutput$TotalEmission)
  #   
  #   recordActivities(paste0("Simulasi skenario aksi PRK tahun ", yearIntervention), "Berhasil", paste0(Sys.time()))
  #   notif_id <<- showNotification("Simulasi skenario intervensi aksi telah berhasil", duration = 4, closeButton = TRUE, type = "warning")
  #   
  #   interventionResults$GDP_table = mGDPOutput
  #   interventionResults$mGDPseries = mGDPseries
  #   interventionResults$income_percapita_table = mIncomePerCapitaOutput
  #   interventionResults$income_table = mIncomeOutput
  #   interventionResults$labour_table = mLabourOutput
  #   interventionResults$energy_consumption_table = mEnergyConsOutput
  #   interventionResults$energy_emission_table = mEnergyEmissionOutput
  #   interventionResults$waste_consumption_table = mWasteDispOutput
  #   interventionResults$waste_emission_table = mWasteEmissionOutput
  #   interventionResults$total_emission_table = mTotalEmissionOutput
  #   
  #   updateTabItems(session, "tabs", selected = "pageEight")
  # })
  # 
  # output$plotlyResultsInter <- renderPlotly({
  #   GDP_table <- interventionResults$GDP_table
  #   income_percapita_table <- interventionResults$income_percapita_table  
  #   income_table <- interventionResults$income_table
  #   labour_table <- interventionResults$labour_table
  #   energy_consumption_table <- interventionResults$energy_consumption_table 
  #   energy_emission_table <- interventionResults$energy_emission_table 
  #   waste_consumption_table <- interventionResults$waste_consumption_table  
  #   waste_emission_table <- interventionResults$waste_emission_table 
  #   total_emission_table <- interventionResults$total_emission_table
  #   
  #   if(input$interResults == "Proyeksi PDRB"){
  #     graph <- GDP_table[GDP_table$year==input$selectedYearInter,]
  #     gplot14<-ggplot(data=graph, aes(x=sector, y=GDP)) + 
  #       geom_bar(fill="green", stat="identity") + 
  #       coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
  #     ggplotly(gplot14)
  #   } else if(input$interResults == "Proyeksi Upah per Kapita"){
  #     gplot15<-ggplot(data=income_percapita_table, aes(x=year, y=Income.per.capita, group=1)) + geom_line() + geom_point()
  #     ggplotly(gplot15)
  #   } else if(input$interResults == "Proyeksi Upah Gaji"){
  #     graph <- income_table[income_table$year==input$selectedYearInter,]
  #     gplot16<-ggplot(data=graph, aes(x=sector, y=income)) +
  #       geom_bar(fill="green", stat="identity") +
  #       coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
  #     ggplotly(gplot16)
  #   } else if(input$interResults == "Proyeksi Tenaga Kerja"){
  #     graph <- labour_table[labour_table$year==input$selectedYearInter,]
  #     gplot17<-ggplot(data=graph, aes(x=sector, y=labour)) +
  #       geom_bar(fill="green", stat="identity") +
  #       coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
  #     ggplotly(gplot17)
  #   } else if(input$interResults == "Proyeksi Konsumsi Energi"){
  #     graph <- energy_consumption_table[energy_consumption_table$year==input$selectedYearInter,]
  #     gplot18<-ggplot(data=graph, aes(x=sector, y=Tconsumption)) +
  #       geom_bar(fill="green", stat="identity") +
  #       coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
  #     ggplotly(gplot18)
  #   } else if(input$interResults == "Proyeksi Emisi Terkait Konsumsi Energi"){
  #     graph <- energy_emission_table[energy_emission_table$year==input$selectedYearInter,]
  #     gplot19<-ggplot(data=graph, aes(x=sector, y=Temission)) +
  #       geom_bar(fill="green", stat="identity") +
  #       coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
  #     ggplotly(gplot19)
  #   } else if(input$interResults == "Proyeksi Buangan Limbah"){
  #     graph <- waste_consumption_table[waste_consumption_table$year==input$selectedYearInter,]
  #     gplot20<-ggplot(data=graph, aes(x=sector, y=Tconsumption)) +
  #       geom_bar(fill="green", stat="identity") +
  #       coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
  #     ggplotly(gplot20)
  #   } else if(input$interResults == "Proyeksi Emisi Terkait Buangan Limbah"){
  #     graph <- waste_emission_table[waste_emission_table$year==input$selectedYearInter,]
  #     gplot21<-ggplot(data=graph, aes(x=sector, y=Temission)) +
  #       geom_bar(fill="green", stat="identity") +
  #       coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
  #     ggplotly(gplot21)
  #   } else if(input$interResults == "Proyeksi Total Emisi"){
  #     gplot22<-ggplot(data=total_emission_table, aes(x=Year, y=TotalEmission, group=1)) + geom_line() + geom_point()
  #     ggplotly(gplot22)
  #   }
  #   
  # })
  # 
  # output$tableResultsInter <- renderDataTable({
  #   GDP_table <- interventionResults$GDP_table
  #   income_percapita_table <- interventionResults$income_percapita_table  
  #   income_table <- interventionResults$income_table
  #   labour_table <- interventionResults$labour_table
  #   energy_consumption_table <- interventionResults$energy_consumption_table 
  #   energy_emission_table <- interventionResults$energy_emission_table 
  #   waste_consumption_table <- interventionResults$waste_consumption_table  
  #   waste_emission_table <- interventionResults$waste_emission_table 
  #   total_emission_table <- interventionResults$total_emission_table
  #   
  #   if(input$interResults == "Proyeksi PDRB"){
  #     tables <- GDP_table[GDP_table$year==input$selectedYearInter,]
  #     tables
  #   } else if(input$interResults == "Proyeksi Upah per Kapita"){
  #     return(NULL)
  #   } else if(input$interResults == "Proyeksi Upah Gaji"){
  #     tables <- income_table[income_table$year==input$selectedYearInter,]
  #     tables
  #   } else if(input$interResults == "Proyeksi Tenaga Kerja"){
  #     tables <- labour_table[labour_table$year==input$selectedYearInter,]
  #     tables
  #   } else if(input$interResults == "Proyeksi Konsumsi Energi"){
  #     tables <- energy_consumption_table[energy_consumption_table$year==input$selectedYearInter,]
  #     tables
  #   } else if(input$interResults == "Proyeksi Emisi Terkait Konsumsi Energi"){
  #     tables <- energy_emission_table[energy_emission_table$year==input$selectedYearInter,]
  #     tables
  #   } else if(input$interResults == "Proyeksi Buangan Limbah"){
  #     tables <- waste_consumption_table[waste_consumption_table$year==input$selectedYearInter,]
  #     tables
  #   } else if(input$interResults == "Proyeksi Emisi Terkait Buangan Limbah"){
  #     tables <- waste_emission_table[waste_emission_table$year==input$selectedYearInter,]
  #     tables
  #   } else if(input$interResults == "Proyeksi Total Emisi"){
  #     return(NULL)
  #   }
  #   datatable(tables, extensions = "FixedColumns", options=list(pageLength=50, scrollX=TRUE, scrollY="500px", fixedColumns=list(leftColumns=1)), rownames=FALSE)%>%
  #     formatRound(columns=c(3:length(tables)),2)
  # }) #, extensions = "FixedColumns", options=list(pageLength=50, scrollX=TRUE, scrollY="600px", fixedColumns=list(leftColumns=1)), rownames=FALSE)  
  # 
  # output$downloadTableInter <- downloadHandler(
  #   filename = input$interResults,
  #   contentType = "text/csv",
  #   content = function(file) {
  #     GDP_table <- interventionResults$GDP_table
  #     income_percapita_table <- interventionResults$income_percapita_table  
  #     income_table <- interventionResults$income_table
  #     labour_table <- interventionResults$labour_table
  #     energy_consumption_table <- interventionResults$energy_consumption_table 
  #     energy_emission_table <- interventionResults$energy_emission_table 
  #     waste_consumption_table <- interventionResults$waste_consumption_table  
  #     waste_emission_table <- interventionResults$waste_emission_table 
  #     total_emission_table <- interventionResults$total_emission_table
  #     
  #     if(input$interResults == "Proyeksi PDRB"){
  #       tables <- GDP_table[GDP_table$year==input$selectedYear,]
  #     } else if(input$interResults == "Proyeksi Upah per Kapita"){
  #       return(NULL)
  #     } else if(input$interResults == "Proyeksi Upah Gaji"){
  #       tables <- income_table[income_table$year==input$selectedYear,]
  #     } else if(input$interResults == "Proyeksi Tenaga Kerja"){
  #       tables <- labour_table[labour_table$year==input$selectedYear,]
  #     } else if(input$interResults == "Proyeksi Konsumsi Energi"){
  #       tables <- energy_consumption_table[energy_consumption_table$year==input$selectedYear,]
  #     } else if(input$interResults == "Proyeksi Emisi Terkait Konsumsi Energi"){
  #       tables <- energy_emission_table[energy_emission_table$year==input$selectedYear,]
  #     } else if(input$interResults == "Proyeksi Buangan Limbah"){
  #       tables <- waste_consumption_table[waste_consumption_table$year==input$selectedYear,]
  #     } else if(input$interResults == "Proyeksi Emisi Terkait Buangan Limbah"){
  #       tables <- waste_emission_table[waste_emission_table$year==input$selectedYear,]
  #     } else if(input$interResults == "Proyeksi Total Emisi"){
  #       return(NULL)
  #     }
  #     write.table(tables, file, quote=FALSE, row.names=FALSE, sep=",")
  #   }
  # )  
  # 
  # # output$tableIOInter <- renderTable({ }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = 'xs')  
  # 
  # output$percentOfEmRed <- renderValueBox({
  #   emissionBAU <- bauResults$total_emission_table
  #   emissionInv <- interventionResults$total_emission_table
  #   
  #   yearIntervention <- input$yearInter
  #   
  #   totalEmissionBAU <- emissionBAU[which(emissionBAU$Year==yearIntervention),]$TotalEmission
  #   totalEmissionInv <- emissionInv[which(emissionInv$Year==yearIntervention),]$TotalEmission
  #   
  #   percentEm <- ((totalEmissionInv - totalEmissionBAU) / totalEmissionBAU) * 100 * -1 # minus as a contrast value
  #   percentEm <- round(percentEm,digits=2)
  #   valueBox(
  #     paste0(percentEm, " %"), "Persentase Penurunan Emisi", color="purple"
  #   )
  # })
  # 
  # output$percentOfGDPGrowth <- renderValueBox({
  #   GDPseriesBAU <- bauResults$GDPSeries
  #   GDPseriesInv <- interventionResults$GDP_table
  #   
  #   yearIntervention <- input$yearInter
  #   
  #   totalGDPBAU <- sum(GDPseriesBAU[, which(colnames(GDPseriesBAU) == paste0("y", yearIntervention))])
  #   selectedGDP <- GDPseriesInv[GDPseriesInv$year==yearIntervention,]
  #   GDPvalues <- as.matrix(selectedGDP$GDP)
  #   totalGDPInv <- colSums(GDPvalues)
  #   
  #   percentGDP <- ((totalGDPInv - totalGDPBAU) / totalGDPBAU) * 100
  #   percentGDP <- round(percentGDP,digits = 2)
  #   
  #   valueBox(
  #     paste0(percentGDP, " %"), "Persentase Pertumbuhan PDRB", color="yellow"
  #   )
  # })
  # 
  # output$curveEmRed <- renderPlotly({
  #   emissionBAU <- bauResults$total_emission_table
  #   emissionInv <- interventionResults$total_emission_table
  #   
  #   cumSumBAU <- subset(emissionBAU, select=c(Year, CummulativeEmission))
  #   cumSumInv <- subset(emissionInv, select=c(Year, CummulativeEmission))
  #   
  #   cumSumBAU$Scenario<-"BAU"
  #   cumSumInv$Scenario<-input$scenarioName
  #   
  #   tblCumSumScenario <- rbind(cumSumBAU, cumSumInv)
  #   
  #   gplot23<-ggplot(tblCumSumScenario, aes(x=Year, y=CummulativeEmission, group=Scenario)) +
  #     geom_line(aes(color=Scenario))+
  #     geom_point(aes(color=Scenario))+
  #     labs(x = "Tahun", y = "Emisi")+
  #     ggtitle("Grafik Proyeksi Emisi")
  #   finalResults$plot23<-gplot23
  #   ggplotly(gplot23)
  # })
  # 
  # output$curveGDPGrowth <- renderPlotly({
  #   gdpBAU <- bauResults$GDP_table
  #   gdpInv <- interventionResults$GDP_table
  #   
  #   totalGDPBAUPerYear <- aggregate(gdpBAU$GDP, by=list(Year=gdpBAU$year), FUN=sum)
  #   totalGDPInvPerYear <- aggregate(gdpInv$GDP, by=list(Year=gdpInv$year), FUN=sum)
  #   
  #   cumSumBAU <- cumsum(totalGDPBAUPerYear)
  #   cumSumInv <- cumsum(totalGDPInvPerYear)
  #   
  #   cumSumBAU$Scenario <- "BAU"
  #   cumSumInv$Scenario <- input$scenarioName
  #   
  #   colnames(cumSumBAU)[2] <- "TotalGDP"
  #   colnames(cumSumInv)[2] <- "TotalGDP"
  #   
  #   tblCumSumScenario <- rbind(cumSumBAU, cumSumInv)
  #   
  #   gplot24<-ggplot(tblCumSumScenario, aes(x=Year, y=TotalGDP, group=Scenario)) +
  #     geom_line(aes(color=Scenario))+
  #     geom_point(aes(color=Scenario))+
  #     labs(x = "Tahun", y = "PDRB")+
  #     ggtitle("Grafik Proyeksi PDRB")
  #   finalResults$plot24<-gplot24
  #   ggplotly(gplot24)
  # })
  # 
  # output$curveIntensityEmission <- renderPlotly({
  #   emissionBAU <- bauResults$total_emission_table
  #   emissionInv <- interventionResults$total_emission_table
  #   
  #   cumSumBAU <- subset(emissionBAU, select=c(Year, CummulativeEmission))
  #   cumSumInv <- subset(emissionInv, select=c(Year, CummulativeEmission))
  #   
  #   cumSumBAU$Scenario<-"BAU"
  #   cumSumInv$Scenario<-input$scenarioName
  #   
  #   tblCumSumScenario <- rbind(cumSumBAU, cumSumInv)
  #   
  #   gdpBAU <- bauResults$GDP_table
  #   gdpInv <- interventionResults$GDP_table
  #   
  #   totalGDPBAUPerYear <- aggregate(gdpBAU$GDP, by=list(Year=gdpBAU$year), FUN=sum)
  #   totalGDPInvPerYear <- aggregate(gdpInv$GDP, by=list(Year=gdpInv$year), FUN=sum)
  #   
  #   colnames(totalGDPBAUPerYear)[2] <- "TotalGDP"
  #   colnames(totalGDPInvPerYear)[2] <- "TotalGDP"
  #   
  #   intensityBAU <- merge(cumSumBAU, totalGDPBAUPerYear, by="Year")
  #   intensityInv <- merge(cumSumInv, totalGDPInvPerYear, by="Year")
  #   
  #   intensityBAU$intensitas <- intensityBAU$CummulativeEmission / intensityBAU$TotalGDP
  #   intensityInv$intensitas <- intensityInv$CummulativeEmission / intensityInv$TotalGDP
  #   
  #   tblIntensity <- rbind(intensityBAU[intensityBAU$Year > input$dateFrom,], intensityInv[intensityInv$Year > input$dateFrom,])
  #   
  #   finalResults$tabel1<-tblIntensity
  #   
  #   gplot25<-ggplot(tblIntensity, aes(x=Year, y=intensitas, group=Scenario)) +
  #     geom_line(aes(color=Scenario))+
  #     geom_point(aes(color=Scenario))+
  #     labs(x = "Tahun", y = "Intensitas Emisi")+
  #     ggtitle("Grafik Proyeksi Intensitas Emisi")
  #   finalResults$plot25<-gplot25
  #   ggplotly(gplot25)
  # })
  
  output$downloadResults <- downloadHandler(
    filename = paste0(allDataProv$prov, "_hasil.doc"),
    content = function(file){
      title <- "\\b\\fs32 Dokumen Hasil Analisis PPRK-D\\b0\\fs20"
      fileresult = file.path(tempdir(), paste0(allDataProv$prov, "_hasil.doc"))
      rtffile <- RTF(fileresult, font.size = 9)
      addParagraph(rtffile, title)
      addNewLine(rtffile)
      addNewLine(rtffile)
      addParagraph(rtffile, "\\b\\fs20 Gambar 1. Grafik Emisi Nilai Proyeksi Emisi Kumulatif\\b0\\fs20.")
      addPlot(rtffile, plot.fun = print, width = 5, height = 3, res = 300, finalResults$plot23)
      addNewLine(rtffile)
      addParagraph(rtffile, "\\b\\fs20 Gambar 2. Grafik Emisi Proyeksi Nilai PDRB \\b0\\fs20.")
      addPlot(rtffile, plot.fun = print, width = 5, height = 3, res = 300, finalResults$plot24)
      addNewLine(rtffile)
      addParagraph(rtffile, "\\b\\fs20 Gambar 3. Grafik Proyeksi Intensitas Emisi\\b0\\fs20.")
      addPlot(rtffile, plot.fun = print, width = 5, height = 3, res = 300, finalResults$plot25)
      addNewLine(rtffile)
      addNewLine(rtffile)
      addTable(rtffile, finalResults$tabel1, font.size = 8)
      addParagraph(rtffile, "\\b\\fs20 Table 1. Tabel PDRB\\b0\\fs20.")
      addNewLine(rtffile)
      addNewLine(rtffile)
      done(rtffile)
      
      file.copy(fileresult, file)
    }
  )
  
  output$tableUserLog <- renderDataTable({
    userActivities$listOfActs
  })
  
  steps <- reactive(data.frame(
    element = c(
      "tabs",
      ".sidebar-menu",
      "#pengaturan",
      ".sidebar-menu",
      "#historis > li > a[data-value=pageOne]",
      "#yearIO",
      ".nav-tabs-custom",
      "#historis > li > a[data-value=pageTwo]",
      "#categorySector + .selectize-control",
      "#pprkResults + .selectize-control",
      "#downloadReport",
      ".sidebar-menu",
      "#bau > li > a[data-value=pageFour]",
      "#typeIntervention + .selectize-control",
      "#dateFrom",
      "#dateTo",
      "#generateBAUTable",
      ".js-irs-0",
      "#saveTableBauType",
      "#tableBAUType",
      "#buttonBAU",
      "#bau > li > a[data-value=pageFive]",
      "#bauResults + .selectize-control",
      "#plotlyResultsBAU",
      ".sidebar-menu",
      "#intervensi > li > a[data-value=pageSeven]",
      "#interTableOutput + .selectize-control",
      "#scenarioName",
      "#yearInter + .selectize-control",
      "#selectizeSector",
      "#rowIntervention",
      "#buttonInter",
      "#intervensi > li > a[data-value=pageEight]",
      "#percentOfEmRed",
      "#percentOfGDPGrowth",
      "#curveEmRed",
      "#curveGDPGrowth",
      "#curveIntensityEmission",
      "#downloadResults" 
      
    ),
    intro = c(
      "Selamat datang di panduan interaktif redcluwe.id.<br/><br/>Anda akan melakukan simulasi pertumbuhan ekonomi provinsi dengan data yang tersedia. Elemen yang tersorot akan ditampilkan sesuai dengan respon Anda, sementara elemen lainnya akan berwarna gelap. Pada setiap langkah panduan yang dilewati, anda juga akan diminta untuk menjalankan sebuah perintah maupun memasukkan suatu input yang diberi tanda \"<strong>Petunjuk</strong>\".<br/><br/>Klik <strong>Berikutnya</strong> untuk mengikuti keseluruhan panduan ini.",
      "Berikut ini adalah menu-menu yang akan digunakan sebagai input simulasi pertumbuhan ekonomi dan pilihan untuk menampilkan halaman hasil simulasi.<br/><br/><strong>Petunjuk:</strong> Silahkan klik menu <strong>Pengaturan</strong> untuk memilih data sesuai provinsi yang akan dilakukan simulasi kemudian klik <strong>Berikutnya</strong>.",
      "Pilih nama provinsi, kemudian isi kolom nama pengguna, nama lengkap pengguna, dan password.<br/><br/><strong>Petunjuk:</strong> Silahkan klik tombol <strong>Masuk</strong>.",
      "<strong>Petunjuk:</strong> Silahkan klik menu <strong>Historis</strong>.",
      "<strong>Petunjuk:</strong> Silahkan klik sub-menu <strong>Input</strong>.",
      "Tahun produksi dari Tabel Input-Output provinsi yang dipilih.",
      "Tabel IO provinsi adalah tabel transaksi barang dan jasa yang terjadi di provinsi tersebut pada satu titik waktu dari tahun tabel tersebut diproduksi. Tabel IO yang digunakan adalah <strong>Tabel Transaksi Domestik Atas Dasar Harga Produsen</strong> dengan satuan moneter <strong>Miliar Rupiah</strong>.
          <br/><br/>Matriks satelit tenaga kerja menunjukkan jumlah tenaga kerja untuk setiap sektor ekonomi.
          <br/><br/>Matriks satelit energi menunjukkan jumlah pemakaian tiap jenis bahan bakar untuk tiap sektor ekonomi dalam satuan <strong>terra Joule</strong>.
          <br/><br/>Matriks satelit limbah menunjukkan jumlah limbah yang diproduksi sektor ekonomi menurut jenis pengelolaan limbah dalam satuan <strong>ton/m3</strong>.",
      "<strong>Petunjuk:</strong> Silahkan klik sub-menu <strong>Result</strong> untuk menampilkan analisis dampak ekonomi wilayah untuk data historis.",
      "<strong>Petunjuk:</strong> Pilih kategori hasil yang ingin ditampilkan. Terdapat empat kategori yaitu: Ekonomi, Energi, Limbah, Lahan.",
      "<strong>Petunjuk:</strong> Pilih output yang tersedia sesuai kategori yang sudah dipilih seperti PDRB, Linkage, maupun Angka Pengganda.",
      "<strong>Petunjuk:</strong> Silahkan klik Unduh Ringkasan untuk menyimpan hasil analisis historis dampak ekonomi wilayah.",
      "<strong>Petunjuk:</strong> Silahkan klik menu <strong>Skenario Bisnis Seperti Biasa</strong> untuk melakukan proyeksi laju pertumbuhan ekonomi wilayah pada rentang tahun tertentu dan lapangan usaha tertentu dengan kondisi BAU.",
      "<strong>Petunjuk:</strong> Silahkan klik sub-menu <strong>Input</strong>.",
      "<strong>Petunjuk:</strong> Pilih salah satu dari tiga pilihan tipe intervensi BAU.
          <br/><br/><strong>Tipe 1</strong>, menentukan persentase pertumbuhan ekonomi di setiap rentang tahun intervensi pada seluruh lapangan usaha yang ada di provinsi tersebut.
          <br/><br/><strong>Tipe 2</strong>, menentukan persentase pertumbuhan ekonomi pada seluruh lapangan usaha provinsi namun dengan laju yang bervariasi pada setiap rentang tahunnya.
          <br/><br/><strong>Tipe 3</strong>, menentukan persentase pertumbuhan ekonomi yang bervariasi untuk lapangan usaha yang berbeda dan juga di setiap rentang tahunnya.",
      "<strong>Petunjuk:</strong> Pilih tahun awal intervensi.",
      "<strong>Petunjuk:</strong> Pilih tahun akhir intervensi.",
      "<strong>Petunjuk:</strong> Silahkan klik Buat Tabel untuk menampilkan tabel lapangan usaha ekonomi provinsi dengan rentahun terpilih.",
      "<strong>Petunjuk:</strong> Silahkan tentukan persentase laju pertumbuhan ekonomi dengan menggeser slider ke kanan atau ke kiri.",
      "<strong>Petunjuk:</strong> Silahkan klik Simpan Tabel.",
      "Tabel intervensi laju pertumbuhan ekonomi per lapangan usaha akan tampil sesuai dengan input rentang tahun dan persentase yang telah ditentukan sebelumnya.",
      "<strong>Petunjuk:</strong> Silahkan klik Jalankan Simulasi.",
      "<strong>Petunjuk:</strong> Silahkan klik sub-menu <strong>Result</strong> untuk menampilkan hasil proyeksi analisis dampak ekonomi wilayah untuk skenario BAU.",
      "<strong>Petunjuk:</strong> Pilih output proyeksi yang ingin ditampilkan seperti Proyeksi PDRB, Upah per Kapita, Upah Gaji, Tenaga Kerja, Konsumsi Energi, Emisi Terkait Konsumsi Energi, Buangan Limbah, Emisi Terkait Buangan Limbah, Total Emisi, Intensitas Emisi",
      "Grafik ini menunjukkan output proyeksi dari analisis yang ditampilkan.",
      "<strong>Petunjuk:</strong> Silahkan klik menu <strong>Skenario Aksi</strong>.",
      "<strong>Petunjuk:</strong> Silahkan klik sub-menu <strong>Input</strong>.",
      "<strong>Petunjuk:</strong> Pilih tipe intervensi.",
      "<strong>Petunjuk:</strong> Silahkan isi nama aksi perencanaan rendah karbon.",
      "<strong>Petunjuk:</strong> Pilih tahun skenario aksi.",
      "<strong>Petunjuk:</strong> Pilih lapangan usaha terkait yang akan dilakukan intervensi.",
      "<strong>Petunjuk:</strong> Silahkan isi nilai perubahan permintaan akhir untuk masing-masing aksi intervensi dari lapangan usaha terkait.",
      "<strong>Petunjuk:</strong> Silahkan klik Jalankan Simulasi.",
      "<strong>Petunjuk:</strong> Silahkan klik sub-menu <strong>Result</strong> untuk menampilkan hasil proyeksi analisis dampak ekonomi dan lingkungan wilayah untuk skenario aksi di tahun tertentu.",
      "Persentase penurunan emisi pada tahun skenario aksi.",
      "Persentase pertumbuhan PDRB pada tahun skenario aksi",
      "Grafik ini menunjukkan perbandingan proyeksi emisi skenario BAU dengan skenario aksi PRK.",
      "Grafik ini menunjukkan perbandingan proyeksi PDRB skenario BAU dengan skenario aksi PRK.",
      "Grafik ini menunjukkan perbandingan proyeksi intensitas emisi skenario BAU dengan skenario aksi PRK.",
      "<strong>Petunjuk:</strong> Silahkan klik Unduh Hasil Analisis untuk menyimpan hasil analisis pada menu Skenario Aksi."
    )
  ))
  
  observeEvent(input$quickTour,
               introjs(session, 
                       options = list(steps=steps(),
                                      "nextLabel"="Berikutnya",
                                      "prevLabel"="Sebelumnya",
                                      "skipLabel"="Lewati",
                                      "doneLabel"="Selesai",
                                      "scrollToElement"=TRUE,
                                      "exitOnOverlayClick"= TRUE,
                                      "helperNumberLayer"="right",
                                      "tooltipPosition"= "right"),
                       events = list("oncomplete" = I('alert("Bantuan telah selesai")'))
               )
  )
  
  runjs('
        var el2 = document.querySelector(".skin-green");
        el2.className = "skin-green sidebar-mini";
        var clicker = document.querySelector(".sidebar-toggle");
        clicker.id = "switchState";
  ')
  onclick('switchState', runjs({'
        var title = document.querySelector(".logo")
        if (title.style.visibility == "hidden") {
          title.style.visibility = "visible";
        } else {
          title.style.visibility = "hidden";
        }
  '}))
  
  observeEvent(input$defineScenario, {
    showModal(modalDialog(
      title = "Definisikan Aksi Mitigasi Provinsi",
      textInput("scenarioName", "Nama Aksi Mitigasi",
                placeholder = ''
      ),
      textInput("yearFrom", "Tahun Awal",
                placeholder = '2015'
      ),
      textInput("yearTo", "Tahun Awal",
                placeholder = '2030'
      ),
      textInput("scenarioDesc", "Deskripsi Aksi Mitigasi",
                placeholder = ''
      ),
      footer = tagList(
        actionButton("submit", "Kirimkan"),
        modalButton("Batal")
      )
    ))
  })
  
  observeEvent(input$constructScenario, {
    showModal(modalDialog(
      title = "Revisi Titik Intervensi",
      footer = tagList(
        actionButton("economyButton", "Pertumbuhan Ekonomi"),
        actionButton("satelliteButton", "Akun Satelit"),
        br(),
        br(),
        modalButton("Batal")
      ))
    )
  })
  
  observeEvent(input$economyButton, {
    showModal(modalDialog(
      title = "Pertumbuhan Ekonomi",
      selectInput('selectIntervention', label="Pilih Titik Intervensi", 
                  choices=c("Permintaan Akhir (Final Demand)", "Nilai Tambah (Added Value)", "Input Antara (Intermediate Demand)")),
      uiOutput("selectizeSector"),
      actionButton("run", "Jalankan"),
      br(),
      br(),
      actionButton("save", "Simpan"),
      actionButton("load", "Muat"),
      footer = tagList(
        actionButton("submit", "Kirimkan"),
        modalButton("Kembali"))
    ))
  })
  
  observeEvent(input$satelliteButton, {
    showModal(modalDialog(
      title = "Akun Satelit",
      footer = tagList(
        actionButton("consumptionButton", "Nilai Konsumsi"),
        actionButton("emissionFactorButton", "Faktor Emisi"),
        br(),
        br(),
        modalButton("Kembali")
      ))
    )
  })
  
  observeEvent(input$consumptionButton, {
    showModal(modalDialog(
      title = "Nilai Konsumsi",
      uiOutput("selectizeSector"),
      selectInput("yearInter", "Tahun skenario aksi:", choices = 1990:2100, selected=2015),
      actionButton("run", "Jalankan"),
      br(),
      br(),
      uiOutput("selectizeSector"),
      footer = tagList(
        actionButton("submit", "Kirimkan"),
        modalButton("Kembali"))
    ))
  })
  
  observeEvent(input$emissionFactorButton, {
    showModal(modalDialog(
      title = "Nilai Konsumsi",
      selectInput("yearInter", "Tahun skenario aksi:", choices = 1990:2100, selected=2015),
      actionButton("run", "Jalankan"),
      footer = tagList(
        actionButton("submit", "Kirimkan"),
        modalButton("Kembali"))
    ))
  })
}


###*run the apps#### 
shinyApp(ui = ui, server = server)