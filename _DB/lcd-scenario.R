
###BEGIN: initiate all variables ####
selectedProv = "JaBar"
datapath <- paste0("_DB/data/", selectedProv, "/")
datapathCSV <- ("_DB/input csv")


ioSector <- read.table("_DB/jabar_in_redcluwe/01_sektor.csv ", header = F, sep = ",")
ioIntermediateDemand <- readRDS(paste0(datapath, "indem"))
ioFinalDemand <- readRDS(paste0(datapath, "findem"))
ioFinalDemandComponent <- readRDS(paste0(datapath, "findemcom"))
ioAddedValue <- readRDS(paste0(datapath, "addval"))
ioAddedValueComponent <- readRDS(paste0(datapath, "addvalcom"))
ioLeontif <- readRDS(paste0(datapath, "I_A"))
ioLeontiefInverse <- readRDS(paste0(datapath, "leontief"))
ioPeriod <- readRDS(paste0(datapath, "periodIO"))


satelliteLabour <- readRDS(paste0(datapath, "labour"))
satelliteEnergy <- readRDS(paste0(datapath, "energy"))
satelliteWaste <- readRDS(paste0(datapath, "waste"))
satelliteAgriculture <- read.table("_DB/jabar_in_redcluwe/16_satelit_pertanian.csv", header = T, sep = ",")
emissionFactorEnergy <- readRDS(paste0(datapath, "ef_energy"))
emissionFactorWaste <- read.table("_DB/jabar_in_redcluwe/11_faktor_emisi_limbah.csv", header = T, sep = ",")
emissionFactorAgriculture <- read.table("_DB/jabar_in_redcluwe/17_faktor_emisi_pertanian.csv", header = T, sep = ",")
population <- readRDS(paste0(datapath, "currentPopulation"))
populationProjection <- readRDS(paste0(datapath, "population"))
baselineEmission <- readRDS(paste0(datapath, "otherEm"))

growthRate <- read.table("_DB/jabar_in_redcluwe/growth5_1630.csv", header = T, sep = ",")

inProporsiPDRB <- paste0(datapathCSV, "/16_proporsi_pdrb.csv")
proporsiPDRB <- read.table(inProporsiPDRB, header=TRUE, sep=",", stringsAsFactors = F)

# perhitungan table IO
matrixIoIntermediateDemand <- as.matrix(ioIntermediateDemand)
matrixIoAddedValue <- as.matrix(ioAddedValue)
nrowMatrixIoAddedValue <- nrow(matrixIoAddedValue)
ioDimention <- ncol(ioIntermediateDemand)

matrixIoFinalDemand <- as.matrix(ioFinalDemand)
rowSumsMatrixIoFinalDemand <- as.matrix(rowSums(matrixIoFinalDemand))
proportionFinalDemand <- ioFinalDemand/rowSumsMatrixIoFinalDemand
proportionFinalDemand[is.na(proportionFinalDemand)] <- 0

colSumsMatrixIoIntermediateDemand <- colSums(matrixIoIntermediateDemand)
colSumsMatrixIoAddedValue <- colSums(matrixIoAddedValue)
ioTotalOutput <- colSumsMatrixIoIntermediateDemand + colSumsMatrixIoAddedValue # ioTotalInput 
ioTotalOutputInverse <- 1/ioTotalOutput
ioTotalOutputInverse[is.infinite(ioTotalOutputInverse)] <- 0
ioTotalOutputInverse <- diag(ioTotalOutputInverse)




indem_matrix <- as.matrix(ioIntermediateDemand)
addval_matrix <- as.matrix(ioAddedValue)
dimensi <- ncol(indem_matrix)

indem_colsum <- colSums(indem_matrix)
addval_colsum <- colSums(addval_matrix)
fin_con <- 1/(indem_colsum+addval_colsum)
fin_con[is.infinite(fin_con)] <- 0
tinput_invers <- diag(fin_con)
A <- indem_matrix %*% tinput_invers
I <- as.matrix(diag(dimensi))
I_A <- I-A
ioLeontiefInverse <- solve(I_A)




################################################################################
#                                                                              #
#                                    INPUT                                     #
#                                                                              #
################################################################################
gdpRate <- 5/100 #user input nanti g dipakai, krn pakai perhitungan debug mas alfa pakai data growthRate

rowImport <- 1
rowIncome <- 2
rowProfit <- 3
initialYear <- 2016 #user input initialYear = yearFrom
finalYear <- 2030 #user input finalYear = yearTo
iteration <- finalYear - initialYear # iteration+1 = lengthYear

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


# BAU ---------------------------------------------------------------------

satelliteEnergy #energyBau 
emissionFactorEnergy #efBau 


###BEGIN: regional economic impact analysis & historical emission from satellite account####

# GDP y2015
analysisGDP <- colSums(matrixIoAddedValue[rowIncome:nrowMatrixIoAddedValue,]) #tanpa import
analysisTotalGDP <- sum(analysisGDP)

# Emission from energy y2015
emissionFactorEnergyDiagonal <- diag(emissionFactorEnergy[,2], ncol = nrow(emissionFactorEnergy), nrow = nrow(emissionFactorEnergy))
emissionEnergy <- as.matrix(satelliteEnergy[,4:ncol(satelliteEnergy)]) %*% emissionFactorEnergyDiagonal
emissionEnergyTotal <- rowSums(emissionEnergy)

# Emission from waste
emissionFactorWasteDiagonal <- diag(emissionFactorWaste[,2], ncol = nrow(emissionFactorWaste), nrow = nrow(emissionFactorWaste))
emissionWaste <- as.matrix(satelliteWaste[,4:ncol(satelliteWaste)]) %*% emissionFactorWasteDiagonal
emissionWasteTotal <- rowSums(emissionWaste)

# Emission from agriculture-fertilizer
emissionFactorAgricultureDiagonal <- diag(emissionFactorAgriculture[,2], ncol = nrow(emissionFactorAgriculture), nrow = nrow(emissionFactorAgriculture))
emissionAgriculture <- as.matrix(satelliteAgriculture[,4:ncol(satelliteAgriculture)]) %*% emissionFactorAgricultureDiagonal
emissionAgricultureTotal <- rowSums(emissionAgriculture)

# Coefficient Energy Used (CE) & Multiplier Energy (ME) = koefenergi 
analysisCE <- as.matrix(satelliteEnergy[,3]) / ioTotalOutput
analysisME <- ioLeontiefInverse %*% analysisCE
analysisME[is.na(analysisME)] <- 0

# Coefficient Waste Product (CW) & Multiplier Waste (MW)
analysisCW <- as.matrix(satelliteWaste[,3]) / ioTotalOutput
analysisMW <- ioLeontiefInverse %*% analysisCW
analysisMW[is.na(analysisMW)] <- 0

# Coefficient Agriculture-Fertilizer (CA) & Multiplier Agriculture-Fertilizer (MA)
analysisCA <- as.matrix(satelliteAgriculture[,3]) / ioTotalOutput
analysisMA <- ioLeontiefInverse %*% analysisCA
analysisMA[is.na(analysisMA)] <- 0

# Coefficient technology (intermediate demand) or A
analysisCT <- t( t(matrixIoIntermediateDemand) / ioTotalOutput)


# Coefficient primary input
analysisCPI <- t(t(ioAddedValue) / ioTotalOutput)

###END: regional economic impact analysis ####


###BEGIN: BAU projection####

# Series of GPD & Output projection
bauSeriesOfGDP <- data.frame(Sektor = ioSector[,1], stringsAsFactors = FALSE)
bauSeriesOfGDP$y2015 <- analysisGDP
bauSeriesOfFinalDemand <- rowSumsMatrixIoFinalDemand
bauSeriesOfOutput <- ioTotalOutput

# Series of Intervention Point
bauSeriesOfIntermediateDemand <- list()
bauSeriesOfAddedValue <- list()
bauSeriesOfFinalDemandComponent <- list()
bauSeriesOfImpactLabour <- list()
bauSeriesOfImpactEnergy <- list()
bauSeriesOfImpactWaste <- list()
bauSeriesOfImpactAgriculture <- list()

# Historical consumption and emission data
bauSeriesOfIntermediateDemand$y2015 <- matrixIoIntermediateDemand
bauSeriesOfAddedValue$y2015 <- matrixIoAddedValue
bauSeriesOfFinalDemandComponent$y2015 <- matrixIoFinalDemand
bauSeriesOfImpactLabour$y2015 <- functionSatelliteImpact('labour', satellite = satelliteLabour, matrix_output = as.matrix(ioTotalOutput))
bauSeriesOfImpactEnergy$y2015 <- functionSatelliteImpact('energy', satellite = satelliteEnergy, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorEnergy)
bauSeriesOfImpactWaste$y2015 <- functionSatelliteImpact('waste', satellite = satelliteWaste, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorWaste)
bauSeriesOfImpactAgriculture$y2015 <- functionSatelliteImpact('agriculture', satellite = satelliteAgriculture, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorAgriculture)

growthRateSeries <- growthRate
growthRateSeries$Lap_usaha <- NULL
growthRateSeries <- as.matrix(1+growthRateSeries)

projectionYear <- initialYear
listYear <- paste0("y", ioPeriod)
for(step in 1:(iteration+1)){
  projectionFinalDemand <- growthRateSeries[, step] * bauSeriesOfFinalDemand[, step]
  
  bauSeriesOfFinalDemand <- cbind(bauSeriesOfFinalDemand, projectionFinalDemand)
  projectionOutput <- ioLeontiefInverse %*% projectionFinalDemand
  bauSeriesOfOutput <- cbind(bauSeriesOfOutput, projectionOutput)
  
  # notes on the year
  timeStep <- paste0("y", projectionYear)
  
  # add additional values to the list
  eval(parse(text=paste0("bauSeriesOfFinalDemandComponent$", timeStep, " <- as.matrix(proportionFinalDemand*projectionFinalDemand)"))) # contains NaN
  eval(parse(text=paste0("bauSeriesOfIntermediateDemand$", timeStep, " <-  analysisCT %*% diag(as.vector(projectionOutput), ncol = ioDimention, nrow= ioDimention)")))
  eval(parse(text=paste0("bauSeriesOfAddedValue$", timeStep, " <-  analysisCPI %*% diag(as.vector(projectionOutput), ncol = ioDimention, nrow= ioDimention)")))
  
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

bauSeriesOfFinalDemandTable <- cbind(data.frame(ioSector$V1), bauSeriesOfFinalDemand)
colnames(bauSeriesOfFinalDemandTable) <- c("Sektor", as.character(listYear)) 


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
resultEnergyEmission <- bauSeriesOfImpactEnergy[[2]][[2]] #tahun 2016
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


###END: BAU####

###BEGIN: Scenario energy & transportation simulation####

proportionGDP <- bauSeriesOfGDP$y2015 / ioTotalOutput








###END: Energy & transportation####



################################################################################
#                                                                              #
#                       beberapa komponen SHEET ANALISIS                       #
#                                                                              #
################################################################################
# ioFinalDemand #findem
# ioFinalDemandComponent #findemcom
# colnames(ioFinalDemand) <- c(t(ioFinalDemandComponent))
# ioFinalDemand$`Total Permintaan Akhir` <- rowSums(ioFinalDemand)
# fdSelectYear <- ioFinalDemand$`Total Permintaan Akhir`
# 
# #permintaan akhir
# fdSelectYear <- ioFinalDemand$`Total Permintaan Akhir`
# 
# #output = leontif * permintaan akhir
# outputSelectYear <- ioLeontiefInverse %*% fdSelectYear
# 
# #Total output dari tabel IO -- > yang harusnya rowsum
# 
# ## udah ada diatas
# # matrixIoIntermediateDemand <- as.matrix(ioIntermediateDemand)
# # matrixIoFinalDemand <- as.matrix(ioFinalDemand)
# # proportionFinalDemand <- ioFinalDemand/rowSumsMatrixIoFinalDemand
# # proportionFinalDemand[is.na(proportionFinalDemand)] <- 0
# 
# # mas alfa pakai addvalue, aku pakai FD
# rowSumsMatrixIoIntermediateDemand <- rowSums(matrixIoIntermediateDemand)
# rowSumsMatrixIoAddedValue <- rowSums(matrixIoFinalDemand)
# ioTotalOutputRow <- rowSumsMatrixIoIntermediateDemand + rowSumsMatrixIoAddedValue # ioTotaloutput di atas dengan pakai AV
# 
# #cek
# cek <- outputSelectYear - ioTotalOutputRow
# 
# #PDRB AWAL
# pdrbAwal <- outputSelectYear * proporsiPDRB
# #colSums(pdrbAwal)
# 
# 
# 
# ################################################################################
# #                                                                              #
# #                                 SHEET ENERGI                                 #
# #                                                                              #
# ################################################################################
# #beberapa bagian 
# #Total konsumsi energi per Row
# totalKonsumsiEnergi <- satelliteEnergy[,3]
# #Total output
# totalOutput <- ioTotalOutputRow
# ## koefisien energi
# koefEnergi <- totalKonsumsiEnergi/ioTotalOutputRow
# 
# ## fungsi untuk sheet tabel energi --> hasil ekstrak dari satellliteimpact di apps shiny 
# satelliteImpactEnergy <- function(sat_type = "energy", tbl_sat = data.frame(), 
#                                   emission_lookup = data.frame()){ 
#   if(sat_type == "energy" | sat_type == "waste"){
#     impact <- list() # impact$cons; impact$emission
#     
#     impact$cons <- tbl_sat
#     
#     order_cname <- names(impact$cons)[4:ncol(impact$cons)]
#     em_f <- numeric()
#     for(m in 1:length(order_cname)){
#       em_f <- c(em_f, emission_lookup[which(emission_lookup[,1]==order_cname[m]), 2])
#     }
#     em_f <- diag(em_f, nrow = length(em_f), ncol = length(em_f))
#     
#     
#     #perkalian matriks
#     impact$emission <- impact$cons
#     impact$emission[,4:ncol(impact$emission)] <- as.matrix(impact$cons[,4:ncol(impact$cons)]) %*% em_f
#     impact$emission[,3] <- rowSums(impact$emission[,4: ncol(impact$emission)])
#     colnames(impact$emission)[3] <- "Temission"
#   } 
#   impact$cons[is.na(impact$cons)] <- 0
#   impact$emission[is.na(impact$emission)] <- 0
#   return(impact)
# }
# 
# #BAU: baseline
# tabelEmisiEnergiBAU <- satelliteImpactEnergy('energy', tbl_sat = satelliteEnergy, emission_lookup = emissionFactorEnergy)
# #tabelEmisiEnergiBAU$emission[,3] #total tabel emisi energi BAU
# 
# 
# ################################################################################
# #                                                                              #
# #                                SHEET PROYEKSI                                #
# #                                                                              #
# ################################################################################
# ## bagian FD
# fdCalculate <- function(tbl1, tbl2){
#   for(i in 1:ncol(tbl1)){
#     if(i == 1){
#       tbl1[,i] <- tbl2[,i]*(tbl1[,i] + 1)
#     } else {
#       tbl1[,i] <- tbl1[,i-1]*(tbl1[,i] + 1)
#     }
#   }
#   return(tbl1)
# }
# lengthYear <- iteration + 1
# column_year <- paste0("y", initialYear:finalYear )
# sectorLCD <- ioSector
# lengthSector <- nrow(sectorLCD)
# proyPertumEkonomi <- matrix(gdpRate,nrow = lengthSector,ncol = lengthYear)
# #rownames(proyPertumEkonomi) <- ioSector[,1]
# colnames(proyPertumEkonomi) <- column_year
# 
# 
# fdSelectYear <- ioFinalDemand$`Total Permintaan Akhir` #ini untuk BAU
# fdAllYear <- fdCalculate(tbl1 = proyPertumEkonomi,tbl2 = as.data.frame(fdSelectYear))
# 
# ## bagian Output
# outputAllYear <- ioLeontiefInverse %*% fdAllYear
# 
# 
# ## bagian PDRB
# proyPdrb <- outputAllYear*proporsiPDRB[,1]
# colProyPdrb <- colSums(proyPdrb)
# plot(initialYear : finalYear,colProyPdrb) #plot pdrb
# 
# ## proyeksi konsumsi energi
# #koefisien energi dari sheet energi
# #tabel konsumsi energi proyeksi
# proyKonsumsiEnergi <- outputAllYear*koefEnergi
# 
# # tabel proporsi energi yang diambil dari tahun 2015
# tabelKonsumsiEnergi <-  satelliteEnergy[,-(1:3)] #HILANGKAN ID,SECTOR DAN TOTAL KONS
# propEnergi <- tabelKonsumsiEnergi/totalKonsumsiEnergi
# 
# #terbentuk 15 tabel konsumsi energi
# proyTabelKonsEnergi<-list()
# for (i in 1:ncol(proyKonsumsiEnergi)) {
#   proyTabelKonsEnergi[[i]]<-proyKonsumsiEnergi[,i]*propEnergi
# }
# names(proyTabelKonsEnergi)<-paste0("y",initialYear:finalYear)
# 
# #matriks faktor emisi
# matEfBau <- numeric()
# order_energi_name <- names(satelliteEnergy)[4:ncol(satelliteEnergy)]
# for(m in 1:length(order_energi_name)){
#   matEfBau <- c(matEfBau, emissionFactorEnergy[which(emissionFactorEnergy[,1]==order_energi_name[m]), 2])
# }
# matEfBau <- diag(matEfBau, nrow = length(matEfBau), ncol = length(matEfBau))
# 
# # terbentuk 15 tabel proyeksi emisi
# proyEmisi <- list()
# for (i in 1:lengthYear) {
#   proyEmisi[[i]]<-as.matrix(proyTabelKonsEnergi[[i]]) %*% matEfBau
# }
# names(proyEmisi)<-paste0("y",initialYear:finalYear)
# 
# for (i in 1:lengthYear) {
#   if(i==1){
#     rowsumProyEmisi <- rowSums(proyEmisi[[i]])
#   }else{
#     rowsumProyEmisi<- cbind(rowsumProyEmisi,rowSums(proyEmisi[[i]]))
#   }
# }
# colnames(rowsumProyEmisi) <- paste0("y",initialYear:finalYear)
# 
# #COLSUM proyeksi energi
# colsumProyEmisi <- colSums(rowsumProyEmisi)
# cumProyEmisi <- cumsum(colsumProyEmisi)
# 
# 

