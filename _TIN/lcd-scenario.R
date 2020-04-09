### input BAU

###BEGIN: initiate all variables ####
selectedProv = "JaBar"
datapath <- paste0("_DB/data/", selectedProv, "/")
datapathCSV <- ("_DB/input csv")


ioSector <- readRDS(paste0(datapath, "sector"))
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
emissionFactorEnergy <- readRDS(paste0(datapath, "ef_energy"))
emissionFactorWaste <- readRDS(paste0(datapath, "ef_waste"))
population <- readRDS(paste0(datapath, "currentPopulation"))
populationProjection <- readRDS(paste0(datapath, "population"))
baselineEmission <- readRDS(paste0(datapath, "otherEm"))

inProporsiPDRB <- paste0(datapathCSV, "/16_proporsi_pdrb.csv")
proporsiPDRB <- read.table(inProporsiPDRB, header=TRUE, sep=",", stringsAsFactors = F)


################################################################################
#                                                                              #
#                                    INPUT                                     #
#                                                                              #
################################################################################
gdpRate <- 5/100 #user input
yearFrom <- 2016 #user input
yearTo <- 2030 #user input

# BAU ---------------------------------------------------------------------
ioIntermediateDemand #user input
energyBau <- satelliteEnergy
efBau <- emissionFactorEnergy

################################################################################
#                                                                              #
#                                   SHEET IO                                   #
#                                                                              #
################################################################################
# perhitungan table IO
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
leontief <- solve(I_A)

matrixIoIntermediateDemand <- as.matrix(ioIntermediateDemand) 
matrixIoAddedValue <- as.matrix(ioAddedValue) #asumsikan dulu tidak ada penambahan import, jd tdk berubah importnya
nrowMatrixIoAddedValue <- nrow(matrixIoAddedValue)
ioDimention <- ncol(ioIntermediateDemand) 

colSumsMatrixIoIntermediateDemand <- colSums(matrixIoIntermediateDemand)
colSumsMatrixIoAddedValue <- colSums(matrixIoAddedValue)
ioTotalOutput <- colSumsMatrixIoIntermediateDemand + colSumsMatrixIoAddedValue # ioTotalInput 
ioTotalOutputInverse <- 1/ioTotalOutput
ioTotalOutputInverse[is.infinite(ioTotalOutputInverse)] <- 0
ioTotalOutputInverse <- diag(ioTotalOutputInverse)



################################################################################
#                                                                              #
#                       beberapa komponen SHEET ANALISIS                       #
#                                                                              #
################################################################################
findem <- ioFinalDemand
findemcom <- ioFinalDemandComponent
colnames(findem) <- c(t(findemcom))
findem$`Total Permintaan Akhir` <- rowSums(findem)
fdSelectYear <- findem$`Total Permintaan Akhir`

#permintaan akhir
fdSelectYear <- findem$`Total Permintaan Akhir`

#output = leontif * permintaan akhir
outputSelectYear <- leontief %*% fdSelectYear

#Total output dari tabel IO -- > yang harusnya rowsum
matrixIoIntermediateDemand <- as.matrix(ioIntermediateDemand)
matrixIoFinalDemand <- as.matrix(ioFinalDemand)

rowSumsMatrixIoIntermediateDemand <- rowSums(matrixIoIntermediateDemand)
rowSumsMatrixIoAddedValue <- rowSums(matrixIoFinalDemand)
ioTotalOutputRow <- rowSumsMatrixIoIntermediateDemand + rowSumsMatrixIoAddedValue # ioTotaloutput 

#cek
cek <- outputSelectYear - ioTotalOutputRow

#PDRB AWAL
pdrbAwal <- outputSelectYear * proporsiPDRB
#colSums(pdrbAwal)



################################################################################
#                                                                              #
#                                 SHEET ENERGI                                 #
#                                                                              #
################################################################################
#beberapa bagian 
#Total konsumsi energi per Row
totalKonsumsiEnergi <- energyBau[,3]
#Total output
totalOutput <- ioTotalOutputRow
## koefisien energi
koefEnergi <- totalKonsumsiEnergi/ioTotalOutputRow

## fungsi untuk sheet tabel energi --> hasil ekstrak dari satellliteimpact di apps shiny 
satelliteImpactEnergy <- function(sat_type = "energy", tbl_sat = data.frame(), 
                                  emission_lookup = data.frame()){ 
  if(sat_type == "energy" | sat_type == "waste"){
    impact <- list() # impact$cons; impact$emission
    
    impact$cons <- tbl_sat
    
    order_cname <- names(impact$cons)[4:ncol(impact$cons)]
    em_f <- numeric()
    for(m in 1:length(order_cname)){
      em_f <- c(em_f, emission_lookup[which(emission_lookup[,1]==order_cname[m]), 2])
    }
    em_f <- diag(em_f, nrow = length(em_f), ncol = length(em_f))
    
    
    #perkalian matriks
    impact$emission <- impact$cons
    impact$emission[,4:ncol(impact$emission)] <- as.matrix(impact$cons[,4:ncol(impact$cons)]) %*% em_f
    impact$emission[,3] <- rowSums(impact$emission[,4: ncol(impact$emission)])
    colnames(impact$emission)[3] <- "Temission"
  } 
  impact$cons[is.na(impact$cons)] <- 0
  impact$emission[is.na(impact$emission)] <- 0
  return(impact)
}

#BAU: baseline
tabelEmisiEnergiBAU <- satelliteImpactEnergy('energy', tbl_sat = energyBau, emission_lookup = efBau)
#tabelEmisiEnergiBAU$emission[,3] #total tabel emisi energi BAU


################################################################################
#                                                                              #
#                                SHEET PROYEKSI                                #
#                                                                              #
################################################################################
## bagian FD
fdCalculate <- function(tbl1, tbl2){
  for(i in 1:ncol(tbl1)){
    if(i == 1){
      tbl1[,i] <- tbl2[,i]*(tbl1[,i] + 1)
    } else {
      tbl1[,i] <- tbl1[,i-1]*(tbl1[,i] + 1)
    }
  }
  return(tbl1)
}
lengthYear <- (yearTo - yearFrom)+1
column_year <- paste0("y", yearFrom:yearTo )
sectorLCD <- ioSector
lengthSector <- nrow(sectorLCD)
proyPertumEkonomi <- matrix(gdpRate,nrow = lengthSector,ncol = lengthYear)
#rownames(proyPertumEkonomi) <- ioSector[,1]
colnames(proyPertumEkonomi) <- column_year


fdSelectYear <- findem$`Total Permintaan Akhir` #ini untuk BAU
fdAllYear <- fdCalculate(tbl1 = proyPertumEkonomi,tbl2 = as.data.frame(fdSelectYear))

## bagian Output
outputAllYear <- leontief %*% fdAllYear


## bagian PDRB
proyPdrb <- outputAllYear*proporsiPDRB[,1]
colProyPdrb <- colSums(proyPdrb)
plot(yearFrom : yearTo,colProyPdrb) #plot pdrb

## proyeksi konsumsi energi
#koefisien energi dari sheet energi
#tabel konsumsi energi proyeksi
proyKonsumsiEnergi <- outputAllYear*koefEnergi

# tabel proporsi energi yang diambil dari tahun 2015
tabelKonsumsiEnergi <-  energyBau[,-(1:3)] #HILANGKAN ID,SECTOR DAN TOTAL KONS
propEnergi <- tabelKonsumsiEnergi/totalKonsumsiEnergi

#terbentuk 15 tabel konsumsi energi
proyTabelKonsEnergi<-list()
for (i in 1:ncol(proyKonsumsiEnergi)) {
  proyTabelKonsEnergi[[i]]<-proyKonsumsiEnergi[,i]*propEnergi
}
names(proyTabelKonsEnergi)<-paste0("y",yearFrom:yearTo)

#matriks faktor emisi
matEfBau <- numeric()
order_energi_name <- names(energyBau)[4:ncol(energyBau)]
for(m in 1:length(order_energi_name)){
  matEfBau <- c(matEfBau, efBau[which(efBau[,1]==order_energi_name[m]), 2])
}
matEfBau <- diag(matEfBau, nrow = length(matEfBau), ncol = length(matEfBau))

# terbentuk 15 tabel proyeksi emisi
proyEmisi <- list()
for (i in 1:lengthYear) {
  proyEmisi[[i]]<-as.matrix(proyTabelKonsEnergi[[i]]) %*% matEfBau
}
names(proyEmisi)<-paste0("y",yearFrom:yearTo)

for (i in 1:lengthYear) {
  if(i==1){
    rowsumProyEmisi <- rowSums(proyEmisi[[i]])
  }else{
    rowsumProyEmisi<- cbind(rowsumProyEmisi,rowSums(proyEmisi[[i]]))
  }
}
colnames(rowsumProyEmisi) <- paste0("y",yearFrom:yearTo)

#COLSUM proyeksi energi
colsumProyEmisi <- colSums(rowsumProyEmisi)
cumProyEmisi <- cumsum(colsumProyEmisi)

################################################################################
#                                                                              #
#                            save data to rds file                             #
#                                                                              #
################################################################################
#BAU
# saveRDS(fdAllYear,"_DB/save rds file/fdAllYearBAU.rds") #FD
# saveRDS(proyPdrb,"_DB/save rds file/proyPdrbBAU.rds") #PDRB
# saveRDS(proyTabelKonsEnergi,"_DB/save rds file/proyTabelKonsEnergiBAU.rds") #PROYEKSI  KONSUMSI 
# saveRDS(proyEmisi,"_DB/save rds file/proyEmisiBAU.rds") #PROYEKSI EMISIsaveRDS(rowsumProyEmisiSken2,"_DB/save rds file/rowsumProyEmisiSken2.rds") #rowsum emisi
# saveRDS(rowsumProyEmisi,"_DB/save rds file/rowsumProyEmisiBAU.rds") #rowsum emisi
# saveRDS(colsumProyEmisi,"_DB/save rds file/colsumProyEmisiBAU.rds") #colsum emisi


