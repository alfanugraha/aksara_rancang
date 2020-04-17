### global variable

#ID
selectedSektor <- "energi"
username<-"dw"


### nama 52 Sector
Sector<-ioSector[,1]
Sector <- as.character(Sector)

### DATA MASTER
fdBau <- bauSeriesOfFinalDemandTable[,-2] #tabel 2015 nya ga masuk
fdBau$Sektor <- as.character(fdBau$Sektor) 

## FD zero
fdZero <- fdBau
fdZero[,2:16] <- 0

#untuk plot BAU pdrb
colProyPdrb <- resultGDP %>% 
  filter(between(year, initialYear, finalYear)) %>% 
  group_by(year) %>% 
  summarise(totalPDRB = sum(GDP))

colProyPdrbDF <- colProyPdrb
colProyPdrbDF$year <- paste0("y",colProyPdrbDF$year)
colProyPdrbDF$scenario <- c("BAU")



################################################################################
#                                                                              #
#                                sektor energi                                 #
#                                                                              #
################################################################################




#1. koefisien 
koefisien <- as.numeric(analysisCE) #energi

#2. proporsi y2015
proportionConsumption <- satelliteEnergy[, 4:ncol(satelliteEnergy)] / satelliteEnergy[, 3] #energi
proportionConsumption[is.na(proportionConsumption)] <- 0

#3. daftar nama FAKTOR EMISI 
faktorEmisi <- as.character(emissionFactorEnergy[,1])  ###energi: nama 26 bahan bakar

#4. matriks diagonal faktor emisi
matEfBau <- emissionFactorEnergyDiagonal

#5. list konsumsi energi
listConsumBAU <- lapply(bauSeriesOfImpactEnergy, 
                    function(x){
                      x[[1]]
                    })
listConsumBAU <- listConsumBAU[-1] #tahun 2015 dihilangkan

listConsumZero <- lapply(listConsumBAU, function(x){
  x[, 3:ncol(bauSeriesOfImpactEnergy[[1]][[1]])] <- 0 #dari kolom tcons sampai bahan bakar terakhir
  return(x)
})

#bentuk dataframe
tabelConsumZero <- resultEnergyConsumption 
tabelConsumZero[,4:30] <- 0

#6. cumulatif proyeksi emisi untuk plot proyeksi emisi BAU
tabelEmisi <- resultEnergyEmission


#7. alamat rds untuk menampilkan daftar di ListTableReact
selectedSektor <- "energi"
alamatFile <- paste0("_DB/skenarioData/", selectedProv, "/", "energi")

energyData <- list(koefisien=koefisien,
                   proportionConsumption=proportionConsumption,
                   faktorEmisi=faktorEmisi,
                   matEfBau=matEfBau,
                   tabelEmisi=tabelEmisi,
                   listConsumBAU=listConsumBAU,
                   listConsumZero=listConsumZero,
                   selectedSektor=selectedSektor,
                   alamatFile=alamatFile)

################################################################################
#                                                                              #
#                                sektor limbah                                 #
#                                                                              #
################################################################################

#1. koefisien 
koefisien <- as.numeric(analysisCW) #energi

#2. proporsi y2015
proportionConsumption <- satelliteWaste[, 4:ncol(satelliteWaste)] / satelliteWaste[, 3] #energi
proportionConsumption[is.na(proportionConsumption)] <- 0

#3. daftar nama FAKTOR EMISI 
faktorEmisi <- as.character(emissionFactorWaste[,1])  ###energi: nama 26 bahan bakar

#4. matriks diagonal faktor emisi
matEfBau <- emissionFactorWasteDiagonal

#5. list konsumsi energi
listConsumBAU <- lapply(bauSeriesOfImpactWaste, 
                        function(x){
                          x[[1]]
                        })
listConsumBAU <- listConsumBAU[-1] #tahun 2015 dihilangkan

listConsumZero <- lapply(listConsumBAU, function(x){
  x[, 3:ncol(bauSeriesOfImpactWaste[[1]][[1]])] <- 0 #dari kolom tcons sampai bahan bakar terakhir
  return(x)
})

#dataframe
tabelConsumBAU <- resultWasteDisposal
tabelConsumZero <- tabelConsumBAU 
tabelConsumZero[,4:30] <- 0

#6. cumulatif proyeksi emisi untuk plot proyeksi emisi BAU
tabelEmisi <- resultWasteEmission

#7. alamat rds untuk menampilkan daftar di ListTableReact
selectedSektor <- "limbah"
alamatFile <- paste0("_DB/skenarioData/", selectedProv, "/", selectedSektor)

wasteData <- list(koefisien=koefisien,
                   proportionConsumption=proportionConsumption,
                   faktorEmisi=faktorEmisi,
                   matEfBau=matEfBau,
                  tabelEmisi=tabelEmisi,
                  listConsumBAU=listConsumBAU,
                  listConsumZero=listConsumZero,
                  selectedSektor=selectedSektor,
                  alamatFile=alamatFile)

################################################################################
#                                                                              #
#                                sektor pertanian                              #
#                                                                              #
################################################################################

#1. koefisien 
koefisien <- as.numeric(analysisCA) #energi

#2. proporsi y2015
proportionConsumption <- satelliteAgriculture[, 4:ncol(satelliteAgriculture)] / satelliteAgriculture[, 3] #energi
proportionConsumption[is.na(proportionConsumption)] <- 0

#3. daftar nama FAKTOR EMISI 
faktorEmisi <- as.character(emissionFactorAgriculture[,1])  ###energi: nama 26 bahan bakar

#4. matriks diagonal faktor emisi
matEfBau <- emissionFactorAgricultureDiagonal

#5. list konsumsi energi
listConsumBAU <- lapply(bauSeriesOfImpactAgriculture, 
                        function(x){
                          x[[1]]
                        })
listConsumBAU <- listConsumBAU[-1] #tahun 2015 dihilangkan

listConsumZero <- lapply(listConsumBAU, function(x){
  x[, 3:ncol(bauSeriesOfImpactAgriculture[[1]][[1]])] <- 0 #dari kolom tcons sampai bahan bakar terakhir
  return(x)
})

#dataframe
tabelConsumBAU <- resultFertilizerUsed
tabelConsumZero <- tabelConsumBAU 
tabelConsumZero[,4:30] <- 0

#6. cumulatif proyeksi emisi untuk plot proyeksi emisi BAU
tabelEmisi <- resultFertilizerEmission

#7. alamat rds untuk menampilkan daftar di ListTableReact
selectedSektor <- "pertanian"
alamatFile <- paste0("_DB/skenarioData/", selectedProv, "/", selectedSektor)

agriData <- list(koefisien=koefisien,
                  proportionConsumption=proportionConsumption,
                  faktorEmisi=faktorEmisi,
                  matEfBau=matEfBau,
                 tabelEmisi=tabelEmisi,
                 listConsumBAU=listConsumBAU,
                 listConsumZero=listConsumZero,
                 selectedSektor=selectedSektor,
                 alamatFile=alamatFile)

