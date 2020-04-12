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

#untuk plot BAU pdrb
colProyPdrb <- resultGDP %>% 
  filter(between(year, initialYear, finalYear)) %>% 
  group_by(year) %>% 
  summarise(totalPDRB = sum(GDP))

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

#5. cumulatif proyeksi emeisi untuk plot proyeksi emisi
tabelConsum <- resultEnergyEmission

#6. alamat rds untuk menampilkan daftar di ListTableReact
selectedSektor <- "energi"
alamatFile <- paste0("_DB/skenarioData/", selectedProv, "/", "energi")

energyData <- list(koefisien=koefisien,
                   proportionConsumption=proportionConsumption,
                   faktorEmisi=faktorEmisi,
                   matEfBau=matEfBau,
                   tabelConsum=tabelConsum,
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

#5. cumulatif proyeksi emeisi untuk plot proyeksi emisi
tabelConsum <- resultWasteEmission

#6. alamat rds untuk menampilkan daftar di ListTableReact
selectedSektor <- "limbah"
alamatFile <- paste0("_DB/skenarioData/", selectedProv, "/", selectedSektor)

wasteData <- list(koefisien=koefisien,
                   proportionConsumption=proportionConsumption,
                   faktorEmisi=faktorEmisi,
                   matEfBau=matEfBau,
                   tabelConsum=tabelConsum,
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

#5. cumulatif proyeksi emeisi untuk plot proyeksi emisi
tabelConsum <- resultFertilizerEmission

#6. alamat rds untuk menampilkan daftar di ListTableReact
selectedSektor <- "pertanian"
alamatFile <- paste0("_DB/skenarioData/", selectedProv, "/", selectedSektor)

agriData <- list(koefisien=koefisien,
                  proportionConsumption=proportionConsumption,
                  faktorEmisi=faktorEmisi,
                  matEfBau=matEfBau,
                  tabelConsum=tabelConsum,
                 selectedSektor=selectedSektor,
                 alamatFile=alamatFile)

