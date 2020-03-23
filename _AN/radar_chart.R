#data preparation
prov = "Banten"
io_folder <- paste0( prov, "/")
data_folder <- paste0("data/", prov, "/")
if(!dir.exists(data_folder)) dir.create(data_folder, mode = 777)

inLabour<- paste0(io_folder, "/07_tenaga_kerja.csv")
inEnergy<- paste0(io_folder, "/08_satelit_energi.csv")
inWaste<- paste0(io_folder, "/09_satelit_limbah.csv")
inEmissionFactorEnergiTable<- paste0(io_folder, "/10_faktor_emisi_energi.csv")
inEmissionFactorWasteTable<- paste0(io_folder, "/11_faktor_emisi_limbah.csv")
inPopTable <- paste0(io_folder, "/12_population.csv")
inEmOtherTable <- paste0(io_folder, "/13_emission_from_other.csv")
inLandTable <- paste0(io_folder, "/14_satelit_lahan.csv")

labour <- read.table(inLabour, header=TRUE, sep=",")
energy <- read.table(inEnergy, header=TRUE, sep=",")
waste <- read.table(inWaste, header=TRUE, sep=",")
ef_energy <- read.table(inEmissionFactorEnergiTable, header=TRUE, sep=",")
ef_waste <- read.table(inEmissionFactorWasteTable, header=TRUE, sep=",")
population <- read.table(inPopTable, header=TRUE, sep=",")
otherEm <- read.table(inEmOtherTable, header=TRUE, sep=",")
landtable <- read.table(inLandTable, header=TRUE, sep=",")

setwd(data_folder)
saveRDS(labour , "labour")
saveRDS(energy , "energy")
saveRDS(waste , "waste")
saveRDS(ef_energy , "ef_energy")
saveRDS(ef_waste , "ef_waste")
saveRDS(landtable , "landtable")
saveRDS(population, "population")
saveRDS(otherEm , "otherEm")

setwd("d:/MRV/Baseline/Emisi All Sector/Tin_tengah/")





prov = "DIY"
io_folder <- paste0( prov, "/")
data_folder <- paste0("data/", prov, "/")
if(!dir.exists(data_folder)) dir.create(data_folder, mode = 777)
# inEnergy<- paste0(io_folder, "/08_satelit_energi.csv")
inEmissionFactorWasteTable<- paste0(io_folder, "/11_faktor_emisi_limbah.csv")
# energy <- read.table(inEnergy, header=TRUE, sep=",")
ef_waste <- read.table(inEmissionFactorWasteTable, header=TRUE, sep=",")
setwd(data_folder)
# saveRDS(energy , "energy")
saveRDS(ef_waste , "ef_waste")

setwd("d:/MRV/Baseline/Emisi All Sector/Tin_tengah/")



usersList <- data.frame(id=NULL, fullname=NULL, username=NULL, password=NULL, provinsi=NULL)
tes <- data.frame(id=0001, fullname="Alfa Nugraha", username="alfanugraha", password=digest("alfa", algo="md5", serialize=F), provinsi="Aceh")

tes = data.frame(time=Sys.time(), action= "EBT", year=2011, username="alfanugraha", provinsi="Aceh", sector="Tambang", fd_value=100)
if(file.exists("prk")){
  tes1<-readRDS("prk")
  tes1<-rbind(tes1, tes)
} else {
  saveRDS(tes, "prk"))
}
  


#radar chrart
tabel_radarchart <- test[test$Sektor=="Kelapa Sawit",]
tabel_radar<-tabel_radarchart
tabel_radar$Sektor <- NULL
tabel_radarmax <- data.frame(pengganda1=max(test$pengganda1), 
                             pengganda2=max(test$pengganda2), 
                             pengganda3=max(test$pengganda3), 
                             pengganda4=max(test$pengganda4)
                             )
tabel_radarmin <- data.frame(pengganda1=min(test$pengganda1), 
                             pengganda2=min(test$pengganda2), 
                             pengganda3=min(test$pengganda3), 
                             pengganda4=min(test$pengganda4)
                             )
tabel_radar <- rbind(tabel_radarmax, tabel_radarmin, tabel_radar)
radarchart(tabel_radar)



isian<-data.frame('Sektor'=c("a","b","c","d","e"),'y2011'=c(0.2,0.2,0.2,0.2,0.2),'y2012'=c(0.2,0.2,0.2,0.2,0.2),'y2013'=c(0.2,0.2,0.2,0.2,0.2))
