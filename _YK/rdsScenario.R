### SULAWESI SELATAN ####
### Sistem Pertanian Terintegrasi
rdsSimantriEnergy <- readRDS("_DB/skenarioData/Sulawesi_Selatan/energi/dw_Sulawesi_Selatan_2020-06-03_13-29-40_sistempertanianterintegrasi")
rdsSimantriWaste <- readRDS("_DB/skenarioData/Sulawesi_Selatan/limbah/dw_Sulawesi_Selatan_2020-06-03_13-26-01_sistempertanianterintegrasi")
rdsSimantriAgri <- readRDS("_DB/skenarioData/Sulawesi_Selatan/pertanian/dw_Sulawesi_Selatan_2020-05-30_14-14-27_sistempertanianterintegrasi")

rdsSimantriAgri$satSelisihEnergy <- rdsSimantriEnergy$satSelisih
rdsSimantriAgri$satSelisihWaste <- rdsSimantriWaste$satSelisih

### Pengembangan Ternak Terpadu
rdsTernakEnergy <- readRDS("_DB/skenarioData/Sulawesi_Selatan/energi/dw_Sulawesi_Selatan_2020-06-03_13-30-00_pengembanganternakterpadu")
rdsTernakWaste <- readRDS("_DB/skenarioData/Sulawesi_Selatan/limbah/dw_Sulawesi_Selatan_2020-06-03_13-29-04_pengembanganternakterpadu")
rdsTernakAgri <- readRDS("_DB/skenarioData/Sulawesi_Selatan/pertanian/dw_Sulawesi_Selatan_2020-05-30_14-14-45_pengembanganternakterpadu")

rdsTernakAgri$satSelisihEnergy <- rdsSimantriEnergy$satSelisih
rdsTernakAgri$satSelisihWaste <- rdsSimantriWaste$satSelisih

### SUMATERA SELATAN ####
### Pengelolaan Tanaman Terpadu
rdsPTTEnergy <- readRDS("_DB/skenarioData/SumSel/energi/dw_SumSel_2020-06-04_16-20-39_PengelolaanTanamanTerpadu(PTT)")
rdsPTTWaste <- readRDS("_DB/skenarioData/SumSel/limbah/dw_SumSel_2020-06-04_16-20-15_PengelolaanTanamanTerpadu(PTT)")
rdsPTTAgri <- readRDS("_DB/skenarioData/SumSel/pertanian/dw_SumSel_2020-06-04_13-42-52_PengelolaanTanamanTerpadu(PTT)")

rdsPTTAgri$satSelisihEnergy <- rdsPTTEnergy$satSelisih
rdsPTTAgri$satSelisihWaste <- rdsPTTWaste$satSelisih

saveRDS(rdsPTTAgri, "_DB/skenarioData/SumSel/pertanian/dw_SumSel_2020-06-04_13-42-52_PengelolaanTanamanTerpadu(PTT)") 

### Alat Pencacah Pupuk Organik
rdsAPPOWaste <- readRDS("_DB/skenarioData/SumSel/limbah/dw_SumSel_2020-06-04_17-27-13_AlatPencacahPupukOrganik(APPO)")
rdsAPPOAgri <- readRDS("_DB/skenarioData/SumSel/pertanian/dw_SumSel_2020-06-04_13-42-30_AlatPencacahPupukOrganik(APPO)")

rdsAPPOAgri$satSelisihWaste <- rdsPTTWaste$satSelisih

saveRDS(rdsAPPOAgri, "_DB/skenarioData/SumSel/pertanian/dw_SumSel_2020-06-04_13-42-30_AlatPencacahPupukOrganik(APPO)") 

### JAWA BARAT ####
### IPAL Aerobik ###
rdsIPALWaste <- readRDS("_DB/skenarioData/Jabar/limbah/dw_JaBar_2020-06-16_22-49-40_PengelolaanAirLimbahSecaraTerpusatAerobik")

saveRDS(rdsIPALWaste, "_DB/skenarioData/Jabar/limbah/dw_JaBar_2020-06-16_22-49-40_PengelolaanAirLimbahSecaraTerpusatAerobik") 

### Komposting TPA ###
rdsTPAWaste <- readRDS("_DB/skenarioData/Jabar/limbah/dw_JaBar_2020-06-16_22-50-03_RencanaKompostingdiTPA")

saveRDS(rdsTPAWaste, "_DB/skenarioData/Jabar/limbah/dw_JaBar_2020-06-16_22-50-03_RencanaKompostingdiTPA") 

### Pembangunan TPS 3R ###
rdsTPSWaste <- readRDS("_DB/skenarioData/Jabar/limbah/dw_JaBar_2020-06-16_22-52-07_RencanaPembangunananOperasionalTPSTerpadu3R")

saveRDS(rdsTPSWaste, "_DB/skenarioData/Jabar/limbah/dw_JaBar_2020-06-16_22-52-07_RencanaPembangunananOperasionalTPSTerpadu3R") 
