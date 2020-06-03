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
