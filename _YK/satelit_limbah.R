inSector <- "D:/YUMNA/ICRAF/aksara_rancang/_YK/TENGAH/Jawa_Barat/01_sektor.csv"
inIntermediateDemand <- "D:/YUMNA/ICRAF/aksara_rancang/_YK/TENGAH/Jawa_Barat/02_input_antara.csv"
inFinalDemandComp <- "D:/YUMNA/ICRAF/aksara_rancang/_YK/TENGAH/Jawa_Barat/03_komponen_permintaan_akhir.csv"
inFinalDemand <- "D:/YUMNA/ICRAF/aksara_rancang/_YK/TENGAH/Jawa_Barat/04_permintaan_akhir.csv"
inAddedValueComp <- "D:/YUMNA/ICRAF/aksara_rancang/_YK/TENGAH/Jawa_Barat/05_komponen_input_primer.csv"
inAddedValue <- "D:/YUMNA/ICRAF/aksara_rancang/_YK/TENGAH/Jawa_Barat/06_input_primer.csv"
inLabour<- "D:/YUMNA/ICRAF/aksara_rancang/_YK/TENGAH/Jawa_Barat/07_tenaga_kerja.csv"
inWaste<- "D:/YUMNA/ICRAF/aksara_rancang/_YK/TENGAH/Jawa_Barat/09_satelit_limbah.csv"
inEmissionFactorLandWasteTable<- "D:/YUMNA/ICRAF/aksara_rancang/_YK/TENGAH/Jawa_Barat/11_faktor_emisi_limbah.csv"
# inEnergy<- "_YK/TENGAH/Jawa_Barat/08_satelit_energi.csv"
# inEmissionFactorEnergiTable<- "_YK/TENGAH/Jawa_Barat/10_faktor_emisi_energi.csv"


sector <- read.table(inSector, header=FALSE, sep=",")
indem <- read.table(inIntermediateDemand, header=FALSE, sep=",")
findem <- read.table(inFinalDemand, header=FALSE, sep=",")
addval <- read.table(inAddedValue, header=FALSE, sep=",")
findemcom <- read.table(inFinalDemandComp, header=FALSE, sep=",")
addvalcom <- read.table(inAddedValueComp, header=FALSE, sep=",")
labour <- read.table(inLabour, header=TRUE, sep=",")
waste <- read.table(inWaste, header=TRUE, sep=",")
ef_waste <- read.table(inEmissionFactorLandWasteTable, header=TRUE, sep=",")
# energy <- read.table(inEnergy, header=TRUE, sep=",")
# ef_energy <- read.table(inEmissionFactorEnergiTable, header=TRUE, sep=",")

### TAB IO ####
## Konversi ke matrix ##
income_row <- 2
indem_matrix <- as.matrix(indem)

addval_matrix <- as.matrix(addval)
num_addval <- nrow(addval_matrix)
num_indem <- ncol(indem_matrix)

## Menjumlahkan Kolom dan Baris ##
indem_colsum <- colSums(indem_matrix)
addval_colsum <- colSums(addval_matrix)
fin_con <- 1/(indem_colsum+addval_colsum)
fin_con[is.infinite(fin_con)] <- 0
tinput_invers <- diag(fin_con)

## Matriks Leontief ##
A <- indem_matrix %*% tinput_invers
I <- as.matrix(diag(num_indem))
I_A <- I-A
library(matlib)
leontief <- inv(I_A)

## Koefisien Nilai Tambah ##
addedval_coef<- addval_matrix %*% tinput_invers

### TAB ANALISIS ####
## Backward & Forward Linkage ##
BL <- colSums(leontief)
BL <- BL/(mean(BL))
FL <- rowSums(leontief)
FL <- FL/(mean(FL))

## Tenaga Kerja ##
total_output <- (indem_colsum+addval_colsum)
#labour_coef <- labour$Tenaga_Kerja/total_output
labour_coef <- tinput_invers %*% as.matrix(labour[,3])
multiplierLabour <- leontief %*% labour_coef

## Multiplier Output ##
multiplierOutput <- colSums(leontief)

## Pendapatan ##
pendapatan <- as.data.frame(addval_matrix[2,])
colnames(pendapatan)<-"Pendapatan"
koef_pendapatan <- as.matrix(pendapatan/total_output)
multiplierPendapatan <- leontief %*% koef_pendapatan
colnames(multiplierPendapatan)<-"Pengganda Pendapatan"

## Permintaan Akhir ##
total_findem <- rowSums(findem)

#Output = Leontief * PA
output <- leontief %*% total_findem
cek_selisih <- output - total_output #beda dengan yang di excel


## Total GDP/PDRB (bentuk kondisi dimana tidak ada "Impor" pada input primer) ##
if(addvalcom$V1[1]=="Impor"){
  GDP <- colSums(addval_matrix[2:num_addval,])
} else {
  GDP <- colSums(addval_matrix[1:num_addval,])
}

## Proporsi PDRB ##
prop_pdrb <- GDP/total_output
pdrb_awal <- output * prop_pdrb

## Hasil simulasi ##
findem_simulasi <- total_findem * (1+0.05) #satuan persen
total_output_simulasi <- leontief %*% findem_simulasi
pdrb_simulasi <- total_output_simulasi * prop_pdrb

## SATELIT LIMBAH ####

## Multiplier Waste Product ##
waste_coef <- tinput_invers %*% as.matrix(waste$Total)
multiplierWaste <- leontief %*% waste_coef

## Koefisien Produk Limbah ##
total_pdrb <- as.data.frame(GDP)
koef_prodLimbah <- waste$Total/sum(total_pdrb$GDP)

## Emisi Produk Limbah ##
num_ef <- length(ef_waste$Faktor.Emisi)
ef_matrix <- diag(as.vector(ef_waste$Faktor.Emisi), ncol = num_ef, nrow = num_ef)
waste_matrix <- as.matrix(waste[,3:18]) %*% ef_matrix
emisi_produk_limbah <- rowSums(waste_matrix)

gdpRate <- 0.05
tahun_awal <- 2016
tahun_akhir <- 2030
year <- tahun_awal:tahun_akhir

for (i in year){
  sector <- as.matrix(sector)
  colnames(sector) <- "Lapangan Usaha"
  gdpRateTable <- matrix(gdpRate,ncol = length(year), nrow = length(sector))
  rownames(gdpRateTable) <- sector
  colnames(gdpRateTable) <- year
  # gdpRateTable<-as.data.frame(gdpRateTable)
}

#isi masih sama, ini baru 1 tahun aja
for (i in 1:length(sector)){
  for (j in 1:length(year)){
    total_findem <- as.matrix(total_findem)
    nilai_proyeksi_fd[i] <- total_findem[i,1] * (1+gdpRateTable[i,j])
    nilai_proyeksi_fd<-cbind(nilai_proyeksi_fd[i])
    matriks_proyeksi_fd <- matrix(nilai_proyeksi_fd,ncol = length(year), nrow = length(sector))
    colnames(matriks_proyeksi_fd) <- year
    proyeksi_findem <- cbind(sector, total_findem, matriks_proyeksi_fd)
    
    return(matriks_proyeksi_fd)
  }
}

tbl1 <- data.frame(row.names = LETTERS[1:5],
                   Tahun1 = sample(1:5, 5),
                   Tahun2 = sample(1:5, 5),
                   Tahun3 = sample(1:5, 5),
                   Tahun4 = sample(1:5, 5),
                   Tahun5 = sample(1:5, 5),
                   stringsAsFactors = FALSE)
tbl2 <- data.frame(row.names = LETTERS[1:5],
                   TahunIni = 1:5*1000,
                   stringsAsFactors = F)
tbl_hitung <- function(tbl1, tbl2){
  # library(dplyr)
  for(i in 1:ncol(tbl1)){
    if(i == 1){
      tbl1[,i] <- tbl2[,i]*(tbl1[,i] + 1)
    } else {
      tbl1[,i] <- tbl1[,i-1]*(tbl1[,i] + 1)
    }
  }
  return(tbl1)
}
tbl3 <- tbl_hitung(tbl1, tbl2)










# waste_matrix <- diag(as.vector(energy_coef), ncol = num_indem, nrow = num_indem)
# InvWaste_matrix <- diag(as.vector(1/waste_coef), ncol = num_indem, nrow = num_indem)
# multiplierWaste <- waste_matrix %*% leontief %*% InvWaste_matrix
# multiplierWaste <- as.matrix(colSums(multiplierWaste), num_indem, 1)
# multiplierWaste[is.na(multiplierWaste)] <- 0

# labour_matrix <- diag(as.vector(labour_coef), ncol = num_indem, nrow = num_indem)
# InvLabour_matrix <- diag(as.vector(1/labour_coef), ncol = num_indem, nrow = num_indem)
# multiplierLabour <- labour_matrix %*% leontief %*% InvLabour_matrix
# multiplierLabour <- as.matrix(colSums(multiplierLabour), num_indem, 1)
# multiplierLabour[is.na(multiplierLabour)] <- 0

# GDP (bentuk kondisi dimana tidak ada "Impor" pada input primer)
if(addvalcom$V1[1]=="Impor"){
  GDP <- colSums(addval_matrix[2:num_addval,])
} else {
  GDP <- colSums(addval_matrix[1:num_addval,])
}

# Multiplier Income (bentuk kondisi dimana tidak ada "Impor" pada input primer)
if(addvalcom$V1[1]=="Impor"){
  income_coef <- tinput_invers %*% as.matrix(addval_matrix[income_row,])
} else {
  income_coef <- tinput_invers %*% as.matrix(addval_matrix[(income_row-1),])
}

multiplierIncome <- leontief %*% income_coef
multiplierIncome[is.na(multiplierIncome)] <- 0


# Multiplier Energy Used
energy_coef <- tinput_invers %*% as.matrix(energy[,3])
energy_matrix <- diag(as.vector(energy_coef), ncol = num_indem, nrow = num_indem)
InvEnergy_matrix <- diag(as.vector(1/energy_coef), ncol = num_indem, nrow = num_indem)
multiplierEnergy <- energy_matrix %*% leontief %*% InvEnergy_matrix
multiplierEnergy <- as.matrix(colSums(multiplierEnergy), num_indem, 1)
multiplierEnergy[is.na(multiplierEnergy)] <- 0

# Ratio Wages / Business Surplus
ratio_ws <- t(as.matrix(addval[2,] / addval[3,]))
ratio_ws[is.na(ratio_ws)] <- 0
ratio_ws[ratio_ws == Inf] <- 0
colnames(ratio_ws) <- "ratio_ws"
# Koefisien Intensitas Energi
# total sectoral energy cons / sectoral GDP
coef_energy <- as.matrix(energy[,3]) / sum(addval_matrix[2:num_addval,])
# Koefisien Produk Limbah
coef_waste <- as.matrix(waste[,3]) / sum(addval_matrix[2:num_addval,])
# Emission from energy
f_energy_diag <- diag(ef_energy[,2], ncol = nrow(ef_energy), nrow = nrow(ef_energy))
em_energy <- as.matrix(energy[,4:ncol(energy)]) %*% f_energy_diag
em_energy_total <- rowSums(em_energy)
# Emission from waste
f_waste_diag <- diag(ef_waste[,2], ncol = nrow(ef_waste), nrow = nrow(ef_waste))
em_waste <- as.matrix(waste[,4:ncol(waste)]) %*% f_waste_diag
em_waste_total <- rowSums(em_waste)
# Wages
wages <- as.matrix(t(addval[2,]))
colnames(wages) <- "wages"

# Income per capita
income_per_capita <- sum(as.matrix(addval_matrix[income_row,])) #/input$popDensTable

result <- cbind(sector,
                BL,
                FL, 
                GDP, 
                multiplierOutput, 
                multiplierIncome,
                multiplierLabour,
                multiplierEnergy,
                multiplierWaste,
                wages,
                ratio_ws, 
                coef_energy,
                coef_waste,
                em_energy_total,
                em_waste_total
)
colnames(result)[1] <- "Sektor"

list_table <- list(result=result,
                   sector=sector, 
                   indem=indem, 
                   findem=findem, 
                   addval=addval, 
                   labour=labour, 
                   energy=energy, 
                   findemcom=findemcom, 
                   addvalcom=addvalcom,
                   waste=waste,
                   ef_waste=ef_waste,
                   ef_energy=ef_energy,
                   income_per_capita=income_per_capita
) 


analysisResult <- result
income_per_capita <- income_per_capita

###Normalization - Plotly: Radar Chart
selectMultiSector<-"Yumna/TENGAH/Jawa_Barat/01_sektor.csv"
selectMultiSector <- read.table(selectMultiSector, header=FALSE, sep=",")
selectedSector <- "Tanaman Pangan "
multiplierTable <- subset(analysisResult, select = c(Sektor, multiplierIncome, multiplierOutput, multiplierLabour, multiplierEnergy, multiplierWaste))
tabel_radarchart <- multiplierTable[multiplierTable==selectedSector,]
normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

tabel_radar<-normalize(tabel_radarchart[2:6])
nilai_temp<-t(tabel_radar)

multiplierIncome<-normalize(multiplierTable$multiplierIncome)
multiplierOutput<-normalize(multiplierTable$multiplierOutput)
multiplierLabour<-normalize(multiplierTable$multiplierLabour)
multiplierEnergy<-normalize(multiplierTable$multiplierEnergy)
multiplierWaste<-normalize(multiplierTable$multiplierWaste)
multiplierValue<-cbind(sector,multiplierIncome,multiplierOutput,multiplierLabour,multiplierEnergy,multiplierWaste)
colnames(multiplierValue)[1]<-"Sektor"
multiplierValue<-as.data.frame(multiplierValue)
tabel_radarchart <- multiplierValue[multiplierValue==selectedSector,]

nilai<-data.frame('multiplierIncome','multiplierOutput','multiplierLabour','multiplierEnergy','multiplierWaste')
colnames(nilai)<-c('multiplierIncome','multiplierOutput','multiplierLabour','multiplierEnergy','multiplierWaste')
nilai_temp<-t(nilai)
nilai_temp$data<-t(tabel_radarchart[,2:6])
nilai_temp$V1<-NULL
# nilai_temp$multiplier<-t(nilai)
View(nilai_temp$data)

plot_ly(
  type='scatterpolar',
  r = c(nilai_temp),
  theta = c('multiplierIncome','multiplierOutput','multiplierLabour','multiplierEnergy','multiplierWaste'),
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
