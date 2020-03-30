inSector <- "input/input_tablesJambi/1_sector.csv"
inIntermediateDemand <- "input/input_tablesJambi/2_intermediate_demand.csv"
inFinalDemandComp <- "input/input_tablesJambi/3_final_demand_component.csv"
inFinalDemand <- "input/input_tablesJambi/4_final_demand.csv"
inAddedValueComp <- "input/input_tablesJambi/5_value_added_component.csv"
inAddedValue <- "input/input_tablesJambi/6_value_added.csv"     
inLabour <- "input/input_tablesJambi/7_satellite_labour.csv"
inEnergy <- "input/input_tablesJambi/8_satellite_energy.csv"
inWaste <- "input/input_tablesJambi/9_satellite_waste.csv"
inEmissionFactorEnergiTable <- "input/input_tablesJambi/10_emission_factor_energy.csv"
inEmissionFactorLandWasteTable <- "input/input_tablesJambi/11_emission_factor_waste.csv"

sector <- read.table(inSector, header=FALSE, sep=",")
indem <- read.table(inIntermediateDemand, header=FALSE, sep=",")
findem <- read.table(inFinalDemand, header=FALSE, sep=",")
addval <- read.table(inAddedValue, header=FALSE, sep=",")
labour <- read.table(inLabour, header=TRUE, sep=",")
energy <- read.table(inEnergy, header=TRUE, sep=",")
waste <- read.table(inWaste, header=TRUE, sep=",")
ef_energy <- read.table(inEmissionFactorEnergiTable, header=TRUE, sep=",")
ef_waste <- read.table(inEmissionFactorLandWasteTable, header=TRUE, sep=",")
findemcom <- read.table(inFinalDemandComp, header=FALSE, sep=",")
addvalcom <- read.table(inAddedValueComp, header=FALSE, sep=",")

#Convert to matrix
income_row <- 2
indem_matrix <- as.matrix(indem)
addval_matrix <- as.matrix(addval)
num_addval <- nrow(addval_matrix)
dimensi <- ncol(indem_matrix)

indem_colsum <- colSums(indem_matrix)
addval_colsum <- colSums(addval_matrix)
fin_con <- 1/(indem_colsum+addval_colsum)
fin_con[is.infinite(fin_con)] <- 0
tinput_invers <- diag(fin_con)
A <- indem_matrix %*% tinput_invers
I <- as.matrix(diag(dimensi))
I_A <- I-A
library(matlib)
leontief <- inv(I_A)

# Backward Linkage
BL <- colSums(leontief)
BL <- BL/(mean(BL))
# Forward Linkage
FL <- rowSums(leontief)
FL <- FL/(mean(FL))
# GDP (bentuk kondisi dimana tidak ada "Impor" pada input primer)
if(addvalcom$V1[1]=="Impor"){
  GDP <- colSums(addval_matrix[2:num_addval,])
} else {
  GDP <- colSums(addval_matrix[1:num_addval,])
}
# Multiplier Output
multiplierOutput <- colSums(leontief)
# Multiplier Income (bentuk kondisi dimana tidak ada "Impor" pada input primer)
if(addvalcom$V1[1]=="Impor"){
  income_coef <- tinput_invers %*% as.matrix(addval_matrix[income_row,])
} else {
  income_coef <- tinput_invers %*% as.matrix(addval_matrix[(income_row-1),])
}

multiplierIncome <- leontief %*% income_coef
multiplierIncome[is.na(multiplierIncome)] <- 0

# Labour
labour_coef <- tinput_invers %*% as.matrix(labour[,3])
labour_matrix <- diag(as.vector(labour_coef), ncol = dimensi, nrow = dimensi)
InvLabour_matrix <- diag(as.vector(1/labour_coef), ncol = dimensi, nrow = dimensi)
multiplierLabour <- labour_matrix %*% leontief %*% InvLabour_matrix
multiplierLabour <- as.matrix(colSums(multiplierLabour), dimensi, 1)
multiplierLabour[is.na(multiplierLabour)] <- 0
# Multiplier Energy Used
energy_coef <- tinput_invers %*% as.matrix(energy[,3])
energy_matrix <- diag(as.vector(energy_coef), ncol = dimensi, nrow = dimensi)
InvEnergy_matrix <- diag(as.vector(1/energy_coef), ncol = dimensi, nrow = dimensi)
multiplierEnergy <- energy_matrix %*% leontief %*% InvEnergy_matrix
multiplierEnergy <- as.matrix(colSums(multiplierEnergy), dimensi, 1)
multiplierEnergy[is.na(multiplierEnergy)] <- 0
# Multiplier Waste Product
waste_coef <- tinput_invers %*% as.matrix(waste[,3])
waste_matrix <- diag(as.vector(energy_coef), ncol = dimensi, nrow = dimensi)
InvWaste_matrix <- diag(as.vector(1/waste_coef), ncol = dimensi, nrow = dimensi)
multiplierWaste <- waste_matrix %*% leontief %*% InvWaste_matrix
multiplierWaste <- as.matrix(colSums(multiplierWaste), dimensi, 1)
multiplierWaste[is.na(multiplierWaste)] <- 0
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
categorySector<-"Ekonomi"
pprkResults<-"PDRB"
graph <- data.frame(Sektor="", Analisis="")

if(categorySector=="Ekonomi"){
  if(pprkResults == "PDRB"){
    graph <- subset(analysisResult, select = c(Sektor, GDP))
    GDPvalues <- as.matrix(analysisResult$GDP)
    GDPTotal <- colSums(GDPvalues)
  } else if(pprkResults == "Backward Linkage"){
    graph <- subset(analysisResult, select = c(Sektor, DBL))
  } else if(pprkResults == "Forward Linkage"){
    graph <- subset(analysisResult, select = c(Sektor, DFL))
  } else if(pprkResults == "Angka Pengganda Output"){
    graph <- subset(analysisResult, select = c(Sektor, multiplierOutput))
  } else if(pprkResults == "Angka Pengganda Pendapatan Rumah Tangga"){
    graph <- subset(analysisResult, select = c(Sektor, multiplierIncome))
  } else if(pprkResults == "Angka Pengganda Tenaga Kerja"){
    graph <- subset(analysisResult, select = c(Sektor, multiplierLabour))
  } else if(pprkResults == "Upah gaji"){
    graph <- subset(analysisResult, select = c(Sektor, wages))
  } else if(pprkResults == "Rasio Upah gaji per Surplus Usaha"){
    graph <- subset(analysisResult, select = c(Sektor, ratio_ws))
  } else if(pprkResults == "Pendapatan per kapita")
  
  if(pprkResults == "Perbandingan Angka Pengganda"){
    multiplierTable <- subset(analysisResult, select = c(Sektor, multiplierIncome, multiplierOutput, multiplierLabour, multiplierEnergy, multiplierWaste))
    tabel_radarchart <- multiplierTable[multiplierTable==selectedSector,]
    tabel_radar <- tabel_radarchart
    tabel_radar$Sektor <- NULL
    tabel_radarmax <- data.frame(multiplierIncome=max(multiplierTable$multiplierIncome),
                                 multiplierOutput=max(multiplierTable$multiplierOutput),
                                 multiplierLabour=max(multiplierTable$multiplierLabour),
                                 multiplierEnergy=max(multiplierTable$multiplierEnergy),
                                 multiplierWaste=max(multiplierTable$multiplierWaste)
    )
    tabel_radarmin <- data.frame(multiplierIncome=min(multiplierTable$multiplierIncome),
                                 multiplierOutput=min(multiplierTable$multiplierOutput),
                                 multiplierLabour=min(multiplierTable$multiplierLabour),
                                 multiplierEnergy=min(multiplierTable$multiplierEnergy),
                                 multiplierWaste=min(multiplierTable$multiplierWaste)
    )
    tabel_radar <- rbind(tabel_radarmax, tabel_radarmin, tabel_radar)
    radarchart(tabel_radar)
  } else {
    colnames(graph) <- c("Sektor", "Analisis")
    ggplot(data=graph, aes(x=Sektor, y=Analisis, fill=Sektor)) +
      geom_bar(stat="identity", colour="black") + theme_minimal() +
      coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")
  }
}

##Bar chart##
library(plotly)
library(ggplot2)
library(dplyr)
colnames(graph) <- c("Sektor", "Analisis")
ggplot(data=graph, aes(x=Sektor, y=Analisis, fill=Sektor)) +
  geom_bar(colour="black", stat="identity") + theme_minimal() +
  coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai")

# graph2<-graph[with(graph,order(graph$Analisis)),]
# graph2<-as.data.frame(graph2)
c<-ggplot(data=graph, aes(x=reorder(Sektor,Analisis), y=Analisis,fill=Sektor))+
  geom_bar(stat="identity") + theme_minimal() + 
  coord_flip() + guides(fill=FALSE) + xlab("Sektor") + ylab("Nilai") 

c+ scale_fill_grey(start = 0.8,end=0.2)
c+scale_fill_gradient(low="yellow",high="red")

library(viridis)
c+scale_color_viridis(discrete = TRUE, option="D") + scale_fill_viridis(discrete = TRUE)
ggplotly(c)

plot_ly(graph, x=~Analisis, y=~Sektor, fill=~Sektor) %>%
  add_bars(orientation = 'h',name=~Sektor) %>%
  layout(barmode = 'stack',
         xaxis = list(title = "Analisis"),
         yaxis = list(title ="Sektor"))

##Scaterplot##
gplot3<-ggplot(data=GDP_all, aes(x=year, y=PDRB, group=1)) + geom_line() + geom_point()
ggplotly(gplot3)

year<-c(1:10)
PDRB<-c(200,100,800,400,500,340,190,400,700,250)

plot_ly(graph,x=~year, y=~PDRB) %>%
add_trace(
  type = "scatter",
  mode = "markers+lines")() %>%
  layout(xaxis = list(title = "Year"),
         yaxis = list(title ="PDRB"))


###SPIDER/RADAR CHART

selectMultiSector<-"input/input_tablesJambi/1_sector.csv"
selectMultiSector <- read.table(selectMultiSector, header=FALSE, sep=",")
selectedSector <- "Padi"
multiplierTable <- subset(analysisResult, select = c(Sektor, multiplierIncome, multiplierOutput, multiplierLabour, multiplierEnergy, multiplierWaste))
tabel_radarchart <- multiplierTable[multiplierTable==selectedSector,]
tabel_radar <- tabel_radarchart
tabel_radar$Sektor <- NULL
tabel_radarmax <- data.frame(multiplierIncome=max(multiplierTable$multiplierIncome), 
                               multiplierOutput=max(multiplierTable$multiplierOutput), 
                               multiplierLabour=max(multiplierTable$multiplierLabour), 
                               multiplierEnergy=max(multiplierTable$multiplierEnergy),
                               multiplierWaste=max(multiplierTable$multiplierWaste) 
)
tabel_radarmin <- data.frame(multiplierIncome=min(multiplierTable$multiplierIncome),  
                               multiplierOutput=min(multiplierTable$multiplierOutput),  
                               multiplierLabour=min(multiplierTable$multiplierLabour),  
                               multiplierEnergy=min(multiplierTable$multiplierEnergy),
                               multiplierWaste=min(multiplierTable$multiplierWaste) 
)
tabel_radar <- rbind(tabel_radarmax, tabel_radarmin, tabel_radar)
radarchart(tabel_radar)

library(ggradar)
ggradar(tabel_radar) #error di nilai

library(ggiraphExtra)
ggRadar(tabel_radar)

#Nilainya itu apa?
plot_ly(
  type='scatterpolar',
  r = c(nilai_temp$data),
  theta = c('multiplierIncome','multiplierOutput','multiplierLabour','multiplierEnergy','multiplierWaste')
) %>%
  layout(
    polar=list(
      radialaxis=list(
        visible=T,
        range=
      )
    ),
    showlegend=F
  )

nilai<-data.frame('multiplierIncome','multiplierOutput','multiplierLabour','multiplierEnergy','multiplierWaste')
colnames(nilai)<-c('multiplierIncome','multiplierOutput','multiplierLabour','multiplierEnergy','multiplierWaste')
tabel_radar2 <- rbind(nilai,tabel_radarmax, tabel_radarmin, tabel_radar)
ggplot(data=tabel_radar,  aes(x=colnames(tabel_radar), y=tabel_radar)) + 
  geom_point(size=5) + 
  geom_line() + 
  # xlab("Decils") + 
  # ylab("% difference in nº Pk") + 
  # ylim(-50,25) + ggtitle("CL")  + 
  geom_hline(aes(yintercept=0), lwd=1, lty=2) + 
  #scale_x_discrete(limits=c(orden_deciles)) +
  coord_polar()


multiplierIncome<-normalize(multiplierTable$multiplierIncome)
multiplierOutput<-normalize(multiplierTable$multiplierOutput)
multiplierLabour<-normalize(multiplierTable$multiplierLabour)
multiplierEnergy<-normalize(multiplierTable$multiplierEnergy)
multiplierWaste<-normalize(multiplierTable$multiplierWaste)
multiplierValue<-cbind(sector,multiplierIncome,multiplierOutput,multiplierLabour,multiplierEnergy,multiplierWaste)
colnames(multiplierValue)[1]<-"Sektor"
multiplierValue<-as.data.frame(multiplierValue)
tabel_radarchart <- multiplierTable[multiplierTable==selectedSector,]

nilai<-data.frame('multiplierIncome','multiplierOutput','multiplierLabour','multiplierEnergy','multiplierWaste')
colnames(nilai)<-c('multiplierIncome','multiplierOutput','multiplierLabour','multiplierEnergy','multiplierWaste')
nilai_temp<-t(nilai)
nilai_temp$data<-t(tabel_radarchart[,2:6])
nilai_temp$V1<-NULL
nilai_temp$multiplier<-t(nilai)
View(nilai_temp$data)

plot_ly(
  type='scatterpolar',
  r = c(nilai_temp$data),
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
