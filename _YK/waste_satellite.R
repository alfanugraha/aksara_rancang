prov = "JaBar"
io_folder <- paste0("~/aksara_rancang/_YK/raw/", prov, "/")
data_folder <- paste0("~/aksara_rancang/_YK/data/", prov, "/")

unit = "Million IDR"
area_name = prov
I_O_period = 2015
currentPopulation = 46030000

inSector <- paste0(io_folder, "/01_sektor.csv")
inIntermediateDemand <- paste0(io_folder, "/02_input_antara.csv")
inFinalDemandComp <- paste0(io_folder, "/03_komponen_permintaan_akhir.csv")
inFinalDemand<- paste0(io_folder, "/04_permintaan_akhir.csv")
inAddedValueComp<- paste0(io_folder, "/05_komponen_input_primer.csv")
inAddedValue<- paste0(io_folder, "/06_input_primer.csv")
inLabour<- paste0(io_folder, "/07_tenaga_kerja.csv")
inEnergy<- paste0(io_folder, "/08_satelit_energi.csv")
inWaste<- paste0(io_folder, "/09_satelit_limbah.csv")
inEmissionFactorEnergiTable<- paste0(io_folder, "/10_faktor_emisi_energi.csv")
inEmissionFactorWasteTable<- paste0(io_folder, "/11_faktor_emisi_limbah.csv")
inPopTable <- paste0(io_folder, "/12_population.csv")
inEmOtherTable <- paste0(io_folder, "/13_emission_from_other.csv")
inLandTable <- paste0(io_folder, "/14_satelit_lahan.csv")
inEmissionFactorLandTable <- paste0(io_folder, "/15_faktor_emisi_lahan.csv")
# inLandDemandTable <- paste0(io_folder, "/14_land_demand.csv")
# inLandDistTable <- paste0(io_folder, "/15_land_dist_matrix_prop.csv")

sector <- read.table(inSector, header=FALSE, sep=",")
indem <- read.table(inIntermediateDemand, header=FALSE, sep=",")
findem <- read.table(inFinalDemand, header=FALSE, sep=",")
addval <- read.table(inAddedValue, header=FALSE, sep=",")
labour <- read.table(inLabour, header=TRUE, sep=",")
energy <- read.table(inEnergy, header=TRUE, sep=",")
waste <- read.table(inWaste, header=TRUE, sep=",")
ef_energy <- read.table(inEmissionFactorEnergiTable, header=TRUE, sep=",")
ef_waste <- read.table(inEmissionFactorWasteTable, header=TRUE, sep=",")
findemcom <- read.table(inFinalDemandComp, header=FALSE, sep=",")
addvalcom <- read.table(inAddedValueComp, header=FALSE, sep=",")
population <- read.table(inPopTable, header=TRUE, sep=",")
otherEm <- read.table(inEmOtherTable, header=TRUE, sep=",")
landtable <- read.table(inLandTable, header=TRUE, sep=",")
ef_land <- read.table(inEmissionFactorLandTable, header=TRUE, sep=",")
# landDemand_prop <- read.table(inLandDistTable, header=TRUE, sep=",")


# working_dir = paste0("D:/PPRK/pprk_apps/data/", prov, "/")
# int_con_file = paste0(working_dir,"int_dem.csv")
# add_val_file = paste0(working_dir,"add_value.csv")
# fin_dem_file = paste0(working_dir,"fin_dem.csv")
# add_val_struc_file = paste0(working_dir,"add_value_struc.csv")
# fin_dem_struc_file = paste0(working_dir,"fin_dem_struc.csv")
# sector_file = paste0(working_dir,"sector.csv")
# labour_file = paste0(working_dir,"labor.csv")



library(reshape2)
library(ggplot2)
library(foreign)
library(rtf)

#SET WORKING DIRECTORY
# setwd(working_dir)
# 
# #READ INPUT FILE
# int_con <- read.table(int_con_file, header = FALSE, sep = ",")
# add_val <- read.table(add_val_file, header = FALSE, sep = ",")
# fin_dem <- read.table(fin_dem_file, header = FALSE, sep = ",")
# fin_dem_struc <- read.table(fin_dem_struc_file, header = FALSE, sep = ",")
# add_val_struc <- read.table(add_val_struc_file, header = FALSE, sep = ",")
# sector <- read.table(sector_file, header = FALSE, sep = ",")
# labour <- read.table(labour_file, header = FALSE, sep = ",")
# int_con.m <- as.matrix(int_con)
# add_val.m <- as.matrix(add_val)
# dim <- ncol(int_con.m)

#CALCULATE INVERS LEONTIEF
# int_con.ctot <- colSums(int_con.m)
# add_val.ctot <- colSums(add_val.m)
# fin_con <- 1 / (int_con.ctot + add_val.ctot)
# fin_con[is.infinite(fin_con)] <- 0
# t.input.invers <- diag(fin_con)
# A <- int_con.m %*% t.input.invers
# I <- as.matrix(diag(dim))
# I_A <- I - A
# Leontief <- solve(I_A)

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
leontief <- solve(I_A)

#DIRECT BACKWARD LINKAGES
DBL <- colSums(leontief)
DBL <- DBL/(mean(DBL))
DBL <- cbind(sector, DBL)
colnames(DBL)[3] <- "DBL"
order_DBL <- as.data.frame(DBL[order(-DBL$DBL), ])
order_DBL10 <- head(order_DBL, n = 20)
colnames(order_DBL10)[1] <- "SECTOR"
colnames(order_DBL10)[2] <- "CATEGORY"
BPD_graph <-
  ggplot(data = order_DBL10, aes(x = SECTOR, y = DBL, fill = CATEGORY)) +
  geom_bar(colour = "black", stat = "identity") + coord_flip() +
  guides(fill = FALSE) + xlab("Sectors") + ylab("Value")

#DIRECT FORWARD LINKAGES
DFL <- rowSums(leontief)
DFL <- DFL / (mean(DFL))
DFL <- cbind(sector, DFL)
colnames(DFL)[3] <- "DFL"
order_DFL <- as.data.frame(DFL[order(-DFL$DFL), ])
order_DFL10 <- head(order_DFL, n = 20)
colnames(order_DFL10)[1] <- "SECTOR"
colnames(order_DFL10)[2] <- "CATEGORY"
FPD_graph <-
  ggplot(data = order_DFL10, aes(x = SECTOR, y = DFL, fill = CATEGORY)) +
  geom_bar(colour = "black", stat = "identity") + coord_flip() +
  guides(fill = FALSE) + xlab("Sectors") + ylab("Value")


#CREATE LINKAGES TABLE
DBL_temp <- colSums(leontief)
BPD_temp <- DBL_temp / (mean(as.matrix(DBL_temp)))
DFL_temp <- rowSums(leontief)
FPD_temp <- DFL_temp / (mean(as.matrix(DFL_temp)))
DBL_temp <- as.data.frame(round(DBL_temp, digits = 2))
BPD_temp <- as.data.frame(round(BPD_temp, digits = 2))
DFL_temp <- as.data.frame(round(DFL_temp, digits = 2))
FPD_temp <- as.data.frame(round(FPD_temp, digits = 2))
Linkages_table <- cbind(sector, DBL_temp, DFL_temp, BPD_temp, FPD_temp)
colnames(Linkages_table)[1] <- "SECTOR"
colnames(Linkages_table)[2] <- "CATEGORY"
colnames(Linkages_table)[3] <- "DBL"
colnames(Linkages_table)[4] <- "DFL"
colnames(Linkages_table)[5] <- "BPD"
colnames(Linkages_table)[6] <- "FPD"
PRS_graph <- ggplot(Linkages_table, aes(x = BPD, y = FPD, color = CATEGORY)) +
  geom_point(shape = 19, size = 5) +
  geom_hline(aes(yintercept = 1), colour = "#BB0000", linetype = "dashed") + 
  geom_vline(aes(xintercept = 1), colour = "#BB0000", linetype ="dashed")

#SELECTION OF PRIMARY SECTOR
P.sector <- cbind(DBL, DFL)
colnames (P.sector) [1] <- "Sectors"
P.sector[4] <- NULL
P.sector[4] <- NULL
P.sector.selected <- P.sector[which(P.sector$DBL >= 1), ]
P.sector.selected <- P.sector.selected[which(P.sector.selected$DFL >= 1), ]
colnames(P.sector.selected)[1] <- "SECTOR"
colnames(P.sector.selected)[2] <- "CATEGORY"

#GDP
GDP.val <- colSums(addval_matrix[2:num_addval,])
GDP.val.m <- as.matrix(GDP.val)
GDP.val.m <- as.numeric(GDP.val.m)
OUTPUT.val <- as.data.frame(addval_matrix[2, ] + addval_matrix[3, ] + addval_matrix[1, ] + addval_matrix[4, ] + addval_matrix[5, ] + addval_matrix[6, ] + indem_colsum)
OUTPUT.val.m <- as.matrix(OUTPUT.val)
OUTPUT.val.m <- as.numeric(OUTPUT.val.m)
GDP <- cbind(sector, GDP.val, OUTPUT.val)
colnames(GDP)[1] <- "SECTOR"
colnames(GDP)[2] <- "CATEGORY"
colnames(GDP)[3] <- "GDP"
colnames(GDP)[4] <- "OUTPUT"
GDP$GDP_PROP <- GDP$GDP / GDP$OUTPUT
GDP[is.na(GDP)] <- 0
colnames(GDP)[5] <- "P_OUTPUT"
GDP_tot <- as.matrix(GDP$GDP)
GDP_tot <- colSums(GDP_tot)
GDP$P_GDP <- round((GDP$GDP / GDP_tot), digits = 2)
order_GDP <- as.data.frame(GDP[order(-GDP$GDP), ])
order_GDP10 <- head(order_GDP, n = 20)
GDP_graph <-
  ggplot(data = order_GDP10, aes(x = SECTOR, y = GDP, fill = SECTOR)) +
  geom_bar(colour = "black", stat = "identity") + coord_flip() +
  guides(fill = FALSE) + xlab("Sectors") + ylab("GDP")
GDP$GDP <- round(GDP$GDP, digits = 1)
GDP$OUTPUT <- round(GDP$OUTPUT, digits = 1)
GDP$P_OUTPUT <- round(GDP$P_OUTPUT, digits = 2)
GDP$P_GDP <- round(GDP$P_GDP, digits = 2)


#OUTPUT MULTIPLIER
Out.multiplier <- colSums(leontief)
Out.multiplier <- cbind(sector, Out.multiplier)
order_Out.multiplier <- as.data.frame(Out.multiplier[order(-Out.multiplier$Out.multiplier), ])
order_Out.multiplier <- head(order_Out.multiplier, n = 20)
OMPL_graph <- ggplot(data = order_Out.multiplier, aes(x = V1, y = Out.multiplier, fill = V2)) +
  geom_bar(colour = "black", stat = "identity") + coord_flip() +
  guides(fill = FALSE) + xlab("Sectors") + ylab("Output multiplier")

#INCOME MULTIPLIER
GDP.val.inc <- as.data.frame(addval_matrix[2, ])
V.income <- as.matrix(GDP.val.inc * fin_con)
Inc.multiplier <- leontief %*% V.income
multiplier <- cbind(Out.multiplier, Inc.multiplier)
Inc.multiplier <- cbind(sector, Inc.multiplier)
colnames(Inc.multiplier)[3] <- "Inc.multiplier"
order_Inc.multiplier <- as.data.frame(Inc.multiplier[order(-Inc.multiplier$Inc.multiplier), ])
order_Inc.multiplier <- head(order_Inc.multiplier, n = 20)
IMPL_graph <- ggplot(data = order_Inc.multiplier, aes(x = V1, y = Inc.multiplier, fill = V2)) +
  geom_bar(colour = "black", stat = "identity") + coord_flip() +
  guides(fill = FALSE) + xlab("Sectors") + ylab("Income multiplier")

#LABOUR MULTIPLIER
labour.m <- as.matrix(labour$Labour * fin_con)
labour.m <- labour.m / 1000000
Lab.multiplier <- leontief %*% labour.m
multiplier <- cbind(multiplier, Lab.multiplier)
colnames(multiplier)[1] <- "SECTOR"
colnames(multiplier)[2] <- "CATEGORY"
colnames(multiplier)[5] <- "Lab.multiplier"
multiplier$Out.multiplier <- round(multiplier$Out.multiplier, digits = 3)
Lab.multiplier <- cbind(sector, Lab.multiplier)
colnames(Lab.multiplier)[3] <- "Lab.multiplier"
order_Lab.multiplier <- as.data.frame(Lab.multiplier[order(-Lab.multiplier$Lab.multiplier), ])
order_Lab.multiplier <- head(order_Lab.multiplier, n = 20)
LMPL_graph <- ggplot(data = order_Lab.multiplier, aes(x = V1, y = Lab.multiplier, fill = V2)) +
  geom_bar(colour = "black", stat = "identity") + coord_flip() +
  guides(fill = FALSE) + xlab("Sectors") + ylab("Labour multiplier")
# colnames(multiplier)[4] <- "Inc.multiplier"
# multiplier$Inc.multiplier <- round(multiplier$Inc.multiplier, digits = 3)


#COMBINE MULTIPLIER
sel.multiplier <- multiplier[which(multiplier$Out.multiplier > 1), ]
sel.multiplier <- sel.multiplier[which(sel.multiplier$Inc.multiplier > 1), ]


#EXPORT OUTPUT
# Leontief_df <- as.data.frame(leontief)
# Leontief_matrix <- "Leontief_matrix.dbf"
# write.dbf(Leontief_df, Leontief_matrix, factor2char = TRUE, max_nchar = 254)
# Linkages <- "Sectoral_linkages.dbf"
# write.dbf(Linkages_table, Linkages, factor2char = TRUE, max_nchar = 254)
# PDRB <- "Sectoral_GDP"
# write.dbf(GDP, PDRB, factor2char = TRUE, max_nchar = 254)
# PENGGANDA <- "Sectoral_multiplier"
# write.dbf(multiplier, PENGGANDA, factor2char = TRUE, max_nchar = 254)

#WRITE REPORT
title <- "\\b\\fs32 LUMENS-Trade-off Analysis (TA) Project Report\\b0\\fs20"
sub_title <- "\\b\\fs28 Sub-modules 2: Regional economic-Descriptive analysis (Single I-O)\\b0\\fs20"
line <- paste("------------------------------------------------------------------------------------------------------------------------------------------------")
chapter1 <- "\\b\\fs24 1.Analysis of Sectoral Linkages \\b0\\fs20"
chapter2 <- "\\b\\fs24 2.Analysis of GDP \\b0\\fs20"
chapter2_1 <- "\\b\\i\\fs20 Total GDP \\b0\\i0\\fs20"
chapter3 <- "\\b\\fs24 2.Analysis of multiplier \\b0\\fs20"
filereport = paste0(data_folder, prov, "_analisa_deskriptif.doc")
rtffile <- RTF(filereport, font.size = 9)
addParagraph(rtffile, title)
addParagraph(rtffile, sub_title)
addNewLine(rtffile)
addParagraph(rtffile, line)
addParagraph(rtffile, line)
addNewLine(rtffile)
addParagraph(rtffile, chapter1)
addNewLine(rtffile)
addParagraph(rtffile, "\\b\\fs20 Table 1. Sectoral linkages\\b0\\fs20.")
addTable(rtffile, Linkages_table, font.size = 8)
addNewLine(rtffile)
addPlot(rtffile, plot.fun = print, width = 5, height = 3, res = 300, BPD_graph)
addParagraph(rtffile, "\\b\\fs20 Figure 1. Ten sectors with highest Backward power of dispersion\\b0\\fs20.")
addNewLine(rtffile)
addPlot(rtffile, plot.fun = print, width = 5, height = 3, res = 300, FPD_graph)
addParagraph(rtffile, "\\b\\fs20 Figure 2. Ten sectors with highest Forward power of dispersion\\b0\\fs20.")
addNewLine(rtffile)
addPlot(rtffile, plot.fun = print, width = 6, height = 4, res = 300, PRS_graph)
addParagraph(rtffile, "\\b\\fs20 Figure 3. Sectoral typology based on linkages analysis\\b0\\fs20.")
addNewLine(rtffile)
addParagraph(rtffile, "\\b\\fs20 Table 2. Primary sectors based on potential linkage\\b0\\fs20.")
addTable(rtffile, P.sector.selected, font.size = 8)
addNewLine(rtffile)
addPageBreak(rtffile)
addParagraph(rtffile, chapter2)
addNewLine(rtffile)
addParagraph(rtffile, chapter2_1)
addParagraph(rtffile, GDP_tot)
addNewLine(rtffile)
addParagraph(rtffile, "\\b\\fs20 Table 3. Sectoral GDP\\b0\\fs20.")
addTable(rtffile, GDP, font.size = 8)
addNewLine(rtffile)
addPlot(rtffile, plot.fun = print, width = 5, height = 3, res = 300, GDP_graph)
addParagraph(rtffile, "\\b\\fs20 Figure 4. Twenty sectors with highest GDP\\b0\\fs20.")
addPageBreak(rtffile)
addParagraph(rtffile, chapter3)
addNewLine(rtffile)
addParagraph(rtffile, "\\b\\fs20 Table 4. Sectoral multiplier\\b0\\fs20.")
addTable(rtffile, multiplier, font.size = 8)
addNewLine(rtffile)
addPlot(rtffile, plot.fun = print, width = 5, height = 3, res = 300, OMPL_graph)
addParagraph(rtffile, "\\b\\fs20 Figure 5. Twenty sectors with highest Output multiplier\\b0\\fs20.")
addNewLine(rtffile)
addPlot(rtffile, plot.fun = print, width = 5, height = 3, res = 300, IMPL_graph)
addParagraph(rtffile, "\\b\\fs20 Figure 6. Twenty sectors with highest Income multiplier\\b0\\fs20.")
addNewLine(rtffile)
addPlot(rtffile, plot.fun = print, width = 5, height = 3, res = 300, LMPL_graph)
addParagraph(rtffile, "\\b\\fs20 Figure 5. Twenty sectors with highest Labour multiplier\\b0\\fs20.")
done(rtffile)

setwd(data_folder)
saveRDS(sector , "sector")
saveRDS(indem , "indem")
saveRDS(findem , "findem")
saveRDS(addval , "addval")
saveRDS(labour , "labour")
saveRDS(energy , "energy")
saveRDS(waste , "waste")
saveRDS(ef_energy , "ef_energy")
saveRDS(ef_waste , "ef_waste")
saveRDS(findemcom , "findemcom")
saveRDS(addvalcom , "addvalcom")
saveRDS(landtable , "landtable")
saveRDS(population, "population")
saveRDS(otherEm , "otherEm")
saveRDS(ef_land , "ef_land")
# saveRDS(landDemand , "landDemand")
# saveRDS(landDemand_prop, "landDemand_prop")
saveRDS(I_A, "I_A")
saveRDS(leontief, "leontief")
saveRDS(GDP, "GDPAll")
saveRDS(Linkages_table, "linkagesTable")
saveRDS(multiplier, "multiplierAll")
saveRDS(I_O_period, "periodIO")
saveRDS(rtffile, "rtffile")

setwd("d:/PPRK/aksara_rancang/")
# 
# save(sector , file="sector")
# save(indem , file="indem")
# save(findem , file="findem")
# save(addval , file="addval")
# save(labour , file="labour")
# save(energy , file="energy")
# save(waste , file="waste")
# save(ef_energy , file="ef_energy")
# save(ef_waste , file="ef_waste")
# save(findemcom , file="findemcom")
# save(addvalcom , file="addvalcom")
# save(population, file="population")
# save(otherEm , file="otherEm")
# save(landDemand , file="landDemand")
# save(landDemand_prop, file="landDemand_prop")
# save(I_A, file="I_A")
# save(leontief, file="leontief")
# save(GDP, file="GDPAll")
# save(Linkages_table, file="linkagesTable")
# save(multiplier, file="multiplierAll")
# save(I_O_period, file="periodIO")
# save(rtffile, file="rtffile")
# 
# load(datapath, "sector")
# load(datapath, "indem")
# load(datapath, "findem")
# load(datapath, "addval")
# load(datapath, "labour")
# load(datapath, "energy")
# load(datapath, "waste")
# load(datapath, "ef_energy")
# load(datapath, "ef_waste")
# load(datapath, "findemcom")
# load(datapath, "addvalcom")
# load(datapath, "population")
# load(datapath, "otherEm")
# load(datapath, "landDemand")
# load(datapath, "landDemand_prop")
# load(datapath, "I_A")
# load(datapath, "leontief")
# load(datapath, "GDPAll")
# load(datapath, "linkagesTable")
# load(datapath, "multiplierAll")
# load(datapath, "periodIO")
# load(datapath, "rtffile")
# 
# sector <- readRDS(paste0(datapath, "sector"))
# indem <- readRDS(paste0(datapath, "indem"))
# findem <- readRDS(paste0(datapath, "findem"))
# addval <- readRDS(paste0(datapath, "addval"))
# labour <- readRDS(paste0(datapath, "labour"))
# energy <- readRDS(paste0(datapath, "energy"))
# waste <- readRDS(paste0(datapath, "waste"))
# ef_energy <- readRDS(paste0(datapath, "ef_energy"))
# ef_waste <- readRDS(paste0(datapath, "ef_waste"))
# findemcom <- readRDS(paste0(datapath, "findemcom"))
# addvalcom <- readRDS(paste0(datapath, "addvalcom"))
# population <- readRDS(paste0(datapath, "population"))
# otherEm <- readRDS(paste0(datapath, "otherEm"))
# landDemand <- readRDS(paste0(datapath, "landDemand"))
# landDemand_prop <- readRDS(paste0(datapath, "landDemand_prop"))
# I_A <- readRDS(paste0(datapath, "I_A"))
# leontief <- readRDS(paste0(datapath, "leontief"))
# GDPAll <- readRDS(paste0(datapath, "GDPAll"))
# linkagesTable <- readRDS(paste0(datapath, "linkagesTable"))
# multiplierAll <- readRDS(paste0(datapath, "multiplierAll"))
# periodIO <- readRDS(paste0(datapath, "periodIO"))
# rtffile <- readRDS(paste0(datapath, "rtffile"))

# readRDS("sector")
# readRDS("indem")
# readRDS("findem")
# readRDS("addval")
# readRDS("labour")
# readRDS("energy")
# readRDS("waste")
# readRDS("ef_energy")
# readRDS("ef_waste")
# readRDS("findemcom")
# readRDS("addvalcom")
# readRDS("population")
# readRDS("otherEm")
# readRDS("landDemand")
# readRDS("landDemand_prop")
# 
# ## saving necessary file to RDS
# saveRDS (add_val, "add_val")
# saveRDS (fin_dem, "fin_dem")
# saveRDS (fin_dem_struc, "fin_dem_struc")
# saveRDS (add_val, "add_val")
# saveRDS (add_val_struc, "add_val_struc")
# saveRDS (sector, "sector")
# saveRDS (int_con, "int_con")
# saveRDS (labour, "labour")
# saveRDS (I_A, "I_A")
# saveRDS (GDP, "GDP")
# saveRDS (Leontief, "Leontief")
# saveRDS (Linkages_table, "Linkages_table")
# saveRDS (multiplier, "multiplier")
# saveRDS (I_O_period, "I_O_period")
# saveRDS (rtffile, "rtffile")
# 
# ## load necessary file to RDS
# readRDS ("add_val")
# readRDS ("fin_dem")
# readRDS ("fin_dem_struc")
# readRDS ("add_val")
# readRDS ("add_val_struc")
# readRDS ("sector")
# readRDS ("int_con")
# readRDS ("labour")
# readRDS ("I_A")
# readRDS ("GDP")
# readRDS ("Leontief")
# readRDS ("Linkages_table")
# readRDS ("multiplier")
# readRDS ("I_O_period")
# readRDS ("rtffile")
# readRDS ("prov_list")



# inLandDemandTable <- paste0(io_folder, "/land_demand2017.csv")
# inLandDistTable <- paste0(io_folder, "/land_dist_matrix_prop.csv")
# landDemand <- read.table(inLandDemandTable, header=TRUE, sep=",")
# landDemand_prop <- read.table(inLandDistTable, header=TRUE, sep=",")

