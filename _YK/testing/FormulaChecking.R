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

# Row explicit definition for Income (Wages & Salary)
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

# Backward Linkage
DBL <- colSums(leontief)
DBL <- DBL/(mean(DBL))
# Forward Linkage
DFL <- rowSums(leontief)
DFL <- DFL/(mean(DFL))
# GDP
GDP <- colSums(addval_matrix[2:num_addval,])
# Multiplier Output
multiplierOutput <- colSums(leontief)
# Multiplier Income
income_coef <- tinput_invers %*% as.matrix(addval_matrix[income_row,])
income_matrix <- diag(as.vector(income_coef), ncol = dimensi, nrow = dimensi)
InvIncome_matrix <- diag(as.vector(1/income_coef), ncol = dimensi, nrow = dimensi)
multiplierIncome <- income_matrix %*% leontief %*% InvIncome_matrix
multiplierIncome <- as.matrix(colSums(multiplierIncome), dimensi, 1)
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
income_per_capita <- sum(as.matrix(addval_matrix[income_row,])) / input$popDensTable

result <- cbind(sector,
                DBL,
                DFL, 
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
return(list_table)
}
