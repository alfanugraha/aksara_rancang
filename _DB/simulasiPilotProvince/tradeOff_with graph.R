library(gtools)
library(plyr)
library(stringr)
library(scales)
library(xlsx)

# source("_DB/debug_TIN.R")
source("_DB/simulasiPilotProvince/debug_TIN.R")
whatAnalysis <- "kajiUlangPlusDelta" # "redcluwePlusDelta" or "kajiUlangPlusDelta" 

# read all intervention scenario  
scenarioPath<-data.frame(path =list.files(paste0("_DB/skenarioData/", selectedProv), all.files=TRUE, full.names = TRUE, recursive = TRUE))
# scenarioPath<-data.frame(path=scenarioPath[-c(3,7,8,12),]) #delete after use 
scenarioPath<-data.frame(scenarioPath[!grepl(pattern="desktop.ini", scenarioPath$path),], stringsAsFactors = FALSE)
colnames(scenarioPath)<-"path"
  
for (i in 1:nrow(scenarioPath)){
  if(grepl("/energi/", scenarioPath$path[i])==TRUE){
    scenarioPath$type[i] <- "energi"
  } else if (grepl("/lahan/", scenarioPath$path[i])==TRUE){
    scenarioPath$type[i] <- "lahan"
  } else if (grepl("/pertanian/", scenarioPath$path[i])==TRUE){
    scenarioPath$type[i] <- "pertanian"
  } else if (grepl("/limbah/", scenarioPath$path[i])==TRUE){
    scenarioPath$type[i] <- "limbah"
  }
}


scenarioPath$ID<-paste0("scen",1:nrow(scenarioPath), ".", scenarioPath$type)
for (i in 1:ncol(scenarioPath)){
  # scenarioFile[i,]<-readRDS(scenarioPath[i,])
  eval(parse(text = paste0(scenarioPath$ID,"<-readRDS('",scenarioPath$path,"')")))
}


#cek apakah ada sat satelit yang null
cekSatSelisih<-matrix(NA, nrow=nrow(scenarioPath), ncol=1)
rownames(cekSatSelisih)<-scenarioPath$ID
for (i in scenarioPath$ID){
  eval(parse(text = paste0("cekSatSelisih['",i,"',1]<-is.null(",i,"$satSelisih)"))) 
}



# calculate delta for each intervention
for (a in scenarioPath$ID){
  # eval(parse(text = paste0(a,"$scenarioResultGDP$GDP<-",a,"$scenarioResultGDP$GDP-resultGDP$GDP")))
  # eval(parse(text = paste0(a,"$scenarioResultIncome$income<-",a,"$scenarioResultIncome$income-resultIncome$income")))
  # eval(parse(text = paste0(a,"$scenarioResultLabour$labour<-",a,"$scenarioResultLabour$labour-resultLabour$labour")))
  # eval(parse(text = paste0(a,"$scenarioResultEnergyConsumption[,4:ncol(",a,"$scenarioResultEnergyConsumption)]<-",a,"$scenarioResultEnergyConsumption[,4:ncol(",a,"$scenarioResultEnergyConsumption)]-resultEnergyConsumption[,4:ncol(resultEnergyConsumption)]")))
  # eval(parse(text = paste0(a,"$scenarioResultEnergyEmission[,4:ncol(",a,"$scenarioResultEnergyEmission)]<-",a,"$scenarioResultEnergyEmission[,4:ncol(",a,"$scenarioResultEnergyEmission)]-resultEnergyEmission[,4:ncol(resultEnergyEmission)]")))
  # eval(parse(text = paste0(a,"$scenarioResultWasteDisposal[,4:ncol(",a,"$scenarioResultWasteDisposal)]<-",a,"$scenarioResultWasteDisposal[,4:ncol(",a,"$scenarioResultWasteDisposal)]-resultWasteDisposal[,4:ncol(resultWasteDisposal)]")))
  # eval(parse(text = paste0(a,"$scenarioResultWasteEmission[,4:ncol(",a,"$scenarioResultWasteEmission)]<-",a,"$scenarioResultWasteEmission[,4:ncol(",a,"$scenarioResultWasteEmission)]-resultWasteEmission[,4:ncol(resultWasteEmission)]")))
  # eval(parse(text = paste0(a,"$scenarioResultFertilizerUsed[,4:ncol(",a,"$scenarioResultFertilizerUsed)]<-",a,"$scenarioResultFertilizerUsed[,4:ncol(",a,"$scenarioResultFertilizerUsed)]-resultFertilizerUsed[,4:ncol(resultFertilizerUsed)]")))
  # eval(parse(text = paste0(a,"$scenarioResultFertilizerEmission[,4:ncol(",a,"$scenarioResultFertilizerEmission)]<-",a,"$scenarioResultFertilizerEmission[,4:ncol(",a,"$scenarioResultFertilizerEmission)]-resultFertilizerEmission[,4:ncol(resultFertilizerEmission)]")))
  # eval(parse(text = paste0(a,"$scenarioResultLandReq$land.requirement<-",a,"$scenarioResultLandReq$land.requirement-resultLandReq$land.requirement")))
  # eval(parse(text = paste0(a,"$scenarioResultLandCover$luas.land.use<-",a,"$scenarioResultLandCover$luas.land.use-resultLandCover$luas.land.use")))
  eval(parse(text = paste0(a,"$scenarioAllResult[,2:7]<-",a,"$scenarioAllResult[,2:7]-bauAllResult[,2:7]")))
}




# create all combinations
scenarioCombination<-list()

#single-scenario combination
i<-1
scenarioCombination[[paste0(i)]]<-data.frame(combinations(nrow(scenarioPath),i,scenarioPath$ID), stringsAsFactors = FALSE)

#energy scenario combination
scenarioPathEnergy<-scenarioPath[scenarioPath$type=="energi",]
scenarioCombination$energy<-data.frame(combinations(nrow(scenarioPathEnergy), nrow(scenarioPathEnergy), scenarioPathEnergy$ID), stringsAsFactors = FALSE)

#waste scenario combination
scenarioPathWaste<-scenarioPath[scenarioPath$type=="limbah",]
scenarioCombination$waste<-data.frame(combinations(nrow(scenarioPathWaste), nrow(scenarioPathWaste), scenarioPathWaste$ID), stringsAsFactors = FALSE)

#land scenario combination
scenarioPathLand<-scenarioPath[scenarioPath$type=="lahan",]
scenarioCombination$land<-data.frame(combinations(nrow(scenarioPathLand), nrow(scenarioPathLand), scenarioPathLand$ID), stringsAsFactors = FALSE)

#agriculture scenario combination
scenarioPathAgriculture<-scenarioPath[scenarioPath$type=="pertanian",]
scenarioCombination$agriculture<-data.frame(combinations(nrow(scenarioPathAgriculture), nrow(scenarioPathAgriculture), scenarioPathAgriculture$ID), stringsAsFactors = FALSE)

#all-2 scenarios combination
for (i in (nrow(scenarioPath)-2) :nrow(scenarioPath)){
# for (i in 1:nrow(scenarioPath)){
  scenarioCombination[[paste0(i)]]<-data.frame(combinations(nrow(scenarioPath),i,scenarioPath$ID), stringsAsFactors = FALSE)
}


# # set rule for combination : each combination has to have >= 4 scens, with minimum 1 scen from each prk sectors
# for (i in 1:length(scenarioCombination)){
#   for (a in 1:nrow(scenarioCombination[[i]])){
#     if (any (c(sum(str_detect(scenarioCombination[[i]][a,], "lahan")), 
#                sum(str_detect(scenarioCombination[[i]][a,], "energi")),
#                sum(str_detect(scenarioCombination[[i]][a,], "limbah")),
#                sum(str_detect(scenarioCombination[[i]][a,], "pertanian"))) == 0)){
#                  scenarioCombination[[i]][a,]<-NA
#                }
#   }
#   scenarioCombination[[i]]<-na.omit(scenarioCombination[[i]])
# }


# create combination ID
for (i in 1:length(scenarioCombination)){
  ID<-matrix(NA, nrow=nrow(scenarioCombination[[i]]), ncol=1)
  for (a in 1:nrow(scenarioCombination[[i]])){
    ID[a,]<-paste0("combination",i,".",a)
  }
  rownames(scenarioCombination[[i]])<-ID
}

# create tradeOffResult table based on BAU table
tradeOffResult<-list()
for (i in 1:length(scenarioCombination)){
  for (a in 1:nrow(scenarioCombination[[i]])){
    combinationName<-rownames(scenarioCombination[[i]])[a]
    # tradeOffResult[[i]][[combinationName]]<-list()
    print(combinationName)
    # tradeOffResult[[combinationName]][['scenarioResultGDP']] <- resultGDP
    # tradeOffResult[[combinationName]][['scenarioResultIncome']] <- resultIncome
    # tradeOffResult[[combinationName]][['scenarioResultLabour']] <- resultLabour
    # tradeOffResult[[combinationName]][['scenarioResultEnergyConsumption']] <- resultEnergyConsumption
    # tradeOffResult[[combinationName]][['scenarioResultEnergyEmission']] <- resultEnergyEmission
    # tradeOffResult[[combinationName]][['scenarioResultWasteDisposal']] <- resultWasteDisposal
    # tradeOffResult[[combinationName]][['scenarioResultWasteEmission']] <- resultWasteEmission
    # tradeOffResult[[combinationName]][['scenarioResultFertilizerUsed']] <- resultFertilizerUsed
    # tradeOffResult[[combinationName]][['scenarioResultFertilizerEmission']] <- resultFertilizerEmission
    # tradeOffResult[[combinationName]][['scenarioResultLandReq']] <- resultLandReq
    # tradeOffResult[[combinationName]][['scenarioResultLandCover']] <- resultLandCover
    # tradeOffResult[[combinationName]][['scenarioResultLandEmission']] <- resultLandEmission
    tradeOffResult[[combinationName]][['scenarioAllResult']] <- bauAllResult
  }
}
# tradeOffResult = sum of all delta intervention + BAU 
for (i in 1:length(scenarioCombination)){
  for (combinationName in rownames(scenarioCombination[[i]])){
    print(combinationName)
    for (x in 1:ncol(scenarioCombination[[i]])){
    # for (x in 1:1){
      scenName<-scenarioCombination[[i]][paste0(combinationName),x]
      print(scenName)
      # ncolWasteDisposal<-ncol(tradeOffResult[[paste0(combinationName)]][['scenarioResultWasteDisposal']])
      # ncolWasteEmission<-ncol(tradeOffResult[[paste0(combinationName)]][['scenarioResultWasteEmission']])
      # 
      # eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioResultGDP']][['GDP']] <- tradeOffResult[['",combinationName,"']][['scenarioResultGDP']][['GDP']]  + ",scenName,"[['scenarioResultGDP']][['GDP']]")))
      # eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioResultIncome']][['income']] <- tradeOffResult[['",combinationName,"']][['scenarioResultIncome']][['income']]  + ",scenName,"[['scenarioResultIncome']][['income']]")))
      # eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioResultLabour']][['labour']] <- tradeOffResult[['",combinationName,"']][['scenarioResultLabour']][['labour']]  + ",scenName,"[['scenarioResultLabour']][['labour']]")))
      # eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioResultEnergyConsumption']][,4:30] <- tradeOffResult[['",combinationName,"']][['scenarioResultEnergyConsumption']][,4:30] + ",scenName,"[['scenarioResultEnergyConsumption']][,4:30]")))
      # eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioResultEnergyEmission']][,4:30] <- tradeOffResult[['",combinationName,"']][['scenarioResultEnergyEmission']][,4:30] + ",scenName,"[['scenarioResultEnergyEmission']][,4:30]")))
      # eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioResultWasteDisposal']][,4:",ncolWasteDisposal,"] <- tradeOffResult[['",combinationName,"']][['scenarioResultWasteDisposal']][,4:",ncolWasteDisposal,"] + ",scenName,"[['scenarioResultWasteDisposal']][,4:",ncolWasteDisposal,"]")))
      # eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioResultWasteEmission']][,4:",ncolWasteEmission,"] <- tradeOffResult[['",combinationName,"']][['scenarioResultWasteEmission']][,4:",ncolWasteEmission,"] + ",scenName,"[['scenarioResultWasteEmission']][,4:",ncolWasteEmission,"]")))
      # eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioResultFertilizerUsed']][,4:9] <- tradeOffResult[['",combinationName,"']][['scenarioResultFertilizerUsed']][,4:9] + ",scenName,"[['scenarioResultFertilizerUsed']][,4:9]")))
      # eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioResultFertilizerEmission']][,4:9] <- tradeOffResult[['",combinationName,"']][['scenarioResultFertilizerEmission']][,4:9] + ",scenName,"[['scenarioResultFertilizerEmission']][,4:9]")))
      # eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioResultLandReq']][['land.requirement']] <- tradeOffResult[['",combinationName,"']][['scenarioResultLandReq']][['land.requirement']]  + ",scenName,"[['scenarioResultLandReq']][['land.requirement']]")))
      # eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioResultLandCover']][['luas.land.use']] <- tradeOffResult[['",combinationName,"']][['scenarioResultLandCover']][['luas.land.use']]  + ",scenName,"[['scenarioResultLandCover']][['luas.land.use']]")))
      # eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioResultLandEmission']][['emission']] <- tradeOffResult[['",combinationName,"']][['scenarioResultLandEmission']][['emission']]  + ",scenName,"[['scenarioResultLandEmission']][['emission']]")))
      eval(parse(text = paste0("tradeOffResult[['",combinationName,"']][['scenarioAllResult']][,2:7] <- tradeOffResult[['",combinationName,"']][['scenarioAllResult']][,2:7] + ",scenName,"[['scenarioAllResult']][,2:7]")))
    }
    tradeOffResult[[paste0(combinationName)]][["scenarioAllResult"]]<-cbind(tradeOffResult[[paste0(combinationName)]][["scenarioAllResult"]], ID=paste0(combinationName))
    }
}


## calculate reduction or growth only 
tradeOffResult.delta<-list() 
tradeOffResult.kajiUlang<-list() 
for (combinationName in names(tradeOffResult)){
  # delta emission, GDP, & emission intensity scenario
  tradeOffResult.delta[[combinationName]][['scenarioAllResult']] <- tradeOffResult[[combinationName]][['scenarioAllResult']]
  tradeOffResult.delta[[combinationName]][['scenarioAllResult']][,2:7] <- tradeOffResult[[combinationName]][['scenarioAllResult']][,2:7] - bauAllResult[,2:7]
  # delta scenario + bau kaji ulang radGRK
  tradeOffResult.kajiUlang[[combinationName]][['scenarioAllResult']] <- tradeOffResult.delta[[combinationName]][['scenarioAllResult']]
  tradeOffResult.kajiUlang[[combinationName]][['scenarioAllResult']][,2:7] <- bauAllResult.visualization[,2:7] + tradeOffResult.delta[[combinationName]][['scenarioAllResult']][,2:7]
}

#recalculate EmissionIntensity & cumulative emission for all tradeOffResult
for (combinationName in names(tradeOffResult.delta)){
  tradeOffResult.delta[[combinationName]][['scenarioAllResult']][,"CummulativeEmission"] <- cumsum(tradeOffResult.delta[[combinationName]][['scenarioAllResult']][,"TotalEmission"])
  tradeOffResult.kajiUlang[[combinationName]][['scenarioAllResult']][,"CummulativeEmission"] <- cumsum(tradeOffResult.kajiUlang[[combinationName]][['scenarioAllResult']][,"TotalEmission"])
  tradeOffResult[[combinationName]][['scenarioAllResult']][,"CummulativeEmission"] <- cumsum(tradeOffResult[[combinationName]][['scenarioAllResult']][,"TotalEmission"])
  
  tradeOffResult.delta[[combinationName]][['scenarioAllResult']][,"EmissionIntensity"] <- tradeOffResult.delta[[combinationName]][['scenarioAllResult']][,"TotalEmission"] / tradeOffResult.delta[[combinationName]][['scenarioAllResult']][,"ResultTotalGDP"] 
  tradeOffResult.kajiUlang[[combinationName]][['scenarioAllResult']][,"EmissionIntensity"] <- tradeOffResult.kajiUlang[[combinationName]][['scenarioAllResult']][,"TotalEmission"] / tradeOffResult.kajiUlang[[combinationName]][['scenarioAllResult']][,"ResultTotalGDP"] 
  tradeOffResult[[combinationName]][['scenarioAllResult']][,"EmissionIntensity"] <- tradeOffResult[[combinationName]][['scenarioAllResult']][,"TotalEmission"] / tradeOffResult[[combinationName]][['scenarioAllResult']][,"ResultTotalGDP"] 
}

#########################################


# create trade off summary 
tradeOffSummary<-data.frame(ID=NA, peningkatan.PDRB=NA,penurunan.emisi=NA, penurunan.intensitasEmisi=NA, stringsAsFactors = FALSE)
for (i in 1:length(tradeOffResult)){
  if (whatAnalysis == "redcluwePlusDelta"){
    scenarioAllResult<- tradeOffResult[[i]][["scenarioAllResult"]] # jika skenario yang digunakan adalah hasil perhitungan delta skenario + BAU EEIO
  } else if (whatAnalysis == "kajiUlangPlusDelta"){
    scenarioAllResult<- tradeOffResult.kajiUlang[[i]][["scenarioAllResult"]] # jika skenario yang digunakan adalah hasil perhitungan delta skenario + BAU kaji ulang rad GRK
  }
  scenarioAllResult.finalYear<-scenarioAllResult[scenarioAllResult$Year==finalYear,]
  bauAllResult.visualization.finalYear<-bauAllResult.visualization[bauAllResult.visualization$Year==finalYear,]
  
  ### if peningkatan / penurunan adalah rata-rata tahunan peningkatan/penurunan dari BAU 
    # peningkatan PDRB <- mean ( GDP intervensi-GDP bau / GDP bau )
    peningkatan.PDRB <- mean((scenarioAllResult$ResultTotalGDP - bauAllResult.visualization$ResultTotalGDP) / bauAllResult.visualization$ResultTotalGDP) *100
    # penurunan emisi <- mean ( emisi intervensi-emisi bau / emisi bau )
    # penurunan.emisi <- -mean((scenarioAllResult$TotalEmission - bauAllResult.visualization$TotalEmission) / bauAllResult.visualization$TotalEmission)*100
    # penurunan intensitas emisi <- - mean ( intensitas emisi intervensi-intensitas emisi bau / intensitas emisi bau )
    penurunan.intensitasEmisi <- -mean((scenarioAllResult$EmissionIntensity - bauAllResult.visualization$EmissionIntensity) / bauAllResult.visualization$EmissionIntensity)*100
    
  
  # if peningkatan / penurunan adalah rata-rata tahunan peningkatan/penurunan dari BAU 
    # penurunan emisi <- (emisi kumulatif intervensi tahun akhir - emisi kumulatif bau tahun akhir)/emisi kumulatif bau tahun akhir
    penurunan.emisi <- as.numeric(-(scenarioAllResult.finalYear$CummulativeEmission - bauAllResult.visualization.finalYear$CummulativeEmission) / bauAllResult.visualization.finalYear$CummulativeEmission*100)
    
  
  tradeOffSummary.addRow<-data.frame(ID=names(tradeOffResult)[[i]],
                                     peningkatan.PDRB=peningkatan.PDRB,
                                     penurunan.emisi=penurunan.emisi,
                                     # penurunan.emisi2=penurunan.emisi2,
                                     penurunan.intensitasEmisi=penurunan.intensitasEmisi, 
                                     stringsAsFactors = FALSE)
  tradeOffSummary<-rbind(tradeOffSummary,tradeOffSummary.addRow)
  
  # tradeOffResult[[i]][["scenarioAllResult"]]$type <- "SCENARIO"
  # tradeOffResult[[i]][["scenarioAllResult"]]<-rbind(tradeOffResult[[i]][["scenarioAllResult"]],bauAllResult)
}
tradeOffSummary<-tradeOffSummary[-is.na(tradeOffSummary),]

########## give name
for(i in 1:nrow(tradeOffSummary)){
  if(grepl("combination1.", tradeOffSummary$ID[i])){
    # skenarioID<-scenarioCombination[[1]][as.character(tradeOffResultCombined$ID[i]),1]
    eval(parse(text = paste0(
      "tradeOffSummary$ScenarioName[i]<-as.character(",scenarioCombination[[1]][as.character(tradeOffSummary$ID[i]),1],"[['namaSken']])"
    )))
    tradeOffSummary$ScenarioID[i]<-scenarioCombination[[1]][as.character(tradeOffSummary$ID[i]),1]
  } else if(grepl("combination2.", tradeOffSummary$ID[i])){
    eval(parse(text = paste0(
      "tradeOffSummary$ScenarioName[i]<-'energy sector scenario'"
    )))
  } else if(grepl("combination3.", tradeOffSummary$ID[i])){
    eval(parse(text = paste0(
      "tradeOffSummary$ScenarioName[i]<-'waste sector scenario'"
    )))
  } else if(grepl("combination4.", tradeOffSummary$ID[i])){
    eval(parse(text = paste0(
      "tradeOffSummary$ScenarioName[i]<-'land sector scenario'"
    )))
  } else if(grepl("combination5.", tradeOffSummary$ID[i])){
    eval(parse(text = paste0(
      "tradeOffSummary$ScenarioName[i]<-'agriculture sector scenario'"
    )))
  } else{
    tradeOffSummary$ScenarioName[i]<- as.character(tradeOffSummary$ID[i])
    tradeOffSummary$ScenarioID[i]<- as.character(tradeOffSummary$ID[i])
    
  }
}


# Categorize each combination based on its performance

# kuadran 1 : emisi naik, pdrb naik
tradeOffSummaryQ1<-filter(tradeOffSummary,tradeOffSummary$penurunan.emisi<=0 & tradeOffSummary$peningkatan.PDRB>0 )
# kuadran 2 : emisi naik, pdrb turun
tradeOffSummaryQ2<-filter(tradeOffSummary,tradeOffSummary$penurunan.emisi<=0 & tradeOffSummary$peningkatan.PDRB<=0 )
# kuadran 3 : emisi turun, pdrb naik** 
tradeOffSummaryQ3<-filter(tradeOffSummary,tradeOffSummary$penurunan.emisi>0 & tradeOffSummary$peningkatan.PDRB>0 )
# kuadran 4 : emisi turun, pdrb turun
tradeOffSummaryQ4<-filter(tradeOffSummary,tradeOffSummary$penurunan.emisi>0 & tradeOffSummary$peningkatan.PDRB<=0 )

# best intervention scenario (from Q3)
bestInterventionScenario<-tradeOffSummaryQ3$ScenarioName[tradeOffSummaryQ3$penurunan.intensitasEmisi == max(tradeOffSummaryQ3$penurunan.intensitasEmisi)]


if (whatAnalysis == "redcluwePlusDelta"){
  tradeOffResultCombined <- cbind(bauAllResult.visualization, ID = "BAU", Category = "BAU")
  for ( i in 1:length (tradeOffResult)){
    if (any(str_detect(tradeOffSummaryQ1$ID, names(tradeOffResult)[[i]]))){
      quadrant <- "Q1"
    } else if (any(str_detect(tradeOffSummaryQ2$ID, names(tradeOffResult)[[i]]))){
      quadrant <- "Q2"
    } else if (any(str_detect(tradeOffSummaryQ3$ID, names(tradeOffResult)[[i]]))){
      quadrant <- "Q3"
    } else if (any(str_detect(tradeOffSummaryQ4$ID, names(tradeOffResult)[[i]]))){
      quadrant <- "Q4"
    }
    tradeOffResultCombined<-rbind(tradeOffResultCombined, cbind(tradeOffResult[[i]][["scenarioAllResult"]], 
                                                                Category = quadrant))
  }
} else if (whatAnalysis == "kajiUlangPlusDelta"){
  tradeOffResultCombined <- cbind(bauAllResult.visualization, ID = "BAU", Category = "BAU")
  for ( i in 1:length (tradeOffResult.kajiUlang)){
    if (any(str_detect(tradeOffSummaryQ1$ID, names(tradeOffResult.kajiUlang)[[i]]))){
      quadrant <- "Q1"
    } else if (any(str_detect(tradeOffSummaryQ2$ID, names(tradeOffResult.kajiUlang)[[i]]))){
      quadrant <- "Q2"
    } else if (any(str_detect(tradeOffSummaryQ3$ID, names(tradeOffResult.kajiUlang)[[i]]))){
      quadrant <- "Q3"
    } else if (any(str_detect(tradeOffSummaryQ4$ID, names(tradeOffResult.kajiUlang)[[i]]))){
      quadrant <- "Q4"
    }
    tradeOffResultCombined<-rbind(tradeOffResultCombined, cbind(tradeOffResult.kajiUlang[[i]][["scenarioAllResult"]], 
                                                                Category = quadrant))
  }
}


###################################### RENAME SCENARIO

for(i in 1:nrow(tradeOffResultCombined)){
  if(grepl("combination1.", tradeOffResultCombined$ID[i])){
    # skenarioID<-scenarioCombination[[1]][as.character(tradeOffResultCombined$ID[i]),1]
    eval(parse(text = paste0(
      "tradeOffResultCombined$ScenarioName[i]<-as.character(",scenarioCombination[[1]][as.character(tradeOffResultCombined$ID[i]),1],"[['namaSken']])"
    )))
  } else if(grepl("combination2.", tradeOffResultCombined$ID[i])){
    eval(parse(text = paste0(
      "tradeOffResultCombined$ScenarioName[i]<-'energy sector scenario'"
    )))
  } else if(grepl("combination3.", tradeOffResultCombined$ID[i])){
    eval(parse(text = paste0(
      "tradeOffResultCombined$ScenarioName[i]<-'waste sector scenario'"
    )))
  } else if(grepl("combination4.", tradeOffResultCombined$ID[i])){
    eval(parse(text = paste0(
      "tradeOffResultCombined$ScenarioName[i]<-'land sector scenario'"
    )))
  } else if(grepl("combination5.", tradeOffResultCombined$ID[i])){
    eval(parse(text = paste0(
      "tradeOffResultCombined$ScenarioName[i]<-'agriculture sector scenario'"
    )))
  } else{
    tradeOffResultCombined$ScenarioName[i]<- as.character(tradeOffResultCombined$ID[i])
  }
}




#----------------------------------- BEGIN: SAVE ----------------------------#

## save reduction / growth to excel as single sheet
tradeOffResult.delta.combined<-list()
for (combinationName in names(tradeOffResult.delta)){
  if (identical(combinationName,names(tradeOffResult.delta)[1])){
    tradeOffResult.delta.combined<-tradeOffResult.delta[[combinationName]][['scenarioAllResult']]
  } else {
    tradeOffResult.delta.combined<-rbind(tradeOffResult.delta.combined, tradeOffResult.delta[[combinationName]][['scenarioAllResult']])
  }
}
# write.xlsx(tradeOffResult.delta.combined, paste0(selectedProv, "_tradeOffResult.delta.combined.xlsx"))
write.csv(tradeOffResult.delta.combined, paste0("_DB/simulasiPilotProvince/result/",selectedProv,"/", whatAnalysis, "/", version,"/delta.csv"))

# ## save reduction / growth to excel as multiple sheets in a file
# file<-paste("SulSel.xlsx", sep="")
# for (combinationName in names(tradeOffResult.delta)){
#   if (identical(combinationName,names(tradeOffResult.delta)[1])){
#     write.xlsx(tradeOffResult.delta[[combinationName]][['scenarioAllResult']], file, sheetName= combinationName)
#   } else {
#     write.xlsx(tradeOffResult.delta[[combinationName]][['scenarioAllResult']], file, sheetName= combinationName, append=TRUE)
#   }
# }

## save tradeOffResult to excel as single sheet
# write.xlsx(tradeOffResultCombined, paste0(selectedProv, "_",whatAnalysis, ".xlsx" ))
write.csv(tradeOffResultCombined, paste0("_DB/simulasiPilotProvince/result/",selectedProv,"/", whatAnalysis, "/", version,"/tradeOffResult.csv"))


## save tradeOffSummary to excel as single sheet
# write.xlsx(tradeOffSummary, paste0(selectedProv, "_",whatAnalysis, "_summary.xlsx" ))
write.csv(tradeOffSummary, paste0("_DB/simulasiPilotProvince/result/",selectedProv,"/", whatAnalysis, "/",version,"/summaryStats.csv"))

# best intervention scenario
write.csv(bestInterventionScenario, paste0("_DB/simulasiPilotProvince/result/",selectedProv,"/", whatAnalysis, "/",version,"/bestScenarioCombination.csv"))

#scenarioCombination
saveRDS(scenarioCombination, paste0("_DB/simulasiPilotProvince/result/",selectedProv,"/", whatAnalysis, "/",version,"/scenarioCombination"))
write.csv(scenarioPath, paste0("_DB/simulasiPilotProvince/result/",selectedProv,"/", whatAnalysis, "/",version,"/scenarioPath.csv"))


# ---------------------------------- END: SAVE ------------------------#


ui<-fluidPage(
fluidRow(
    selectInput('selectTradeOffCombination', 
                label = "pilih kombinasi skenario yang akan ditampilkan",
                choices= c("seluruh kombinasi skenario", 
                           "kombinasi skenario terbaik", 
                           "kombinasi skenario emisi naik & PDRB naik",
                           "kombinasi skenario emisi naik & PDRB turun", 
                           "kombinasi skenario emisi turun & PDRB turun", 
                           "kombinasi skenario emisi turun & PDRB naik"
                )), 
    plotlyOutput('tradeOffPlotEmission'), 
    plotlyOutput('tradeOffPlotGDP'),
    plotlyOutput('tradeOffPlotEmissionIntensity'),
  )
)

server <- function(input,output,session){
  
tradeOffPlot <-reactive({
  print("runtradeoffplot")
  
  # what to plot
  if(input$selectTradeOffCombination=="seluruh kombinasi skenario"){
    table<-tradeOffResultCombined
  } else if (input$selectTradeOffCombination== "kombinasi skenario terbaik"){
    tradeOffSummaryQ3<-filter(tradeOffSummary,tradeOffSummary$penurunan.emisi>0 & tradeOffSummary$peningkatan.PDRB>0 )
    bestScenID<-tradeOffSummaryQ3$ID[tradeOffSummaryQ3$penurunan.intensitasEmisi == max(tradeOffSummaryQ3$penurunan.intensitasEmisi)]
    table<-filter(tradeOffResultCombined, Category=="Q3" & ID==bestScenID)
    table<-rbind(table, filter(tradeOffResultCombined, ID=="BAU"))
  } else if (input$selectTradeOffCombination== "kombinasi skenario emisi naik & PDRB naik"){
    table<-filter(tradeOffResultCombined, Category=="Q1")
    table<-rbind(table, filter(tradeOffResultCombined, ID=="BAU"))
  } else if (input$selectTradeOffCombination== "kombinasi skenario emisi naik & PDRB turun"){
    table<-filter(tradeOffResultCombined, Category=="Q2")
    table<-rbind(table, filter(tradeOffResultCombined, ID=="BAU"))
  } else if (input$selectTradeOffCombination== "kombinasi skenario emisi turun & PDRB turun"){
    table<-filter(tradeOffResultCombined, Category=="Q4")
    table<-rbind(table, filter(tradeOffResultCombined, ID=="BAU"))
  } else if (input$selectTradeOffCombination== "kombinasi skenario emisi turun & PDRB naik"){
    table<-filter(tradeOffResultCombined, Category=="Q3")
    table<-rbind(table, filter(tradeOffResultCombined, ID=="BAU"))
  }
  
  table
  print(table)
})

output$tradeOffPlotEmission<-renderPlotly({
  ggplot(tradeOffPlot(), aes(x=Year, y=CummulativeEmission, group=ScenarioName))+
    geom_line(aes(color=ScenarioName))+
    geom_point(aes(color=ScenarioName))+
    labs(x="Tahun", y="Emisi (ton Co2-eq)")+
    ggtitle("Grafik Proyeksi Emisi Kumulatif")+
    theme(plot.title = element_text(hjust = 0.5))
})

output$tradeOffPlotGDP<-renderPlotly({
  ggplot(tradeOffPlot(), aes(x=Year, y=ResultTotalGDP, group=ScenarioName))+
    geom_line(aes(color=ScenarioName))+
    geom_point(aes(color=ScenarioName))+
    labs(x="Tahun", y="PDRB (juta rupiah)")+
    ggtitle("Grafik Proyeksi PDRB")+
    theme(plot.title = element_text(hjust = 0.5))
})

output$tradeOffPlotEmissionIntensity<-renderPlotly({
  ggplot(tradeOffPlot(), aes(x=Year, y=EmissionIntensity, group=ScenarioName))+
    geom_line(aes(color=ScenarioName))+
    geom_point(aes(color=ScenarioName))+
    labs(x="Tahun", y="Intensitas Emisi (ton CO2-eq/juta rupiah")+
    ggtitle("Grafik Proyeksi Intensitas Emisi")+
    theme(plot.title = element_text(hjust = 0.5))
})

}

app <- shinyApp(ui,server)
runApp(app)
