library(ggplot2)

version<-"version1"
selectedProv<-"SumSel"
whatAnalysis<-"kajiUlangPlusDelta"

# read trade off result

tradeOffResultTable<-read.csv(paste0("_DB/simulasiPilotProvince/result/",selectedProv,"/", whatAnalysis, "/", version,"/tradeOffResult.csv"))
tradeOffResultTable<-data.frame(tradeOffResultTable[,2:ncol(tradeOffResultTable)], stringsAsFactors =  FALSE)
singleScenName<-unique(tradeOffResultTable$ScenarioName[!grepl("combination",tradeOffResultTable$ScenarioName) & !grepl("BAU",tradeOffResultTable$ScenarioName)])
bestScenarioCombination<-as.character(read.csv(paste0("_DB/simulasiPilotProvince/result/",selectedProv,"/", whatAnalysis, "/", version,"/bestScenarioCombination.csv"))[1,2])
scenarioCombination<-readRDS(paste0("_DB/simulasiPilotProvince/result/",selectedProv,"/", whatAnalysis, "/", version,"/scenarioCombination"))



functionTable<-function(whatToPlot=NULL){
  table<-rbind(tradeOffResultTable[tradeOffResultTable$ScenarioName== "BAU", ],
               tradeOffResultTable[tradeOffResultTable$ScenarioName== whatToPlot, ])
  # cumulative emission
  ggplot(table, aes(x=Year, y=CummulativeEmission, group=ScenarioName))+
    geom_line(aes(color=ScenarioName))+
    geom_point(aes(color=ScenarioName))+
    labs(x="Year", y="Emission (tonne Co2-eq)")+
    ggtitle("Grafik Proyeksi Emisi Kumulatif")+
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(paste0("_DB/simulasiPilotProvince/result/",selectedProv,"/", whatAnalysis, "/", version,"/E-",whatToPlot,".png"), width=9, height=4)
  

  # GDP
  ggplot(table, aes(x=Year, y=ResultTotalGDP, group=ScenarioName))+
    geom_line(aes(color=ScenarioName))+
    geom_point(aes(color=ScenarioName))+
    labs(x="Year", y="GDP (million rupiah)")+
    ggtitle("Grafik Proyeksi PDRB")+
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(paste0("_DB/simulasiPilotProvince/result/",selectedProv,"/", whatAnalysis, "/", version,"/GDP-",whatToPlot,".png"), width=9, height=4)
  
  # emission intensity
  ggplot(table, aes(x=Year, y=EmissionIntensity, group=ScenarioName))+
    geom_line(aes(color=ScenarioName))+
    geom_point(aes(color=ScenarioName))+
    labs(x="Year", y="Emission Intensity (tonne CO2-eq/million rupiah)")+
    ggtitle("Grafik Proyeksi Intensitas Emisi")+
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(paste0("_DB/simulasiPilotProvince/result/",selectedProv,"/", whatAnalysis, "/", version,"/EI-",whatToPlot,".png"), width=9, height=4)
}


# single scenario plot
for (aksi in singleScenName){
  functionTable(whatToPlot = aksi)
}

# best scenario combination
functionTable(whatToPlot = bestScenarioCombination)

# scenario all combination
functionTable(whatToPlot = as.character(tradeOffResultTable$ScenarioName[nrow(tradeOffResultTable)]))