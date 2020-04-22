runSimulation1<-eventReactive(input$run_button, {
  selectedRow <- as.numeric(strsplit(input$run_button,"_")[[1]][2])
  fileName<- as.character(ListTableReact()[selectedRow,5]) #nama file dari ListTableReact ada di col=5
  selectedFile<-readRDS(fileName)
  
  initialYear<-selectedFile$tahunAwal
  finalYear <- selectedFile$tahunAkhir
  iteration <- finalYear - initialYear
  ioPeriod <- ioPeriod
  
  if (type == "energy") {
    energyScen= selectedFile
    scenarioFD=energyScen[["fdSelisih"]]
    scenarioFD=scenarioFD[,2:ncol(scenarioFD)]
    scenarioInputLandCover= NULL
    additionalSatelliteEnergy=energyScen[["satSelisih"]]
    additionalSatelliteWaste=NULL
    additionalSatelliteAgriculture=NULL
    additionalEmissionFactorEnergy=NULL
    additionalEmissionFactorWaste=NULL
    additionalEmissionFactorAgriculture=NULL
  }
  
  # Series of GPD & Output projection
  scenarioSeriesOfGDP <- data.frame(Sektor = ioSector[,1], stringsAsFactors = FALSE)
  scenarioSeriesOfFinalDemand <- rowSumsMatrixIoFinalDemand
  scenarioSeriesOfOutput <- ioTotalOutput
  
  # Series of Intervention Point
  scenarioSeriesOfIntermediateDemand <- list()
  scenarioSeriesOfAddedValue <- list()
  scenarioSeriesOfFinalDemandComponent <- list()
  scenarioSeriesOfImpactLabour <- list()
  scenarioSeriesOfImpactEnergy <- list()
  scenarioSeriesOfImpactWaste <- list()
  scenarioSeriesOfImpactAgriculture <- list()
  scenarioSeriesOfImpactLand1<-list()
  scenarioSeriesOfImpactLand2<-list()
  scenarioSeriesOfImpactLand3<-list()
  
  
  # Historical consumption and emission data
  eval(parse(text=paste0("scenarioSeriesOfGDP$y",ioPeriod,"<- analysisGDP")))
  eval(parse(text=paste0("scenarioSeriesOfIntermediateDemand$y",ioPeriod," <- matrixIoIntermediateDemand")))
  eval(parse(text=paste0("scenarioSeriesOfAddedValue$y",ioPeriod," <- matrixIoAddedValue")))
  eval(parse(text=paste0("scenarioSeriesOfFinalDemandComponent$y",ioPeriod," <- matrixIoFinalDemand")))
  eval(parse(text=paste0("scenarioSeriesOfImpactLabour$y",ioPeriod," <- functionSatelliteImpact('labour', satellite = satelliteLabour, matrix_output = as.matrix(ioTotalOutput))")))
  eval(parse(text=paste0("scenarioSeriesOfImpactEnergy$y",ioPeriod," <- functionSatelliteImpact('energy', satellite = satelliteEnergy, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorEnergy)")))
  eval(parse(text=paste0("scenarioSeriesOfImpactWaste$y",ioPeriod," <- functionSatelliteImpact('waste', satellite = satelliteWaste, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorWaste)")))
  eval(parse(text=paste0("scenarioSeriesOfImpactAgriculture$y",ioPeriod," <- functionSatelliteImpact('agriculture', satellite = satelliteAgriculture, matrix_output = as.matrix(ioTotalOutput), emission_factor = emissionFactorAgriculture)")))
  
  
  # historical LRC, land requirement, & land cover
  eval(parse(text=paste0("scenarioSeriesOfImpactLand1$y",ioPeriod,"<-functionSatelliteLand1(type= 'historis', matrix_output= as.matrix(ioTotalOutput))")))
  # LSEI
  eval(parse(text=paste0("scenarioSeriesOfImpactLand2$y",ioPeriod,"<- functionSatelliteLand2(type='historis',carbonStock=carbonStock_his, GDP= as.matrix(bauSeriesOfGDP$y",ioPeriod,"))")))
  
  #projection data
  projectionYear <- initialYear
  listYear <- paste0("y", ioPeriod)
  
  # economic & impact (energy, waste, & agriculture projection
  for(step in 1:(iteration+1)){
    
    # notes on the year
    timeStep <- paste0("y", projectionYear)
    
    projectionFinalDemand <- bauSeriesOfFinalDemand[,timeStep] + scenarioFD[,timeStep]  # input final demand ditambahkan di sini
    
    scenarioSeriesOfFinalDemand <- cbind(scenarioSeriesOfFinalDemand, projectionFinalDemand)
    projectionOutput <- ioLeontiefInverse %*% projectionFinalDemand
    scenarioSeriesOfOutput <- cbind(scenarioSeriesOfOutput, projectionOutput)
    
    # add additional values to the list
    eval(parse(text=paste0("scenarioSeriesOfFinalDemandComponent$", timeStep, " <- as.matrix(proportionFinalDemand*projectionFinalDemand)"))) # contains NaN
    eval(parse(text=paste0("scenarioSeriesOfIntermediateDemand$", timeStep, " <-  analysisCT %*% diag(as.vector(projectionOutput), ncol = ioDimention, nrow= ioDimention)")))
    eval(parse(text=paste0("scenarioSeriesOfAddedValue$", timeStep, " <-  analysisCPI %*% diag(as.vector(projectionOutput), ncol = ioDimention, nrow= ioDimention)")))
    
    # GDP projection
    eval(parse(text = paste0("scenarioSeriesOfGDP$", timeStep, "<- colSums(scenarioSeriesOfAddedValue$", timeStep, "[setdiff(1:nrow(matrixIoAddedValue), rowImport),])")))
    
    # Impact projection
    eval(parse(text= paste0("scenarioSeriesOfImpactLabour$", timeStep, " <- functionSatelliteImpact('labour', satellite = satelliteLabour, matrix_output = as.matrix(projectionOutput))")))
    eval(parse(text= paste0("scenarioSeriesOfImpactEnergy$", timeStep, " <- functionSatelliteImpact('energy',
                                                                                                  satellite = satelliteEnergy,
                                                                                                  matrix_output = as.matrix(projectionOutput),
                                                                                                  emission_factor = emissionFactorEnergy,
                                                                                                  additional_satellite= additionalSatelliteEnergy[['",timeStep,"']],
                                                                                                  additional_emission_factor=additionalEmissionFactorEnergy[['",timeStep,"']])")))
    eval(parse(text= paste0("scenarioSeriesOfImpactWaste$", timeStep, " <- functionSatelliteImpact('waste',
                                                                                                 satellite = satelliteWaste,
                                                                                                 matrix_output = as.matrix(projectionOutput),
                                                                                                 emission_factor = emissionFactorWaste,
                                                                                                 additional_satellite=additionalSatelliteWaste[['",timeStep,"']],
                                                                                                 additional_emission_factor=additionalEmissionFactorWaste[['",timeStep,"']])")))
    eval(parse(text= paste0("scenarioSeriesOfImpactAgriculture$", timeStep, " <- functionSatelliteImpact('agriculture',
                                                                                                        satellite = satelliteAgriculture,
                                                                                                        matrix_output = as.matrix(projectionOutput),
                                                                                                        emission_factor = emissionFactorAgriculture,
                                                                                                        additional_satellite=additionalSatelliteAgriculture[['",timeStep,"']],
                                                                                                        additional_emission_factor=additionalEmissionFactorAgriculture[['",timeStep,"']])")))
    
    listYear <- c(listYear, timeStep)
    projectionYear <- initialYear+step
    
  }
  
  colnames(scenarioSeriesOfOutput) <- as.character(listYear)
  
  scenarioSeriesOfFinalDemandTable <- cbind(data.frame(ioSector$V1), scenarioSeriesOfFinalDemand)
  colnames(scenarioSeriesOfFinalDemandTable) <- c("Sektor", as.character(listYear))
  
  # land cover projection
  
  # non-advance mode
  for (i in 1:2){
    projectionYear <- initialYear
    listYear <- paste0("y", ioPeriod)
    for(step in 1:(iteration+1)){
      # notes on the year
      timeStep <- paste0("y", projectionYear)
      # projection
      eval(parse(text= paste0("scenarioSeriesOfImpactLand1$", timeStep, " <- functionSatelliteLand1(type= 'projection',
                                                                                                      matrix_output= as.matrix(scenarioSeriesOfOutput[,'",timeStep,"']),
                                                                                                      advanceMode = FALSE,
                                                                                                      currYear= projectionYear,
                                                                                                      runNum = ",i,",
                                                                                                      LRCRate= NULL)")))
      listYear <- c(listYear, timeStep)
      projectionYear <- initialYear+step
    }
    # jika tidak ada value landCover yang negatif, break loop
    if(any(unlist(sapply(scenarioSeriesOfImpactLand1,'[[', "landCover"))<0)==FALSE){
      if(i==1){
        textDataLRCRate="historis"
      } else {
        textDataLRCRate="historis yang dimodifikasi"
      }
      print(paste0("laju perubahan LRC yang digunakan untuk membangun proyeksi tutupan lahan adalah data laju LRC ", textDataLRCRate)) # use as UI textoutput
      break
    } else {
      if(i==2){
        print("proyeksi luas tutupan lahan yang dihasilkan bernilai negatif. Silakan masukkan data laju perubahan LRC secara manual")
      }
    }
  }
  
  list(
    selectedFile=selectedFile,
    initialYear=initialYear,
    finalYear=finalYear,
    iteration=iteration,
    ioPeriod=ioPeriod,
    scenarioSeriesOfIntermediateDemand =scenarioSeriesOfIntermediateDemand,
    scenarioSeriesOfAddedValue = scenarioSeriesOfAddedValue,
    scenarioSeriesOfFinalDemandComponent = scenarioSeriesOfFinalDemandComponent,
    scenarioSeriesOfImpactLabour = scenarioSeriesOfImpactLabour,
    scenarioSeriesOfImpactEnergy = scenarioSeriesOfImpactEnergy,
    scenarioSeriesOfImpactWaste = scenarioSeriesOfImpactWaste,
    scenarioSeriesOfImpactAgriculture = scenarioSeriesOfImpactAgriculture,
    scenarioSeriesOfImpactLand1= scenarioSeriesOfImpactLand1
  )
})

observe({
  runSimulation<-runSimulation1()
  selectedFile=runSimulation$selectedFile
  initialYear=runSimulation$initialYear
  finalYear=runSimulation$finalYear
  iteration=runSimulation$iteration
  ioPeriod=runSimulation$ioPeriod
  scenarioSeriesOfIntermediateDemand =runSimulation$scenarioSeriesOfIntermediateDemand
  scenarioSeriesOfAddedValue = runSimulation$scenarioSeriesOfAddedValue
  scenarioSeriesOfFinalDemandComponent = runSimulation$scenarioSeriesOfFinalDemandComponent
  scenarioSeriesOfImpactLabour = runSimulation$scenarioSeriesOfImpactLabour
  scenarioSeriesOfImpactEnergy = runSimulation$scenarioSeriesOfImpactEnergy
  scenarioSeriesOfImpactWaste = runSimulation$scenarioSeriesOfImpactWaste
  scenarioSeriesOfImpactAgriculture = runSimulation$scenarioSeriesOfImpactAgriculture
  scenarioSeriesOfImpactLand1= runSimulation$scenarioSeriesOfImpactLand1
  
  if(any(unlist(sapply(scenarioSeriesOfImpactLand1,'[[', "landCover"))<0)==TRUE){
    showModal(modalDialog("tes"))
} else{
  print("fine")
  }
  
})