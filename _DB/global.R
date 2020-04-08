### input
### nama 52 sector
sector<-readRDS("_DB/data/JaBar/sector")
sector<-sector[,1]
sector <- as.character(sector)

### DATA MASTER
fdBau <- read.table("_DB/csv data/FD_BAU.csv",header = T,sep = "," , stringsAsFactors = F) #kolom skenario utk id skenario
fdBau <- cbind(sector,fdBau)
fdBau$sector <- as.character(fdBau$sector)
#rownames(fdBau)<-sector

#ID
selectedProv<-"JaBar"
selectedSektor <- "energi"
username<-"dw"


### variable untuk aksi satelit energi
energyBauRDS <- readRDS("_DB/rds energy/proyTabelKonsEnergiBAU.rds")


# sat_his <- read.table("_DB/csv data/konsumsi 2016.csv",header = T,sep = ",")
# row.names(sat_his)<-sector
# satIntervensiSken1 <- sat_his[,c(1,5)]

### nama 26 bahan bakar 
bahanBakar <- colnames(energyBauRDS$y2016)