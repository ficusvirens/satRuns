setwd("/wrk/mpeltoni/DONOTREMOVE/PREBASruns/codeshare/forestFlux/pilotAreas/finland/")
source("Rsrc/extractOutFunctions.R")
clims <- c("CurrClim")
mans <- c("NoHarv")
# r_no <- 13
species <- "tot" #1,2,3
startingYear <- c(2014,2019)
varXs <- c(7:10,12:13,16:18,20,21:25,28,35)

for(stYear in startingYear){
  print(stYear)
  
  yearOut=stYear+5
  load(paste0(stYear,"/XYsegID.rdata"))
  for(varX in varXs){
    createTifFromDT(clims, mans, yearOut, varX, species, stYear)
  }
}

