source("Rsrc/settings.r")
source("Rsrc/functions.r")
setwd(generalPath)
if(!dir.exists("outRast")) {
  dir.create("outRast")
}
load(paste0(procDataPath,startingYear,"/XYsegID.rdata"))


clims <- weather
mans <- harvscen


for(varX in varRast){
  createTifFromDT(clims, mans, yearOut, varX, layerDT, startingYear,XYsegID)
  print(varNames[varX])
}
