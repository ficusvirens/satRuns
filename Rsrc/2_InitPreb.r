library(reshape2)
library(plyr)
library("raster")
library(data.table)
require(sm)
require(rgdal)

if (!require(Rprebasso)) {
  # devtools::install_github("checcomi/Rprebasso", force=TRUE,ref="newVersion")
  # devtools::install_github("checcomi/Rprebasso", ref="master")
  #install.packages("C:/Users/peltonie/Downloads/prebassoInprog-master (3)/prebassoInprog-master",  type="source", repos=NULL)
  require(Rprebasso)
}

### Run settings
source("Rsrc/settings.r")

source("Rsrc/functions.r")


load(paste0(procDataPath,startingYear,"/samples.rdata"))
nSamples <- length(samples)
sampleIDs <- 1:nSamples

if(testRun){
  sampleID <- 1
  rcpfile="CurrClim"
}


for (rcpfile in rcps) { ## ---------------------------------------------
  print(date())
  print(rcpfile)
  if(rcpfile=="CurrClim"){
    load(paste(climatepath, rcpfile,".rdata", sep=""))  
    #####process data considering only current climate###
    # dat <- dat[rday %in% 1:10958] #uncomment to select some years (10958 needs to be modified)
    # maxRday <- max(dat$rday)
    # xday <- c(dat$rday,(dat$rday+maxRday),(dat$rday+maxRday*2))
    # dat = rbind(dat,dat,dat)
    # dat[,rday:=xday]
    
  } else{
    load(paste(climatepath, rcpfile, sep=""))  
  }

  gc()
  
  ## Prepare the same initial state for all harvest scenarios that are simulated in a loop below
  data.sample <- samples[[sampleID]]
  # nSample <- nrow(sampleX)
  # data.sample = sample_data.f(sampleX, nSample)
  totAreaSample <- sum(data.sample$area)
  
  ###check if climID matches
  allclIDs <- unique(dat$id)
  samClIds <- unique(data.sample$id)
  if(!all(samClIds %in% allclIDs)){
    opsClim <- samClIds[which(!samClIds %in% allclIDs)]
    dt = data.table(allclIDs, val = allclIDs) # you'll see why val is needed in a sec
    setnames(dt,c("x","val"))
    # setattr(dt, "sorted", "x")  # let data.table know that w is sorted
    setkey(dt, x) # sorts the data
    # binary search and "roll" to the nearest neighbour
    replX <- dt[J(opsClim), roll = "nearest"]
    data.sample$id <- mapvalues(data.sample$id,replX[[1]],replX[[2]])
  }
  
  clim = prep.climate.f(dat, data.sample, startingYear, nYears)
  
  # Region = nfiareas[ID==r_no, Region]
  
  initPrebas = create_prebas_input.f(r_no, clim, data.sample, nYears = nYears,
                                     startingYear = startingYear,domSPrun=domSPrun)
  
  XXXX
  
  
  
  
  
  
  
  
  
  initSoil <- aperm(initPrebas$soilC,c(3:5,1,2))
  
  ###in this loop initialize soil for each pixel according to sample forest center
  for(piX in 1:15){  
    sitX <- which(sampleX$forCentre==piX)
    initSoil[,,1,sitX,1] <- initSoilCstst[[piX]]
  }
  initSoil <- aperm(initSoil,c(4,5,1:3))
  initPrebas$soilC <- initSoil
  
  ##here mix years for weather inputs for Curr Climate
  if(rcpfile=="CurrClim.rdata"){
    set.seed(10)
    resampleYear <- sample(1:nYears,nYears)
    initPrebas$ETSy <- initPrebas$ETSy[,resampleYear]
    initPrebas$P0y <- initPrebas$P0y[,resampleYear,]
    initPrebas$weather <- initPrebas$weather[,resampleYear,,]
    initPrebas$weatherYasso <- initPrebas$weatherYasso[,resampleYear,]
  }
  
  
  # Loop management scenarios ------------------------------------------------
  for (harscen in harvestscenarios) { ## MaxSust fails, others worked.
    print(date())
    print(harscen)
    i = i + 1
    # print(paste(i, (length(harvestscenarios)*length(rcps)*length(regions)), sep="/"))
    # harscen ="Base"
    ## Assign harvesting quota for the region based on volume (in NFI startingYear) and MELA
    Region = nfiareas[ID==r_no, Region]
    if(harscen=="NoHarv"){
      initPrebas$ClCut = initPrebas$defaultThin = rep(0,nSample)
      HarvLim1 = 0
    }else if(harscen=="Tapio"){
      HarvLim1 = 0
    }else{
      HarvLim0 = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "1990-2013"]
      HarvLim0  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim0
      HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2015-2024"]
      HarvLim  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
      HarvLim1 <- rep(as.numeric(HarvLim),10)
      HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2025-2034"]
      HarvLim  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
      HarvLim1 <- c(HarvLim1,rep(as.numeric(HarvLim),10))
      HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2035-2044"]
      HarvLim  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
      HarvLim1 <- c(HarvLim1,rep(as.numeric(HarvLim),10))
      HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2045-2054"]
      HarvLim  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
      HarvLim1 <- c(HarvLim1,rep(as.numeric(HarvLim),10))
      HarvLim = nfiareas[ID==r_no, VOL_fraction]*rem[Scenario == harscen & Area == Region, "2055-2064"]
      HarvLim  = (totAreaSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
      HarvLim1 <- c(HarvLim1,rep(as.numeric(HarvLim),44))
    }
    ## In the model, harvests are always per hectar units. If 1000 pixels (nSample)
    ## are simulated it corresponds to 1000 hectars, although pixels are only 16x16 m2.
    ## Therefore, we need to apply the areal fraction of removals scenarios
    ## nfiareas are in 1000 ha, model takes Harvlim in m3, while removals from Mela are 1000 m3
    #      HarvLim  = (nSample/1000) / nfiareas[ID == r_no, AREA] * 1e3 *HarvLim
    
    system.time(region <- regionPrebas(initPrebas, HarvLim = as.numeric(HarvLim1), minDharv = 15.0))
    
    outSel <- c(1,5:9,11:15,17:19,22,24:33,37:46)
    out = list(annual=region$multiOut[,,outSel,,1],
               dailysample=region$dailyPRELES[floor(seq(1, nSample,
                                                        len=min(c(nSample/100), 100))), , ])
    
    save(out,file=paste0("output/",rcpfile,harscen,"_Reg",r_no,"_sample",sampleID,".rdata"))
    rm(region); gc()
    rm(out); gc()
  }
}
# }
