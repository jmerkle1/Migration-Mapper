
startUdProcessing<-function(seqName){
  checkForFolders(seqName)
}

checkForFolders<-function(seqName){
  udFolder<<-paste0(masterWorkingDirectory,"/UDs")
  footprintFolder<<-paste0(masterWorkingDirectory,"./Footprints")
  metadataFolder<<-paste0(masterWorkingDirectory,"./Metadata")
  populationGridFolder<<-paste0(masterWorkingDirectory,"./PopulationGrid")

  #check the new directories
  if(dir.exists(udFolder)==FALSE){
    dir.create(udFolder)
  }
  if(dir.exists(footprintFolder)==FALSE){
    dir.create(footprintFolder)
  }
  if(dir.exists(metadataFolder)==FALSE){
    dir.create(metadataFolder)
  }
  if(dir.exists(populationGridFolder)==FALSE){
    dir.create(populationGridFolder)
  }

  calculatePopulationGrid(seqName)
}

calculatePopulationGrid<-function(seqName){
  loadingScreenToggle('show',paste0('calculating population grid'))
  CalcPopGrid(datasf=importedDatasetMasterAsSf,
              out.fldr=populationGridFolder,
              mult4buff=configOptions$udConfigOptions$mult4buff,
              cell.size=configOptions$udConfigOptions$cellSize)
  loadingScreenToggle('hide',paste0('calculating population grid'))
  calculateMigDistances(seqName)
}

calculateMigDistances<-function(seqName){
  loadingScreenToggle('show',paste0('calculating migration distances for ',seqName))
  if(seqName=='allSequences'){
    allSequences<-names(availableSequences)
    for(i in 1:length(allSequences)){
      thisSequence<-allSequences[i]
      thisSequenceData<-readRDS(availableSequences[[thisSequence]][1])
      dists <- CalcSeqDistances(datasf=thisSequenceData, id.name="mig")
      write.csv(dists, file=paste0(metadataFolder,"/",thisSequence,"_migration_distance_info.csv"), row.names=FALSE)
    }
  }else{
    thisSequenceData<-readRDS(availableSequences[[seqName]][1])
    dists <- CalcSeqDistances(datasf=thisSequenceData, id.name="mig")
    write.csv(dists, file=paste0(metadataFolder,"/",seqName,"_migration_distance_info.csv"), row.names=FALSE)
  }
  loadingScreenToggle('hide',paste0('calculating migration distances'))

  prepUdProcessing(seqName)
}

prepUdProcessing<-function(seqName){
  resultsTable<<-NULL
  if(seqName=='allSequences'){
    allSequences<-names(availableSequences)
    for(i in 1:length(allSequences)){
      thisSequence<-allSequences[i]
      checkUdFolder(thisSequence)
    }
  }else{
    checkUdFolder(seqName)
  }
}

checkUdFolder<-function(seqName){
  thisUdFolder<-paste0(udFolder,'\\',seqName)
  thisFootprintFolder<-paste0(footprintFolder,'\\',seqName)
  #check the new directories
  if(dir.exists(thisUdFolder)==FALSE){
    dir.create(thisUdFolder)
  }
  if(dir.exists(thisFootprintFolder)==FALSE){
    dir.create(thisFootprintFolder)
  }

  if(length(dir(thisFootprintFolder)) > 0 | length(dir(thisUdFolder)) > 0){

    showModal(modalDialog(
             title="Files Already Exist",
             paste0("It looks like footprints and UDs were previously calculated for ",seqName,'. Would you like to delete these files and reprocess?' ),
             footer = tagList(actionButton("confirmRerun", "Yes delete and reprocess"),
                              actionButton("dontRerun", "No, don't delete")
             )
     ))


    observeEvent(input$confirmRerun,{
      toggleModal(session,'configModal',toggle='close')
      f <- list.files(thisFootprintFolder, include.dirs = F, full.names = T, recursive = T)
      file.remove(f)
      f <- list.files(thisUdFolder, include.dirs = F, full.names = T, recursive = T)
      file.remove(f)
      removeModal()
      runUdProcessing(seqName)
    },ignoreInit=TRUE)

    observeEvent(input$dontRerun,{
      removeModal()
    },ignoreInit=TRUE)

  }else{
    runUdProcessing(seqName)
  }
}

runUdProcessing<-function(seqName){

  thisUdFolder<-paste0(udFolder,'\\',seqName)
  thisFootprintFolder<-paste0(footprintFolder,'\\',seqName)

  loadingScreenToggle('show',paste0('starting UD calcuations using method ',configOptions$udConfigOptions$udMethod," for sequence ",seqName))
  d<-readRDS(availableSequences[[seqName]][1])
  loopit <- unique(d$mig)  # you will loop over each id_yr_seas
  # Setup cluster
  clust <- makeCluster(configOptions$udConfigOptions$numberOfCores)
  opts=configOptions$udConfigOptions$udMethod
  # export the objects you need for your calculations from your environment to each node's environment
  if(is.null(configOptions$udConfigOptions$BMVar)){
    BMVar<-NULL
  }else{
    BMVar=configOptions$udConfigOptions$BMVar
  }

  if(is.null(configOptions$udConfigOptions$smoothParam)){
    smoothParam<-NULL
  }else{
    smoothParam=configOptions$udConfigOptions$smoothParam
  }

  if(is.null(configOptions$udConfigOptions$subsample)){
    subsample<-NULL
  }else{
    subsample=configOptions$udConfigOptions$subsample
  }


  clusterExport(clust,
    varlist=c("subsample","smoothParam","BMVar","d","populationGridFolder","thisFootprintFolder","opts","thisUdFolder","loopit","CalcBBMM","CalcDBBMM","CalcKernel","CalcLineBuff","CalcCTMM","configOptions", "seqName"),
    envir=environment()
  )

  result.tbl <- do.call(rbind, clusterApplyLB(clust, 1:length(loopit), function(i){
     # need library() here for the packages your calculations require for your calculations
     library(sf)
     library(parallel)
     library(raster)

     # grab the sequence of interest
     tmp <- d[d$mig==loopit[i],]

     # prep some params that carry across the functions
     mult4buff <- configOptions$udConfigOptions$mult4buff
     Pop.grd=paste0(populationGridFolder,"/PopGrid_empty.tif")
     contour= configOptions$udConfigOptions$contour
     max.timeout= (configOptions$udConfigOptions$maxTimeout*60)
     date.name="date"
     UD.fldr= thisUdFolder
     Footprint.fldr= thisFootprintFolder

     # use new functions!!!!
     if(opts == "Line Buffer"){
       return(CalcLineBuff(
         seq.sf=tmp,
         seq.name=loopit[i],
         date.name=date.name,
         Footprint.fldr=Footprint.fldr,
         Pop.grd=Pop.grd,
         buff=configOptions$udConfigOptions$buff
       ))
     }

     if(opts == "BBMM"){
       return(CalcBBMM(
         seq.sf=tmp,
         seq.name=loopit[i],
         date.name=date.name,
         UD.fldr=UD.fldr,
         Footprint.fldr=Footprint.fldr,
         Pop.grd=Pop.grd,
         BMVar=BMVar,
         # BMVar=NULL,
         location.error=configOptions$udConfigOptions$locationError,
         max.lag=configOptions$udConfigOptions$maxLag,
         contour=contour,
         time.step=configOptions$udConfigOptions$timeStep,
         mult4buff=mult4buff,
         max.timeout=max.timeout
       ))
     }

     if(opts == "dBBMM"){
      return(CalcDBBMM(
        seq.sf=tmp,
        seq.name=loopit[i],
        date.name=date.name,
        UD.fldr=UD.fldr,
        Footprint.fldr=Footprint.fldr,
        Pop.grd=Pop.grd,
        location.error=configOptions$udConfigOptions$locationError,
        max.lag=configOptions$udConfigOptions$maxLag,
        contour=contour,
        dbbmm.margin=configOptions$udConfigOptions$dbbmmMargin,
        dbbmm.window=configOptions$udConfigOptions$dbbmmWindow,
        mult4buff=mult4buff,
        max.timeout=max.timeout
      ))
     }

     if(opts == "kernel"){
       return(CalcKernel(
         seq.sf=tmp,
         seq.name=loopit[i],
         date.name=date.name,
         UD.fldr=UD.fldr,
         Footprint.fldr=Footprint.fldr,
         Pop.grd=Pop.grd,
         smooth.param=smoothParam,
         contour=contour,
         mult4buff=mult4buff,
         subsample=subsample,
         max.timeout=max.timeout
       ))
     }

     if(opts == "CTMM"){
       return(CalcCTMM(
         seq.sf=tmp,
         seq.name=loopit[i],
         date.name=date.name,
         UD.fldr=UD.fldr,
         Footprint.fldr=Footprint.fldr,
         Pop.grd=Pop.grd,
         Information.Criteria=configOptions$udConfigOptions$informationCriteria,
         contour=contour,
         mult4buff=mult4buff,
         max.timeout=max.timeout
       ))
     }

   }))
   stopCluster(clust)   # you must stop the parallelization framework

   if(is.null(resultsTable)){
     resultsTable<<-result.tbl
   }else{
     resultsTable<<-rbind(resultsTable,result.tbl)
   }
   resultsTable$Start.Date<-as.character(resultsTable$Start.Date)
   resultsTable$End.Date<-as.character(resultsTable$End.Date)
   output$resultsTable <- renderTable(resultsTable)
   # write out result.tbl
   write.csv(result.tbl, file=paste0(metadataFolder,"/",seqName,"_metadata.csv"), row.names = FALSE)
   loadingScreenToggle('hide',paste0('starting UD calcuations using method ',configOptions$udConfigOptions$udMethod," for sequence ",seqName))
}
