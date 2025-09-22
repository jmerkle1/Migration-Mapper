begingMerging<-function(){


  mergeName<<-do.call(paste, c(as.list(seasonsToMerge), sep = "_"))


  showModal(modalDialog(
         title="Choose name for merged files",
         "Please choose a name for this group of merged files. The default is a list of the sequence names, pasted together below. You can change this if you'd prefer a different name. Once ready to proceed, click the continue button below.",
         br(),
         textInput('mergeNameInput', '', value = mergeName, width = NULL, placeholder = NULL),
         footer = tagList(
           actionButton("continueButton", "continue"),
           tags$head(tags$style("#shiny-modal .modal-footer{ display:block}"))
         )
     ))

   observeEvent(input$continueButton,{
     toggleModal(session,'configModal',toggle='close')
     removeModal()
     # check if this folder exists
     newMergeFolder<-paste0(masterWorkingDirectory,'\\finalOutputs\\',mergeName)
     if(dir.exists(newMergeFolder)){
       modalMessager('error','a folder with this name already exists in your finalOutputs folder. Please use a different name or delete that folder and try again')
     }else{
       createDirectories(mergeName)
     }
     # chooseStartDate()
   },ignoreInit=TRUE,once = TRUE)

   observeEvent(input$mergeNameInput,{
     mergeName<<-input$mergeNameInput
   },ignoreInit=TRUE)
}

createDirectories<-function(mergeName){

  if(!is.null(udFootprintsToDrop)){
    write.csv(udFootprintsToDrop,)
  }

  finalOutputsFolder<<-paste0(masterWorkingDirectory,'\\finalOutputs')
  thisMergeFolder<<-paste0(masterWorkingDirectory,'\\finalOutputs\\',mergeName)
  popUseFolder<<-paste0(thisMergeFolder,'\\popUseMerged')
  footprintsMergedFolder<<-paste0(thisMergeFolder,'\\footPrintsMerged')

  

  if(dir.exists(finalOutputsFolder)==FALSE){
    dir.create(finalOutputsFolder)
  }
  if(dir.exists(thisMergeFolder)==FALSE){
    dir.create(thisMergeFolder)
  }
  if(dir.exists(popUseFolder)==FALSE){
    dir.create(popUseFolder)
  }
  if(dir.exists(footprintsMergedFolder)==FALSE){
    dir.create(footprintsMergedFolder)
  }

  if(!is.null(udFootprintsToDrop)){
    write.csv(udFootprintsToDrop,paste0(finalOutputsFolder,'\\',mergeName,'_droppedUds.csv'),row.names = FALSE)
  }

  calculatePopUse()

}

calculatePopUse<-function(){

  udFolder<-paste0(masterWorkingDirectory,'\\Uds')

  # gotta check the ud folders in case they are empty.. if one is empty then that seq was created using CalcLineBuff
  seasonsToMergeUd<-c()
  for(i in 1:length(seasonsToMerge)){
    thisSeason<-seasonsToMerge[i]
    thisSeasonFolder<-paste0(udFolder,'\\',thisSeason)
    thisFolderContent<-list.files(thisSeasonFolder)
    if(length(thisFolderContent)>0){
      seasonsToMergeUd<-c(seasonsToMergeUd,thisSeason)
    }
  }

  if(length(seasonsToMergeUd)==0){
    print('no uds to calculate!!!!!')
    calculatePopFootprint()
    return()
  }

  loadingScreenToggle('show',paste0('calculating population use for seasons ',paste(seasonsToMerge)))

  seasonsToMergeUd<<-seasonsToMergeUd
  theseContLevels<<-processContours(configOptions$popConfigOption$contourLevelsPopUse)

  tryCatch({
    CalcPopUse(UD.fldr=udFolder,
      out.fldr=popUseFolder,
      seas2merge=seasonsToMergeUd,
      udFootprintsToDrop=udFootprintsToDrop,
      contour.levels=theseContLevels,
      merge.order=mergeOrder,
      contour = 99.99,
      contour.type=configOptions$popConfigOption$contourType,
      min_area_drop=as.numeric(configOptions$popConfigOption$minAreaDrop),
      min_area_fill=as.numeric(configOptions$popConfigOption$minAreaFill),
      simplify=configOptions$popConfigOption$simplify,
      ksmooth_smoothness=configOptions$popConfigOption$kSmoothSmoothness,
      out.proj=configOptions$masterCrs
    )
  }, error = function(ex) {
    bigMessage<<-ex
    modalMessager('Error',ex$message)
    loadingScreenToggle('hide',paste0('calculating population footprints for seasons ',paste(seasonsToMerge)))
    stop()
  })

  popUseParams<-data.frame(configOptions$popConfigOption)
  write.csv(popUseParams, file=paste0(popUseFolder,"/",mergeName,"_popUseParams.csv"), row.names = FALSE)



  calculatePopFootprint()

  loadingScreenToggle('hide',paste0('calculating population use for seasons ',paste(seasonsToMerge)))
}


calculatePopFootprint<-function(){

  loadingScreenToggle('show',paste0('calculating population footprints for seasons ',paste(seasonsToMerge)))

  footPrintsFolder<-paste0(masterWorkingDirectory,'\\Footprints')

  tryCatch({
    CalcPopFootprint(Foot.fldr=footPrintsFolder,
      out.fldr=footprintsMergedFolder,
      udFootprintsToDrop=udFootprintsToDrop,
      seas2merge=seasonsToMerge,
      contour.levels=processContours(configOptions$popConfigOption$contourLevelsPopFootprints),      
      min_area_drop=as.numeric(configOptions$popConfigOption$minAreaDrop),
      min_area_fill=as.numeric(configOptions$popConfigOption$minAreaFill),
      simplify=configOptions$popConfigOption$simplify,
      ksmooth_smoothness=configOptions$popConfigOption$kSmoothSmoothness,
      out.proj=configOptions$masterCrs
    )
  }, error = function(ex) {
    modalMessager('Error',ex$message)
    loadingScreenToggle('hide',paste0('calculating population footprints for seasons ',paste(seasonsToMerge)))
    stop()
  })

  popFootprintParams<-data.frame(configOptions$popConfigOption)
  write.csv(popFootprintParams, file=paste0(footprintsMergedFolder,"/",mergeName,"_popFootprintParams.csv"), row.names = FALSE)


  loadingScreenToggle('hide',paste0('calculating population footprints for seasons ',paste(seasonsToMerge)))  

}
