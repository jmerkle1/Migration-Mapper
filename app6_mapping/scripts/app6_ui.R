app6_init<-function(input, output, session){
  input<<-input
  output<<-output
  session<<-session

  whichAppIsRunning<<-'app6'
  observeEvent(input$changeAppsButton, {
    changeToOtherApp()
  },ignoreInit=TRUE)

  observeEvent(input$closeMappButton, {
    closeApp()
  },ignoreInit=TRUE)

volumes<<-getVolumes()()
  shinyDirChoose(input, "loadProjectButton", roots=volumes, filetypes = NULL,allowDirCreate=FALSE)
  observeEvent(input$loadProjectButton,{
      thisSelectedFolder<-getFolderPathFromShinyDirChoose(volumes,input$loadProjectButton)
      if(!is.null(thisSelectedFolder)){
        appSixReload(thisSelectedFolder)
      }      
  },ignoreInit=TRUE)



}

filesHolder<<-list()

dataInit<-function(){

  runjs('
  mapboxer._widget.map.map.addControl(new mapboxgl.ScaleControl({position: "bottom-right", unit:"imperial"}));
  mapboxer._widget.map.map.addControl(new mapboxgl.ScaleControl({position: "bottom-right", unit:"metric"}));
  ')



  sequencesDirectory<-paste0(masterWorkingDirectory,'\\sequenceShapefiles')

  sequencesFiles<-dir(sequencesDirectory,pattern = '*.shp')
  if(length(sequencesFiles)>0){
    loadingScreenToggle('show','loading existing sequence files')
    for(i in 1:length(sequencesFiles)){

      thisFile<-sequencesFiles[i]
      thisFileName<-strsplit(thisFile,'.shp')[[1]][1]
      thisFile <- st_read(paste0(sequencesDirectory,'\\',thisFile))
      thisFile<- st_transform(thisFile, "EPSG:4326") # set CRS
      filesHolder$sequences[[thisFileName]]<<-thisFile
    }
  }

  finalMergedDirectories<-list.dirs(paste0(masterWorkingDirectory,'\\finalOutputs'),recursive=F)
  if(length(finalMergedDirectories)>0){
    for(i in 1:length(finalMergedDirectories)){
      thisMergedDirectory<-finalMergedDirectories[i]
      thisDirectorySplit<-strsplit(thisMergedDirectory,'/')
      thisMergedName<-thisDirectorySplit[[1]][length(thisDirectorySplit[[1]])]
      footprintsDirectory<-paste0(thisMergedDirectory,'\\footPrintsMerged')
      popUseDirectory<-paste0(thisMergedDirectory,'\\popUseMerged')

      footprintsFiles<-dir(footprintsDirectory,pattern = '*.shp')

      popUseFiles<-dir(popUseDirectory,pattern = '*.shp')

      if(length(footprintsFiles)>0){
        loadingScreenToggle('show','loading existing footprint files')
        for(j in 1:length(footprintsFiles)){

          thisFile<-footprintsFiles[j]
          thisFileName<-strsplit(thisFile,'.shp')[[1]][1]
          thisFile <- st_read(paste0(footprintsDirectory,'\\',thisFile))
          thisFile<- st_transform(thisFile, "EPSG:4326") # set CRS
          filesHolder$footprints[[paste0(thisMergedName,'_',thisFileName)]]<<-thisFile
        }
      }
      if(length(popUseFiles)>0){
        loadingScreenToggle('show','loading existing population use files')
        for(j in 1:length(popUseFiles)){
          
          thisFile<-popUseFiles[j]
          thisFileName<-strsplit(thisFile,'.shp')[[1]][1]
          thisFile <- st_read(paste0(popUseDirectory,'\\',thisFile))
          thisFile<- st_transform(thisFile, "EPSG:4326") # set CRS
          filesHolder$popUse[[paste0(thisMergedName,'_',thisFileName)]]<<-thisFile
        }
      }
    }
  }





  loadingScreenToggle('hide','loading existing project files')
  buildSelectMenus()
  mapProjectFiles()
}


buildSelectMenus<-function(){

  loadedFileTypes<-names(filesHolder)


  if('footprints'%in%loadedFileTypes){
    theseFootprintFiles<-names(filesHolder$footprints)
    output$footPrintsSelector <- renderUI({
      checkboxGroupInput(
        'footPrintsSelector',
        "Footprint files ",
        selected=NULL,
        choiceNames=theseFootprintFiles,
        choiceValues=theseFootprintFiles
        )

    })

    observeEvent(input$footPrintsSelector,{
      visibleFootprints<<-input$footPrintsSelector
      for(i in 1:length(theseFootprintFiles)){
        thisFile<-theseFootprintFiles[i]
        thisLayerName<-paste0(thisFile,'Layer')
        if(thisFile%in%visibleFootprints){
          showMapLayer(thisLayerName)
        }else{
          hideMapLayer(thisLayerName)
        }
      }
    },ignoreInit=TRUE,ignoreNULL = FALSE)
  }

  if('popUse'%in%loadedFileTypes){
    thesePopUseFiles<-names(filesHolder$popUse)
    output$popUseSelector <- renderUI({
      checkboxGroupInput(
        'popUseSelector',
        "Population use files ",
        selected=NULL,
        choiceNames=thesePopUseFiles,
        choiceValues=thesePopUseFiles
        )

    })

    observeEvent(input$popUseSelector,{
      visiblePopuse<<-input$popUseSelector
      for(i in 1:length(thesePopUseFiles)){
        thisFile<-thesePopUseFiles[i]
        thisLayerName<-paste0(thisFile,'Layer')
        if(thisFile%in%visiblePopuse){
          showMapLayer(thisLayerName)
        }else{
          hideMapLayer(thisLayerName)
        }
      }
    },ignoreInit=TRUE,ignoreNULL = FALSE)
  }


  if('sequences'%in%loadedFileTypes){
    theseFiles<-names(filesHolder$sequences)
    output$sequencesSelector <- renderUI({
      checkboxGroupInput(
        'sequencesSelector',
        "Sequence files ",
        selected=NULL,
        choiceNames=theseFiles,
        choiceValues=theseFiles
        )
    })

    observeEvent(input$sequencesSelector,{
      visibleSequences<<-input$sequencesSelector
      for(i in 1:length(theseFiles)){
        thisFile<-theseFiles[i]
        thisLayerName<-paste0(thisFile,'Layer')
        if(thisFile%in%visibleSequences){
          showMapLayer(thisLayerName)
        }else{
          hideMapLayer(thisLayerName)
        }
      }
    },ignoreInit=TRUE,ignoreNULL = FALSE)

  }


}
