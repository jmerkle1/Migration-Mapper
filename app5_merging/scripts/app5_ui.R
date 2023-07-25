mergeOrder<<-c('year','id')
seasonsToMerge<<-c()
udFootprintsToDrop<<-NULL

app5_init<-function(input, output, session){
  input<<-input
  output<<-output
  session<<-session


  whichAppIsRunning<<-'app5'
  observeEvent(input$changeAppsButton, {
    changeToOtherApp()
  },ignoreInit=TRUE)

  observeEvent(input$closeMappButton, {
    closeApp()
  },ignoreInit=TRUE)

  observeEvent(input$udFootprintsToDropSelect, {
    udFootprintsToDrop<<-input$udFootprintsToDropSelect
  },ignoreInit=TRUE,ignoreNULL = FALSE)

  observeEvent(input$resetConfigOptionsButton,{
      udPopConfigOptionsInit()
  },ignoreInit=TRUE)

  testInteger <- function(x){
    test <- all.equal(x, as.integer(x), check.attributes = FALSE)
    if(test == TRUE){ return(TRUE) }
    else { return(FALSE) }
  }

  observeEvent(input$processContoursPopFootprintsButton,{
    theseContours<-processContours(input$contourLevelsPopFootprintsInput)
    if(any(is.na(theseContours))){
      modalMessager('error processing contours','you have na values')
      updateTextInput(session, 'contourLevelsPopFootprintsInput', value = configOptions$popConfigOption$contourLevelsPopFootprints)
      return()
    }
    if(any(theseContours>100)){
      modalMessager('error processing contours','you have values greater than 100')
      updateTextInput(session, 'contourLevelsPopFootprintsInput', value = configOptions$popConfigOption$contourLevelsPopFootprints)
      return()
    }
    if(any(theseContours==0)){
      modalMessager('error processing contours','you have values == 0')
      updateTextInput(session, 'contourLevelsPopFootprintsInput', value = configOptions$popConfigOption$contourLevelsPopFootprints)
      return()
    }
    if(any(theseContours<0)){
      modalMessager('error processing contours','you have values < 0')
      updateTextInput(session, 'contourLevelsPopFootprintsInput', value = configOptions$popConfigOption$contourLevelsPopFootprints)
      return()
    }
    # if(!testInteger(theseContours)){
    #   modalMessager('error processing contours','you have non integers')
    #   updateTextInput(session, 'contourLevelsPopFootprintsInput', value = configOptions$popConfigOption$contourLevelsPopFootprints)
    #   return()
    # }
    if(any(!theseContours == sort(theseContours))){
      modalMessager('error processing contours','contours should be in ascending order')
      updateTextInput(session, 'contourLevelsPopFootprintsInput', value = configOptions$popConfigOption$contourLevelsPopFootprints)
      return()
    }
    if(any(duplicated(theseContours))){
      modalMessager('error processing contours','you have duplicated values')
      updateTextInput(session, 'contourLevelsPopFootprintsInput', value = configOptions$popConfigOption$contourLevelsPopFootprints)
      return()
    }
    modalMessager('success','contours processed')
    configOptions$popConfigOption$contourLevelsPopFootprints<<-input$contourLevelsPopFootprintsInput
    saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$processContoursPopUseButton,{
    theseContours<-processContours(input$contourLevelsPopUseInput)
    if(any(is.na(theseContours))){
      modalMessager('error processing contours','you have na values')
      updateTextInput(session, 'contourLevelsPopUseInput', value = configOptions$popConfigOption$contourLevelsPopUse)
      return()
    }
    if(any(theseContours>99)){
      modalMessager('error processing contours','you have values greater than 99')
      updateTextInput(session, 'contourLevelsPopUseInput', value = configOptions$popConfigOption$contourLevelsPopUse)
      return()
    }
    if(any(theseContours==0)){
      modalMessager('error processing contours','you have values == 0')
      updateTextInput(session, 'contourLevelsPopUseInput', value = configOptions$popConfigOption$contourLevelsPopUse)
      return()
    }
    if(!testInteger(theseContours)){
      modalMessager('error processing contours','you have non integers')
      updateTextInput(session, 'contourLevelsPopUseInput', value = configOptions$popConfigOption$contourLevelsPopUse)
      return()
    }
    if(any(!theseContours == sort(theseContours))){
      modalMessager('error processing contours','contours should be in ascending order')
      updateTextInput(session, 'contourLevelsPopUseInput', value = configOptions$popConfigOption$contourLevelsPopUse)
      return()
    }
    if(any(duplicated(theseContours))){
      modalMessager('error processing contours','you have duplicated values')
      updateTextInput(session, 'contourLevelsPopUseInput', value = configOptions$popConfigOption$contourLevelsPopUse)
      return()
    }
    modalMessager('success','contours processed')
    configOptions$popConfigOption$contourLevelsPopUse<<-input$contourLevelsPopUseInput
    saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$minAreaDropInput,{
    configOptions$popConfigOption$minAreaDrop<<-input$minAreaDropInput
    saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$minAreaFillInput,{
    configOptions$popConfigOption$minAreaFill<<-input$minAreaFillInput
    saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$contourTypeInput,{
    configOptions$popConfigOption$contourType<<-input$contourTypeInput
    saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$simplifyInput,{
    configOptions$popConfigOption$simplify<<-input$simplifyInput
    saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$kSmoothSmoothnessInput,{
    configOptions$popConfigOption$kSmoothSmoothness<<-input$kSmoothSmoothnessInput
    saveConfig()
  },ignoreInit=TRUE)



  observeEvent(input$loadProjectButton,{
      tryCatch({
        # rdsLocation <- file.choose();
        rdsLocation <- choose.dir(caption = "select your project folder and press OK")
        appFiveReload(rdsLocation)
      }, error = function(ex) {
        modalMessager('Error',paste0('Try choosing a file again'))
        dawg<<-ex
      })
  },ignoreInit=TRUE)

  observeEvent(input$seasonsToMergeInput,{
    seasonsToMerge<<-input$seasonsToMergeInput
    getIdsAndYearsFromSeasons()
  },ignoreInit=TRUE)

  observeEvent(input$mergeOrderInput,{
    tempValue<-input$mergeOrderInput
    if(length(tempValue)>2){
      newValues<-c(tempValue[2],tempValue[3])
      mergeOrder<<-input$mergeOrderInput
      updateSelectInput(
        session,
        'mergeOrderInput',
        selected = newValues
      )
    }else{
      mergeOrder<<-input$mergeOrderInput
    }
  },ignoreInit=TRUE)


  observeEvent(input$beginMerginButton,{
    if(length(seasonsToMerge)==0){
      modalMessager('Error',paste0('You need to select at least one season to begin merging'))
      return()
    }
    if(length(mergeOrder)==0){
      modalMessager('Error',paste0('You need to choose 1 or two values for the merge order'))
      return()
    }
    begingMerging()
  },ignoreInit=TRUE)

}

buildSequencesButtons<-function(){

  udFolder<<-paste0(masterWorkingDirectory,'\\UDs')
  footPrintsFolder<<-paste0(masterWorkingDirectory,'\\Footprints')

  udDirs<-list.dirs(udFolder,recursive = F)
  availableSequencesToMerge<-c()
  for(i in 1:length(udDirs)){
    thisDir<-udDirs[i]
    theseFiles<-list.files(thisDir)
    if(length(theseFiles)>0){
      thisUdName<-strsplit(thisDir,'UDs/')[[1]][2]
      availableSequencesToMerge<-c(availableSequencesToMerge,thisUdName)
    }
  }

  footprintDirs<-list.dirs(footPrintsFolder,recursive = F)
  for(i in 1:length(footprintDirs)){
    thisDir<-footprintDirs[i]
    theseFiles<-list.files(thisDir)
    if(length(theseFiles)>0){
      thisFootprintName<-strsplit(thisDir,'Footprints/')[[1]][2]
      if(!thisFootprintName%in%availableSequencesToMerge){
        availableSequencesToMerge<-c(availableSequencesToMerge,thisFootprintName)
      }
    }
  }

  updateSelectInput(
    session,
    'seasonsToMergeInput',
    choices = availableSequencesToMerge,
    selected = NULL
  )
}

reloadUdPopConfigOptions<-function(){
  theseConfigOptions<-names(configOptions$popConfigOption)
  for(i in 1:length(theseConfigOptions)){
    thisOptionName<-theseConfigOptions[i]
    thisValue<-configOptions$popConfigOption[[thisOptionName]]
    thisInputName<-paste0(thisOptionName,'Input')
    if(thisOptionName=='contourType'){
      updateSelectInput(session, thisInputName, selected=thisValue)
    }
    else if(thisOptionName=='simplify'){
      updateSelectInput(session, thisInputName, selected=thisValue)
    }else if(thisOptionName=='contourLevelsPopUse' | thisOptionName=='contourLevelsPopFootprints'){
      updateTextInput(session, thisInputName, value=thisValue)
    }else{
      updateNumericInput(session, thisInputName, value = thisValue)
    }
  }
}


udPopConfigOptionsInit<-function(){

    if(!exists('configOptions')){
      configOptions<<-list()
    }

    configOptions$popConfigOption$contourLevelsPopUse<<-'5,10,15,20,30,40,50,60,70,80,90'
    configOptions$popConfigOption$contourLevelsPopFootprints<<-'5,10,15,20,30'
    configOptions$popConfigOption$minAreaDrop<<-20000
    configOptions$popConfigOption$minAreaFill<<-20000
    configOptions$popConfigOption$simplify<<-TRUE
    configOptions$popConfigOption$contourType<<-'Area'
    configOptions$popConfigOption$kSmoothSmoothness<<-2

    saveConfig()
    reloadUdPopConfigOptions()
}



processContours<-function(contoursString){
  contoursProcessed<-strsplit(contoursString,',')
  contoursProcessed<-as.numeric(contoursProcessed[[1]])
  return(contoursProcessed)
}


getIdsAndYearsFromSeasons<-function(){
  if(length(seasonsToMerge)==0){
    hideElement('dropConfigRow')
    return()
  }else{
    showElement('dropConfigRow')
  }
  udFootprintsToDropForMenu<-c()
  for(i in 1:length(seasonsToMerge)){
    thisSeason<-seasonsToMerge[i]
    thisUdDirectory<-paste0(masterWorkingDirectory,'\\UDs\\',thisSeason)
    thisFootprintsDirectory<-paste0(masterWorkingDirectory,'\\Footprints\\',thisSeason)
    theseFiles<-list.files(thisUdDirectory)
    udFootprintsToDropForMenu<-c(udFootprintsToDropForMenu,str_split_fixed(theseFiles,'.tif',2)[,1])
    theseFiles<-list.files(thisFootprintsDirectory)
    udFootprintsToDropForMenu<-c(udFootprintsToDropForMenu,str_split_fixed(theseFiles,'.tif',2)[,1])
  }
  udFootprintsToDropForMenu<-unique(udFootprintsToDropForMenu)
  udFootprintsToDropForMenu<-c('',udFootprintsToDropForMenu)
  updateSelectInput(
    session,
    'udFootprintsToDropSelect',
    selected = NULL,
    choices=udFootprintsToDropForMenu
  )
}
