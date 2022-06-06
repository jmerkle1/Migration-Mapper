app4_init<-function(input, output, session){

  input<<-input
  output<<-output
  session<<-session

  whichAppIsRunning<<-'app4'
  observeEvent(input$changeAppsButton, {
    changeToOtherApp()
  },ignoreInit=TRUE)

  observeEvent(input$closeMappButton, {
    closeApp()
  },ignoreInit=TRUE)


  observeEvent(input$processAllButton,{
      startUdProcessing('allSequences')
  },ignoreInit=TRUE)


  observeEvent(input$resetConfigOptionsButton,{
      udConfigOptionsInit()
  },ignoreInit=TRUE)

  observeEvent(input$numberOfCoresInput,{
      configOptions$udConfigOptions$numberOfCores<<-input$numberOfCoresInput
      saveConfig()
  },ignoreInit=TRUE)


  observeEvent(input$udMethodInput,{
      configOptions$udConfigOptions$udMethod<<-input$udMethodInput
      saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$mult4buffInput,{
      configOptions$udConfigOptions$mult4buff<<-input$mult4buffInput
      saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$cellSizeInput,{
      configOptions$udConfigOptions$cellSize<<-input$cellSizeInput
      saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$buffInput,{
      configOptions$udConfigOptions$buff<<-input$buffInput
      saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$BMVarInput,{
      configOptions$udConfigOptions$BMVar<<-input$BMVarInput
      saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$locationErrorInput,{
      configOptions$udConfigOptions$locationError<<-input$locationErrorInput
      saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$maxLagInput,{
      configOptions$udConfigOptions$maxLag<<-input$maxLagInput
      saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$contourInput,{
      configOptions$udConfigOptions$contour<<-input$contourInput
      saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$timeStepInput,{
      configOptions$udConfigOptions$timeStep<<-input$timeStepInput
      saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$maxTimeoutInput,{
      configOptions$udConfigOptions$maxTimeout<<-input$maxTimeoutInput
      saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$dbbmmMarginInput,{
      configOptions$udConfigOptions$dbbmmMargin<<-input$dbbmmMarginInput
      saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$dbbmmWindowInput,{
      configOptions$udConfigOptions$dbbmmWindow<<-input$dbbmmWindowInput
      saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$smoothParamInput,{
      configOptions$udConfigOptions$smoothParam<<-input$smoothParamInput
      saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$subsampleInput,{
      configOptions$udConfigOptions$subsample<<-input$subsampleInput
      saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$informationCriteriaInput,{
      configOptions$udConfigOptions$informationCriteria<<-input$informationCriteriaInput
      saveConfig()
  },ignoreInit=TRUE)



  observeEvent(input$loadProjectButton,{
      tryCatch({
        # rdsLocation <- file.choose();
        rdsLocation <- choose.dir(caption = "select your project folder and press OK")
        appFourReload(rdsLocation)
      }, error = function(ex) {
        modalMessager('Error',paste0('Try choosing a file again'))
        dawg<<-ex
      })
  },ignoreInit=TRUE)
}

getSequences<-function(){
  sequencesDir<-paste0(masterWorkingDirectory,'\\sequences')
  sequenceFolders<-list.dirs(sequencesDir)
  availableSequences<<-list()
  if(length(sequenceFolders)==1){
    modalMessager('Error',paste0('There are no sequences in your sequences folder. Please go back to app 3 and try exporting sequences again'))
    return()
  }
  if(length(sequenceFolders)==0){
    modalMessager('Error',paste0('No sequences have been created for this project. Please go back to app 3 and create some.'))
    return()
  }
  for(i in 2:length(sequenceFolders)){
    thisFolderContent<-list.files(sequenceFolders[i])
    thisFolderContentFull<-list.files(sequenceFolders[i],full.names=TRUE)
    if(length(thisFolderContent)>0){
      thisSequencesName<-strsplit(thisFolderContent,'\\.')[[1]][1]
      availableSequences[[thisSequencesName]]<<-thisFolderContentFull
      addSequenceButton(i-1,thisSequencesName)
    }
  }
}

addSequenceButton<-function(position,seqName){
  print('-----------')
  print(seqName)
  buttonHolderName<-paste0('sequenceButtonHolder',position)
  buttonName<-paste0('sequenceButton',position)
  output[[buttonHolderName]] <- renderUI({
    actionButton(buttonName, seqName)
  })

  observeEvent(input[[buttonName]], {
    startUdProcessing(seqName)
  },ignoreInit=TRUE)
}

reloadUdConfigOptions<-function(){
  theseConfigOptions<-names(configOptions$udConfigOptions)
  for(i in 1:length(theseConfigOptions)){
    thisOptionName<-theseConfigOptions[i]
    thisValue<-configOptions$udConfigOptions[[thisOptionName]]
    thisInputName<-paste0(thisOptionName,'Input')
    if(thisOptionName=='udMethod'){
      updateSelectInput(session, thisInputName, selected=thisValue)
    }else if(thisOptionName=='informationCriteria'){
      updateSelectInput(session, thisInputName, selected=thisValue)
    }else{
      updateNumericInput(session, thisInputName, value = thisValue)
    }
  }
}

udConfigOptionsInit<-function(){

    if(!exists('configOptions')){
      configOptions<<-list()
    }

    configOptions$udConfigOptions$udMethod<<-'BBMM'

    configOptions$udConfigOptions$mult4buff<<-0.3

    configOptions$udConfigOptions$cellSize<<-500

    configOptions$udConfigOptions$buff<<-200

    configOptions$udConfigOptions$BMVar<<-NULL

    configOptions$udConfigOptions$locationError<<-20

    configOptions$udConfigOptions$maxLag<<-8

    configOptions$udConfigOptions$contour<<-99

    configOptions$udConfigOptions$timeStep<<-5

    configOptions$udConfigOptions$maxTimeout<<-720

    configOptions$udConfigOptions$dbbmmMargin<<-11

    configOptions$udConfigOptions$dbbmmWindow<<-31

    configOptions$udConfigOptions$smoothParam<<-NULL

    configOptions$udConfigOptions$subsample<<-NULL

    configOptions$udConfigOptions$informationCriteria<<-'AIC'

    configOptions$udConfigOptions$numberOfCores<<-detectCores() - 1 # this should be the default, but the user could choose too

    saveConfig()
    reloadUdConfigOptions()
}
