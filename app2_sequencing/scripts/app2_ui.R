app2_init<-function(input, output, session){

  input<<-input
  output<<-output
  session<<-session

  whichBaseMap<<-'default'

  whichAppIsRunning<<-'app2'
  observeEvent(input$changeAppsButton, {
    changeToOtherApp()
  },ignoreInit=TRUE)





  nsdType<<-'nsdBio'

  observeEvent(input$closeMappButton, {
    closeApp()
  },ignoreInit=TRUE)

  observeEvent(input$exportMigtimeButton, {
    exportMigtimeTable()
  },ignoreInit=TRUE)





  observeEvent(input$nsdTypeSelect, {
    if(input$nsdTypeSelect=='NSD'){
      nsdType<<-'nsdBio'
      tempDate<-format(configOptions$bioYearStartDate,'%m-%d')
      plotLabel<<-paste0('Displacement^2 km since ',tempDate)
    }else{
      nsdType<<-'displacementBio'
      tempDate<-format(configOptions$bioYearStartDate,'%m-%d')
      plotLabel<<-paste0('Displacement km since ',tempDate)
    }
    tempIndividual<-currentIndividual
    thisIndex<-which(migtime$id_bioYear==currentIndividual)
    if(thisIndex==1){
      dummyIndividual<-migtime$id_bioYear[2]
    }else{
      dummyIndividual<-migtime$id_bioYear[1]
    }
    updateSelectInput(session,'currentIndividualSelector',selected=dummyIndividual)
    updateSelectInput(session,'currentIndividualSelector',selected=tempIndividual)
  },ignoreInit=TRUE)

  observeEvent(input$basemapButton, {
    if(whichBaseMap=='default'){
      runjs("mapboxer._widget.sequencesMap.map.setPaintProperty('satlayer','raster-opacity',1)")
      whichBaseMap<<-'sat'
    }else{
      runjs("mapboxer._widget.sequencesMap.map.setPaintProperty('satlayer','raster-opacity',0)")
      whichBaseMap<<-'default'
    }
  },ignoreInit=TRUE)



  observeEvent(input$yearStartDateSelector,{
    bioYearStartDate<<-input$yearStartDateSelector
    configOptions$bioYearStartDate<<-bioYearStartDate
    saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$calcMigtimeButton,{
    saveConfig()
    removeModal()
    calcBioYearParams()
  },ignoreInit=TRUE)


  observeEvent(input$rebuildMigtimeButton,{
    showModal(modalDialog(
           title="Rebuild Migtime Table?",
           "Are you sure you want to select a new biological year start date and rebuild the migtime table? Any sequence dates previously selected will be lost.",
           footer = tagList(actionButton("confirmRebuild", "Yes Rebuild Migtime Table"),
                            actionButton("dontRebuild", "No, don't rebuild table"),
                            tags$head(tags$style("#shiny-modal .modal-footer{ display:block}"))
           )
       ))
  },ignoreInit=TRUE)

  observeEvent(input$confirmRebuild,{
    toggleModal(session,'configModal',toggle='close')
    removeModal()
    chooseStartDate()
  },ignoreInit=TRUE)

  observeEvent(input$dontRebuild,{
    removeModal()
  },ignoreInit=TRUE)



  observeEvent(input$dateSlider1,{
    sliderId<-1
    sliderChange(sliderId,input$dateSlider1)
  },ignoreInit=TRUE)
  observeEvent(input$dateSlider2,{
    sliderId<-2
    sliderChange(sliderId,input$dateSlider2)
  },ignoreInit=TRUE)
  observeEvent(input$dateSlider3,{
    sliderId<-3
    sliderChange(sliderId,input$dateSlider3)
  },ignoreInit=TRUE)
  observeEvent(input$dateSlider4,{
    sliderId<-4
    sliderChange(sliderId,input$dateSlider4)
  },ignoreInit=TRUE)
  observeEvent(input$dateSlider5,{
    sliderId<-5
    sliderChange(sliderId,input$dateSlider5)
  },ignoreInit=TRUE)
  observeEvent(input$dateSlider6,{
    sliderId<-6
    sliderChange(sliderId,input$dateSlider6)
  },ignoreInit=TRUE)
  observeEvent(input$dateSlider7,{
    sliderId<-7
    sliderChange(sliderId,input$dateSlider7)
  },ignoreInit=TRUE)
  observeEvent(input$dateSlider8,{
    sliderId<-8
    sliderChange(sliderId,input$dateSlider8)
  },ignoreInit=TRUE)


  observeEvent(input$sequenceName1,{
    sequenceId<-1
    sequenceNameChange(sequenceId,input$sequenceName1)
  },ignoreInit=TRUE)
  observeEvent(input$sequenceName2,{
    sequenceId<-2
    sequenceNameChange(sequenceId,input$sequenceName2)
  },ignoreInit=TRUE)
  observeEvent(input$sequenceName3,{
    sequenceId<-3
    sequenceNameChange(sequenceId,input$sequenceName3)
  },ignoreInit=TRUE)
  observeEvent(input$sequenceName4,{
    sequenceId<-4
    sequenceNameChange(sequenceId,input$sequenceName4)
  },ignoreInit=TRUE)
  observeEvent(input$sequenceName5,{
    sequenceId<-5
    sequenceNameChange(sequenceId,input$sequenceName5)
  },ignoreInit=TRUE)
  observeEvent(input$sequenceName6,{
    sequenceId<-6
    sequenceNameChange(sequenceId,input$sequenceName6)
  },ignoreInit=TRUE)
  observeEvent(input$sequenceName7,{
    sequenceId<-7
    sequenceNameChange(sequenceId,input$sequenceName7)
  },ignoreInit=TRUE)
  observeEvent(input$sequenceName8,{
    sequenceId<-8
    sequenceNameChange(sequenceId,input$sequenceName8)
  },ignoreInit=TRUE)

  observeEvent(input$yearStartDateSelector,{

  },ignoreInit=TRUE)

  observeEvent(input$backwardHandlerButton, {
    forwardBackward('backward')
    emptyDatesCheck()
  },ignoreInit=TRUE)

  observeEvent(input$forwardHandlerButton, {
    forwardBackward('forward')
    emptyDatesCheck()
  },ignoreInit=TRUE)



  observeEvent(input$parametersButton, {
    toggleModal(session,'configModal',toggle='open')
  },ignoreInit=TRUE)

  observeEvent(input$addMigrationButton,{
      addMigration()
  },ignoreInit=TRUE)

  observeEvent(input$howManySequencesSelector,{
    totalSequences<<-input$howManySequencesSelector;
    configOptions$totalSequences<<-totalSequences
    saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$idYearNotesInput,{
    theseNotes<-input$idYearNotesInput

    theseNotes<-gsub('"',"",theseNotes)


    theseNotes<-gsub("'","",theseNotes)


    updateTextInput(session, 'idYearNotesInput', value=theseNotes)


    thisYrIdIndex<-which(migtime$id_bioYear==currentIndividual)

    migtime[thisYrIdIndex,'notes']<<-theseNotes
    updateTable('migtime','notes',paste0('where id_bioYear = "',currentIndividual,'"'),paste0('"',theseNotes,'"'))
  },ignoreInit=TRUE)



  observeEvent(input$loadProjectButton,{
      tryCatch({
        rdsLocation <- choose.dir(caption = "select your project folder and press 'OK'")
        appTwoReload(rdsLocation)
      }, error = function(ex) {
        modalMessager('Error',paste0('Try choosing a file again'))
        dawg<<-ex
      })
  },ignoreInit=TRUE)

}

adjustSequences<-function(){
  for(i in 1:8){
    whichInput=paste0('sequenceName',i)
    whichSlider=paste0('dateSlider',i)
    if(i>totalSequences){
      hide(id = whichInput, anim = TRUE)
      hide(id = whichSlider, anim = TRUE)
    }else{
      showElement(id = whichInput, anim = TRUE)
      showElement(id = whichSlider, anim = TRUE)
    }
  }
  delay(2000,updateSliders())
}



updateSliderDates<-function(){
  minDate<-bioYearStartDate
  maxDate<-bioYearStartDate+364
  for(i in 1:8){
    whichSlider=paste0('dateSlider',i)
    updateSliderInput(session, whichSlider,
      value = c(minDate,maxDate),
      min = minDate,
      max = maxDate
    )
  }
}

sequenceNameChange<-function(sequenceId,sequenceName){
  configOptions[paste0('sequence',sequenceId)][[1]]<<-sequenceName
  saveConfig()
}

sliderChange<-function(sliderId,value){
  if(updatingSliderDates==TRUE){
    return()
  }
  thisYrIdIndex<-which(migtime$id_bioYear==currentIndividual)
  thisPreviousStart<-migtime[thisYrIdIndex,paste0('mig',sliderId,'start')]
  thisPreviousEnd<-migtime[thisYrIdIndex,paste0('mig',sliderId,'end')]
  thisStartDate<-as.Date(value[1])
  thisEndDate<-as.Date(value[2])
  migtime[thisYrIdIndex,paste0('mig',sliderId,'start')]<<-thisStartDate
  migtime[thisYrIdIndex,paste0('mig',sliderId,'end')]<<-thisEndDate
  updateTable('migtime',paste0('mig',sliderId,'start'),paste0('where id_bioYear = "',currentIndividual,'"'),paste0('"',as.character(thisStartDate),'"'))
  updateTable('migtime',paste0('mig',sliderId,'end'),paste0('where id_bioYear = "',currentIndividual,'"'),paste0('"',as.character(thisEndDate),'"'))
  # CHECK TO SEE IF PREVIOUS SLIDER IS WITHIN THIS SEQUENCE.. IF IT IS, PREVIOUS SLIDER TRUMPS AND THIS ONE MOVES FORWARD
  if(sliderId>1){
    previousSliderStart<-migtime[thisYrIdIndex,paste0('mig',sliderId-1,'start')]
    previousSliderEnd<-migtime[thisYrIdIndex,paste0('mig',sliderId-1,'end')]
    # if this start date is not NA and is less than previous slider end, ERROR
    if(thisStartDate<previousSliderEnd){
      thisSliderStartAdjustedDate<-previousSliderEnd
      whichSlider=paste0('dateSlider',sliderId)
      updateSliderInput(session, whichSlider,
        value = c(thisSliderStartAdjustedDate,thisEndDate)
      )
    }
  }
  # CHECK TO SEE IF NEXT SLIDER IS WITHIN THESE BOUNDS.. IF IT IS, THIS ONE TRUMPS AND MOVE THE NEXT ONE FORWARD
  if(sliderId<8){
    nextSliderStart<-migtime[thisYrIdIndex,paste0('mig',sliderId+1,'start')]
    nextSliderEnd<-migtime[thisYrIdIndex,paste0('mig',sliderId+1,'end')]
    if(!is.na(nextSliderStart) && !is.na(nextSliderEnd) && thisEndDate>nextSliderStart){
      nextSliderStartAdjustedDate<-thisEndDate
      if(nextSliderStartAdjustedDate>nextSliderEnd){
        # move to bio year start left change
        nextSliderEnd<-nextSliderStartAdjustedDate
      }
      whichSlider=paste0('dateSlider',sliderId+1)
      updateSliderInput(session, whichSlider,
        value = c(nextSliderStartAdjustedDate,nextSliderEnd)
      )
    }
  }
  updateMapSequencePoints();
}

hasPlotted<<-FALSE

updateSelectMenus<-function(){

  updateSelectInput(session, 'currentIndividualSelector', label = NULL, choices = migtime$id_bioYear)

  observeEvent(input$currentIndividualSelector,{
    currentIndividual<<-input$currentIndividualSelector
    configOptions$currentIndividual<<-currentIndividual
    saveConfig()
    delay(100,toggleAnimal())
    emptyDatesCheck()
  },ignoreInit=TRUE)

  if('currentIndividual'%in%names(configOptions)){
    updateSelectInput(session, 'currentIndividualSelector', label = NULL, choices = migtime$id_bioYear, selected=configOptions$currentIndividual)
    currentIndividual<<-configOptions$currentIndividual
  }else{
    currentIndividual<<-migtime$id_bioYear[1]
  }



  if(!hasPlotted){
    plotInit()
    hasPlotted<<-TRUE
  }
}

updateNotesInput<-function(){
  thisYrIdIndex<-which(migtime$id_bioYear==currentIndividual)
  theseNotes<-migtime[thisYrIdIndex,'notes']
  updateTextInput(session, 'idYearNotesInput', value=theseNotes)
}

toggleAnimal<-function(){  
  mapCurrentIndividual()
  updateSliders()
  saveMigtimeFile()
  updateNotesInput()
}

updatingSliderDates<<-TRUE
updateSliders<-function(){
  updatingSliderDates<<-TRUE
  thisYrIdIndex<<-which(migtime$id_bioYear==currentIndividual)
  thisTempStartYear<-migtime[thisYrIdIndex,'bioYearFull']
  thisTempStartMonth<-as.numeric(strftime(bioYearStartDate, format = "%m"))
  thisTempStartDay<-as.numeric(strftime(bioYearStartDate, format = "%d"))

  thisTempStartDate<-as.Date(paste(thisTempStartYear,thisTempStartMonth,thisTempStartDay,sep='-'))
  thisTempEndDate<-thisTempStartDate+364

  for(i in 1:totalSequences){
    if(i==totalSequences){
      updatingSliderDates<<-FALSE
    }
    whichSlider=paste0('dateSlider',i)
    thisSliderStart<-as.Date(migtime[thisYrIdIndex,paste0('mig',i,'start')])
    thisSliderEnd<-as.Date(migtime[thisYrIdIndex,paste0('mig',i,'end')])
    updateSliderInput(session, whichSlider,
      min=thisTempStartDate,
      max=thisTempEndDate,
      value = c(thisSliderStart,thisSliderEnd)
    )
  }
}


replaceMigtimeDbTable<-function(){
  if(!exists('dbConnection')){
    dbConnection <<- dbConnect(RSQLite::SQLite(), paste0(masterWorkingDirectory,'//workingDb.db'))
  }
  tempMigtime<-migtime
  tempMigtime$mig1start <-as.character(tempMigtime$mig1start)
  tempMigtime$mig1end <-as.character(tempMigtime$mig1end)
  tempMigtime$mig2start <-as.character(tempMigtime$mig2start)
  tempMigtime$mig2end <-as.character(tempMigtime$mig2end)
  tempMigtime$mig3start <-as.character(tempMigtime$mig3start)
  tempMigtime$mig3end <-as.character(tempMigtime$mig3end)
  tempMigtime$mig4start <-as.character(tempMigtime$mig4start)
  tempMigtime$mig4end <-as.character(tempMigtime$mig4end)
  tempMigtime$mig5start <-as.character(tempMigtime$mig5start)
  tempMigtime$mig5end <-as.character(tempMigtime$mig5end)
  tempMigtime$mig6start <-as.character(tempMigtime$mig6start)
  tempMigtime$mig6end <-as.character(tempMigtime$mig6end)
  tempMigtime$mig7start <-as.character(tempMigtime$mig7start)
  tempMigtime$mig7end <-as.character(tempMigtime$mig7end)
  tempMigtime$mig8start <-as.character(tempMigtime$mig8start)
  tempMigtime$mig8end <-as.character(tempMigtime$mig8end)
  dbWriteTable(dbConnection, "migtime", tempMigtime, overwrite=T)

}

forwardBackward<-function(fOrB){
  thisYrIdIndex<-which(migtime$id_bioYear==currentIndividual)
  if(fOrB=='forward'){
    if(thisYrIdIndex==nrow(migtime)){
      return()
    }
    newIndividual<-migtime[thisYrIdIndex+1,'id_bioYear']
  }else{
    if(thisYrIdIndex==1){
      return()
    }
    newIndividual<-migtime[thisYrIdIndex-1,'id_bioYear']
  }
  updateSelectInput(session, 'currentIndividualSelector', selected=newIndividual)
}


saveMigtimeFile<-function(){
  saveTo<-paste0(masterWorkingDirectory,'//migtime.rds')
  saveRDS(migtime,saveTo)
}

colorDropdownItems<-function(datesList){
    runjs("$('div.selectize-control').unbind()")
    runjs("console.log('coloring')")
    runjs(paste0("console.log('",datesList$animalYearsWithNoDates,"')"))
    runjs("console.log('not coloring')")
    runjs(paste0("console.log('",datesList$animalYearsWithDates,"')"))
    jsString<-paste0("$('div.selectize-control').click(function(){
      console.log('coloring!!!')
      $.each(",datesList$animalYearsWithNoDates,",function(i,item){
        $('div[data-value='+item+']').css('color','red')
        })
      $.each(",datesList$animalYearsWithDates,",function(i,item){
        $('div[data-value='+item+']').css('color','black')
        })
      })")
    runjs(jsString)
  }



emptyDatesCheck<-function(){
  emptyDates<-list()
  yesValues<-'['
  noValues<-'['
  for(i in 1:nrow(migtime)){
    migSequencesDefined<-0
    thisIdYear<-migtime[i,'id_bioYear']
    for(j in 1:totalSequences){
      thisSliderStart<-as.Date(migtime[i,paste0('mig',j,'start')])
      thisSliderEnd<-as.Date(migtime[i,paste0('mig',j,'end')])
      if(thisSliderStart!=thisSliderEnd){
        migSequencesDefined<-migSequencesDefined+1
      }
    }
    if(migSequencesDefined>0){
      if(nchar(yesValues)==1){
        yesValues<-paste0(yesValues,'"',thisIdYear,'"')
      }else{
        yesValues<-paste0(yesValues,',','"',thisIdYear,'"')
      }
    }else{
      if(nchar(noValues)==1){
        noValues<-paste0(noValues,'"',thisIdYear,'"')
      }else{
        noValues<-paste0(noValues,',','"',thisIdYear,'"')
      }

    }
  }
  yesValues<-paste0(yesValues,']')
  noValues<-paste0(noValues,']')
  emptyDates$animalYearsWithDates<-yesValues
  emptyDates$animalYearsWithNoDates<-noValues
  delay(1000,colorDropdownItems(emptyDates))
}

exportMigtimeTable<-function(){
  if(!exists('migtime')){
    modalMessager('Error','You have not yet built the migtime table. Click the "configuration parameters button to get started"')
    return()
  }
  migtimeToExport<-migtime
  for(i in 1:nrow(migtimeToExport)){
    for(j in 1:8){
      thisMigStartName<-paste0('mig',j,'start')
      thisMigEndName<-paste0('mig',j,'end')
      thisMigStart<-migtimeToExport[i,thisMigStartName]
      thisMigEnd<-migtimeToExport[i,thisMigEndName]
      if(thisMigStart==thisMigEnd){
        migtimeToExport[i,thisMigStartName]<-''
        migtimeToExport[i,thisMigEndName]<-''
      }
    }
  }

  fileExportFolder<-paste0(masterWorkingDirectory,'\\EXPORTS')
  if(dir.exists(fileExportFolder)==FALSE){
    dir.create(fileExportFolder)
  }

  fileExportName<<-'migtimeExport'

  if(file.exists(paste0(fileExportFolder,'\\',fileExportName,'.csv'))){
    time<-Sys.time()
    time<-gsub(" ", "", time, fixed = TRUE)
    time<-gsub("-", "", time, fixed = TRUE)
    time<-gsub(":", "", time, fixed = TRUE)
    fileExportName<<-paste0(fileExportName,'_',time)
  }

  write.csv(migtimeToExport,paste0(fileExportFolder,'\\',fileExportName,'.csv'),row.names=F)
  modalMessager('Success',paste0('File was exported to EXPORTS subfolder in project directory ','with a filename of ',fileExportName,'.csv'))

}
