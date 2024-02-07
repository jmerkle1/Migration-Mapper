averagingMethodOne<<-'mean'
averagingMethodTwo<<-'mean'
shouldAverage<<-FALSE

app3_init<-function(input, output, session){

  input<<-input
  output<<-output
  session<<-session

  exportShapes<<-'Yes, export Shapefiles as well'

  whichAppIsRunning<<-'app3'

  observeEvent(input$changeAppsButton, {
    changeToOtherApp()
  },ignoreInit=TRUE)

  observeEvent(input$closeMappButton, {
    closeApp()
  },ignoreInit=TRUE)


  observeEvent(input$shouldAverageSelector,{
      if(input$shouldAverageSelector=='no do not create sequences'){
        shouldAverage<<-FALSE
        hideElement('averagingMethodRow')
      }else{
        shouldAverage<<-TRUE
        showElement('averagingMethodRow')
      }
  },ignoreInit=TRUE)



  observeEvent(input$exportShapesSelector,{
      exportShapes<<-input$exportShapesSelector
  },ignoreInit=TRUE)


  observeEvent(input$averageMethodSelectorOne,{
      averagingMethodOne<<-input$averageMethodSelectorOne
  },ignoreInit=TRUE)

  observeEvent(input$averageMethodSelectorTwo,{
      averagingMethodTwo<<-input$averageMethodSelectorTwo
  },ignoreInit=TRUE)


  observeEvent(input$clearSequencesButton,{
      importedDatasetMaster@data$mig<-''
  },ignoreInit=TRUE)


  volumes<<-getVolumes()()
  shinyDirChoose(input, "loadProjectButton", roots=volumes, filetypes = NULL,allowDirCreate=FALSE)
  observeEvent(input$loadProjectButton,{
      thisSelectedFolder<-getFolderPathFromShinyDirChoose(volumes,input$loadProjectButton)
      if(!is.null(thisSelectedFolder)){
        appThreeReload(thisSelectedFolder)
      }      
  },ignoreInit=TRUE)


  observeEvent(input$calcDefinedSequencesButton,{
    calculateDefinedSequences()
  },ignoreInit=TRUE)

  observeEvent(input$calcInBetweenSequencesButton,{
    calculateInBetweenSequences()
  },ignoreInit=TRUE)

  observeEvent(input$calcCustomSequencesButton,{
    calculateCustomSequences()
  },ignoreInit=TRUE)

  observeEvent(input$calcOtherSequencesButton,{
    toggleModal(session,'customSeasonModal',toggle='open')
    updateSelectInput(session, "typeOfDatesSelector",selected = ' ')
    updateDateInput(session,'startDateSelector',value='2022-01-01')
    updateDateInput(session,'endDateSelector',value='2022-01-01')
  },ignoreInit=TRUE)

  observeEvent(input$customSeasonTextInput,{
    print(input$customSeasonTextInput)
  },ignoreInit=TRUE)


  observeEvent(input$typeOfDatesSelector,{
    thisSelectedValue<-input$typeOfDatesSelector
    if(thisSelectedValue==' '){
      hideCustomSeasonsInputs()
      hideExistingSeasonsInputs()
    }
    if(thisSelectedValue=='Force season to be between migration dates selected in App 2'){
      showExistingSeasonsInputs()
      hideCustomSeasonsInputs()
    }
    if(thisSelectedValue=='Define custom start/end dates for season'){
      showCustomSeasonsInputs()
      hideExistingSeasonsInputs()
    }
  },ignoreInit=TRUE)

}

hideExistingSeasonsInputs<-function(){
  hideElement('existingSeasonsRow')
}

hideCustomSeasonsInputs<-function(){
  hideElement('customSeasonsRow')
}

showExistingSeasonsInputs<-function(){
  updateTextInput(session, 'customSeasonTextInput', value = '')
  updateTextInput(session, 'definedSeasonTextInput', value = '')
  showElement('existingSeasonsRow')
  delay(100,updateSeasonSelectMenus())
}

showCustomSeasonsInputs<-function(){
  updateTextInput(session, 'customSeasonTextInput', value = '')
  updateTextInput(session, 'definedSeasonTextInput', value = '')
  showElement('customSeasonsRow')
  delay(100,updateSeasonSelectMenus())
}

calcSeasonAverages<-function(){

  seasonDetails<<-list()

  bioYearStartDateJulian<-yday(configOptions$bioYearStartDate)

  for(i in 1:configOptions$totalSequences){
    thisMigStart<-paste0('mig',i,'start')
    thisMigEnd<-paste0('mig',i,'end')
    theseObservations<-migtime[which(migtime[thisMigStart]!=migtime[thisMigEnd]),]

    theseStartDates<-theseObservations[thisMigStart]
    theseEndDates<-theseObservations[thisMigEnd]

    startDateAverages<-getDateAverages(theseStartDates[,1])
    thisMeanStartDate<-format(startDateAverages[1], format="%m-%d")
    thisQ95StartDate<-format(startDateAverages[2], format="%m-%d")
    thisQ05StartDate<-format(startDateAverages[3], format="%m-%d")

    endDateAverages<-getDateAverages(theseEndDates[,1])
    thisMeanEndDate<-format(endDateAverages[1], format="%m-%d")
    thisQ95EndDate<-format(endDateAverages[2], format="%m-%d")
    thisQ05EndDate<-format(endDateAverages[3], format="%m-%d")


    seasonDetails[[paste0('mig',i)]]$start$mean<<-thisMeanStartDate
    seasonDetails[[paste0('mig',i)]]$end$mean<<-thisMeanEndDate
    seasonDetails[[paste0('mig',i)]]$start$q95<<-thisQ95StartDate
    seasonDetails[[paste0('mig',i)]]$end$q95<<-thisQ95EndDate
    seasonDetails[[paste0('mig',i)]]$start$q05<<-thisQ05StartDate
    seasonDetails[[paste0('mig',i)]]$end$q05<<-thisQ05EndDate
    seasonDetails[[paste0('mig',i)]]$totalObs<<-nrow(theseObservations)
    seasonDetails[[paste0('mig',i,'start')]]$mean<<-thisMeanStartDate
    seasonDetails[[paste0('mig',i,'end')]]$mean<<-thisMeanEndDate
    seasonDetails[[paste0('mig',i,'start')]]$q95<<-thisQ95StartDate
    seasonDetails[[paste0('mig',i,'end')]]$q95<<-thisQ95EndDate
    seasonDetails[[paste0('mig',i,'start')]]$q05<<-thisQ05StartDate
    seasonDetails[[paste0('mig',i,'end')]]$q05<<-thisQ05EndDate
  }

  bioYears<-unique(migtime$bioYearFull)

  for(i in 1:length(bioYears)){
    thisBioYear<-bioYears[i]
    for(j in 1:configOptions$totalSequences){
      thisMigStart<-paste0('mig',j,'start')
      thisMigEnd<-paste0('mig',j,'end')
      theseObservations<-migtime[which(migtime[thisMigStart]!=migtime[thisMigEnd] & migtime$bioYearFull==thisBioYear),]
      if(nrow(theseObservations)==0){
        seasonDetails[[thisBioYear]][[paste0('mig',j)]]$start$mean<<-999
        seasonDetails[[thisBioYear]][[paste0('mig',j)]]$end$mean<<-999
        seasonDetails[[thisBioYear]][[paste0('mig',j)]]$start$q95<<-999
        seasonDetails[[thisBioYear]][[paste0('mig',j)]]$end$q95<<-999
        seasonDetails[[thisBioYear]][[paste0('mig',j)]]$start$q05<<-999
        seasonDetails[[thisBioYear]][[paste0('mig',j)]]$end$q05<<-999
        seasonDetails[[thisBioYear]][[paste0('mig',j)]]$totalObs<<-nrow(theseObservations)
        seasonDetails[[thisBioYear]][[paste0('mig',j,'start')]]$mean<<-999
        seasonDetails[[thisBioYear]][[paste0('mig',j,'end')]]$mean<<-999
        seasonDetails[[thisBioYear]][[paste0('mig',j,'start')]]$q95<<-999
        seasonDetails[[thisBioYear]][[paste0('mig',j,'end')]]$q95<<-999
        seasonDetails[[thisBioYear]][[paste0('mig',j,'start')]]$q05<<-999
        seasonDetails[[thisBioYear]][[paste0('mig',j,'end')]]$q05<<-999
      }else{
        theseStartDates<-theseObservations[thisMigStart]
        theseEndDates<-theseObservations[thisMigEnd]

        startDateAverages<-getDateAverages(theseStartDates[,1])
        thisMeanStartDate<-format(startDateAverages[1], format="%m-%d")
        thisQ95StartDate<-format(startDateAverages[2], format="%m-%d")
        thisQ05StartDate<-format(startDateAverages[3], format="%m-%d")

        endDateAverages<-getDateAverages(theseEndDates[,1])
        thisMeanEndDate<-format(endDateAverages[1], format="%m-%d")
        thisQ95EndDate<-format(endDateAverages[2], format="%m-%d")
        thisQ05EndDate<-format(endDateAverages[3], format="%m-%d")





        seasonDetails[[thisBioYear]][[paste0('mig',j)]]$start$mean<<-thisMeanStartDate
        seasonDetails[[thisBioYear]][[paste0('mig',j)]]$end$mean<<-thisMeanEndDate
        seasonDetails[[thisBioYear]][[paste0('mig',j)]]$start$q95<<-thisQ95StartDate
        seasonDetails[[thisBioYear]][[paste0('mig',j)]]$end$q95<<-thisQ95EndDate
        seasonDetails[[thisBioYear]][[paste0('mig',j)]]$start$q05<<-thisQ05StartDate
        seasonDetails[[thisBioYear]][[paste0('mig',j)]]$end$q05<<-thisQ05EndDate
        seasonDetails[[thisBioYear]][[paste0('mig',j)]]$totalObs<<-nrow(theseObservations)
        seasonDetails[[thisBioYear]][[paste0('mig',j,'start')]]$mean<<-thisMeanStartDate
        seasonDetails[[thisBioYear]][[paste0('mig',j,'end')]]$mean<<-thisMeanEndDate
        seasonDetails[[thisBioYear]][[paste0('mig',j,'start')]]$q95<<-thisQ95StartDate
        seasonDetails[[thisBioYear]][[paste0('mig',j,'end')]]$q95<<-thisQ95EndDate
        seasonDetails[[thisBioYear]][[paste0('mig',j,'start')]]$q05<<-thisQ05StartDate
        seasonDetails[[thisBioYear]][[paste0('mig',j,'end')]]$q05<<-thisQ05EndDate
      }
    }
  }

  renderSeasonsText()
}

updateSeasonSelectMenus<-function(){
  seasons<-seasonDetails$seasonNames
  startEndNames<-list()
  for(i in 1:length(seasons)){
    thisSeason<-seasons[i]
    # startEndNames<-c(seasonsAll,paste0(thisSeason,' start'))
    # startEndNames<-c(seasonsAll,paste0(thisSeason,' end'))
    startEndNames[[paste0(thisSeason,' start')]]<-paste0(seasonDetails$seasonLookup[[thisSeason]],'start')
    startEndNames[[paste0(thisSeason,' end')]]<-paste0(seasonDetails$seasonLookup[[thisSeason]],'end')
  }
  updateSelectInput(session, "startSeasonSelector",
    choices = startEndNames,
    selected = NULL
  )
  updateSelectInput(session, "endSeasonSelector",
    choices = startEndNames,
    selected = NULL
  )
}



renderSeasonsText<-function(){

  seasonDetails$seasonNames<<-c()


  textStringIntro<-paste0("<span>In app 3 you identified ", configOptions$totalSequences," seasons. These seasons were named ")

  textString<-''

  for(i in 1:configOptions$totalSequences){
    thisSequenceName<-configOptions[[paste0('sequence',i)]]
    seasonDetails$seasonLookup[[thisSequenceName]]<<-paste0('mig',i)
    seasonDetails$seasonNames<<-c(seasonDetails$seasonNames,thisSequenceName)
    if(i==configOptions$totalSequences){
      textStringIntro<-paste0(textStringIntro,'and ',thisSequenceName,'.')
    }else if(i==as.numeric(configOptions$totalSequences)-1){
      textStringIntro<-paste0(textStringIntro,thisSequenceName,' ')
    }else{
      textStringIntro<-paste0(textStringIntro,thisSequenceName,', ')
    }

    textStringIntro<-paste0(textStringIntro,'</span><br>')

    textString<-paste0(textString,'<h4>',configOptions[[paste0('sequence',i)]],'</h4>
    <h6>Total Dates From User- ',seasonDetails[[paste0('mig',i)]]$totalObs,'<h6>
    <h6>Mean Start & End Dates- start:',seasonDetails[[paste0('mig',i)]]$start$mean,', end:',seasonDetails[[paste0('mig',i)]]$end$mean,'<h6>
    <h6>95th percentile Start & Dates- start:',seasonDetails[[paste0('mig',i)]]$start$q95,', end:',seasonDetails[[paste0('mig',i)]]$end$q95,'<h6>
    <h6>5th percentile Start & Dates- start:',seasonDetails[[paste0('mig',i)]]$start$q05,', end:',seasonDetails[[paste0('mig',i)]]$end$q05,'<h6>
    ')

  }


  output$seasonsIntroText <- renderUI({
    HTML(textStringIntro)
  })

  output$seasonsDefinedText <- renderUI({
    HTML(textString)
  })

  output$seasonsUndefinedText <- renderUI({
    HTML('<span>You can also export custom seasons which were not selected in app2. Click the button below to define a custom season and export it as a sequence</span>')
  })

  showElement('calcDefinedSequencesButton')
  showElement('calcOtherSequencesButton')


}


getDateAverages<-function(datesVector){
  # convert the bioyear start date to a julian day
  bioYearStartDateJulian<-yday(configOptions$bioYearStartDate)
  # change to julian day
  datesVectorJulian<-yday(datesVector)
  # subtract the julian bioyear start date
  datesJulianRelativized<-datesVectorJulian-bioYearStartDateJulian
  # if any are negative, they need to be +365'd because they're at the end of the bio year
  datesJulianRelativized[datesJulianRelativized < 0]<-datesJulianRelativized[datesJulianRelativized < 0]+365
  # take the mean of this vector and then add the bioJulian days back on
  meanDay<<-mean(datesJulianRelativized)+bioYearStartDateJulian
  quantile95<-quantile(datesJulianRelativized+bioYearStartDateJulian,0.95)
  quantile05<-quantile(datesJulianRelativized+bioYearStartDateJulian,0.05)
  # turn it to a date starting in 2022
  thisAverageDate<-as.Date(meanDay,origin='2022-01-01')
  thisAverageDate95<-as.Date(quantile95,origin='2022-01-01')
  thisAverageDate05<-as.Date(quantile05,origin='2022-01-01')
  return(c(thisAverageDate,thisAverageDate95,thisAverageDate05))
}
