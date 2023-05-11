calculateDefinedSequences<-function(){

  loadingScreenToggle('show','calculating sequences')

  sequencesFolder<-paste0(masterWorkingDirectory,'\\sequences')
  if(dir.exists(sequencesFolder)==FALSE){
    dir.create(sequencesFolder)
  }

  for(i in 1:configOptions$totalSequences){
    thisSequenceName<-configOptions[[paste0('sequence',i)]]
    thisSequenceName<-str_replace_all(thisSequenceName, "[^[:alnum:]]", "")
    thisFolder<-paste0(sequencesFolder,'\\',thisSequenceName)
    if(dir.exists(thisFolder)==FALSE){
      dir.create(thisFolder)
    }
    if(length(dir(thisFolder))> 0){
      unlink(paste0(thisFolder,'\\*'))
    }
  }

  for(i in 1:configOptions$totalSequences){
    thisSequenceName<-configOptions[[paste0('sequence',i)]]
    thisSequenceName<-str_replace_all(thisSequenceName, "[^[:alnum:]]", "")
    thisSequencesRows<-c()
    for(j in 1:nrow(migtime)){
      thisFullYear<-migtime[j,'bioYearFull']
      thisIdYr<-migtime[j,'id_bioYear']
      thisMigStart<-paste0('mig',i,'start')
      thisMigEnd<-paste0('mig',i,'end')
      thisMigStart<-migtime[j,thisMigStart]
      thisMigEnd<-migtime[j,thisMigEnd]
      # if there is an actual sequence defined in the migtime table
      if(thisMigStart!=thisMigEnd){
        theseRows<-which(
          importedDatasetMaster@data$id_bioYear==thisIdYr &
          importedDatasetMaster@data$newMasterDate >= thisMigStart &
          importedDatasetMaster@data$newMasterDate <= thisMigEnd
        )

        thisSequencesRows<-c(thisSequencesRows,theseRows)

        theseSequencePoints<-importedDatasetMaster[thisSequencesRows,]

        # -----------------------------
        # -----------------------------
        # DROPPING PROBLEMS AND MORTALITIES BY DEFAULT
        # -----------------------------
        # -----------------------------
        theseSequencePoints<-theseSequencePoints[which(theseSequencePoints@data$problem != 1),]
        theseSequencePoints<-theseSequencePoints[which(theseSequencePoints@data$mortality != 1),]
        theseSequencePoints$mig<-paste0(theseSequencePoints@data$newUid,'_',theseSequencePoints@data$bioYear,'_',thisSequenceName)


        sequencesSummary(theseSequencePoints)
        theseSequencePointsForExport<-theseSequencePoints
        if('originalProjection' %in% names(configOptions)){
          theseSequencePointsForExport<-spTransform(theseSequencePointsForExport, CRS(configOptions$originalProjection))
        }
        theseSequencePointsForExport<-st_as_sf(theseSequencePointsForExport)
        theseSequencePointsForExport<-theseSequencePointsForExport[,c('newUid','bioYearFull','newMasterDate','mig')]
        names(theseSequencePointsForExport)<-c('id','bioYearFull','date','id_yrbio_season','geometry')
        theseSequencePointsForExport$date<-as.character(theseSequencePointsForExport$date)
        theseSequencePointsForExport$season<-thisSequenceName
        if(exportShapes!='No, do not export shapefiles'){
          theseSequencePointsForExport$method<-'sliderSelected'
          exportShapeFiles(theseSequencePointsForExport,thisSequenceName)
        }
        theseSequencePoints<-theseSequencePoints[,c('newUid','newMasterDate','mig')]
        names(theseSequencePoints)<-c('id','date','mig')
        theseSequencePoints<-st_as_sf(theseSequencePoints)
        thisFolder<-paste0(sequencesFolder,'\\',thisSequenceName)
        saveRDS(theseSequencePoints,paste0(thisFolder,'\\',thisSequenceName,'.rds'))



      }else{
        # # if there are no dates selected in the migtime table then we'll use averages for this year start end
        # thisSequenceAverageStartDate<-seasonDetails[[thisFullYear]][[paste0('mig',i)]]$start[[averagingMethod]]
        # thisSequenceAverageEndDate<-seasonDetails[[thisFullYear]][[paste0('mig',i)]]$end[[averagingMethod]]
        # # if there were no start dates for this year, we'll use the average start for all years
        # if(thisSequenceAverageStartDate==999){
        #   thisSequenceAverageStartDate<-seasonDetails[[paste0('mig',i)]]$start[[averagingMethod]]
        # }
        # # if there were no end dates for this year, we'll use the average start for all years
        # if(thisSequenceAverageEndDate==999){
        #   thisSequenceAverageEndDate<-seasonDetails[[paste0('mig',i)]]$end[[averagingMethod]]
        # }
        # thisSequenceAverageStartDate<-paste0(thisFullYear,'-',thisSequenceAverageStartDate)
        # thisSequenceAverageEndDate<-paste0(thisFullYear,'-',thisSequenceAverageEndDate)
        # theseRows<-which(
        #   importedDatasetMaster@data$id_bioYear==thisIdYr &
        #   importedDatasetMaster@data$newMasterDate >= thisSequenceAverageStartDate &
        #   importedDatasetMaster@data$newMasterDate <= thisSequenceAverageEndDate
        # )
        # thisSequencesRows<-c(thisSequencesRows,theseRows)
      }
    }
  }

  totalSequences<-length(unique(theseSequencePoints$mig))
  totalUid<-length(unique(theseSequencePoints$id))
  modalMessager('success', paste0('done calculating sequences for your season(s)! This include ',totalSequences,' sequences from ',totalUid,' individuals. See sequences folder or choose another season to calculate more sequences.'))
  loadingScreenToggle('hide','calculating sequences')
}

calculateInBetweenSequences<-function(){
  # this is to keep track of those row ids that span a bio year
  rowIdsThatSpanABioYear<-c()
  thisSequenceName<-input$definedSeasonTextInput
  if(nchar(thisSequenceName)==0){
    modalMessager('error','you must choose a name for your season')
    return()
  }
  for(i in 1:configOptions$totalSequences){
    definedSequenceName<-configOptions[[paste0('sequence',i)]]
    if(definedSequenceName==thisSequenceName){
      modalMessager('error',paste0('The name ',thisSequenceName,' was already used for a sequence defined in app2. Please choose another'))
      return()
    }
  }




  startSeason<-input$startSeasonSelector
  endSeason<-input$endSeasonSelector
  migNamesList<-names(migtime)


  startSeasonIndex<-which(migNamesList==startSeason)
  endSeasonIndex<-which(migNamesList==endSeason)
  # if the index of the start season is after the index of the end season
  # we'll use the other function and return()
  if(startSeasonIndex>endSeasonIndex){
    calculateInBetweenSequencesForSpanYear(thisSequenceName)
    return()
  }

  if(startSeason==endSeason){
    modalMessager('error','start season and end season cannot be the same')
    return()
  }




  toggleModal(session,'customSeasonModal')
  loadingScreenToggle('show','calculating sequences')








  # these are the rows that will be added to this sequence
  thisSequencesRows<-c()
  methodsHolder<-c()
  for(j in 1:nrow(migtime)){
    thisFullYear<-migtime[j,'bioYearFull']
    thisIdYr<-migtime[j,'id_bioYear']
    thisAid<-migtime[j,'newUid']
    thisMigStart<-migtime[j,startSeason]
    thisMigEnd<-migtime[j,endSeason]

    if( grepl( 'start', startSeason, fixed = TRUE) ){
        startSeasonMatch<-gsub("start", "end", startSeason)
    }else{
        startSeasonMatch<-gsub("end", "start", startSeason)
    }

    if( grepl( 'start', endSeason, fixed = TRUE) ){
        endSeasonMatch<-gsub("start", "end", endSeason)
    }else{
        endSeasonMatch<-gsub("end", "start", endSeason)
    }

    thisMigStartPartner<-migtime[j,startSeasonMatch]
    thisMigEndPartner<-migtime[j,endSeasonMatch]




    # if there is an actual sequence defined in the migtime table and we're not crossing over bio year
    if(thisMigStart!=thisMigStartPartner && thisMigEnd!=thisMigEndPartner){
      theseRows<-which(
        importedDatasetMaster@data$id_bioYear==thisIdYr &
        importedDatasetMaster@data$newMasterDate >= thisMigStart &
        importedDatasetMaster@data$newMasterDate < thisMigEnd
      )
      thisSequencesRows<-c(thisSequencesRows,theseRows)
      thisDateSelectionType='sliderSelected'      
      theseSelectionMethods<-rep(thisDateSelectionType,length(theseRows))
      methodsHolder<-c(methodsHolder,theseSelectionMethods)
    }else{
      if(shouldAverage==FALSE){
        next
      }
      thisDateSelectionType='averaged'      
      # if there are no dates selected in the migtime table then we'll use averages for this year start end
      thisSequenceAverageStartDate<-seasonDetails[[thisFullYear]][[startSeason]][[averagingMethodOne]]
      # if there were no end dates for this year, we'll use the average end for all years
      thisSequenceAverageEndDate<-seasonDetails[[thisFullYear]][[endSeason]][[averagingMethodTwo]]

      # if there were no start dates for this year, we'll use the average start for all years
      if(thisSequenceAverageStartDate==999){
        thisSequenceAverageStartDate<-seasonDetails[[startSeason]][[averagingMethodOne]]
      }
      # if there were no end dates for this year, we'll use the average start for all years
      if(thisSequenceAverageEndDate==999){
        thisSequenceAverageEndDate<-seasonDetails[[endSeason]][[averagingMethodTwo]]
      }

      # ---------------------------------
      # ---------------------------------
      # ---------if still 999 need to do something else------------
      if(thisSequenceAverageStartDate==999 | thisSequenceAverageEndDate==999){
        stop()
      }

      # paste on the current year for the start & end dates
      thisSequenceAverageStartDate<-paste0(thisFullYear,'-',thisSequenceAverageStartDate)
      thisSequenceAverageEndDate<-paste0(thisFullYear,'-',thisSequenceAverageEndDate)

      theseRows<-which(
        importedDatasetMaster@data$id_bioYear==thisIdYr &
        importedDatasetMaster@data$newMasterDate >= thisSequenceAverageStartDate &
        importedDatasetMaster@data$newMasterDate <= thisSequenceAverageEndDate
      )

      theseSelectionMethods<-rep(thisDateSelectionType,length(theseRows))
      methodsHolder<-c(methodsHolder,theseSelectionMethods)
      thisSequencesRows<-c(thisSequencesRows,theseRows)
    }
  }

  if(length(thisSequencesRows)>1){
    # remove anything non alphanumeric from the sequence name so the folders and files don't get weird
    thisSequenceName<-str_replace_all(thisSequenceName, "[^[:alnum:]]", "")
    theseSequencePoints<-importedDatasetMaster[thisSequencesRows,]

    theseSequencePoints$method<-methodsHolder

    # -----------------------------
    # -----------------------------
    # DROPPING PROBLEMS AND MORTALITIES BY DEFAULT
    # -----------------------------
    # -----------------------------
    theseSequencePoints<-theseSequencePoints[which(theseSequencePoints@data$problem != 1),]
    theseSequencePoints<-theseSequencePoints[which(theseSequencePoints@data$mortality != 1),]


    theseSequencePoints@data$mig<-paste0(theseSequencePoints@data$newUid,'_',theseSequencePoints@data$bioYear,'_',thisSequenceName)

    sequencesSummary(theseSequencePoints)
    theseSequencePointsForExport<-theseSequencePoints
    if('originalProjection' %in% names(configOptions)){
      theseSequencePointsForExport<-spTransform(theseSequencePointsForExport, CRS(configOptions$originalProjection))
    }
    theseSequencePointsForExport<-st_as_sf(theseSequencePointsForExport)
    theseSequencePointsForExport<-theseSequencePointsForExport[,c('newUid','bioYearFull','newMasterDate','mig','method')]
    names(theseSequencePointsForExport)<-c('id','bioYearFull','date','id_yrbio_season','method','geometry')
    theseSequencePointsForExport$date<-as.character(theseSequencePointsForExport$date)
    theseSequencePointsForExport$season<-thisSequenceName
    if(exportShapes!='No, do not export shapefiles'){
      exportShapeFiles(theseSequencePointsForExport,thisSequenceName)
    }

    theseSequencePoints<-theseSequencePoints[,c('newUid','newMasterDate','mig')]
    names(theseSequencePoints)<-c('id','date','mig')
    theseSequencePoints<-st_as_sf(theseSequencePoints)
    sequencesFolder<-paste0(masterWorkingDirectory,'\\sequences')
    if(dir.exists(sequencesFolder)==FALSE){
      dir.create(sequencesFolder)
    }
    thisFolder<-paste0(sequencesFolder,'\\',thisSequenceName)
    if(dir.exists(thisFolder)==FALSE){
      dir.create(thisFolder)
    }
    if(length(dir(thisFolder))> 0){
      unlink(paste0(thisFolder,'\\*'))
    }
    saveRDS(theseSequencePoints,paste0(thisFolder,'\\',thisSequenceName,'.rds'))
  }

  totalSequences<-length(unique(theseSequencePoints$mig))
  totalUid<-length(unique(theseSequencePoints$id))
  modalMessager('success', paste0('done calculating sequences for your season(s)! This include ',totalSequences,' sequences from ',totalUid,' individuals. See sequences folder or choose another season to calculate more sequences.'))
  loadingScreenToggle('hide','calculating sequences')
  hideExistingSeasonsInputs()
}

calculateInBetweenSequencesForSpanYear<-function(thisSequenceName){

  tempDForSpan<-importedDatasetMaster[0,]


  toggleModal(session,'customSeasonModal',toggle='close')
  loadingScreenToggle('show','calculating sequences')

  thisSequenceName<-str_replace_all(thisSequenceName, "[^[:alnum:]]", "")


  # this is to keep track of those row ids that span a bio year
  rowIdsThatSpanABioYear<-c()
  startSeason<-input$startSeasonSelector
  endSeason<-input$endSeasonSelector


  migStartIndex<-gregexpr("[[:digit:]]+", startSeason)[[1]][1]
  migStartId<-substr(startSeason,migStartIndex,migStartIndex)
  migStartStartOrEnd<-strsplit(startSeason,migStartId)[[1]][2]
  if(migStartStartOrEnd=='end'){
    migStartPartner<-paste0('mig',migStartId,'start')
  }else{
    migStartPartner<-paste0('mig',migStartId,'end')
  }


  migEndIndex<-gregexpr("[[:digit:]]+", endSeason)[[1]][1]
  migEndId<-substr(endSeason,migEndIndex,migEndIndex)
  migEndStartOrEnd<-strsplit(endSeason,migEndId)[[1]][2]
  if(migEndStartOrEnd=='end'){
    migEndPartner<-paste0('mig',migEndId,'start')
  }else{
    migEndPartner<-paste0('mig',migEndId,'end')
  }

  hideExistingSeasonsInputs()



  # these are the rows that will be added to this sequence
  thisSequencesRows<-c()
  methodsHolder<-c()
  for(j in 1:nrow(migtime)){
      thisFullYear<-migtime[j,'bioYearFull']
      thisBioYear<-migtime[j,'bioYear']
      thisIdYr<-migtime[j,'id_bioYear']
      thisAid<-migtime[j,'newUid']
      thisMigStart<-migtime[j,startSeason]


      # if we're already on the last row of migtime.. there is no end date avaialble so stop
      if(j+1>nrow(migtime)){
        next
      }
      nextAid<-migtime[j+1,'newUid']
      # if the next aid is a different animal then stop
      if(nextAid!=thisAid){
        next
      }
      nextBioYear<-as.numeric(migtime[j+1,'bioYearFull'])
      # if the next next bio year for this animal is not the next year then stop
      if(nextBioYear!=as.numeric(thisFullYear)+1){
        next
      }
      # if we're still here, then the enddate is in the next row of the migtime table
      thisMigEnd<-migtime[j+1,endSeason]



      # -------------------------------------
      # -------------------------------------
      # ---------- first deal with migstart...
      # does it have a period defined..
      thisMigStartPartner<-migtime[j,migStartPartner]
      ## if the dates are the same here then we need to use the mean value
      #  if the dates are the same here then we need to skip since no periods were defined
      if(thisMigStart==thisMigStartPartner){
        if(shouldAverage==FALSE){
          next
        }
        thisSequenceAverageStartDate<-seasonDetails[[thisFullYear]][[startSeason]][[averagingMethodOne]]
        # if there were no start dates for this year, we'll use the average start for all years
        if(thisSequenceAverageStartDate==999){
          thisSequenceAverageStartDate<-seasonDetails[[startSeason]][[averagingMethodOne]]
        }
        # ---------------------------------
        # ---------------------------------
        # ---------if still 999 need to do something else------------
        if(thisSequenceAverageStartDate==999){
          stop()
        }
        thisMigStart<-paste0(thisFullYear,'-',thisSequenceAverageStartDate)       
      }

      # -------------------------------------
      # -------------------------------------
      # ---------- now deal with migend...
      # does it have a period defined..
      thisMigEndPartner<-migtime[j+1,migEndPartner]
      ## if the dates are the same here then we need to use the mean value
      # if the dates are teh same here we need to skip
      if(thisMigEnd==thisMigEndPartner){
        if(shouldAverage==FALSE){
          next
        }

        thisSequenceAverageEndDate<-seasonDetails[[toString(as.numeric(thisFullYear)+1)]][[endSeason]][[averagingMethodOne]]    
        # if there were no start dates for this year, we'll use the average start for all years
        if(thisSequenceAverageEndDate==999){
          thisSequenceAverageEndDate<-seasonDetails[[endSeason]][[averagingMethodTwo]]
        }
        # ---------------------------------
        # ---------------------------------
        # ---------if still 999 need to do something else------------
        if(thisSequenceAverageEndDate==999){
          stop()
        }
        # this should fix issue with overspanning data when averaging across bio years etc
        thisMigEnd<-paste0(as.numeric(thisFullYear)+1,'-',thisSequenceAverageEndDate)
        thisDateSelectionType='averaged'      
      }else{
        thisDateSelectionType='sliderSelected'
      }

    

      theseRows<-which(
        importedDatasetMaster@data$newUid==thisAid &
        importedDatasetMaster@data$newMasterDate >= thisMigStart &
        importedDatasetMaster@data$newMasterDate < thisMigEnd
      )

      if(length(theseRows)>0){
        tempDToAdd<-importedDatasetMaster[theseRows,]

    

        # when spanning two years.. I used to use a combined bioyear_bioyear+1
        tempDToAdd@data$bioYear<-paste0(thisBioYear,'_',as.numeric(thisBioYear)+1)
        # changing back to just the first bioyear
        # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        # tempDToAdd@data$bioYear<-thisBioYear
        tempDToAdd@data$method<-thisDateSelectionType


        tempDForSpan<-rbind(tempDForSpan,tempDToAdd)

        # theseSelectionMethods<-rep(thisDateSelectionType,length(theseRows))      
        # methodsHolder<-c(methodsHolder,theseSelectionMethods)

      }

      
      thisSequencesRows<-c(thisSequencesRows,theseRows)
    }

    


  if(length(thisSequencesRows)>1){
    theseSequencePoints<-tempDForSpan
    theseSequencePoints@data$mig<-paste0(theseSequencePoints@data$newUid,'_',theseSequencePoints@data$bioYear,'_',thisSequenceName)
    
    sequencesSummary(theseSequencePoints)

    thisSequenceName<-thisSequenceName

    # -----------------------------
    # -----------------------------
    # DROPPING PROBLEMS AND MORTALITIES BY DEFAULT
    # -----------------------------
    # -----------------------------


    theseSequencePoints<-theseSequencePoints[which(theseSequencePoints@data$problem != 1),]
    theseSequencePoints<-theseSequencePoints[which(theseSequencePoints@data$mortality != 1),]

    theseSequencePointsForExport<-theseSequencePoints
    if('originalProjection' %in% names(configOptions)){
      theseSequencePointsForExport<-spTransform(theseSequencePointsForExport, CRS(configOptions$originalProjection))
    }
    dawg<<-theseSequencePointsForExport
    theseSequencePointsForExport<-st_as_sf(theseSequencePointsForExport)
    theseSequencePointsForExport<-theseSequencePointsForExport[,c('newUid','bioYearFull','newMasterDate','mig','method')]
    names(theseSequencePointsForExport)<-c('id','bioYearFull','date','id_yrbio_season','method','geometry')
    theseSequencePointsForExport$date<-as.character(theseSequencePointsForExport$date)
    theseSequencePointsForExport$season<-thisSequenceName
    if(exportShapes!='No, do not export shapefiles'){
      exportShapeFiles(theseSequencePointsForExport,thisSequenceName)
    }

    theseSequencePoints<-theseSequencePoints[,c('newUid','newMasterDate','mig')]
    names(theseSequencePoints)<-c('id','date','mig')
    theseSequencePoints<-st_as_sf(theseSequencePoints)
    sequencesFolder<-paste0(masterWorkingDirectory,'\\sequences')
    if(dir.exists(sequencesFolder)==FALSE){
      dir.create(sequencesFolder)
    }
    thisSequenceName<-str_replace_all(thisSequenceName, "[^[:alnum:]]", "")
    thisFolder<-paste0(sequencesFolder,'\\',thisSequenceName)
    if(dir.exists(thisFolder)==FALSE){
      dir.create(thisFolder)
    }
    if(length(dir(thisFolder))> 0){
      unlink(paste0(thisFolder,'\\*'))
    }
    saveRDS(theseSequencePoints,paste0(thisFolder,'\\',thisSequenceName,'.rds'))
  }
  totalSequences<-length(unique(theseSequencePoints$mig))
  totalUid<-length(unique(theseSequencePoints$id))
  modalMessager('success', paste0('done calculating sequences for your season(s)! This include ',totalSequences,' sequences from ',totalUid,' individuals. See sequences folder or choose another season to calculate more sequences.'))
  loadingScreenToggle('hide','calculating sequences')

}


calculateCustomSequences<-function(){

  loadingScreenToggle('show','calculating sequences')

  rowIdsThatSpanABioYear<-c()

  thisSequenceName<-input$customSeasonTextInput
  if(nchar(thisSequenceName)==0){
    modalMessager('error','you must choose a name for your season')
    loadingScreenToggle('hide','calculating sequences')
    return()
  }
  thisSequenceName<-str_replace_all(thisSequenceName, "[^[:alnum:]]", "")

  for(i in 1:configOptions$totalSequences){
    definedSequenceName<-configOptions[[paste0('sequence',i)]]
    if(definedSequenceName==thisSequenceName){
      modalMessager('error',paste0('The name ',thisSequenceName,' was already used for a sequence defined in app2. Please choose another'))
      loadingScreenToggle('hide','calculating sequences')
      return()
    }
  }

  toggleModal(session,'customSeasonModal',toggle='close')

  seasonStartDate<<-input$startDateSelector
  seasonEndDate<<-input$endDateSelector

  seasonStartDateJ<-yday(seasonStartDate)
  seasonEndDateJ<-yday(seasonEndDate)
  bioYearStartDateJulian<-yday(configOptions$bioYearStartDate)



  doesSequenceSpanBioYears<-FALSE

  if(seasonEndDateJ<seasonStartDateJ){
    doesSequenceSpanBioYears<-TRUE
    tempDForSpan<-importedDatasetMaster[0,]
  }

  if(seasonStartDateJ<bioYearStartDateJulian & seasonEndDateJ>=bioYearStartDateJulian){
    doesSequenceSpanBioYears<-TRUE
    tempDForSpan<-importedDatasetMaster[0,]
  }


  if(seasonStartDate==seasonEndDate){
    modalMessager('error','start start date and end date cannot be the same')
    loadingScreenToggle('hide','calculating sequences')
    return()
  }



  seasonStartDate<-format(input$startDateSelector, format="%m-%d")
  seasonEndDate<-format(input$endDateSelector, format="%m-%d")
  thisSequencesRows<-0
  for(j in 1:nrow(migtime)){
    thisFullYear<-migtime[j,'bioYearFull']
    thisShortYear<-migtime[j,'bioYear']
    thisIdYr<-migtime[j,'id_bioYear']
    thisAid<-migtime[j,'newUid']
    thisSequenceStartDate<-paste0(thisFullYear,'-',seasonStartDate)
    if(doesSequenceSpanBioYears){
      thisSequenceEndDate<-paste0(as.numeric(thisFullYear)+1,'-',seasonEndDate)
      theseRows<-which(
        importedDatasetMaster@data$newUid==thisAid &
        importedDatasetMaster@data$newMasterDate >= thisSequenceStartDate &
        importedDatasetMaster@data$newMasterDate <= thisSequenceEndDate
      )
    }else{
      thisSequenceEndDate<-paste0(thisFullYear,'-',seasonEndDate)
      theseRows<-which(
        importedDatasetMaster@data$newUid==thisAid &
        importedDatasetMaster@data$newMasterDate >= thisSequenceStartDate &
        importedDatasetMaster@data$newMasterDate <= thisSequenceEndDate
      )
    }

    if(length(theseRows>0) & doesSequenceSpanBioYears){
      tempDToAdd<-importedDatasetMaster[theseRows,]
      # this was where there were 2 years for span years
      # tempDToAdd@data$bioYear<-paste0(thisShortYear,'_',as.numeric(thisShortYear)+1)
      tempDToAdd@data$bioYear<-thisShortYear

      tempDForSpan<-rbind(tempDForSpan,tempDToAdd)

    }

    thisSequencesRows<-c(thisSequencesRows,theseRows)
  }

  if(length(thisSequencesRows)>1){
    if(doesSequenceSpanBioYears){
      theseSequencePoints<-tempDForSpan
    }else{
      theseSequencePoints<-importedDatasetMaster[thisSequencesRows,]
    }

    theseSequencePoints<-theseSequencePoints[which(theseSequencePoints@data$problem != 1),]
    theseSequencePoints<-theseSequencePoints[which(theseSequencePoints@data$mortality != 1),]
    theseSequencePoints@data$mig<-paste0(theseSequencePoints@data$newUid,'_',theseSequencePoints@data$bioYear,'_',thisSequenceName)

    sequencesSummary(theseSequencePoints)
    theseSequencePointsForExport<-theseSequencePoints
    if('originalProjection' %in% names(configOptions)){
      theseSequencePointsForExport<-spTransform(theseSequencePointsForExport, CRS(configOptions$originalProjection))
    }
    theseSequencePointsForExport<-st_as_sf(theseSequencePointsForExport)
    theseSequencePointsForExport<-theseSequencePointsForExport[,c('newUid','bioYearFull','newMasterDate','mig')]
    names(theseSequencePointsForExport)<-c('id','bioYearFull','date','id_yrbio_season','geometry')
    theseSequencePointsForExport$date<-as.character(theseSequencePointsForExport$date)
    theseSequencePointsForExport$season<-thisSequenceName
    if(exportShapes!='No, do not export shapefiles'){
      theseSequencePointsForExport$method<-'custom'
      exportShapeFiles(theseSequencePointsForExport,thisSequenceName)
    }

    theseSequencePoints<-theseSequencePoints[,c('newUid','newMasterDate','mig')]
    names(theseSequencePoints)<-c('id','date','mig')
    theseSequencePoints<-st_as_sf(theseSequencePoints)
    sequencesFolder<-paste0(masterWorkingDirectory,'\\sequences')
    if(dir.exists(sequencesFolder)==FALSE){
      dir.create(sequencesFolder)
    }
    thisSequenceName<-str_replace_all(thisSequenceName, "[^[:alnum:]]", "")
    thisFolder<-paste0(sequencesFolder,'\\',thisSequenceName)
    if(dir.exists(thisFolder)==FALSE){
      dir.create(thisFolder)
    }
    if(length(dir(thisFolder))> 0){
      unlink(paste0(thisFolder,'\\*'))
    }
    saveRDS(theseSequencePoints,paste0(thisFolder,'\\',thisSequenceName,'.rds'))
  }
  loadingScreenToggle('hide','calculating sequences')
  totalSequences<-length(unique(theseSequencePoints$mig))
  totalUid<-length(unique(theseSequencePoints$id))
  modalMessager('success', paste0('done calculating sequences for your season(s)! This include ',totalSequences,' sequences from ',totalUid,' individuals. See sequences folder or choose another season to calculate more sequences.'))
  hideCustomSeasonsInputs()
}


sequencesSummary<-function(thisData){
  thisData<<-thisData
  print('--total rows in sequence')
  print(dim(thisData))
  print('--total aids in data')
  print(unique(thisData@data$newUid))
  print('--min date')
  print(min(thisData@data$newMasterDate))
  print('--max date')
  print(max(thisData@data$newMasterDate))
  print('-- total migs')
  print(unique(thisData@data$mig))
}

exportShapeFiles<-function(theseSequencePoints,thisSequenceName){




  sequenceShapesDirectory<-paste0(masterWorkingDirectory,'\\sequenceShapefiles')
  if(dir.exists(sequenceShapesDirectory)==FALSE){
    dir.create(sequenceShapesDirectory)
  }



  st_write(theseSequencePoints, sequenceShapesDirectory, paste0(thisSequenceName,'_points'), driver="ESRI Shapefile", quiet=TRUE, append=FALSE, delete_layer = TRUE)  

    u <- unique(theseSequencePoints$id_yrbio_season)  # d is sf df, and id_yrbio_season is a column representing some season or id

    lns <- do.call(c, lapply(1:length(u), function(e){
      return(st_cast(st_combine(theseSequencePoints[theseSequencePoints$id_yrbio_season == u[e],]), "LINESTRING"))
    }))

    lns <- data.frame(id_yrbio_season=u,
                      firstdate=do.call(c, lapply(u, function(e){min(theseSequencePoints$date[theseSequencePoints$id_yrbio_season==e], na.rm=TRUE)})),
                      lastdate=do.call(c, lapply(u, function(e){max(theseSequencePoints$date[theseSequencePoints$id_yrbio_season==e], na.rm=TRUE)})),
                      method=do.call(c, lapply(u, function(e){max(theseSequencePoints$method[theseSequencePoints$id_yrbio_season==e], na.rm=TRUE)})),
                      geometry=lns)

  lns <- st_as_sf(lns, sf_column_name = "geometry")

  lns

  st_write(lns, sequenceShapesDirectory, paste0(thisSequenceName,'_lines'), driver="ESRI Shapefile", quiet=TRUE, append=FALSE, delete_layer = TRUE)



}
