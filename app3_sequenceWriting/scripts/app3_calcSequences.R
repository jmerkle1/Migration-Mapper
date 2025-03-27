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
    thisSequencesRows<<-c()
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
          importedDatasetMaster$id_bioYear==thisIdYr &
          importedDatasetMaster$newMasterDate >= thisMigStart &
          importedDatasetMaster$newMasterDate <= thisMigEnd
        )

        thisSequencesRows<<-c(thisSequencesRows,theseRows)

        theseSequencePoints<-importedDatasetMaster[thisSequencesRows,]

        # -----------------------------
        # -----------------------------
        # DROPPING PROBLEMS AND MORTALITIES BY DEFAULT
        # -----------------------------
        # -----------------------------        
        theseSequencePoints<-theseSequencePoints[which(theseSequencePoints$problem != 1),]
        theseSequencePoints<-theseSequencePoints[which(theseSequencePoints$mortality != 1),]
        theseSequencePoints$mig<-paste0(theseSequencePoints$newUid,'_',theseSequencePoints$bioYear,'_',thisSequenceName)


        sequencesSummary(theseSequencePoints)        
        theseSequencePointsForExport<-theseSequencePoints        
        theseSequencePointsForExport<-theseSequencePointsForExport[,c('newUid','bioYearFull','newMasterDate','mig','lon','lat','x','y')]
        names(theseSequencePointsForExport)<-c('id','bioYearFull','date','id_yrbio_season','lon','lat','x','y')
        theseSequencePointsForExport$date<-as.character(theseSequencePointsForExport$date)
        theseSequencePointsForExport$season<-thisSequenceName
        # theseSequencePointsForExport<-st_as_sf(theseSequencePointsForExport,coords = c("lon", "lat"), crs = configOptions$masterCrs4326)
        theseSequencePointsForExport<-st_as_sf(theseSequencePointsForExport,coords = c("x", "y"), crs = configOptions$masterCrs)
        if(exportShapes!='No, do not export shapefiles'){
          theseSequencePointsForExport$method<-'sliderSelected'
          exportShapeFiles(theseSequencePointsForExport,thisSequenceName)
        }
        theseSequencePoints<-theseSequencePoints[,c('newUid','newMasterDate','mig','lon','lat','x','y')]
        names(theseSequencePoints)<-c('id','date','mig','lon','lat','x','y')        
        # theseSequencePoints<-st_as_sf(theseSequencePoints,coords = c("lon", "lat"), crs = configOptions$masterCrs4326)
        theseSequencePoints<-st_as_sf(theseSequencePoints,coords = c("x", "y"), crs = configOptions$masterCrs)
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
        # thisSequencesRows<<-c(thisSequencesRows,theseRows)
      }
    }
  }

  totalSequences<-length(unique(theseSequencePoints$mig))
  totalUid<-length(unique(theseSequencePoints$id))
  modalMessager('success', paste0('done calculating sequences for your season(s)! This include ',totalSequences,' sequences from ',totalUid,' individuals. See sequences folder or choose another season to calculate more sequences.'))
  loadingScreenToggle('hide','calculating sequences')
}

calculateInBetweenSequences<-function(){  
  
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
  thisSequencesRows<<-c()
  methodsHolder<<-c()
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
        importedDatasetMaster$id_bioYear==thisIdYr &
        importedDatasetMaster$newMasterDate >= thisMigStart &
        importedDatasetMaster$newMasterDate < thisMigEnd
      )
      thisSequencesRows<<-c(thisSequencesRows,theseRows)
      thisDateSelectionType='sliderSelected'      
      theseSelectionMethods<-rep(thisDateSelectionType,length(theseRows))
      methodsHolder<<-c(methodsHolder,theseSelectionMethods)
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
        importedDatasetMaster$id_bioYear==thisIdYr &
        importedDatasetMaster$newMasterDate >= thisSequenceAverageStartDate &
        importedDatasetMaster$newMasterDate <= thisSequenceAverageEndDate
      )

      theseSelectionMethods<-rep(thisDateSelectionType,length(theseRows))
      methodsHolder<<-c(methodsHolder,theseSelectionMethods)
      thisSequencesRows<<-c(thisSequencesRows,theseRows)
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
    theseSequencePoints<-theseSequencePoints[which(theseSequencePoints$problem != 1),]
    theseSequencePoints<-theseSequencePoints[which(theseSequencePoints$mortality != 1),]
    
    theseSequencePoints$mig<-paste0(theseSequencePoints$newUid,'_',theseSequencePoints$bioYear,'_',thisSequenceName)

    sequencesSummary(theseSequencePoints)
    theseSequencePointsForExport<-theseSequencePoints
    # if('originalProjection' %in% names(configOptions)){
    #   theseSequencePointsForExport<-spTransform(theseSequencePointsForExport, CRS(configOptions$originalProjection))
    # }    
    theseSequencePointsForExport<-theseSequencePointsForExport[,c('newUid','bioYearFull','newMasterDate','mig','lon','lat','x','y')]
    names(theseSequencePointsForExport)<-c('id','bioYearFull','date','id_yrbio_season','lon','lat','x','y')
    theseSequencePointsForExport$date<-as.character(theseSequencePointsForExport$date)
    theseSequencePointsForExport$season<-thisSequenceName
    # theseSequencePointsForExport<-st_as_sf(theseSequencePointsForExport,coords = c("lon", "lat"), crs = configOptions$masterCrs4326)
    theseSequencePointsForExport<-st_as_sf(theseSequencePointsForExport,coords = c("x", "y"), crs = configOptions$masterCrs)
    if(exportShapes!='No, do not export shapefiles'){
      exportShapeFiles(theseSequencePointsForExport,thisSequenceName)
    }

    theseSequencePoints<-theseSequencePoints[,c('newUid','newMasterDate','mig','lon','lat','x','y')]
    names(theseSequencePoints)<-c('id','date','mig','lon','lat','x','y')    
    # theseSequencePoints<-st_as_sf(theseSequencePoints,coords = c("lon", "lat"), crs = configOptions$masterCrs4326)
    theseSequencePoints<-st_as_sf(theseSequencePoints,coords = c("x", "y"), crs = configOptions$masterCrs)
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

# calculateInBetweenSequencesForSpanYearWithNoNextPartner<-function(thisSequenceName,thisMigtimeRow,startSeason,endSeason,migStartPartner,migEndPartner){  
#   thisFullYear<-migtime[thisMigtimeRow,'bioYearFull']
#   thisBioYear<-migtime[thisMigtimeRow,'bioYear']
#   thisIdYr<-migtime[thisMigtimeRow,'id_bioYear']
#   thisAid<-migtime[thisMigtimeRow,'newUid']
#   thisMigStart<-migtime[thisMigtimeRow,startSeason]

#   if( grepl( 'start', startSeason, fixed = TRUE) ){
#         startSeasonMatch<-gsub("start", "end", startSeason)
#     }else{
#         startSeasonMatch<-gsub("end", "start", startSeason)
#     }

#     if( grepl( 'start', endSeason, fixed = TRUE) ){
#         endSeasonMatch<-gsub("start", "end", endSeason)
#     }else{
#         endSeasonMatch<-gsub("end", "start", endSeason)
#     }

#     thisMigStartPartner<-migtime[thisMigtimeRow,startSeasonMatch]
#     thisMigEndPartner<-migtime[thisMigtimeRow,endSeasonMatch]

#     # -------------------------------------
#     # -------------------------------------
#     # ---------- first deal with migstart...
#     # does it have a period defined..
#     thisMigStartPartner<-migtime[thisMigtimeRow,migStartPartner]
#     ## if the dates are the same here then we need to use the mean value
#     #  if the dates are the same here then we need to skip since no periods were defined
    
#     if(thisMigStart==thisMigStartPartner){
#       if(shouldAverage==FALSE){
#         return()
#       }
#       thisSequenceAverageStartDate<-seasonDetails[[thisFullYear]][[startSeason]][[averagingMethodOne]]
#       # if there were no start dates for this year, we'll use the average start for all years
#       if(thisSequenceAverageStartDate==999){
#         thisSequenceAverageStartDate<-seasonDetails[[startSeason]][[averagingMethodOne]]
#       }
#       # ---------------------------------
#       # ---------------------------------
#       # ---------if still 999 need to do something else------------
#       if(thisSequenceAverageStartDate==999){
#         stop()
#       }
#       thisMigStart<-paste0(thisFullYear,'-',thisSequenceAverageStartDate)       
#       thisDateSelectionType='averaged'      
#     }else{
#       thisDateSelectionType='sliderSelected'      
#     }

#     nextYearBioYearStart<-configOptions$bioYearStartDate
#     year(nextYearBioYearStart)<-as.numeric(thisFullYear)+1
#     thisMigEnd<-nextYearBioYearStart-1

#     print('mig start and end')
#       print(thisMigStart)
#       print(thisMigEnd)

#     theseRows<-which(
#         importedDatasetMaster$newUid==thisAid &
#         importedDatasetMaster$newMasterDate >= thisMigStart &
#         importedDatasetMaster$newMasterDate < thisMigEnd
#       )
#     if(length(theseRows)>0){
#       tempDToAdd<-importedDatasetMaster[theseRows,]
#       # when spanning two years.. I used to use a combined bioyear_bioyear+1        
#       tempDToAdd$bioYear<-paste0(thisBioYear,'_',as.numeric(thisBioYear)+1)
#       # changing back to just the first bioyear
#       # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!        
#       tempDToAdd$method<-thisDateSelectionType
#       tempDForSpan<<-rbind(tempDForSpan,tempDToAdd)
#     }    
#     thisSequencesRows<<-c(thisSequencesRows,theseRows)
# }


calculateInBetweenSequencesForSpanYearWithNoPrevPartner<-function(thisSequenceName,thisMigtimeRow,endSeason,migEndPartner){  
  thisFullYear<-migtime[thisMigtimeRow,'bioYearFull']
  thisBioYear<-migtime[thisMigtimeRow,'bioYear']
  thisIdYr<-migtime[thisMigtimeRow,'id_bioYear']
  thisAid<-migtime[thisMigtimeRow,'newUid']
  
  thisMigEnd<-migtime[thisMigtimeRow,endSeason]
  thisMigEndPartner<-migtime[thisMigtimeRow,migEndPartner]

  # does it have a period defined..    
  #  if the dates are the same here then we need to skip since no periods were defined
  thisDateSelectionType='averaged'    
  if(thisMigEnd==thisMigEndPartner){
    if(shouldAverage==FALSE){             
      return()
    }
    thisSequenceAverageStartDate<-seasonDetails[[thisFullYear]][[endSeason]][[averagingMethodOne]]
    # if there were no start dates for this year, we'll use the average start for all years
    if(thisSequenceAverageStartDate==999){
      thisSequenceAverageStartDate<-seasonDetails[[endSeason]][[averagingMethodOne]]
    }
    # ---------------------------------
    # ---------------------------------
    # ---------if still 999 need to do something else------------
    if(thisSequenceAverageStartDate==999){
      stop()
    }
    thisMigEnd<-paste0(thisFullYear,'-',thisSequenceAverageStartDate)   
    thisDateSelectionType='sliderSelected'    
  }

  # this mig start will just be the first day of this bio year  
  thisMigStart<-configOptions$bioYearStartDate
  year(thisMigStart)<-as.numeric(thisFullYear)  

  theseRows<-which(
      importedDatasetMaster$newUid==thisAid &
      importedDatasetMaster$newMasterDate >= thisMigStart &
      importedDatasetMaster$newMasterDate < thisMigEnd
    )
  if(length(theseRows)>0){
    tempDToAdd<-importedDatasetMaster[theseRows,]    
    tempDToAdd$bioYear<-paste0(as.numeric(thisBioYear)-1,'_',thisBioYear)    
    tempDToAdd$method<-thisDateSelectionType
    tempDForSpan<<-rbind(tempDForSpan,tempDToAdd)
  }    
  thisSequencesRows<<-c(thisSequencesRows,theseRows)
}

calculateInBetweenSequencesForSpanYearWithNoNextPartner<-function(thisSequenceName,thisMigtimeRow,thisMigStart,migStartPartner,startSeason){  
  thisFullYear<-migtime[thisMigtimeRow,'bioYearFull']
  thisBioYear<-migtime[thisMigtimeRow,'bioYear']
  thisIdYr<-migtime[thisMigtimeRow,'id_bioYear']
  thisAid<-migtime[thisMigtimeRow,'newUid']

  # does it have a period defined..    
  #  if the dates are the same here then we need to skip since no periods were defined
  thisDateSelectionType='averaged'    
  
  # -------------------------------------
  # -------------------------------------
  # ---------- first deal with migstart...
  # does it have a period defined..
  thisMigStartPartner<-migtime[thisMigtimeRow,migStartPartner]
  ## if the dates are the same here then we need to use the mean value  
  if(thisMigStart==thisMigStartPartner){
    if(shouldAverage==FALSE){             
      # next
      return()
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
    thisMigStart<-as.Date(paste0(thisFullYear,'-',thisSequenceAverageStartDate))
  }

  # this mig start will just be the first day of this bio year  
  thisMigEnd<-configOptions$bioYearStartDate
  year(thisMigStart)<-as.numeric(thisFullYear)  
  year(thisMigEnd)<-as.numeric(thisFullYear)  
  

  thisStartJulian<-yday(as.Date(thisMigStart))
  thisEndJulian<-yday(as.Date(thisMigEnd))
  
  if(thisEndJulian>thisStartJulian){      
    year(thisMigStart)<-as.numeric(thisFullYear)+1
    year(thisMigEnd)<-as.numeric(thisFullYear)+1
  }else{    
    year(thisMigEnd)<-as.numeric(thisFullYear)+1
  }

  print('thisMigStart no next partner 485')
  print(thisMigStart)
  print('thisMigEnd')
  print(thisMigEnd)
  print('               ')
  print('               ')
  print('               ')


  theseRows<-which(
      importedDatasetMaster$newUid==thisAid &
      importedDatasetMaster$newMasterDate >= thisMigStart &
      importedDatasetMaster$newMasterDate < thisMigEnd
    )
  if(length(theseRows)>0){
    tempDToAdd<-importedDatasetMaster[theseRows,]    
    tempDToAdd$bioYear<-paste0(thisBioYear,'_',as.numeric(thisBioYear)+1)    
    tempDToAdd$method<-thisDateSelectionType
    tempDForSpan<<-rbind(tempDForSpan,tempDToAdd)
  }    
  thisSequencesRows<<-c(thisSequencesRows,theseRows)
}



calculateInBetweenSequencesForSpanYear<-function(thisSequenceName){
  tempDForSpan<<-importedDatasetMaster[0,]

  toggleModal(session,'customSeasonModal',toggle='close')
  loadingScreenToggle('show','calculating sequences')

  thisSequenceName<-str_replace_all(thisSequenceName, "[^[:alnum:]]", "")


  # this is to keep track of those row ids that span a bio year  
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
  thisSequencesRows<<-c()
  methodsHolder<<-c()
  for(j in 1:nrow(migtime)){
  # for(j in 1:67){
      thisFullYear<-migtime[j,'bioYearFull']
      thisBioYear<-migtime[j,'bioYear']
      thisIdYr<-migtime[j,'id_bioYear']
      thisAid<-migtime[j,'newUid']
      thisMigStart<-migtime[j,startSeason]

      prevAid<-migtime[j-1,'newUid']
      nextAid<-migtime[j+1,'newUid']      
      if(length(prevAid)==0 || is.na(prevAid)){
        prevAid=-99999999
      }
      if(length(nextAid)==0 || is.na(nextAid)){
        nextAid=-99999999
      }

      if(is.na(thisFullYear)){
        thisFullYear=-99999999999
      }

      

      # if(!thisAid%in%c('UintaMD-015','UintaMD-003','Uinta-024')){
      #   next
      # }

      print('  ****  ')
      print(thisAid)
      print(thisFullYear)
      print('    ')
      
      
      # if the previous bio year for this animal is not this year minus 1
      # or the next aid is not this aid      
      # then  need to create a small segment for last winter
      prevBioYear<-as.numeric(migtime[j-1,'bioYearFull'])
      if(length(prevBioYear)==0 || is.na(prevBioYear)){
        prevBioYear=-99999999
      }      
      
      if(prevBioYear!=as.numeric(thisFullYear)-1){
        calculateInBetweenSequencesForSpanYearWithNoPrevPartner(thisSequenceName,j,endSeason,migEndPartner)
        print('!!!!!!!! no prev partner')   
        # next     
      }



      
      # if the next next bio year for this animal is not the next year 
      # or the next aid is not this aid      
      # then  need to set last day of this current bio year
      nextBioYear<-as.numeric(migtime[j+1,'bioYearFull'])
      if(nextBioYear!=as.numeric(thisFullYear)+1 || nextAid!=thisAid){
        print('****************** no next partner')   
        calculateInBetweenSequencesForSpanYearWithNoNextPartner(thisSequenceName,j,thisMigStart,migStartPartner,startSeason)
        next
        # nextYearBioYearStart<-configOptions$bioYearStartDate
        # year(nextYearBioYearStart)<-as.numeric(thisFullYear)+1
        # thisMigEnd<-nextYearBioYearStart-1
      }else{
        thisMigEnd<-migtime[j+1,endSeason]
      }

      # -------------------------------------
      # -------------------------------------
      # ---------- now deal with migstart...
      # does it have a period defined..
      wasStartAveraged=FALSE
      thisMigStartPartner<-migtime[j,migStartPartner]
      ## if the dates are the same here then we need to use the mean value
      #  if the dates are the same here then we need to skip since no periods were defined
      if(thisMigStart==thisMigStartPartner){
        if(shouldAverage==FALSE){             
          next
        }
        wasStartAveraged=TRUE
        print('going to average migstart... 653!!')
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
      if(is.na(thisMigEndPartner)){
        thisMigEndPartner=-9999999
      }
      # if the dates are the same here then we need to use the mean value
      
      if(thisMigEnd==thisMigEndPartner){
        if(shouldAverage==FALSE){            
          next
        }        

        thisSequenceAverageEndDate<-seasonDetails[[toString(as.numeric(thisFullYear)+1)]][[endSeason]][[averagingMethodOne]]    

        # if there were no start dates for this year, we'll use the average start for all years
        if(thisSequenceAverageEndDate==999){
          thisSequenceAverageEndDate<-seasonDetails[[endSeason]][[averagingMethodTwo]]
          print('averaging end date')

        }
        # ---------------------------------
        # ---------------------------------
        # ---------if still 999 need to do something else------------
        if(thisSequenceAverageEndDate==999){
          stop()
        }

        # ----------------------
        # if the migration end date (JULIAN) is greater than the start
        # then don't need to add a year
        # thisSequenceAverageStartDate<-seasonDetails[[thisFullYear]][[startSeason]][[averagingMethodOne]]
        # thisStartJulian<-yday(as.Date(paste0(thisFullYear,'-',thisSequenceAverageStartDate)))
        thisStartJulian<-yday(as.Date(thisMigStart))
        thisEndJulian<-yday(as.Date(paste0(thisFullYear,'-',thisSequenceAverageEndDate)))        

        if(thisEndJulian>thisStartJulian){
          yearToAdd<-0
          thisMigStart<-as.Date(thisMigStart)          
          if(wasStartAveraged==TRUE){
            thisMigStart<-thisMigStart+365
          }
          # thisMigStart<-thisMigStart+365
          thisMigEnd<-paste0(as.numeric(thisFullYear)+yearToAdd,'-',thisSequenceAverageEndDate)
        }else{
          yearToAdd<-1
          # this should fix issue with overspanning data when averaging across bio years etc
          thisMigEnd<-paste0(as.numeric(thisFullYear)+yearToAdd,'-',thisSequenceAverageEndDate)
        }
        print('avg end date')
        print(thisSequenceAverageEndDate)
        print('this mig end')
        print(thisMigEnd)        
        thisDateSelectionType='averaged'      
      }else{
        thisDateSelectionType='sliderSelected'
      }

      print('thisMigStart 740')
      print(thisMigStart)
      print('thisMigEnd')
      print(thisMigEnd)      
      print('               ')
      print('               ')

      theseRows<-which(
        importedDatasetMaster$newUid==thisAid &
        importedDatasetMaster$newMasterDate >= thisMigStart &
        importedDatasetMaster$newMasterDate < thisMigEnd
      )

      if(length(theseRows)>0){
        tempDToAdd<-importedDatasetMaster[theseRows,]        
        tempDToAdd$bioYear<-paste0(thisBioYear,'_',as.numeric(thisBioYear)+1)           
        tempDToAdd$method<-thisDateSelectionType
        tempDForSpan<<-rbind(tempDForSpan,tempDToAdd)
      }      
      thisSequencesRows<<-c(thisSequencesRows,theseRows)
    }   



  if(length(thisSequencesRows)>1){
    theseSequencePoints<-tempDForSpan    
    theseSequencePoints$mig<-paste0(theseSequencePoints$newUid,'_',theseSequencePoints$bioYear,'_',thisSequenceName)
    
    sequencesSummary(theseSequencePoints)

    thisSequenceName<-thisSequenceName

    # -----------------------------
    # -----------------------------
    # DROPPING PROBLEMS AND MORTALITIES BY DEFAULT
    # -----------------------------
    # -----------------------------    
    theseSequencePoints<-theseSequencePoints[which(theseSequencePoints$problem != 1),]
    theseSequencePoints<-theseSequencePoints[which(theseSequencePoints$mortality != 1),]

    theseSequencePointsForExport<-theseSequencePoints    
    theseSequencePointsForExport<-theseSequencePointsForExport[,c('newUid','bioYearFull','newMasterDate','mig','lon','lat','x','y')]
    names(theseSequencePointsForExport)<-c('id','bioYearFull','date','id_yrbio_season','lon','lat','x','y')
    theseSequencePointsForExport$date<-as.character(theseSequencePointsForExport$date)
    theseSequencePointsForExport$season<-thisSequenceName
    # theseSequencePointsForExport<-st_as_sf(theseSequencePointsForExport,coords = c("lon", "lat"), crs = configOptions$masterCrs4326)
    theseSequencePointsForExport<-st_as_sf(theseSequencePointsForExport,coords = c("x", "y"), crs = configOptions$masterCrs)
    if(exportShapes!='No, do not export shapefiles'){
      exportShapeFiles(theseSequencePointsForExport,thisSequenceName)
    }
    
    theseSequencePoints<-theseSequencePoints[,c('newUid','newMasterDate','mig','lon','lat','x','y')]
    names(theseSequencePoints)<-c('id','date','mig','lon','lat','x','y')
    # theseSequencePoints<-st_as_sf(theseSequencePoints,coords = c("lon", "lat"), crs = configOptions$masterCrs4326)
    theseSequencePoints<-st_as_sf(theseSequencePoints,coords = c("x", "y"), crs = configOptions$masterCrs)
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
  if(!exists('theseSequencePoints')){
      modalMessager('error', paste0('Your selection resulted in no outputs.. please check your data and make sure that unique animals have data that spans to the next year'))
      loadingScreenToggle('hide','calculating sequences')
    }else{
      totalSequences<-length(unique(theseSequencePoints$mig))
      totalUid<-length(unique(theseSequencePoints$id))
      modalMessager('success', paste0('done calculating sequences for your season(s)! This include ',totalSequences,' sequences from ',totalUid,' individuals. See sequences folder or choose another season to calculate more sequences.'))
      loadingScreenToggle('hide','calculating sequences')
    }
  
}


calculateCustomSequences<-function(){
  loadingScreenToggle('show','calculating sequences')

  
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

  seasonStartDate<-input$startDateSelector
  seasonEndDate<-input$endDateSelector

  # seasonStartDate<-as.POSIXlt('01-01-2022', format = "%m-%d-%y")
  # seasonEndDate<-as.POSIXlt('03-15-2022', format = "%m-%d-%y")
  seasonStartDateJ<-yday(seasonStartDate)
  seasonEndDateJ<-yday(seasonEndDate)
  bioYearStartDateJulian<-yday(configOptions$bioYearStartDate)

  doesSequenceSpanBioYears<-FALSE

  if(seasonEndDateJ>bioYearStartDateJulian){
    doesSequenceSpanBioYears<-TRUE
  }

  # seasonStartDate<-as.POSIXlt('01-01-2022', format = "%m-%d-%y")
  # seasonEndDate<-as.POSIXlt('03-15-2022', format = "%m-%d-%y")

  if(seasonEndDateJ>seasonStartDateJ){
    yearToAdd<-0
  }else{
    yearToAdd<-1
    tempDForSpan<<-importedDatasetMaster[0,]
  }



  # if the julian end date comes before the start date then add a year
  # if(seasonEndDateJ<seasonStartDateJ){
  #   doesSequenceSpanBioYears<-TRUE
  #   print('spans bio year!!!!!! 566')
  #   tempDForSpan<<-importedDatasetMaster[0,]
  # }


  #  if the julian start date comes before the bioYearStart
  #  and the end date comes after the bioYear Start
  # if(seasonStartDateJ<bioYearStartDateJulian & seasonEndDateJ>=bioYearStartDateJulian){
  #   doesSequenceSpanBioYears<-TRUE
  #   print('spans bio year!!!!!! 571')
  #   tempDForSpan<<-importedDatasetMaster[0,]
  # }


  if(seasonStartDate==seasonEndDate){
    modalMessager('error','start start date and end date cannot be the same')
    loadingScreenToggle('hide','calculating sequences')
    return()
  }


  seasonStartDate<-format(input$startDateSelector, format="%m-%d")
  seasonEndDate<-format(input$endDateSelector, format="%m-%d")
  thisSequencesRows<<-0
  migrationNameArray<-c()
  for(j in 1:nrow(migtime)){
   
    thisFullYear<-migtime[j,'bioYearFull']
    thisBioYear<-migtime[j,'bioYear']
    thisIdYr<-migtime[j,'id_bioYear']
    thisAid<-migtime[j,'newUid']
    thisSequenceStartDate<-paste0(thisFullYear,'-',seasonStartDate)
    thisSequenceEndDate<-paste0(as.numeric(thisFullYear)+yearToAdd,'-',seasonEndDate)      
   



    

    theseRows<-which(
      importedDatasetMaster$newUid==thisAid &
      importedDatasetMaster$newMasterDate >= thisSequenceStartDate &
      importedDatasetMaster$newMasterDate <= thisSequenceEndDate
    )

    if(length(theseRows)>0){
      thisTempMigNameArray<-vector(mode="character", length=length(theseRows))
      if(yearToAdd==0){
          thisTempMigNameArray[thisTempMigNameArray==""] <- paste0(thisAid,'_',thisBioYear,'_',thisSequenceName)
        }else{
          thisTempMigNameArray[thisTempMigNameArray==""] <- paste0(thisAid,'_',thisBioYear,'_',as.numeric(thisBioYear)+1,'_',thisSequenceName)
        }
      
      migrationNameArray<-c(migrationNameArray,thisTempMigNameArray)
    }

    
    prevAid<-migtime[j-1,'newUid']      
    if(length(prevAid)==0 || is.na(prevAid)){
      prevAid=-99999999
    }

    prevBioYear<-as.numeric(migtime[j-1,'bioYearFull'])
    if(length(prevBioYear)==0 || is.na(prevBioYear)){
      prevBioYear=-99999999
    }
    

    # if this is spanning a bio year    
    if(yearToAdd>0){
      # and the previous animal id is not this one or the previous year in migtime is not this year
      # then need to get the second part of previous years points
      if(thisAid!=prevAid || prevBioYear!=as.numeric(thisFullYear)-1){
        lastSequenceEndDate<-paste0(thisFullYear,'-',seasonEndDate)  
        theseRowsTemp<-which(
          importedDatasetMaster$newUid==thisAid &          
          importedDatasetMaster$newMasterDate <= lastSequenceEndDate
        )  
        if(length(theseRowsTemp)>0){          
          thisTempMigNameArray<-vector(mode="character", length=length(theseRowsTemp))
          thisTempMigNameArray[thisTempMigNameArray==""] <- paste0(thisAid,'_',as.numeric(thisBioYear)-1,'_',thisBioYear,'_',thisSequenceName)
          migrationNameArray<-c(migrationNameArray,thisTempMigNameArray)
          theseRows<-c(theseRows,theseRowsTemp)
        }
      }
    }
    
      
      




    # if(doesSequenceSpanBioYears){
    #   thisSequenceEndDate<-paste0(as.numeric(thisFullYear)+1,'-',seasonEndDate)      
    #   theseRows<-which(
    #     importedDatasetMaster$newUid==thisAid &
    #     importedDatasetMaster$newMasterDate >= thisSequenceStartDate &
    #     importedDatasetMaster$newMasterDate <= thisSequenceEndDate
    #   )
    # }else{
    #   thisSequenceEndDate<-paste0(thisFullYear,'-',seasonEndDate)      
    #   theseRows<-which(
    #     importedDatasetMaster$newUid==thisAid &
    #     importedDatasetMaster$newMasterDate >= thisSequenceStartDate &
    #     importedDatasetMaster$newMasterDate <= thisSequenceEndDate
    #   )
    # }

    # if(length(theseRows>0) & doesSequenceSpanBioYears){
    if(length(theseRows>0) & yearToAdd>0){  
      tempDToAdd<-importedDatasetMaster[theseRows,]
      # this was where there were 2 years for span years      
      # tempDToAdd$bioYear<-thisShortYear
      tempDToAdd$bioYear<-paste0(thisBioYear,'_',as.numeric(thisBioYear)+1)        
      tempDForSpan<<-rbind(tempDForSpan,tempDToAdd)
    }
    thisSequencesRows<<-c(thisSequencesRows,theseRows)
  }

  if(length(thisSequencesRows)>1){
    # if(doesSequenceSpanBioYears){
    if(yearToAdd>0){
      theseSequencePoints<-tempDForSpan
    }else{
      theseSequencePoints<-importedDatasetMaster[thisSequencesRows,]
    }

    # this is causing an error for those that span a bio year
    # theseSequencePoints$mig<-paste0(theseSequencePoints$newUid,'_',theseSequencePoints$bioYear,'_',thisSequenceName)
    theseSequencePoints$mig<-migrationNameArray

    # drop problems and mortalities
    theseSequencePoints<-theseSequencePoints[which(theseSequencePoints$problem != 1),]
    theseSequencePoints<-theseSequencePoints[which(theseSequencePoints$mortality != 1),]    
    
    
    

    sequencesSummary(theseSequencePoints)
    theseSequencePointsForExport<-theseSequencePoints
    theseSequencePointsForExport<-theseSequencePointsForExport[,c('newUid','bioYearFull','newMasterDate','mig','lon','lat','x','y')]
    names(theseSequencePointsForExport)<-c('id','bioYearFull','date','id_yrbio_season','lon','lat','x','y')
    theseSequencePointsForExport$date<-as.character(theseSequencePointsForExport$date)
    theseSequencePointsForExport$season<-thisSequenceName
    # theseSequencePointsForExport<-st_as_sf(theseSequencePointsForExport,coords = c("lon", "lat"), crs = configOptions$masterCrs4326)
    theseSequencePointsForExport<-st_as_sf(theseSequencePointsForExport,coords = c("x", "y"), crs = configOptions$masterCrs)
    if(exportShapes!='No, do not export shapefiles'){
      theseSequencePointsForExport$method<-'custom'
      exportShapeFiles(theseSequencePointsForExport,thisSequenceName)
    }    
    theseSequencePoints<-theseSequencePoints[,c('newUid','newMasterDate','mig','lon','lat','x','y')]
    names(theseSequencePoints)<-c('id','date','mig','lon','lat','x','y')
    # theseSequencePoints<-st_as_sf(theseSequencePoints,coords = c("lon", "lat"), crs = configOptions$masterCrs4326)
    theseSequencePoints<-st_as_sf(theseSequencePoints,coords = c("x", "y"), crs = configOptions$masterCrs)
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
  # 
  # finished
  loadingScreenToggle('hide','calculating sequences')
  totalSequences<-length(unique(theseSequencePoints$mig))
  totalUid<-length(unique(theseSequencePoints$id))
  modalMessager('success', paste0('done calculating sequences for your season(s)! This include ',totalSequences,' sequences from ',totalUid,' individuals. See sequences folder or choose another season to calculate more sequences.'))
  hideCustomSeasonsInputs()
}


sequencesSummary<-function(thisData){
  thisData<<-thisData
  
}

exportShapeFiles<-function(theseSequencePoints,thisSequenceName){




  sequenceShapesDirectory<-paste0(masterWorkingDirectory,'\\sequenceShapefiles')
  if(dir.exists(sequenceShapesDirectory)==FALSE){
    dir.create(sequenceShapesDirectory)
  }

    st_write(theseSequencePoints, sequenceShapesDirectory, paste0(thisSequenceName,'_points'), driver="ESRI Shapefile", quiet=TRUE, append=FALSE, delete_layer = TRUE)  

    linesData<-Points2Lines(theseSequencePoints,'date','id_yrbio_season')
    # linesData<-Points2Lines(theseSequencePoints)
    st_write(linesData, sequenceShapesDirectory, paste0(thisSequenceName,'_lines'), driver="ESRI Shapefile", quiet=TRUE, append=FALSE, delete_layer = TRUE)

}
