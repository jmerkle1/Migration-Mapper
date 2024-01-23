movementParamsProcessing<-function(){

  loadingScreenToggle('show','checking for errors and calculating movement parameters')

  progressIndicator('checking for duplicates','start')
  dupCheck<-paste0(importedDatasetMaster$newUid,importedDatasetMaster$newMasterDate)
  areDupd<-duplicated(dupCheck)
  if(any(areDupd)){
    importedDatasetMaster <<- importedDatasetMaster[!duplicated(dupCheck),]
  }
  progressIndicator('calculating duplicates','stop')

      
          

    progressIndicator('calculating burst','start')
    #REORDER DATA AS PER BURST REQUIREMENTS
    importedDatasetMaster<<-importedDatasetMaster[order(importedDatasetMaster$newUid, importedDatasetMaster$newMasterDate),]    
    importedDatasetMaster$burst<<-CalcBurst(importedDatasetMaster,T,'newUid','newMasterDate',10800)
    progressIndicator('checking for burst','stop')




    progressIndicator('calculating date parameters','start')
    # importedDatasetMaster@data$month <<- as.numeric(strftime(importedDatasetMaster@data$newMasterDate, format = "%m", tz = selectedTimezone))
    # importedDatasetMaster@data$day <<- as.numeric(strftime(importedDatasetMaster@data$newMasterDate, format = "%d", tz = selectedTimezone))
    # importedDatasetMaster@data$year <<- as.numeric(strftime(importedDatasetMaster@data$newMasterDate, format = "%Y", tz = selectedTimezone))
    importedDatasetMaster$month <<- as.numeric(strftime(importedDatasetMaster$newMasterDate, format = "%m", tz = selectedTimezone))
    importedDatasetMaster$day <<- as.numeric(strftime(importedDatasetMaster$newMasterDate, format = "%d", tz = selectedTimezone))
    importedDatasetMaster$year <<- as.numeric(strftime(importedDatasetMaster$newMasterDate, format = "%Y", tz = selectedTimezone))    
    importedDatasetMaster$jul <<- as.numeric(strftime(importedDatasetMaster$newMasterDate, format = "%j", tz = selectedTimezone))    
    importedDatasetMaster$id_yr <<- paste(importedDatasetMaster$newUid, importedDatasetMaster$year, sep="_")
    progressIndicator('calculating date parameters','stop')
    

    progressIndicator('calculating nsds','start')
    idYrs<-unique(importedDatasetMaster$id_yr)

    importedDatasetMaster$nsdYear<<-NA
    importedDatasetMaster$displacementYear<<-NA
    importedDatasetMaster$nsdOverall<<-NA
    importedDatasetMaster$displacementOverall<<-NA

    for(i in 1:length(idYrs)){      
      temp<- importedDatasetMaster[importedDatasetMaster$id_yr==idYrs[i],]
      importedDatasetMaster[importedDatasetMaster$id_yr==idYrs[i],"nsdYear"]<<- (sqrt((mean(temp$x[1:20])-temp$x)^2 + (mean(temp$y[1:20])-temp$y)^2)^2)/1000000
      importedDatasetMaster[importedDatasetMaster$id_yr==idYrs[i],"displacementYear"]<<- (sqrt((mean(temp$x[1:20])-temp$x)^2 + (mean(temp$y[1:20])-temp$y)^2))/1000
    }
    aids<-unique(importedDatasetMaster$newUid)
    for(i in 1:length(aids)){      
      temp<-importedDatasetMaster[importedDatasetMaster$newUid==aids[i],]
      importedDatasetMaster[importedDatasetMaster$newUid==aids[i],"nsdOverall"]<<- (sqrt((mean(temp$x[1:20])-temp$x)^2 + (mean(temp$y[1:20])-temp$y)^2)^2)/1000000
      importedDatasetMaster[importedDatasetMaster$newUid==aids[i],"displacementOverall"]<<- (sqrt((mean(temp$x[1:20])-temp$x)^2 + (mean(temp$y[1:20])-temp$y)^2))/1000
    }

    progressIndicator('calculating nsds','stop')

    progressIndicator('calculating movement parameters','start')    
    tempx<-importedDatasetMaster$x
    tempy<-importedDatasetMaster$y
    templat<-importedDatasetMaster$lat
    templon<-importedDatasetMaster$lon
    importedDatasetMaster<<-CalcMovParams(st_as_sf(importedDatasetMaster,coords = c("x", "y"), crs = configOptions$masterCrs),'newUid','newMasterDate')    
    # needed this for graphing
    importedDatasetMaster$fixRateHours<<-importedDatasetMaster$dt/3600
    importedDatasetMaster$x<<-tempx
    importedDatasetMaster$y<<-tempy
    importedDatasetMaster$lat<<-templat
    importedDatasetMaster$lon<<-templon

    print(names(importedDatasetMaster))
    
    progressIndicator('calculating movement parameters','stop')


    findProblemPoints()
  }

findProblemPoints<-function(){

  loadingScreenToggle('show','checking for errors and calculating movement parameters')

  progressIndicator('finding problem points','start')
  importedDatasetMaster$problem<<-0

  probPoints<-FindProblemPts(st_as_sf(importedDatasetMaster,coords = c("x", "y"), crs = configOptions$masterCrs),date_name='newMasterDate',id_name='newUid',speedlim=configOptions$maxSpeedParameter)
  if(any(probPoints)){
    importedDatasetMaster[probPoints,'problem']<<-1
  }  
  print(names(importedDatasetMaster))
  progressIndicator('finding problem points','stop')

  
  checkForAllNaSpeedsEtc()
}

checkForAllNaSpeedsEtc<-function(){
  
  idYrs<-unique(importedDatasetMaster$id_yr)
  for(i in 1:length(idYrs)){     
      temp<- importedDatasetMaster[importedDatasetMaster$id_yr==idYrs[i],]      
      nonNaSpeeds<-length(which(!is.na(temp$speed)))      
      if(nonNaSpeeds<3){        
        importedDatasetMaster[importedDatasetMaster$id_yr==idYrs[i],"problem"]<<-1
      }
      nonNaBurst<-length(which(!is.na(temp$burst)))
      if(nonNaBurst<3){
        importedDatasetMaster[importedDatasetMaster$id_yr==idYrs[i],"problem"]<<-1
      }
      nonNaNsd<-length(which(!is.na(temp$nsdYear)))
      if(nonNaNsd<3){
        importedDatasetMaster[importedDatasetMaster$id_yr==idYrs[i],"problem"]<<-1
      }
  }
  checkMortalities()
}


checkMortalities<-function(){
  
  progressIndicator('check for mortalities in dataset','start')

  if('mortality'%in%names(importedDatasetMaster)){
    toggleModal(session,'configModal',toggle='close')
  }
  
  importedDatasetMaster$mortality<<-0
  mortalities<-Check4Morts(st_as_sf(importedDatasetMaster,coords = c("x", "y"), crs = configOptions$masterCrs),configOptions$mortDistance,configOptions$mortTime,'newUid','newMasterDate')  
  if(!is.null(mortalities)){
    for(i in 1:nrow(mortalities)){
      thisIndivid<-mortalities[i,'newUid']
      thisStart<-mortalities[i,'date_start']
      thisEnd<-mortalities[i,'date_end']      
      theseMorts<- importedDatasetMaster$newUid==thisIndivid &
      importedDatasetMaster$newMasterDate>=thisStart &
      importedDatasetMaster$newMasterDate<=thisEnd
      importedDatasetMaster[theseMorts,'mortality']<<-1
    }
  }
  progressIndicator('check for mortalities in dataset','stop')

  importedDatasetMaster<<-st_drop_geometry(importedDatasetMaster)


  saveConfig()
  saveWorkingFile()






  loadingScreenToggle('hide','checking for errors and calculating movement parameters')

  if(hasMapRendered){
    delay(2000,addPointsToMap())
  }else{
    delay(2000,mapInit())
  }
}
