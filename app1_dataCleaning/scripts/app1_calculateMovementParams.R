movementParamsProcessing<-function(){

  loadingScreenToggle('show','checking for errors and calculating movement parameters')

  progressIndicator('checking for duplicates','start')
  # dupCheck<-paste0(importedDatasetMaster@data$newUid,importedDatasetMaster@data$newMasterDate)
  dupCheck<-paste0(importedDatasetMaster$newUid,importedDatasetMaster$newMasterDate)
  areDupd<-duplicated(dupCheck)
  if(any(areDupd)){
    importedDatasetMaster <<- importedDatasetMaster[!duplicated(dupCheck),]
  }
  progressIndicator('calculating duplicates','stop')

      
          

    progressIndicator('calculating burst','start')
    #REORDER DATA AS PER BURST REQUIREMENTS
    # importedDatasetMaster<<-importedDatasetMaster[order(importedDatasetMaster@data$newUid, importedDatasetMaster@data$newMasterDate),]
    importedDatasetMaster<<-importedDatasetMaster[order(importedDatasetMaster$newUid, importedDatasetMaster$newMasterDate),]
    # run the burst script
    # importedDatasetMaster@data$burst<<-creat.burst(importedDatasetMaster@data)    
    # importedDatasetMaster$burst<<-creat.burst(st_drop_geometry(importedDatasetMaster))
    # importedDatasetMaster$burst<<-CalcBurst(st_drop_geometry(importedDatasetMaster),T,'newUid','newMasterDate',10800)
    importedDatasetMaster$burst<<-CalcBurst(importedDatasetMaster,T,'newUid','newMasterDate',10800)
    progressIndicator('checking for burst','stop')




    progressIndicator('calculating date parameters','start')
    # importedDatasetMaster@data$month <<- as.numeric(strftime(importedDatasetMaster@data$newMasterDate, format = "%m", tz = selectedTimezone))
    # importedDatasetMaster@data$day <<- as.numeric(strftime(importedDatasetMaster@data$newMasterDate, format = "%d", tz = selectedTimezone))
    # importedDatasetMaster@data$year <<- as.numeric(strftime(importedDatasetMaster@data$newMasterDate, format = "%Y", tz = selectedTimezone))
    importedDatasetMaster$month <<- as.numeric(strftime(importedDatasetMaster$newMasterDate, format = "%m", tz = selectedTimezone))
    importedDatasetMaster$day <<- as.numeric(strftime(importedDatasetMaster$newMasterDate, format = "%d", tz = selectedTimezone))
    importedDatasetMaster$year <<- as.numeric(strftime(importedDatasetMaster$newMasterDate, format = "%Y", tz = selectedTimezone))


    # importedDatasetMaster@data$jul <<- as.numeric(strftime(importedDatasetMaster@data$newMasterDate, format = "%j", tz = selectedTimezone))    
    # importedDatasetMaster@data$id_yr <<- paste(importedDatasetMaster@data$newUid, importedDatasetMaster@data$year, sep="_")
    importedDatasetMaster$jul <<- as.numeric(strftime(importedDatasetMaster$newMasterDate, format = "%j", tz = selectedTimezone))    
    importedDatasetMaster$id_yr <<- paste(importedDatasetMaster$newUid, importedDatasetMaster$year, sep="_")
    progressIndicator('calculating date parameters','stop')



    # progressIndicator('adding x y columns','start')
    # # importedDatasetMaster@data$x<<-importedDatasetMaster@coords[,1]
    # # importedDatasetMaster@data$y<<-importedDatasetMaster@coords[,2]
    # coords<-st_coordinates(importedDatasetMaster)
    # importedDatasetMaster$x<<-coords[,1]
    # importedDatasetMaster$y<<-coords[,2]
    # progressIndicator('adding x y columns','stop')

    # progressIndicator('adding x y columns','start')
    # coords<-st_coordinates(importedDatasetMaster)
    # importedDatasetMaster$x<<-coords[,1]
    # importedDatasetMaster$y<<-coords[,2]
    # progressIndicator('adding x y columns','stop')





    progressIndicator('calculating nsds','start')


    

    idYrs<-unique(importedDatasetMaster$id_yr)

    importedDatasetMaster$nsdYear<<-NA
    importedDatasetMaster$displacementYear<<-NA
    importedDatasetMaster$nsdOverall<<-NA
    importedDatasetMaster$displacementOverall<<-NA

    for(i in 1:length(idYrs)){
      # temp<- importedDatasetMaster@data[importedDatasetMaster@data$id_yr==idYrs[i],]
      # importedDatasetMaster@data[importedDatasetMaster@data$id_yr==idYrs[i],"nsdYear"]<- (sqrt((mean(temp'x'[1:20])-temp$x)^2 + (mean(temp$y[1:20])-temp$y)^2)^2)/1000000
      # importedDatasetMaster@data[importedDatasetMaster@data$id_yr==idYrs[i],"displacementYear"]<- (sqrt((mean(temp$x[1:20])-temp$x)^2 + (mean(temp$y[1:20])-temp$y)^2))/1000
      # temp<- st_drop_geometry(importedDatasetMaster[importedDatasetMaster$id_yr==idYrs[i],])
      temp<- importedDatasetMaster[importedDatasetMaster$id_yr==idYrs[i],]
      importedDatasetMaster[importedDatasetMaster$id_yr==idYrs[i],"nsdYear"]<<- (sqrt((mean(temp$x[1:20])-temp$x)^2 + (mean(temp$y[1:20])-temp$y)^2)^2)/1000000
      importedDatasetMaster[importedDatasetMaster$id_yr==idYrs[i],"displacementYear"]<<- (sqrt((mean(temp$x[1:20])-temp$x)^2 + (mean(temp$y[1:20])-temp$y)^2))/1000
    }
    aids<-unique(importedDatasetMaster$newUid)
    for(i in 1:length(aids)){
      # temp<- importedDatasetMaster@data[importedDatasetMaster@data$newUid==aids[i],]
      # importedDatasetMaster@data[importedDatasetMaster@data$newUid==aids[i],"nsdOverall"]<- (sqrt((mean(temp$x[1:20])-temp$x)^2 + (mean(temp$y[1:20])-temp$y)^2)^2)/1000000
      # importedDatasetMaster@data[importedDatasetMaster@data$newUid==aids[i],"displacementOverall"]<- (sqrt((mean(temp$x[1:20])-temp$x)^2 + (mean(temp$y[1:20])-temp$y)^2))/1000
      # temp<- st_drop_geometry(importedDatasetMaster[importedDatasetMaster$newUid==aids[i],])
      temp<-importedDatasetMaster[importedDatasetMaster$newUid==aids[i],]
      importedDatasetMaster[importedDatasetMaster$newUid==aids[i],"nsdOverall"]<<- (sqrt((mean(temp$x[1:20])-temp$x)^2 + (mean(temp$y[1:20])-temp$y)^2)^2)/1000000
      importedDatasetMaster[importedDatasetMaster$newUid==aids[i],"displacementOverall"]<<- (sqrt((mean(temp$x[1:20])-temp$x)^2 + (mean(temp$y[1:20])-temp$y)^2))/1000
    }

    progressIndicator('calculating nsds','stop')

    progressIndicator('calculating movement parameters','start')
    # importedDatasetMaster@data<<-mov.param(importedDatasetMaster@data)
    # importedDatasetMaster<<-mov.param(importedDatasetMaster)    
    tempx<-importedDatasetMaster$x
    tempy<-importedDatasetMaster$y
    importedDatasetMaster<<-CalcMovParams(st_as_sf(importedDatasetMaster,coords = c("x", "y"), crs = configOptions$masterCrs),'newUid','newMasterDate')    
    # needed this for graphing
    importedDatasetMaster$fixRateHours<<-importedDatasetMaster$dt/3600
    importedDatasetMaster$x<<-tempx
    importedDatasetMaster$y<<-tempy
    
    progressIndicator('calculating movement parameters','stop')


    findProblemPoints()
  }

findProblemPoints<-function(){

  loadingScreenToggle('show','checking for errors and calculating movement parameters')

  progressIndicator('finding problem points','start')
  # importedDatasetMaster@data$problem<<-0
  # maxSpeedMPerSec<-(configOptions$maxSpeedParameter*1000)/3600
  # importedDatasetMaster[which(importedDatasetMaster@data$speed>maxSpeedMPerSec),'problem']<<-1
  importedDatasetMaster$problem<<-0

  probPoints<-FindProblemPts(importedDatasetMaster,date_name='newMasterDate',id_name='newUid',speedlim=configOptions$maxSpeedParameter)
  if(any(probPoints)){
    importedDatasetMaster[probPoints,'problem']<<-1
  }  
  # importedDatasetMaster<<-st_drop_geometry(importedDatasetMaster)
  # maxSpeedMPerSec<-(configOptions$maxSpeedParameter*1000)/3600
  # importedDatasetMaster[which(importedDatasetMaster$speed>maxSpeedMPerSec),'problem']<<-1

  progressIndicator('finding problem points','stop')

  
  checkForAllNaSpeedsEtc()
}

checkForAllNaSpeedsEtc<-function(){
  
  idYrs<-unique(importedDatasetMaster$id_yr)
  for(i in 1:length(idYrs)){
      # temp<- importedDatasetMaster@data[importedDatasetMaster@data$id_yr==idYrs[i],]      
      # nonNaSpeeds<-length(which(!is.na(temp$speed)))      
      # if(nonNaSpeeds<3){        
      #   importedDatasetMaster@data[importedDatasetMaster@data$id_yr==idYrs[i],"problem"]<<-1
      # }
      # nonNaBurst<-length(which(!is.na(temp$burst)))
      # if(nonNaBurst<3){
      #   importedDatasetMaster@data[importedDatasetMaster@data$id_yr==idYrs[i],"problem"]<<-1
      # }
      # nonNaNsd<-length(which(!is.na(temp$nsdYear)))
      # if(nonNaNsd<3){
      #   importedDatasetMaster@data[importedDatasetMaster@data$id_yr==idYrs[i],"problem"]<<-1
      # }
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

  # importedDatasetMaster@data$mortality<<-0
  importedDatasetMaster$mortality<<-0

  # mortalities<-mort.check(importedDatasetMaster)

  mortalities<-Check4Morts(st_as_sf(importedDatasetMaster,coords = c("x", "y"), crs = configOptions$masterCrs),configOptions$mortDistance,configOptions$mortTime,'newUid','newMasterDate')  
  if(!is.null(mortalities)){
    for(i in 1:nrow(mortalities)){
      thisIndivid<-mortalities[i,'newUid']
      thisStart<-mortalities[i,'date_start']
      thisEnd<-mortalities[i,'date_end']
      # creates a boolean for which rows are within each mort period
      # theseMorts<- importedDatasetMaster@data$newUid==thisIndivid &
      # importedDatasetMaster@data$newMasterDate>=thisStart &
      # importedDatasetMaster@data$newMasterDate<=thisEnd
      # importedDatasetMaster@data[theseMorts,'mortality']<<-1
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
