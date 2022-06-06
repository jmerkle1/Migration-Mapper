movementParamsProcessing<-function(){

  loadingScreenToggle('show','checking for errors and calculating movement parameters')

  progressIndicator('checking for duplicates','start')
  dupCheck<-paste0(importedDatasetMaster@data$newUid,importedDatasetMaster@data$newMasterDate)
  areDupd<-duplicated(dupCheck)
  if(any(areDupd)){
    importedDatasetMaster <<- importedDatasetMaster[!duplicated(dupCheck),]
  }
  progressIndicator('calculating duplicates','stop')

    progressIndicator('calculating burst','start')
    #REORDER DATA AS PER BURST REQUIREMENTS
    importedDatasetMaster<<-importedDatasetMaster[order(importedDatasetMaster@data$newUid, importedDatasetMaster@data$newMasterDate),]
    # run the burst script
    importedDatasetMaster@data$burst<<-creat.burst(importedDatasetMaster@data)
    progressIndicator('checking for burst','stop')




    progressIndicator('calculating date parameters','start')
    importedDatasetMaster@data$month <<- as.numeric(strftime(importedDatasetMaster@data$newMasterDate, format = "%m", tz = selectedTimezone))
    importedDatasetMaster@data$day <<- as.numeric(strftime(importedDatasetMaster@data$newMasterDate, format = "%d", tz = selectedTimezone))
    importedDatasetMaster@data$year <<- as.numeric(strftime(importedDatasetMaster@data$newMasterDate, format = "%Y", tz = selectedTimezone))


    importedDatasetMaster@data$jul <<- as.numeric(strftime(importedDatasetMaster@data$newMasterDate, format = "%j", tz = selectedTimezone))
    # importedDatasetMaster@data$jul2<<- ifelse(importedDatasetMaster@data$month == 1, importedDatasetMaster@data$jul+max(importedDatasetMaster@data$jul), importedDatasetMaster@data$jul)
    importedDatasetMaster@data$id_yr <<- paste(importedDatasetMaster@data$newUid, importedDatasetMaster@data$year, sep="_")
    progressIndicator('calculating date parameters','stop')



    progressIndicator('adding x y columns','start')
    importedDatasetMaster@data$x<<-importedDatasetMaster@coords[,1]
    importedDatasetMaster@data$y<<-importedDatasetMaster@coords[,2]
    progressIndicator('adding x y columns','stop')





    progressIndicator('calculating nsds','start')

    idYrs<-unique(importedDatasetMaster$id_yr)

    for(i in 1:length(idYrs)){
      temp<- importedDatasetMaster@data[importedDatasetMaster@data$id_yr==idYrs[i],]
      importedDatasetMaster@data[importedDatasetMaster@data$id_yr==idYrs[i],"nsdYear"]<- (sqrt((mean(temp$x[1:20])-temp$x)^2 + (mean(temp$y[1:20])-temp$y)^2)^2)/1000000
      importedDatasetMaster@data[importedDatasetMaster@data$id_yr==idYrs[i],"displacementYear"]<- (sqrt((mean(temp$x[1:20])-temp$x)^2 + (mean(temp$y[1:20])-temp$y)^2))/1000
    }
    aids<-unique(importedDatasetMaster$newUid)
    for(i in 1:length(aids)){
      temp<- importedDatasetMaster@data[importedDatasetMaster@data$newUid==aids[i],]
      importedDatasetMaster@data[importedDatasetMaster@data$newUid==aids[i],"nsdOverall"]<- (sqrt((mean(temp$x[1:20])-temp$x)^2 + (mean(temp$y[1:20])-temp$y)^2)^2)/1000000
      importedDatasetMaster@data[importedDatasetMaster@data$newUid==aids[i],"displacementOverall"]<- (sqrt((mean(temp$x[1:20])-temp$x)^2 + (mean(temp$y[1:20])-temp$y)^2))/1000
    }



    progressIndicator('calculating nsds','stop')

    progressIndicator('calculating movement parameters','start')
    importedDatasetMaster@data<<-mov.param(importedDatasetMaster@data)
    progressIndicator('calculating movement parameters','stop')


    findProblemPoints()
  }

findProblemPoints<-function(){

  loadingScreenToggle('show','checking for errors and calculating movement parameters')

  progressIndicator('finding problem points','start')
  importedDatasetMaster@data$problem<<-0
  maxSpeedMPerSec<-(configOptions$maxSpeedParameter*1000)/3600
  importedDatasetMaster[which(importedDatasetMaster@data$speed>maxSpeedMPerSec),'problem']<<-1
  progressIndicator('finding problem points','stop')

  checkMortalities()
}


checkMortalities<-function(){
  progressIndicator('check for mortalities in dataset','start')

  if('mortality'%in%names(importedDatasetMaster)){
    toggleModal(session,'configModal',toggle='close')
  }

  importedDatasetMaster@data$mortality<<-0

  mortalities<-mort.check(importedDatasetMaster@data)
  if(!is.null(mortalities)){
    for(i in 1:nrow(mortalities)){
      thisIndivid<-mortalities[i,'newUid']
      thisStart<-mortalities[i,'date_start']
      thisEnd<-mortalities[i,'date_end']
      # creates a boolean for which rows are within each mort period
      theseMorts<- importedDatasetMaster@data$newUid==thisIndivid &
      importedDatasetMaster@data$newMasterDate>=thisStart &
      importedDatasetMaster@data$newMasterDate<=thisEnd

      importedDatasetMaster@data[theseMorts,'mortality']<<-1
    }
  }
  progressIndicator('check for mortalities in dataset','stop')


  saveConfig()
  saveWorkingFile()






  loadingScreenToggle('hide','checking for errors and calculating movement parameters')

  if(hasMapRendered){
    delay(2000,addPointsToMap())
  }else{
    delay(2000,mapInit())
  }
}
