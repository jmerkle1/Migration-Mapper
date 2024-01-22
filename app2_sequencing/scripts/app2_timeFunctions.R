calcFirstPassage<-function(){
  loadingScreenToggle('show','Calculating First Passage Times')
  progressIndicator('Calculating First Passage Times','start')
  # ids <- unique(importedDatasetMaster@data$newUid)
  ids <- unique(importedDatasetMaster$newUid)
  # need to use the loop because as.ltraj is a memory hog.
  fpt.dat <- do.call(rbind, lapply(1:length(ids), function(i){
    # sub <- importedDatasetMaster@data[importedDatasetMaster@data$newUid == ids[i], c("newUid","newMasterDate","x","y")]  # subset data
    sub <- importedDatasetMaster[importedDatasetMaster$newUid == ids[i], c("newUid","newMasterDate","x","y")]  # subset data
    # turn into ltraj object
    sub <- adehabitatLT::as.ltraj(sub[,c("x", "y")],
                                sub$newMasterDate, id = sub$newUid)
    # calculate first passage time
    fptsub <- adehabitatLT::fpt(sub, c(50, 150, 300), "hours")[[1]]
    return(data.frame(FPT50=fptsub$r1, FPT150=fptsub$r2, FPT300=fptsub$r3))
  }))

  # importedDatasetMaster@data[,c('FPT50','FPT150','FPT300')]<<-NULL
  importedDatasetMaster[,c('FPT50','FPT150','FPT300')]<<-NULL

  # importedDatasetMaster@data <<- cbind(importedDatasetMaster@data, fpt.dat)
  importedDatasetMaster <<- cbind(importedDatasetMaster, fpt.dat)
  progressIndicator('Calculating First Passage Times','stop')

  # rowsWithLongFixes<-which(importedDatasetMaster@data$fixRateHours>24)
  rowsWithLongFixes<-which(importedDatasetMaster$fixRateHours>24)
  if(length(rowsWithLongFixes)>0){
    fptsToNa <- lapply(rowsWithLongFixes, function(x) (x-1):(x+1))
    fptsToNa<-unlist(fptsToNa)
    fptsToNa<-fptsToNa[!duplicated(fptsToNa)]
    # importedDatasetMaster@data[fptsToNa,c('FPT50','FPT150','FPT300')]<<-NA
    importedDatasetMaster[fptsToNa,c('FPT50','FPT150','FPT300')]<<-NA
  }
  loadingScreenToggle('hide','Calculating First Passage Times')
}


calcBioYearParams<-function(){

  loadingScreenToggle('show','Calculating Bio Year Params')
  progressIndicator('Calculating Bio Year Params','start')

  # uniqueYears<-unique(format(as.Date(importedDatasetMaster@data[,"newMasterDate"]),"%y"))
  # uniqueYearsFull<-unique(format(as.Date(importedDatasetMaster@data[,"newMasterDate"]),"%Y"))
  uniqueYears<-unique(format(as.Date(importedDatasetMaster[,"newMasterDate"]),"%y"))
  uniqueYearsFull<-unique(format(as.Date(importedDatasetMaster[,"newMasterDate"]),"%Y"))

  for(i in 1:length(uniqueYearsFull)){
    thisYear<-uniqueYearsFull[i]
    nextYear<-as.numeric(thisYear)+1
    lastYear<-as.numeric(thisYear)-1

    thisYearShort<-uniqueYears[i]
    nextYearShort<-as.numeric(thisYearShort)+1
    lastYearShort<-as.numeric(thisYearShort)-1

    isLeapYear=function(year){
      year<-as.numeric(year)
      return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
    }

    startMonth<-format(as.Date(bioYearStartDate),'%m')
    startDay<-format(as.Date(bioYearStartDate),'%d')
    endMonth<-format(as.Date(bioYearStartDate)-1,'%m')
    endDay<-format(as.Date(bioYearStartDate)-1,'%d')

    lastYearEndMonth<-endMonth
    lastYearEndDay<-endDay

    thisYearStartDay<-startDay
    thisYearStartMonth<-startMonth

    thisYearEndDay<-endDay
    thisYearEndMonth<-endMonth

    lastYearEndDate<-as.Date(paste(thisYear,lastYearEndMonth,lastYearEndDay,sep="-"),"%Y-%m-%d")
    thisYearStartDate<-as.Date(paste(thisYear,startMonth,startDay,sep="-"),"%Y-%m-%d")
    thisYearEndDate<-as.Date(paste(nextYear,thisYearEndMonth,thisYearEndDay,sep="-"),"%Y-%m-%d")


    # importedDatasetMaster@data[
    #   as.Date(importedDatasetMaster@data$newMasterDate)>=thisYearStartDate &
    #   as.Date(importedDatasetMaster@data$newMasterDate)<=thisYearEndDate,"bioYear"
    # ]<<-thisYearShort

    # importedDatasetMaster@data[
    #   as.Date(importedDatasetMaster@data$newMasterDate)>=thisYearStartDate &
    #   as.Date(importedDatasetMaster@data$newMasterDate)<=thisYearEndDate,"bioYearFull"
    # ]<<-thisYear

    importedDatasetMaster[
      as.Date(importedDatasetMaster$newMasterDate)>=thisYearStartDate &
      as.Date(importedDatasetMaster$newMasterDate)<=thisYearEndDate,"bioYear"
    ]<<-thisYearShort

    importedDatasetMaster[
      as.Date(importedDatasetMaster$newMasterDate)>=thisYearStartDate &
      as.Date(importedDatasetMaster$newMasterDate)<=thisYearEndDate,"bioYearFull"
    ]<<-thisYear

  }

  # FOR THOSE SEQUENCES AT THE START OF AN ANIMAL BEFORE BIO START DATE.... WHAT TO DO? DROPPING FOR NOW
  # importedDatasetMaster@data$id_bioYear <<- paste(importedDatasetMaster@data$newUid, importedDatasetMaster@data$bioYear, sep="_")
  importedDatasetMaster$id_bioYear <<- paste(importedDatasetMaster$newUid, importedDatasetMaster$bioYear, sep="_")
  progressIndicator('Calculating Bio Year Params','stop')
  loadingScreenToggle('hide','Calculating Bio Year Params')
  buildMigtime()
}

buildMigtime<-function(){
  calcFirstPassage();
  loadingScreenToggle('show','Building Migtime Table and calculating NSD')
  progressIndicator('Building Migtime Table and calculating NSD','start')

  # migtime <<- importedDatasetMaster@data[duplicated(importedDatasetMaster@data$id_bioYear)==FALSE,c("id_bioYear","newUid","bioYear","bioYearFull")]
  migtime <<- importedDatasetMaster[duplicated(importedDatasetMaster$id_bioYear)==FALSE,c("id_bioYear","newUid","bioYear","bioYearFull")]
  migtime <<- migtime[!is.na(migtime$bioYear),]

  migtime <<- migtime[order(migtime$newUid, migtime$id_bioYear),]

  migtime$tempStartDay<<-as.numeric(strftime(bioYearStartDate, format = "%d"))
  migtime$tempStartMonth<<-as.numeric(strftime(bioYearStartDate, format = "%m"))

  tempStartDate<-paste(migtime$bioYearFull,migtime$tempStartMonth,migtime$tempStartDay,sep="-")

  tempEndDate<-as.Date(tempStartDate)

  migtime$mig1start <<- as.Date(tempStartDate)
  migtime$mig1end <<- as.Date(tempEndDate)
  migtime$mig2start <<- as.Date(tempStartDate)
  migtime$mig2end <<- as.Date(tempEndDate)
  migtime$mig3start <<- as.Date(tempStartDate)
  migtime$mig3end <<- as.Date(tempEndDate)
  migtime$mig4start <<- as.Date(tempStartDate)
  migtime$mig4end <<- as.Date(tempEndDate)
  migtime$mig5start <<- as.Date(tempStartDate)
  migtime$mig5end <<- as.Date(tempEndDate)
  migtime$mig6start <<- as.Date(tempStartDate)
  migtime$mig6end <<- as.Date(tempEndDate)
  migtime$mig7start <<- as.Date(tempStartDate)
  migtime$mig7end <<- as.Date(tempEndDate)
  migtime$mig8start <<- as.Date(tempStartDate)
  migtime$mig8end <<- as.Date(tempEndDate)
  migtime$notes <<- '' 

  # importedDatasetMaster@data$nsdBio<<-0
  # importedDatasetMaster@data$displacementBio<<-0
  importedDatasetMaster$nsdBio<<-0
  importedDatasetMaster$displacementBio<<-0

  for(i in 1:nrow(migtime)){
    thisIdYr<-migtime$id_bioYear[i]
    print(i)
    print(thisIdYr)
    
    # temp<-importedDatasetMaster@data[importedDatasetMaster@data$id_bioYear==thisIdYr,]
    temp<-importedDatasetMaster[importedDatasetMaster$id_bioYear==thisIdYr,]
    temp<-temp[which(temp$problem != 1),]
    temp<-temp[which(temp$mortality != 1),]
    # if there are no rows for this individual/id, then it should not have a row in migtime table
    if(nrow(temp)==0){
      print('no nsd!!!')      
    }else{

      # whichPoints<-which(importedDatasetMaster@data$id_bioYear==thisIdYr &
      #   importedDatasetMaster@data$problem != 1 &
      #   importedDatasetMaster@data$mortality != 1)
      
      # thesePoints<-importedDatasetMaster@data[whichPoints,c('x','y')]
      
      # importedDatasetMaster@data[whichPoints,"nsdBio"]<<-
      #   (sqrt((mean(thesePoints$x[1:20])-thesePoints$x)^2 + (mean(thesePoints$y[1:20])-thesePoints$y)^2)^2)/1000000

      # importedDatasetMaster@data[whichPoints,"displacementBio"]<<-
      #   (sqrt((mean(thesePoints$x[1:20])-thesePoints$x)^2 + (mean(thesePoints$y[1:20])-thesePoints$y)^2))/1000
      whichPoints<-which(importedDatasetMaster$id_bioYear==thisIdYr &
        importedDatasetMaster$problem != 1 &
        importedDatasetMaster$mortality != 1)
      
      thesePoints<-importedDatasetMaster[whichPoints,c('x','y')]
      
      importedDatasetMaster[whichPoints,"nsdBio"]<<-
        (sqrt((mean(thesePoints$x[1:20])-thesePoints$x)^2 + (mean(thesePoints$y[1:20])-thesePoints$y)^2)^2)/1000000

      importedDatasetMaster[whichPoints,"displacementBio"]<<-
        (sqrt((mean(thesePoints$x[1:20])-thesePoints$x)^2 + (mean(thesePoints$y[1:20])-thesePoints$y)^2))/1000
      
    }    
  }


  for(i in 1:nrow(migtime)){
    thisIdYr<-migtime$id_bioYear[i]
    # temp<-importedDatasetMaster@data[importedDatasetMaster@data$id_bioYear==thisIdYr,]
    temp<-importedDatasetMaster[importedDatasetMaster$id_bioYear==thisIdYr,]
    temp<-temp[which(temp$problem != 1),]
    temp<-temp[which(temp$mortality != 1),]
    # if there are no rows for this individual/id, then it should not have a row in migtime table
    if(nrow(temp)==0){
      migtime<<-migtime[-i,]
    }
  }

  progressIndicator('Building Migtime Table and calculating NSD','stop')
  saveWorkingFile()
  saveMigtime()
  if(!hasMapRendered){
    mapInit()
  }else{
    mapCurrentIndividual()
    updateSliders()
    updateMapSequencePoints()
    updateSelectInput(session,'currentIndividualSelector',selected=configOptions$currentIndividual)
    adjustSequences()
  }
  loadingScreenToggle('hide','Building Migtime Table and calculating NSD')
}
