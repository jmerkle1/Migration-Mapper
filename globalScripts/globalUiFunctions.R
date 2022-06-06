appRoot<<-getwd()
appRoot<<-dirname(appRoot)

startOther<<-function(appToRun){
    if(appToRun=='app1'){
      runApp(paste0(appRoot,'/app1_dataCleaning/',appToRun,'.R'),launch.browser=T)
    }
    if(appToRun=='app2'){
      runApp(paste0(appRoot,'/app2_sequencing/',appToRun,'.R'),launch.browser=T)
    }
    if(appToRun=='app3'){
      runApp(paste0(appRoot,'/app3_sequenceWriting/',appToRun,'.R'),launch.browser=T)
    }
    if(appToRun=='app4'){
      runApp(paste0(appRoot,'/app4_uds/',appToRun,'.R'),launch.browser=T)
    }
    if(appToRun=='app5'){
      runApp(paste0(appRoot,'/app5_merging/',appToRun,'.R'),launch.browser=T)
    }
    if(appToRun=='app6'){
      runApp(paste0(appRoot,'/app6_mapping/',appToRun,'.R'),launch.browser=T)
    }

}


modalMessager <<- function(header, body) {
    showModal(
      modalDialog(
        title = header,
        body,
        footer=modalButton("OK"),
        tags$head(tags$style("#shiny-modal .modal-footer{ display:block}"))
      )
    )
  }

  appJumpList=list(
    'App 1' = 'app1',
    'App 2' = 'app2',
    'App 3' = 'app3',
    'App 4' = 'app4',
    'App 5' = 'app5',
    'App 6' = 'app6'
  )

  firstOpen<<-TRUE

  changeToOtherApp<<-function(){
    showModal(modalDialog(
           title="Change to Other App",
           "Choose one of the options below to jump to another app.",
           br(),
           selectInput('changeAppsSelect','choose the app to jump to',choices=appJumpList,selected=whichAppIsRunning),
           fade = FALSE
       ))

       if(exists('openObserver')){
         openObserver$destroy()
         firstOpen<<-TRUE
       }
       openObserver<<-observeEvent(input$changeAppsSelect, {
         appToRun<<-input$changeAppsSelect
         if(firstOpen){
           firstOpen<<-FALSE
         }else{

           onStop(function() {
             startOther(appToRun)
           })
           runjs("window.close(); console.log('closing')")
           stopApp()
         }
       },ignoreInit=TRUE)
  }




  progressIndicator<<-function(message,startStop,updateValue){
      if(startStop=="start")  {
        progress <<- shiny::Progress$new()
        progress$set(message = message, value = 50)
      }
      else if(startStop=="update"){
        progress$set(message = message, value = updateValue)
      } else {
        progress$set(message = message, value = 100)
        progress$close()
      }
    }


closeApp<<-function(){
  print('closing')
  runjs("window.close(); console.log('closing')")
  stopApp()
}

saveSessionInfo<<-function(){
  sessionInfo<-list()
  sessionInfo$masterWorkingDirectory<-masterWorkingDirectory
  sessionInfo$time<-Sys.time()
  saveTo<-paste0(dirname(getwd()),'//session.rds')
  print(saveTo)
  saveRDS(sessionInfo,saveTo)
}



saveWorkingFile<<-function(){
  progressIndicator('saving project files','start')
  loadingScreenToggle('show','saving project files')
  workingFile$masterWorkingDirectory<<-masterWorkingDirectory
  workingFile$importedDatasetMaster<<-importedDatasetMaster
  saveRDS(workingFile,paste0(masterWorkingDirectory,'//workingFile.rds'),)
  dbConnection <<- dbConnect(RSQLite::SQLite(), paste0(masterWorkingDirectory,'//workingDb.db'))
  dbWriteTable(dbConnection, "importedDatasetMaster", importedDatasetMaster@data, overwrite=T)
  importedDatasetMaster@data <- dbGetQuery(dbConnection, "SELECT * FROM importedDatasetMaster")
  progressIndicator('saving project files','stop')
  loadingScreenToggle('hide','')
  saveSessionInfo()
}

saveMigtime<-function(){
  dbConnection <<- dbConnect(RSQLite::SQLite(), paste0(masterWorkingDirectory,'//workingDb.db'))

  migtimeTemp<-migtime
  migtimeTemp$mig1start <- as.character(migtimeTemp$mig1start)
  migtimeTemp$mig1end <- as.character(migtimeTemp$mig1end)
  migtimeTemp$mig2start <- as.character(migtimeTemp$mig2start)
  migtimeTemp$mig2end <- as.character(migtimeTemp$mig2end)
  migtimeTemp$mig3start <- as.character(migtimeTemp$mig3start)
  migtimeTemp$mig3end <- as.character(migtimeTemp$mig3end)
  migtimeTemp$mig4start <- as.character(migtimeTemp$mig4start)
  migtimeTemp$mig4end <- as.character(migtimeTemp$mig4end)
  migtimeTemp$mig5start <- as.character(migtimeTemp$mig5start)
  migtimeTemp$mig5end <- as.character(migtimeTemp$mig5end)
  migtimeTemp$mig6start <- as.character(migtimeTemp$mig6start)
  migtimeTemp$mig6end <- as.character(migtimeTemp$mig6end)
  migtimeTemp$mig7start <- as.character(migtimeTemp$mig7start)
  migtimeTemp$mig7end <- as.character(migtimeTemp$mig7end)
  migtimeTemp$mig8start <- as.character(migtimeTemp$mig8start)
  migtimeTemp$mig8end <- as.character(migtimeTemp$mig8end)

  if(exists('migtime')){
    dbWriteTable(dbConnection, "migtime", migtimeTemp, overwrite=T)
  }
  # dbGetQuery(dbConnection, "SELECT * FROM migtime")
}

loadingScreenToggle<-function(hideShow,msg){
  if(hideShow=='show'){
    html('loadingMessage',msg)
    showElement(id = 'loadingScreen', anim = FALSE)
  }else{
    hideElement(id = 'loadingScreen', anim = FALSE)
  }
}


checkForSession<<-function(fromApp){
  sessionCheckLocation<-paste0(dirname(getwd()),'//session.rds')
  if(file.exists(sessionCheckLocation)){
    sessionInfo<-readRDS(sessionCheckLocation)
    msgInfo<-paste0('A previous MAPP session was detected. This session was last active at ',sessionInfo$time,' using the project file stored at ',sessionInfo$masterWorkingDirectory,'. Would you like to reload the data from this session? If you click "no, clear session", you will need to choose a different project folder. This will not delete project files.')
    showModal(modalDialog(
           title="Reload previous session?",
           msgInfo,
           footer = tagList(actionButton("confirmReload", "Yes Reload Previous Session"),
                            actionButton("clearSession", "No, clear session")
           )
       ))

       observeEvent(input$confirmReload, {
         print('reloading session')
         loadingScreenToggle('show','reloading session')
         if(fromApp=='app1'){
           appOneReload(paste0(sessionInfo$masterWorkingDirectory))
         }
         if(fromApp=='app2'){
           appTwoReload(paste0(sessionInfo$masterWorkingDirectory))
         }
         if(fromApp=='app3'){
           appThreeReload(paste0(sessionInfo$masterWorkingDirectory))
         }
         if(fromApp=='app4'){
           appFourReload(paste0(sessionInfo$masterWorkingDirectory))
         }
         if(fromApp=='app5'){
           appFiveReload(paste0(sessionInfo$masterWorkingDirectory))
         }
         if(fromApp=='app6'){
           appSixReload(paste0(sessionInfo$masterWorkingDirectory))
         }
     })

     observeEvent(input$clearSession, {
       file.remove(sessionCheckLocation)
       removeModal()
       print('clearing session')
   })

  }
}

loadDependencies<-function(dependencies){
  for(i in 1:length(dependencies)){
    if(dependencies[i] %in% installed.packages()==FALSE){
      install.packages(dependencies[i])
      require(dependencies[i],character.only=TRUE)
    } else{
      require(dependencies[i],character.only=TRUE)
    }
  }
}




find_UTM_zone <- function(longitude, latitude) {

  if("numeric" %in% class(longitude) == FALSE)
    stop("longitude is not a numeric value or vector")
  if("numeric" %in% class(latitude) == FALSE)
    stop("latitude is not a numeric value or vector")
  if(length(longitude)!=length(latitude))
    stop("Your longitude and latitude are not the same length")

  if(length(longitude)==1){
    # Special zones for Svalbard and Norway
    if (latitude >= 72.0 && latitude < 84.0 )
      if (longitude >= 0.0  && longitude <  9.0)
        return(31);
    if (longitude >= 9.0  && longitude < 21.0)
      return(33)
    if (longitude >= 21.0 && longitude < 33.0)
      return(35)
    if (longitude >= 33.0 && longitude < 42.0)
      return(37)
    (floor((longitude + 180) / 6) %% 60) + 1
  }else{
    # loop through each value
    return(do.call(c, lapply(1:length(longitude), function(i){
      # Special zones for Svalbard and Norway
      if (latitude[i] >= 72.0 && latitude[i] < 84.0 )
        if (longitude[i] >= 0.0  && longitude[i] <  9.0)
          return(31);
      if (longitude[i] >= 9.0  && longitude[i] < 21.0)
        return(33)
      if (longitude[i] >= 21.0 && longitude[i] < 33.0)
        return(35)
      if (longitude[i] >= 33.0 && longitude[i] < 42.0)
        return(37)
      (floor((longitude[i] + 180) / 6) %% 60) + 1
    })))
  }
}


pointsToLines<-function(d){
  ## list of Lines per id, each with one Line in a list
  x <- lapply(split(d, d$newUid), function(x) Lines(list(Line(coordinates(x))), x$newUid[1L]))
  # the corrected part goes here:
  lines <- SpatialLines(x)
  data <- data.frame(newUid = unique(d$newUid))
  rownames(data) <- data$newUid
  l <- SpatialLinesDataFrame(lines, data)
  return(l)
}

saveConfig<-function(){
  if(exists('masterWorkingDirectory')){
    saveRDS(configOptions,paste0(masterWorkingDirectory,'//configOptions.rds'))
  }
}
