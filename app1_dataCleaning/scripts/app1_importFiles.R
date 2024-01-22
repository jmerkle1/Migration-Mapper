prepareFileImport<<-function(){
  filesToImport<<-input$fileUploadSelector
  fileImportTracker<<-list()
  importedShapefilesHolder<<-list()

  loadingScreenToggle('show','importing files')

    for(i in 1:length(filesToImport)){

      if(i==1){
        configOptions$firstFileName<<-strsplit(filesToImport[i],'.shp')[[1]][1]
      }

      fileToImport <- tools::file_path_sans_ext(filesToImport[i])
      # HANDLER TO INDICATE THIS IS THE LAST ONE
      if(i<length(filesToImport)){
        importShapefile(fileToImport,FALSE,i)
      } else{
        importShapefile(fileToImport,TRUE,i)
      }
    }


    loadingScreenToggle('hide','importing files')
}



importShapefile<-function(fileToImport,lastOne,i){
    ##--------------------------------read the shapfile and overwrite the
    ##--------------------------------global importedDataset variable
    # first check if the file has already been imported.. this weeds out multiple
    # clicks on upload button or re-uploads etc
    if(!is.null(fileImportTracker[[fileToImport]])){
      if(fileImportTracker[[fileToImport]]=="inProgress"){
        return()
      }
      if(fileImportTracker[[fileToImport]]=="failed"){
        return()
      }
      if(fileImportTracker[[fileToImport]]==TRUE){
        return()
      }
    }

    # dddd<<-sf::as_Spatial(sf::st_read(paste0(dataFolder,'\\',fileToImport,'.shp')))

    importedDataset <<- tryCatch({
      fileImportTracker[[fileToImport]]<<-"inProgress"
      progressIndicator(paste('Importing ',fileToImport,' Please wait',sep=""),'start')
      st_read(paste0(dataFolder,'\\',fileToImport,'.shp'))
      # sf::as_Spatial(sf::st_read(paste0(dataFolder,'\\',fileToImport,'.shp')))
      # st_read(paste0(dataFolder,'\\',fileToImport,'.shp'))
    },
    error = function(cond) {
      fileImportTracker[[fileToImport]]<<-'failed'
      progressIndicator('Import Error','stop')

      modalMessager(
        "DATA IMPORT ERROR",
        paste(
          "There was a fatal error importing
          your datasets. Please check the data and try again. Detailed error from
          R is : ",
          cond,
          sep = ""
        )
      )
      return()
      })
    # },
    # warning = function(cond) {
    #   # fileImportTracker[[fileToImport]]<<-'failed'
    #   # progressIndicator('Import Error','stop')
    #   # modalMessager("DATA IMPORT WARNING", cond)
    #   # return()
    # })
    importSuccessHandler(fileToImport,lastOne,i,mergingFiles)
  }




  importSuccessHandler<-function(fileToImport,lastOne,i,mergingFiles){
    # # keep track of position in imports --- to do.. this will probs get goofy if we delete and reimport over and over again
    importIterator<-i
    progressIndicator(paste(fileToImport,' Imported successfully!',sep=""),'stop')
    loadingScreenToggle('hide','')

    # add this shapefile to list holder of imported shapefiles
    importedShapefilesHolder[[fileToImport]]<<-assign(fileToImport,importedDataset)

    
    # change columns names to UPPERCASE!!
    # names(importedShapefilesHolder[[fileToImport]]@data)<<-toupper(names(importedShapefilesHolder[[fileToImport]]@data))
    newNames<-toupper(names(importedShapefilesHolder[[fileToImport]]))
    whichIsGeom<-which(newNames=='GEOMETRY')
    newNames[whichIsGeom]<-'geometry'
    names(importedShapefilesHolder[[fileToImport]])<<-newNames
    

    colsToCheck<-c("LAT","LON","newUid","elev","comments","rowIds","newMasterDate","burst","month","day","year","jul","id_yr","x","y","nsdYear","displacementYear","nsdOverall","displacementOverall","dist","dt","speed","abs.angle","rel.angle","fixRateHours","problem","mortality")
    colsToCheck<-toupper(colsToCheck)
    for(i in 1:length(colsToCheck)){
      thisCol<-colsToCheck[i]
      # if(thisCol %in% names(importedShapefilesHolder[[fileToImport]]@data)){
      #   whichCol<-which(names(importedShapefilesHolder[[fileToImport]]@data) == thisCol)
      #   names(importedShapefilesHolder[[fileToImport]]@data)[whichCol]<<-paste0(thisCol,'_')
      # }
      if(thisCol %in% names(importedShapefilesHolder[[fileToImport]])){
        whichCol<-which(names(importedShapefilesHolder[[fileToImport]]) == thisCol)
        names(importedShapefilesHolder[[fileToImport]])[whichCol]<<-paste0(thisCol,'_')
      }
    }





    # temp ui element for reference
    tempUiElement<-paste("uploadedShapefile",importIterator,sep="")

    clearShapefileSelector()

    if(lastOne==TRUE){
      output$importSuccessText<-renderUI({
        HTML(paste0('<strong>You succesfully imported ',length(importedShapefilesHolder),' files</strong>'))
      })


      loadingScreenToggle('hide','')
      checkColumnsPrjs()

    }


  }





  checkColumnsPrjs<-function(){
    #### check if columns are the same between datasets
    # firstNames<-names(importedShapefilesHolder[[1]]@data)
    firstNames<-names(importedShapefilesHolder[[1]])
    for(i in 1:length(importedShapefilesHolder)){
      # theseNames<-names(importedShapefilesHolder[[i]]@data)
      theseNames<-names(importedShapefilesHolder[[i]])
      if(!setequal(firstNames,theseNames)){
        modalMessager('COLUMN NAMES ERROR',"Column names are not the same between your datasets.
        Reformat your data and try again")
        #### TO DO --- if this happens, we need to remove the shapefiles and the loaded buttons
        clearShapefileSelector()
        return()
      }
    }


    # check that projections are the same between datasets
    for(i in 1:length(importedShapefilesHolder)){
      # if(importedShapefilesHolder[[1]]@proj4string@projargs!=importedShapefilesHolder[[i]]@proj4string@projargs){
      #   modalMessager('PROJECTION ERROR',"Projections are not the same between your datasets.
      #   Reformat your data and try again")
      #   #### TO DO --- if this happens, we need to remove the shapefiles and the loaded buttons
      #   clearShapefileSelector()
      #   return()
      # }
    }
    # configOptions$originalProjection<<-importedShapefilesHolder[[1]]@proj4string@projargs
    # configOptions$originalColumns<<-names(importedShapefilesHolder[[1]])
    
    saveConfig()
    showWorkingDirectorySelect();
  }

  showFilesUploadedIndicator<-function(){

  }

  showWorkingDirectorySelect<-function(){
    hideElement(id = 'importDataRow', anim = TRUE)
    showElement(id = 'folderSelectSection', anim = TRUE)

    hide('loadProjectButton')
    show('exportDataButton')
    output$workingDirectoryTitle<-renderUI({
      HTML('<strong>(2) Click the button below to choose and empty Project Folder where all outputs will be stored.</strong>')
    })

    output$chooseWorkingDirButton<-renderUI({
      actionButton("chooseWorkingDirButton", "Click to Choose Directory")
    })

    ##------------------choose a folder where all export files will be stored
    observeEvent(input$chooseWorkingDirButton, {
      masterWorkingDirectory<<-NULL

      shinyjs::disable("chooseWorkingDirButton")

      masterWorkingDirectory<<-choose.dir(dataFolder)
      print('**********************')
      print(masterWorkingDirectory)

      if(is.na(masterWorkingDirectory) | is.null(masterWorkingDirectory)){
        shinyjs::enable("chooseWorkingDirButton")
        return()
      }

      if(!dir.exists(masterWorkingDirectory)){
        modalMessager('error','Please try selecting this folder again')
        shinyjs::enable("chooseWorkingDirButton")
        return()
      }




      files<-list.files(masterWorkingDirectory)

      if(length(files)>0){
        modalMessager('error','The folder you chose is not empty.
        This will cause errors in analysis. Please empty the folder or
        choose a different directory and try again.')
        shinyjs::enable("chooseWorkingDirButton")
        masterWorkingDirectory<<-NULL
        return()
      }

      configOptions$masterWorkingDirectory<<-masterWorkingDirectory
      saveConfig()


      output$selectedWorkingDirectoryLabel<-renderUI({
        strong(paste0('Your data will be exported to: ',masterWorkingDirectory))
      })


      output$selectedWorkingDirectoryLabel<-renderUI({
        HTML(paste0('<strong>',masterWorkingDirectory,'</strong>'))
      })

      showColumnChoiceInfo()
      shinyjs::enable("chooseWorkingDirButton")
    },ignoreInit=TRUE)

  }


  showColumnChoiceInfo<-function(){

    hideElement(id = 'importDataRow', anim = TRUE)
    hideElement(id = 'folderSelectSection', anim = TRUE)
    showElement(id = 'uidSeletorRow', anim = TRUE)


    if(!exists('importedDatasetMaster')){
      columnNames<-names(importedShapefilesHolder[[1]])
      ##------------------ show the first 20 rows of data
      rowsToShow<-st_drop_geometry(importedShapefilesHolder[[1]][1:20,])
    }else{
      # columnNames<-names(importedDatasetMaster@data)
      columnNames<-names(importedDatasetMaster)
      # importedDatasetMaster@data['comments']<<-''
      importedDatasetMaster['comments']<<-''
      rowsToShow<-st_drop_geometry(importedDatasetMaster[1:20,])
    }



    if('timestamp'%in%names(rowsToShow)){
      rowsToShow$timestamp<-as.character(rowsToShow$timestamp)
    }
    output$aidConfigTable <- renderTable(rowsToShow)

    output$uniqueIdSelector<-renderUI({
      selectInput(
        "uniqueIdSelector",
        "",
        c("",'NaN',columnNames),
        selected = NULL,
        multiple = FALSE
      )
    })
    output$uniqueIdSelectorGo<-renderUI({
      actionButton('uniqueIdSelectorGo','Execute')
    })
    observeEvent(input$uniqueIdSelectorGo, {

      if(is.null(masterWorkingDirectory)){
        modalMessager('Error','You need to select an empty working directory to continue')
        return()
      }


      if(!exists('importedDatasetMaster')){
        mergeShapfilesHandler()
      }else{
        newUid<-input$uniqueIdSelectorGo
        # importedDatasetMaster$newUid<<-importedDatasetMaster@data[,newUid]
        importedDatasetMaster$newUid<<-importedDatasetMaster[,newUid]
        showDateTimeSelectionPanel()
      }

    })

    if(thisIsTestingRun){
      mergeShapfilesHandler()
    }

  }



  filesToImport<-c("gps_318.shp","gps_318dead.shp","gps_320.shp","gps_317.shp","gps_321.shp","gps_344.shp","gps_346.shp")
  availableShapefiles<<-c("gps_318.shp","gps_318dead.shp","gps_320.shp","gps_317.shp","gps_321.shp","gps_344.shp","gps_346.shp")
  dataFolder<-"D:\\sampleData"
  fileImportTracker<<-list()
  importedShapefilesHolder<<-list()
  thisIsTestingRun<<-FALSE

  testImport<<-function(){

    thisIsTestingRun<<-TRUE
    dateColumns<<-c('date','hour')
    selectedUid<<-"aid"
    validatorObject<<-list()
    validatorObject$year<<-c('year','1',"%Y","date")
    validatorObject$month<<-c('month','2',"%m","date")
    validatorObject$day<<-c('day','3',"%d","date")
    validatorObject$hour<<-c('hour','1',"%H","hour")


    loadingScreenToggle('show','importing files')
    for(i in 1:length(filesToImport)){
      fileToImport <- tools::file_path_sans_ext(filesToImport[i])
      # HANDLER TO INDICATE THIS IS THE LAST ONE
      if(i<length(filesToImport)){
        importShapefile(fileToImport,FALSE,i)
      } else{
        importShapefile(fileToImport,TRUE,i)
      }
    }
  }


  downloadMovebankData<-function(user,pw,movebankId){


    show('downloadSpinner')




    storedLogin<-movebankLogin('username' = user, 'password' = pw)





    loadingScreenToggle('show','downloading movebank data.. please be patient.. depending on file size, this can take a while.')

    movebankData <<- tryCatch({
      getMovebankData(as.numeric(movebankId),'login' = storedLogin)
    },
    error = function(cond) {
      modalMessager('movebank error',cond$message)
      loadingScreenToggle('hide','downloading movebank data.. please be patient.. depending on file size, this can take a while.')
      return()
    },
    warning = function(cond) {
      modalMessager('movebank error',cond$message)
      loadingScreenToggle('hide','downloading movebank data.. please be patient.. depending on file size, this can take a while.')
      return()
    })

    if(!is.null(movebankData)){

      processMovebankData(data.frame(movebankData))

    }





  }

  processMovebankData<-function(movebankData){


    theseDataNames<-names(movebankData)
    if('location_long'%in%theseDataNames){
      thisLongField<-'location_long'
    }
    if('location_long.1'%in%theseDataNames){
      thisLongField<-'location_long.1'
    }
    if('location_lat'%in%theseDataNames){
      thisLatField<-'location_lat'
    }
    if('location_lat.1'%in%theseDataNames){
      thisLatField<-'location_lat.1'
    }
    # xy <- mydf[,c(1,2)]
    importedDatasetMaster <<- SpatialPointsDataFrame(
                               coords = movebankData[,c(thisLongField,thisLatField)],
                               data = movebankData,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
                             )

     # importedDatasetMaster@data[["lon"]]<<-as.numeric(importedDatasetMaster@data[,thisLongField])
     # importedDatasetMaster@data[["lat"]]<<-as.numeric(importedDatasetMaster@data[,thisLatField])
     # importedDatasetMaster@data$rowIds<<-row.names(importedDatasetMaster@data)
     importedDatasetMaster[["lon"]]<<-as.numeric(importedDatasetMaster[,thisLongField])
     importedDatasetMaster[["lat"]]<<-as.numeric(importedDatasetMaster[,thisLatField])
     # importedDatasetMaster$rowIds<<-row.names(importedDatasetMaster)
     rowIds<-c(1:nrow(importedDatasetMaster))
     importedDatasetMaster$rowIds<<-rowIds

     
     # prj to utm
     # importedDatasetMaster<<-spTransform(importedDatasetMaster,CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
     # midLatLong <- c(importedDatasetMaster@data[1,'lat'],importedDatasetMaster@data[1,'lon'])
     midLatLong <- c(importedDatasetMaster[1,'lat'],importedDatasetMaster[1,'lon'])
     # zone <- find_UTM_zone(midLatLong[1], midLatLong[2])
     zone <- find_UTM_zone(midLatLong[2], midLatLong[1])
     UTMcrs <- paste0("+proj=utm +zone=", zone, " +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
     configOptions$masterCrs<<-UTMcrs
     importedDatasetMaster <<- spTransform(importedDatasetMaster, CRS(UTMcrs))


     loadingScreenToggle('hide','')
     showFilesUploadedIndicator();
     showWorkingDirectorySelect();

     # toggleModal(session,'movebankModal',toggle='close')
  }
