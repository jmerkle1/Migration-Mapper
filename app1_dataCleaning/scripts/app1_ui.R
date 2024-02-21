app1_init<-function(input,output,session){
  input<<-input
  output<<-output
  session<<-session

  volumes<<-getVolumes()()  
  
  observeEvent(input$confirmProjectRebuild,{
      thisSelectedFolder<-getFolderPathFromShinyDirChoose(volumes,input$confirmProjectRebuild)
      if(is.null(thisSelectedFolder)){
        return ()
      }    
      rebuildOlderProject(thisSelectedFolder)  
  },ignoreInit=TRUE)
  # shinyDirChoose(input, "confirmProjectRebuild", roots=volumes, filetypes = NULL, allowDirCreate = FALSE)  

  # observeEvent(input$confirmProjectRebuild,{
  #   rebuildOlderProject()
  #   # toggleModal(session,'rebuild30modal',toggle='close')
  #   removeModal()    
  # },ignoreInit=TRUE)

  whichAppIsRunning<<-'app1'
  observeEvent(input$changeAppsButton, {
    changeToOtherApp()
  },ignoreInit=TRUE)


  observeEvent(input$closeMappButton, {
    closeApp()
  },ignoreInit=TRUE)

  observeEvent(input$exportDataButton,{

    if('firstFileName'%in%names(configOptions)){
      fileExportName<<-configOptions$firstFileName
    }else{
      fileExportName<<-'exportedFile'
    }

    if(exists('exportObserver')){
      exportObserver$destroy()
    }

    isExportRunning<<-FALSE
    showModal(modalDialog(
           title="Choose a name for file to export",
           "Choose a name below for the file that will be exported. The default name is the name of the first imported file. The file will be exported into a subfolder called EXPORTS within your working folder. Once you choose a name click the EXPORT SHAPEFILE to continue.",
           br(),
           textInput('fileExportInput', '', value = fileExportName, width = NULL, placeholder = NULL),
           actionButton("exportShapefileButton", "EXPORT SHAPEFILE"),
           footer = modalButton("CANCEL EXPORT")
       ))

       exportObserver<<-observeEvent(input$exportShapefileButton,{
           exportShapefile()
       },ignoreInit=TRUE,once = TRUE)


       observeEvent(input$fileExportInput,{
         fileExportName<<-input$fileExportInput
       },ignoreInit=TRUE)

  },ignoreInit=TRUE)



  observeEvent(input$timezoneSelector,{
      selectedTimezone<<-input$timezoneSelector
  },ignoreInit=TRUE)




observeEvent(input$movebankLoginButton,{
  toggleModal(session,'movebankModal',toggle='open')
},ignoreInit=TRUE)

  observeEvent(input$downloadMovebankDataButton,{
    toggleModal(session,'movebankModal',toggle='close')
    delay(100,
      downloadMovebankData(input$movebankUserInput,input$movebankPasswordInput,input$movebankStudyInput)
    )
  },ignoreInit=TRUE)

  
  

  
  shinyDirChoose(input, "loadProjectButton", roots=volumes, filetypes = NULL,allowDirCreate=FALSE)
  observeEvent(input$loadProjectButton,{
      thisSelectedFolder<-getFolderPathFromShinyDirChoose(volumes,input$loadProjectButton)
      if(!is.null(thisSelectedFolder)){
        appOneReload(thisSelectedFolder)
      }      
  },ignoreInit=TRUE)

  
  


  




  observeEvent(input$maxSpeedSelector,{
      configOptions$maxSpeedParameter<<-input$maxSpeedSelector
      saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$mortDistance,{
      configOptions$mortDistance<<-input$mortDistance
      saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$mortTime,{
      configOptions$mortTime<<-input$mortTime
      saveConfig()
  },ignoreInit=TRUE)

  observeEvent(input$calcMoveParamsButton,{
      findProblemPoints()
  },ignoreInit=TRUE)




  observeEvent(input$getStartedButton, {
    toggleModal(session,'welcomeModal',toggle='close')
  },ignoreInit=TRUE)

  toggleModal(session,'welcomeModal',toggle='open')


  observeEvent(input$parametersButton, {
    toggleModal(session,'configModal',toggle='open')
  },ignoreInit=TRUE)


  shinyDirChoose(input, "chooseDirButton", roots=volumes, filetypes = NULL,allowDirCreate=FALSE)
  observeEvent(input$chooseDirButton, {  
    dataFolder<<-getFolderPathFromShinyDirChoose(volumes,input$chooseDirButton)  
    if(is.null(dataFolder)){
      return()
    }

    print(dataFolder)
    availableShapefiles <<- list.files(dataFolder, pattern = '.shp$')
    if (length(availableShapefiles) == 0) {
      modalMessager(
        "Folder Selection Error",
        "No valid shapefile are present in this directory. Please check the
        directory and try again"
      )
      return
    }
    availableShapefiles <- append("", availableShapefiles)

    ##--------------------------------make a label showing selected folder
    output$selectedDirectoryLabel <- renderUI({
      p(paste("You successfully imported ", dataFolder, sep = ""))
    })

    ##--------------------------------render the dropdown for available shapes
    output$selectedShapefileHeaderLabel <- renderUI({
      strong('(2) Choose shapefile(s) from the selected data directory')
    })
    output$fileUploadSelectorHolder <- renderUI({
      selectInput(
        "fileUploadSelector",
        "",
        availableShapefiles,
        selected = NULL,
        multiple = TRUE
      )
    })

    ##------------------ start file import
    output$fileUploadExecute<-renderUI({
        actionButton('fileUploadExecute','Begin File Import')
    })
})


observeEvent(input$fileUploadExecute, {
    if(exists('importedDatasetMaster')){
      toggleModal(session,'moreDataModal',toggle='toggle')
      return()
    }
    if(is.null(input$fileUploadSelector)){
      modalMessager('Error','You need to select a shapefile to continue import')
      return()
    }
    prepareFileImport()
  })


  dtvRunning<<-FALSE;
  observeEvent(input$processDatesButton,{
          if(!dtvRunning){
            dateTimeValidator()
          }
      })



}


exportShapefile=function(){
    fileExportFolder<-paste0(masterWorkingDirectory,'\\EXPORTS')
    if(dir.exists(fileExportFolder)==FALSE){
      dir.create(fileExportFolder)
    }

    if(file.exists(paste0(fileExportFolder,'\\',fileExportName,'.shp'))){
      time<-Sys.time()
      time<-gsub(" ", "", time, fixed = TRUE)
      time<-gsub("-", "", time, fixed = TRUE)
      time<-gsub(":", "", time, fixed = TRUE)
      fileExportName<<-paste0(fileExportName,'_',time)
    }
    
    tryCatch({
      if('originalProjection' %in% names(configOptions)){
        dataToExport<-spTransform(importedDatasetMaster, CRS(configOptions$originalProjection))
        dataToExport<-dataToExport[,c(configOptions$originalColumns,'problem','mortality','comments')]
      }else{
        dataToExport<-importedDatasetMaster
        dataToExport<-dataToExport[,c(configOptions$originalColumns,'problem','mortality','comments')]
      }
      loadingScreenToggle('show',paste0('exporting file to ',fileExportFolder))
      # riteOGR(dataToExport, fileExportFolder, fileExportName, driver = "ESRI Shapefile")
      st_write(dataToExport,paste0(fileExportFolder,'/',fileExportName,'.shp'))
      modalMessager('File Exported',paste0('File exported succesfully.'))
      loadingScreenToggle('hide',paste0('exporting file to ',exportDirectory))
    }, error = function(ex) {
      print(ex)
      modalMessager('Error',paste0('Try choosing a directory again'))
      loadingScreenToggle('hide',paste0('exporting file to ',fileExportFolder))
    })
  }





clearShapefileSelector<-function(){
    updateSelectInput(session=session, "fileUploadSelector",
      label = "",
      choices = availableShapefiles,
      selected = NULL
    )
  }
