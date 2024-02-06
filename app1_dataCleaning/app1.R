objs <- ls(pos = ".GlobalEnv")
rm(list = objs, pos = ".GlobalEnv")
rm(list = ls())
gc()

source("scripts/globalVars.R",local=TRUE)
source("scripts/app1_ui.R",local=TRUE)
source("scripts/app1_importFiles.R",local=TRUE)
source("scripts/app1_mergeProjectFiles.R",local=TRUE)
source("scripts/app1_dateTimeFormatting.R",local=TRUE)
source("scripts/app1_calculateMovementParams.R",local=TRUE)
source("scripts/app1_mapImportedData.R",local=TRUE)
source("scripts/app1_summarizeAid.R",local=TRUE)

source("../globalScripts/globalUiFunctions.R",local=TRUE)
source("../globalScripts/sqlLiteQueries.R",local=TRUE)

source("wmiScripts/CalcBurst.R",local=TRUE)
source("wmiScripts/FindProblemPts.R",local=TRUE)
source("wmiScripts/CalcMovParams.R",local=TRUE)
source("wmiScripts/Check4Morts.R",local=TRUE)



dependencies<-c("shiny","sf","circular","shinyjs","shinyBS","ggplot2","mapboxer","adehabitatHR",'RSQLite','move','shinycssloaders','terra','tcltk')
loadDependencies(dependencies)

# lubridate can cause issues when loaded in app2 if the R session is not terminated before reloading app1
if("lubridate" %in% (.packages())){
  detach("package:lubridate", unload=TRUE)
}

Sys.setenv(MAPBOX_API_TOKEN = "pk.eyJ1Ijoid21pLW1lcmtsZSIsImEiOiJja3RrYmluMnMxazRlMm9xbnN3bXluYjQzIn0.wOmx_vSC944YRdF8LSjZRQ")

ui <- fluidPage(
  tags$head(tags$style("body{ overflow-x:hidden}")),  
  uiOutput("loading"),  
  HTML("<div id='loadingScreen' style='width:100%; display:none; height:200%; background-color:rgba(0, 0, 0,0.5); color:white; position:absolute; top:0px; left:0px; z-index:5000;'>
  <div id='loadingMessage' style='position:absolute; top:10%; text-align:center; font-size:15px; color:white; width:100%;'></div>
  <img src='spinner.gif' style='position:absolute; top:25%; left:45%;'>
  </div>"),
  useShinyjs(),

      bsModal("rebuild30modal", "Rebuild Older Migration Mapper Project", NULL, size = "medium",
        p('It appears you are trying to load an older Migration Mapper project. To use your project with this version you will need to rebuild your project folder.'),
        p('To do this, create a new empty project folder and then click the "choose new project folder" button below.'),
        actionButton("confirmProjectRebuild", "choose new project folder"),
        tags$head(tags$style("#moreDataModal .modal-footer{ display:none}"))
      ),

      bsModal("movebankModal", "Movebank Project Download", NULL, size = "medium",
            strong('user name'),
            textInput('movebankUserInput', '', value = "", width = NULL, placeholder = NULL),
            br(),
            strong('password'),
            passwordInput('movebankPasswordInput', '', value = "", width = NULL, placeholder = NULL),
            br(),
            strong('Movebank ID'),
            textInput('movebankStudyInput', '', value = "", width = NULL, placeholder = NULL),
            actionButton("downloadMovebankDataButton", "Download Data"),
            hidden(
              fluidRow( id="downloadSpinner",withSpinner(plotOutput("downloadSpinnerDummy")))
            ),
            tags$head(tags$style("#moreDataModal .modal-footer{ display:none}"))
      ),
      bsModal("configModal", "Configuration Options", NULL, size = "medium",
            p('Adjusting these parameters will influence how mortalities and problem points are flagged in your dataset. If these flags occur, points will not be dropped, they will just be flagged in new columns named "mortality" and "problem".'),
            h4('Maximum Speed',style="background-color:#3b3b3b; color:#d9d9d9; padding:5px;"),
            numericInput("maxSpeedSelector", "Max Speed (km/hr)", 10.8, step=0.1,),
            h4('Mortality Options',style="background-color:#3b3b3b; color:#d9d9d9; padding:5px;"),
            numericInput("mortDistance", "Minimum Distance (meters)", 50),
            numericInput("mortTime", "Time unit (hours)", 48),
            # strong('the button below will rebuild your migtime table used in app2. Note if you have already selected dates, these will all be lost.'),
            hidden(
            fluidRow(id='recalcInstructions',
            p('Press the button below to recalculate movement parameters. Note that any flags already added to your data will be lost.')
            )),
            hidden(actionButton("calcMoveParamsButton", "Recalculate")),
            tags$head(tags$style("#moreDataModal .modal-footer{ display:none}"))
      ),

  column(12,
  HTML("<div style='width:110% !important; margin-left:-3rem !important; height:10rem !important; padding:4rem !important; background-color:black; color:white; text-align:center !important;'>
    <span style='text-align: center !important; font-size:3rem; width:100% !important; position:absolute !important; top:0px !important; left:0px !important; color:white;>Migration Mapper - Module 1</span>'>
    Migration Mapper 3.1 - App 1
    </div>"),
  actionButton("changeAppsButton", style = "width:15%; font-weight:bolder; position:absolute !important; top:5.5rem !important; left:42.5% !important; border:0px;", "Jump to another Module"),
  actionButton("parametersButton", style = "font-weight:bolder; position:absolute !important; top:5px !important; left:-5px !important;", "Configuration Parameters"),
  actionButton("loadProjectButton", style = "font-weight:bolder; position:absolute !important; top:49px !important; left:-5px !important;", "Reload Existing Project Folder"),
  actionButton("closeMappButton", style = "font-weight:bolder; position:absolute !important; top:5px !important; right:5px !important;", "X - CLOSE MAPP"),
  hidden(actionButton("exportDataButton", style = "font-weight:bolder; position:absolute !important; top:49px !important; left:-5px !important;", "Export Updated File"))
  ),
  fluidRow(id='importDataRow',
  column(12,
        HTML(
          "You can upload one ESRI shapefile with many individuals, or multiple
          shapefiles each representing a single individual. If importing unique
          files for each individual, it is necessary that all files have identical
          columns, data formats and projections. If importing a merged file, it
          is necessary that the file includes a column delineating unique animal IDs.
          <br>
          <br>
          You also may way want to review additional parameters by clicking on the
          button titled 'CONFIGURATION PARAMETERS'. This  will open a window which
          allows you to configure the maximum speed that the program will flag as an error as well as the parameters (minimum movement rate over a specified period of time) that the program will flag as a potential mortality event.
          <br>
          <br>
          Using the button below, choose the directory containing
          your dataset(s). If you're uploading multiple files, they must
          all be in the same directory<br><br>
          "
        )
      ),
      column(3,
        strong('(1) Choose directory containing files to import by clicking the button below'),
        br(),
        actionButton("chooseDirButton", "Click to Choose Folder"),
        uiOutput("selectedDirectoryLabel"),
        uiOutput("fileUploadSelectorHolder"),
        uiOutput("fileUploadExecute"),
        strong('Uploaded File(s): Click to delete'),
        uiOutput("uploadedShapefile1"),
        uiOutput("uploadedShapefile2"),
        uiOutput("uploadedShapefile3"),
        uiOutput("uploadedShapefile4"),
        uiOutput("uploadedShapefile5"),
        uiOutput("uploadedShapefile6"),
        uiOutput("uploadedShapefile7"),
        uiOutput("uploadedShapefile8"),
        uiOutput("uploadedShapefile9"),
        uiOutput("uploadedShapefile10"),
        uiOutput("uploadedShapefile11"),
        uiOutput("uploadedShapefile12"),
        uiOutput("uploadedShapefile13"),
        uiOutput("uploadedShapefile14"),
        uiOutput("uploadedShapefile15"),
        uiOutput("uploadedShapefile16"),
        uiOutput("uploadedShapefile17"),
        uiOutput("uploadedShapefile18"),
        uiOutput("uploadedShapefile19"),
        uiOutput("uploadedShapefile20"),
        br(),
        br(),
        strong('Login and retrieve data from Movebank'),
        br(),
        actionButton("movebankLoginButton", "retrieve movebank data")
        ),
      column(12,
        uiOutput("importSuccessText"),
        uiOutput("importSuccessOverview"),
      )
    ),
    fluidRow(id='filesUploadedSection',
    column(12,

    )),
    hidden(
    fluidRow(id='folderSelectSection',
    column(6,
      column(12,
        uiOutput("workingDirectoryTitle"),
        br(),
        uiOutput("chooseWorkingDirButton"),
        uiOutput("selectedWorkingDirectoryLabel")
      )
    ))),
    hidden(
      fluidRow( id="uidSeletorRow",
      column(12,
          HTML('<p>Your dataset(s) have been successfully imported. You now need to
          identify a unique animal identifier. Using the dropdown below, select the
          column that contains a unique ID for each animal in your dataset. This
          will be mandatory if you have imported a single shapefile with merged
          individuals. If you have imported many files with multiple individuals
          and do not have a unique ID column, choose "NaN" from the dropdown, and
          a unique ID will be created from the name of each file.'),
          column(12,
            HTML('<p>Once you make a choice in the dropdown, press the EXECUTE button to
            continue preparing your data.</p>'),
            uiOutput("uniqueIdSelector"),
            uiOutput("uniqueIdSelectorGo"),
            uiOutput("selectedShapefileLabel")
          ),
          column(12,
            tableOutput("aidConfigTable")
          )
        )
    )),
    hidden(
      fluidRow( id="dateTimeRow",
      column(12,
        p('Date/Time data comes in a variety of formats. We understand your
        date/time information could be stored in one column or spread out across
        several columns. To start, pick the column or columns from the list below
        that contain date/time information. Once you finish selecting the columns,
        click "DONE SELECTING DATE COLUMN(S)."'),
        uiOutput("dateColumnSelector"),
        checkboxGroupInput(
          "dateColumnSelector",
          "",
          choiceNames=NULL,
          choiceValues=NULL,
          selected=NULL,
          inline=TRUE
        ),
        actionButton("doneChoosingDateColumnsButton", "DONE SELECTING DATE COLUMN(S).")
      ),
      column(8,align="right",
        tableOutput("dateConfigTable1")
      )
    )
  ),
  hidden(
    fluidRow(id="dateTimeElementsRow",
    column(12,
      HTML('<p>The next step is select which date/time elements are contained
      in each column and in what order they appear. This is important so the App
      can interpret the date/time data correctly.
      <br><br>
      Shown here are menus that allow you to indicate which date/time elements
      are in each column you selected on the previous page. Using the dropdown
      menu chose the elements from each column in the exact order they appear.
      <br><br>
      When choosing a column with multiple date/time elements, delineators can be ignored. This software will
      automatically detect these and parse accordingly.
      <br><br>
      Once you are ready to proceed click the "PROCESS DATES" button.
      <br><br>
      Note that when you click the process dates button, certain points and individuals
      may be removed from your dataset. For example, if an individual had unreasonable speeds
      or was categorized as mortalities, they could be removed from your dataset. It is recomended
      that you review the configuration tab before clicking the process dates button.
      </p>'
      ),
      uiOutput("timeProcessingResults")
    ),
    column(12,
      uiOutput("dateConfigUi1"),
      uiOutput("dateConfigUi2"),
      uiOutput("dateConfigUi3"),
      uiOutput("dateConfigUi4"),
      uiOutput("dateConfigUi5"),
      uiOutput("dateConfigUi6"),
      uiOutput("dateConfigUi7"),
      uiOutput("dateConfigUi8"),
      uiOutput("dateConfigUi9"),
      uiOutput("dateConfigUi10"),
      uiOutput("dateConfigUi11"),
      uiOutput("dateConfigUi12"),
      uiOutput("dateSeperatorSelector"),
      uiOutput("timeSeperatorSelector"),
      HTML('Time zone will default to GMT. If you would like to format your times using another timezone, choose the UTC offset associated with you timezone below'),
      selectizeInput('timezoneSelector', 'What time zone is your data stored in (default is GMT)?', OlsonNames(), selected = 'GMT', multiple = FALSE, options = NULL),
      uiOutput("timeFormatResults"),
      actionButton("processDatesButton", "PROCESS DATES")
    ),
    column(12,
      tableOutput("dateConfigTable2")
    )
  )
),
hidden(
  fluidRow( id="importedDataMapRow",
  fixedRow(

  ),
  tags$head(tags$style(HTML('
      .modal-lg {
      position: absolute !important;
      right:10px !important;
      width:40% !important;
      }
    '))),
    # tags$head(tags$script(src="js/mapboxer.js")),
    tags$head(tags$script(src="https://api.mapbox.com/mapbox-gl-js/plugins/mapbox-gl-draw/v1.2.2/mapbox-gl-draw.js")),
    tags$link(rel = "stylesheet", type = "text/css", href = "https://api.mapbox.com/mapbox-gl-js/plugins/mapbox-gl-draw/v1.2.2/mapbox-gl-draw.css"),
  bsModal("pointClickModal", "PointData", NULL, size = "large",
        column(5,
          actionButton("previousPointButton", "← previous point"),
        ),
        column(2,
        ),
        column(5,
          actionButton("nextPointButton", "next point →"),
        ),
        column(6,
        selectInput('isMortalitySelector', 'Is this point classified as mortality?',c('yes','no')),
        ),
        column(6,
        selectInput('isProblemSelector', 'Is this point classified as a problem point?',c('yes','no')),
        ),
        column(12,
        textInput('commentInput', 'Comments for this point'),
        ),
        uiOutput("pointClickData")
  ),
  bsModal("manyPointsSelectedModal", "Points Selected", NULL, size = "large",
        column(12,
          p('You have made a selection of many points. You can do a batch action and classify all these points as mortalities, problems or add comments to all points. **This action is not reversable**'),
        ),
        column(6,
          selectInput('manyPointsIsMortalitySelector', 'Reclassify all these points as mortalities?',c('','yes','no'),selected=''),
        ),
        column(6,
          selectInput('manyPointsIsProblemSelector', 'Reclassify all these points as problem points?',c('','yes','no'),selected=''),
        ),
        column(12,
          textInput('manyPointsCommentInput', 'Comments for this group of points'),
        )
  ),

  column(12,
    column(7,
          column(12,
            column(12,
            p('Your data have been succesfully imported and prepared for additional analysis.
            You can view individuals belows and add notes to points or mark points as being mortalities or problems.
            You can close this window at any time or choose to export a shapefile using the "export project file" button above.')
            ),
            column(2,
              actionButton('backwardHandlerButton',style="margin-top:23px !important;",'Previous Animal ←')
            ),
            column(2,
              selectInput('individualsSelector', 'Choose animal', multiple = FALSE, c())
            ),
            column(2,
              actionButton('forwardHandlerButton',style="margin-top:23px !important;",'Next Animal →')
            ),
            column(2,
              selectInput('yearSelector', 'Choose year', multiple = FALSE, c())
            ),
            column(2,
              actionButton('basemapButton',style="margin-top:23px !important;",'Toggle Basemap')
            ),
            column(2,
            ),
            column(12,
              p('Click on a point in the right hand figures to zoom in on the map. Click on a point (or select multiple points using the polygon tool in upper left corner of the map) on the map to view information about that point and/or mark it as a problem or mortality, or simply add a comment.'),
              HTML('<div>Points that were identified as problem points are shown in <strong style="color:#dd00ff !important; background-color:black !important;  padding:3px !important;">magenta</strong> and those points classified as mortalities are shown in <strong style="color:#dfff00 !important; background-color:black !important; padding:3px !important;">yellow</strong></div>'),
            ),
            uiOutput('polygonHolder'),
            mapboxerOutput('importedDataMapBox', width = "100%",height='60vh'),
            column(3,
              h4('mcp info'),
              uiOutput("mcpInfo")
            ),
            column(3,
              h4('fix rate'),
              uiOutput("fixRateInfo")
            ),
            column(3,
              h4('points per time'),
              uiOutput("pointsPerTimeInfo")
            ),
            column(3,
              h4('date range info'),
              uiOutput("dateRangeInfo")
            )
          )
     ),
     column(5,
       plotOutput("speedPlot",height='25vh',click = "plot_click",hover = hoverOpts(id = "plot_hover", delay = 0)),
        plotOutput("fixRatePlot",height='25vh',click = "plot_click",hover = hoverOpts(id = "plot_hover", delay = 0)),
        plotOutput("nsdPlot",height='25vh',click = "plot_click",hover = hoverOpts(id = "plot_hover", delay = 0))

     )
   ),
)
)
)

server <- function(input, output, session) {
  app1_init(input, output, session)
  # elevation<<-raster("globalDem/etopocompressed.tif")
  elevation<<-rast("globalDem/etopocompressed.tif")
  checkForSession('app1')
  hide(id='dateTimeRow')
  onStop(function() {
   stopApp()
 })
}

appOneReload <- function(filePath){
  rdsLocation<-paste0(filePath,'//workingFile.rds')
  print(rdsLocation)
  if(file.exists(rdsLocation)){
    loadingScreenToggle('show','loading existing project file')
    workingFile<<-readRDS(rdsLocation)
    importedDatasetMaster<<-workingFile$importedDatasetMaster
    masterWorkingDirectory<<-filePath
    if(typeof(importedDatasetMaster)=='S4'){
        loadingScreenToggle('hide','')
        toggleModal(session,'rebuild30modal',toggle='open')
      }else{
        workingFile$masterWorkingDirectory<<-filePath
        masterWorkingDirectory<<-filePath
        loadConfig()
        dbConnection <<- dbConnect(RSQLite::SQLite(), paste0(masterWorkingDirectory,'//workingDb.db'))
        updateMasterTableFromDatabase()
        removeModal()
        # print(!'newMasterDate'%in%names(importedDatasetMaster@data))
        print(!'newMasterDate'%in%names(importedDatasetMaster))
        hideElement(id = 'importDataRow', anim = TRUE)
        showElement(id = 'importedDataMapRow', anim = TRUE)
        hide('loadProjectButton')
        showElement('exportDataButton')
        mapInit()
        loadingScreenToggle('hide','')
        saveWorkingFile();
      }    
  }else{
    modalMessager('Error',paste0('Data file from this session does not exist at ',filePath,'. Please try loading the data file manually using the "Reload Existing Project Folder" button.'))
    sessionCheckLocation<-paste0(dirname(getwd()),'//session.rds')
    file.remove(sessionCheckLocation)
  }
}

loadConfig<-function(){
  configOptions<<-readRDS(paste0(masterWorkingDirectory,'//configOptions.rds'))
  configOptions$masterWorkingDirectory<<-masterWorkingDirectory
  updateNumericInput(session, 'maxSpeedSelector', value= configOptions$maxSpeedParameter )
  updateNumericInput(session, 'mortDistance', value= configOptions$mortDistance)
  updateNumericInput(session, 'mortTime', value=configOptions$mortTime)
}

rebuildOlderProject<-function(){
  print('rebuild here')

  newProjectFolder <- choose.dir(caption = "select your project folder and press OK")
  files<-list.files(newProjectFolder)
  if(length(files)>0){
    delay(100,
      modalMessager('error','The folder you chose is not empty.
      Please empty the folder or choose a different directory and try again.')        
    )    
    return()
  }

  print('migtime exists')
  print(file.exists(paste0(masterWorkingDirectory,'//migtime.rds')))

  if(file.exists(paste0(masterWorkingDirectory,'//migtime.rds'))){    
    migtime<<-readRDS(paste0(masterWorkingDirectory,'//migtime.rds'))    
    saveRDS(migtime,paste0(newProjectFolder,'//migtime.rds'))
  }

  toggleModal(session,'rebuild30modal',toggle='close')
  configOptions<<-readRDS(paste0(masterWorkingDirectory,'//configOptions.rds'))
  importedDatasetMaster<<-data.frame(importedDatasetMaster)  
  crs4326<-crs(elevation)
  configOptions$masterCrs4326<<-crs4326
  midLatLong <- c(importedDatasetMaster[1,]$lat,importedDatasetMaster[1,]$lon)
  zone <- find_UTM_zone(midLatLong[2], midLatLong[1])
  UTMcrs <- paste0("+proj=utm +zone=", zone, " +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")  
  configOptions$masterCrs<<-UTMcrs
  masterWorkingDirectory<<-newProjectFolder
  if(exists('migtime')){
    saveMigtime()
  }
  

  

  saveWorkingFile()
  saveConfig()

  mapInit()



  # importedDatasetMaster<<-data.frame(importedDatasetMaster)  
  # configOptions<<-readRDS(paste0(masterWorkingDirectory,'//configOptions.rds'))
  # configOptions$masterWorkingDirectory<<-masterWorkingDirectory

  if(1==2){
    workingFile$masterWorkingDirectory<<-filePath
    masterWorkingDirectory<<-filePath
    loadConfig()
    dbConnection <<- dbConnect(RSQLite::SQLite(), paste0(masterWorkingDirectory,'//workingDb.db'))
    updateMasterTableFromDatabase()
    removeModal()
    hideElement(id = 'importDataRow', anim = TRUE)
    showElement(id = 'importedDataMapRow', anim = TRUE)
    hide('loadProjectButton')
    showElement('exportDataButton')
    mapInit()
    loadingScreenToggle('hide','')
    saveWorkingFile();
  }  
}


shiny::devmode(TRUE)
shinyApp(ui, server)
