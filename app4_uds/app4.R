objs <- ls(pos = ".GlobalEnv")
rm(list = objs, pos = ".GlobalEnv")


source("scripts/app4_ui.R",local=TRUE)
source("scripts/app4_runUd.R",local=TRUE)
source("../globalScripts/globalUiFunctions.R",local=TRUE)
source("../globalScripts/sqlLiteQueries.R",local=TRUE)


source("wmiScripts\\CalcPopGrid.R")
source("wmiScripts\\CalcSeqDistances.R")
source("wmiScripts\\CalcBBMM.R")
source("wmiScripts\\CalcDBBMM.R")
source("wmiScripts\\CalcKernel.R")
source("wmiScripts\\CalcLineBuff.R")
source("wmiScripts\\CalcCTMM.R")

# dependencies<-c("shiny","shinyjs","parallel","RSQLite","adehabitatHR", "R.utils","BBMM","R.utils","dplyr", "ctmm", "move","sf","raster","sp","fields",'shinyBS','shinyFiles')
dependencies<-c("shiny","shinyjs","parallel","RSQLite","adehabitatHR", "R.utils","BBMM","R.utils","dplyr", "ctmm", "move","sf","fields",'shinyBS','shinyFiles')
loadDependencies(dependencies)

ui <- fluidPage(
  tags$head(tags$style("body{ overflow-x:hidden}")),
  HTML("<div id='loadingScreen' style='width:100%; display:none; height:200%; background-color:rgba(0, 0, 0,0.5); color:white; position:absolute; top:0px; left:0px; z-index:500;'>
  <div id='loadingMessage' style='position:absolute; top:10%; text-align:center; font-size:15px; color:white; width:100%;'></div>
  <img src='spinner.gif' style='position:absolute; top:25%; left:45%;'>
  </div>"),
  useShinyjs(),
  column(12,
  HTML("<div style='width:110% !important; margin-left:-3rem !important; height:10rem !important; padding:4rem !important; background-color:black; color:white; text-align:center !important;'>
    <span style='text-align: center !important; font-size:3rem; width:100% !important; position:absolute !important; top:0px !important; left:0px !important; color:white;>Migration Mapper - Module 4</span>'>
    Migration Mapper 3.1 - App 4
    </div>"),
  actionButton("changeAppsButton", style = "width:15%; font-weight:bolder; position:absolute !important; top:5.5rem !important; left:42.5% !important; border:0px;", "Jump to another Module"),
  # actionButton("loadProjectButton", style = "font-weight:bolder; position:absolute !important; top:5px !important; left:-5px !important;", "Reload Existing Project Folder"),
  shinyDirButton("loadProjectButton", "Reload Existing Project Folder", "Please select a directory",style = "font-weight:bolder; position:absolute !important; top:5px !important; left:-5px !important;"),
  actionButton("closeMappButton", style = "font-weight:bolder; position:absolute !important; top:5px !important; right:5px !important;", "X - CLOSE MAPP")
  ),
  # HTML("<div style='width:100%; height:3rem; padding:4rem; font-size:3rem; margin-bottom:2rem; padding-bottom:6rem; background-color:black; color:white; text-align:center !important;'>Migration Mapper 3.0</div>"),
  # actionButton("loadProjectButton", style = "font-weight:bolder; position:absolute !important; top:5px !important; left:25px !important;", "Reload Existing Project Folder"),
  # actionButton("closeMappButton", style = "font-weight:bolder; position:absolute !important; top:5px !important; right:25px !important;", "X - CLOSE MAPP"),
  fluidRow(

  column(12,

  column(8,
    p('For each sequence you exported in App3, you can use a movement model to calculate an occurrence distribution (or utilization distribution) and footprint. The many options to customize your movement models are below. By default Migration Mapper uses a regular Brownian Bridge Movement Model to calculate your occurrence distribution. However you can choose from the following methods below:'),
    p('BBMM - Brownian Bridge Movement Model (from BBMM package) or a variant where you can fix the motion variance for BBMM.'),
    p('DBBMM - Dynamic Brownian Bridge Movement Model (from Move package)'),
    p('Kernel UD - Bivariate normal kernel Utilization Distribution (from adehabitatHR package), including an option to subsample data beforehand.'),
    p('CTMM - Continuous Time Movement Model (from package ctmm) and calculate the occurrence distribution.'),
    p('Line Buffer - Connect points with strait lines and then buffer lines based on user specified value. Note - this method does not provide an occurrence distribution, only a footprint.'),
  ),
  column(4,
    p("Process all sequences by pressing the button below"),
    actionButton("processAllButton", "Process All Sequences" ,style="color: #ffffff; background-color: #15006f; border-color: #000000; padding:1rem;"),
    p(''),
    p('You can also process just one of the sequences at a time by clicking the appropriate button below'),
    uiOutput("sequenceButtonHolder1"),
    uiOutput("sequenceButtonHolder2"),
    uiOutput("sequenceButtonHolder3"),
    uiOutput("sequenceButtonHolder4"),
    uiOutput("sequenceButtonHolder5"),
    uiOutput("sequenceButtonHolder6"),
    uiOutput("sequenceButtonHolder7"),
    uiOutput("sequenceButtonHolder8")
  ),

  column(12,
  h4('UD Method'),
  p('What method would you like to use to calculate UDs?'),
  selectInput('udMethodInput', label=NULL,c('BBMM','dBBMM','kernel','CTMM','Line Buffer')),
  )

  ),

  column(7,

    column(12,
      h3('Configuration Parameters'),
      actionButton("resetConfigOptionsButton", "Reset all parameters to default")
    ),

    column(6,
      # ------------------------
      hidden(
        fluidRow( id="numberOfCoresRow",
        h4('Number of Cores For Processing - applies to all methods'),
        p(paste0("How many cores would you like to use for processing? R has detected that your machine has a total of ",detectCores()," cores. Default selection is total available-1. Using all available cores on your machine is not recomended")),
        numericInput('numberOfCoresInput',
        label=NULL,
        value=detectCores() - 1,min = 1, max = detectCores(), step = 1)
        )),
      # ------------------------
      hidden(
        fluidRow( id="mult4buffRow",
        h4('mult4buff - applies to population grid, BBMM, DBBMM, Kernel, CTMM'),
        p('mult4buff - Proportion of current extent or bbox to add for building the grid. Typically 0.2 or 0.3.'),
        numericInput('mult4buffInput',
        label=NULL,
        value=NULL,min = 0, max = 0.99, step = 0.01)
      )),
      # ------------------------
      hidden(
        fluidRow( id="cellSizeRow",
        h4('Cell Size - applies to all methods'),
        p('Cell Size - Numeric providing the cell or grid size (in meters) for output.'),
        numericInput('cellSizeInput',
        label=NULL,
        value=NULL,min = 1, max = 10000, step = 1)
      )),
      # -------------------------------
      hidden(
        fluidRow( id="timeStepRow",
        h4('Time Step - applies to BBMM'),
        p('Time Step - How often (in minutes) that the BB integrates between sequential points.'),
        numericInput('timeStepInput',
        label=NULL,
        value=NULL,min = 0, max = 100, step = 1)
      )),
      # ------------------------
      hidden(
        fluidRow( id="buffRow",
        h4('Line Buffer Radii - applies to line buffer'),
        p('Numeric value indicating the distance (in meters) to buffer lines (acts as radii).'),
        numericInput('buffInput',
        label=NULL,
        value=NULL,min = 1, max = 10000, step = 1)
      )),
      # -------------------------------
      hidden(
        fluidRow( id="dbmmRow",
        h4('dbbmm margin - applies to DBBMM'),
        p('The margin used for the behavioral change point analysis. This number has to be odd. See ??brownian.bridge.dyn for details.'),
        numericInput('dbbmmMarginInput',
        label=NULL,
        value=NULL,min = 1, max = 99, step = 2)
      )),
      # -------------------------------
      hidden(
        fluidRow( id="dbmmWindowRow",
        h4('dbbmm window - applies to DBBMM'),
        p('The size of the moving window along the track. See ??brownian.bridge.dyn for details.'),
        numericInput('dbbmmWindowInput',
        label=NULL,
        value=NULL,min = 1, max = 99, step = 1)
      ))



    ),
    column(6,
      # -------------------------------
      hidden(
        fluidRow( id="BMVarRow",
        h4('BMVar - applies to BBMM'),
        p('If left blank, will run regular BB and calculate motion variance. If a number is specified, it will invoke the Forced motion variance method (values are typically between 1000 and 3000).'),
        numericInput('BMVarInput',
        label=NULL,
        value=NULL,min = 1, max = 10000, step = 1)
      )),
      # -------------------------------
      hidden(
        fluidRow( id="locationErrorRow",
        h4('Location Error - applies to BBMM, DBBMM'),
        p('Location error of your GPS data (in meters).'),
        numericInput('locationErrorInput',
        label=NULL,
        value=NULL,min = 0, max = 10000, step = 1)
      )),
      # -------------------------------
      hidden(
        fluidRow( id="maxLagRow",
        h4('max.lag - applies to BBMM, DBBMM'),
        p('Maximum amount of time (in hours) to allow any two sequential points to be connected when fitting BBMMs and DBBMMs. This is important parameter if you do not have fine scale movement data! Default is 8 hours.'),
        numericInput('maxLagInput',
        label=NULL,
        value=NULL,min = 0, max = 10000, step = 1)
      )),
      # -------------------------------
      hidden(
        fluidRow( id="contourRow",
        h4('Contour Level - applies to BBMM, DBBMM, Kernel UD, CTMM'),
        p('Contour level used to create the footprint from the UD or occurrence distribution (numeric between 1 and 99.999). Typically 95, 99, or 99.9.'),
        numericInput('contourInput',
        # 'Contour level used to create the footprint from the UD or occurrence distribution (numeric between 1 and 99.999). Typically 95, 99, or 99.9.',
        label=NULL,
        value=NULL,min = 0, max = 99.9, step = 0.1)
      )),
      # -------------------------------
      hidden(
        fluidRow( id="maxTimeoutRow",
        h4('max.timeout - applies to BBMM, DBBMM, Kernel UD, CTMM'),
        p('# Max amount of time (in minutes) it should run before timing out. Default is 720 minutes (12 hours)'),
        numericInput('maxTimeoutInput',
        label=NULL,
        value=NULL,min = 1, max = 10000, step = 1)
      )),
      # -------------------------------
      hidden(
        fluidRow( id="smoothParamRow",
        h4('Smooth Param - applies to Kernel UD'),
        p('If left blank (the default), will evoke the "href" method. If numeric, will force the given smoothing parameter.'),
        numericInput('smoothParamInput',
        label=NULL,
        value=NULL,min = 1, max = 99, step = 1)
      )),
      hidden(
        fluidRow( id="subsampleRow",
        h4('subsample - applies to Kernel UD'),
        p('If left blank (the default), no subsampling will take place. If numeric, will be used as the number of random locations to sample per julian day in seq.sf.'),
        numericInput('subsampleInput',
        label=NULL,
        value=NULL,min = 1, max = 99, step = 1)
      )),
      hidden(
        fluidRow( id="informationCriteriaRow",
        h4('Information Criteria - applies to CTMM'),
        p('Information criteria used for model selection. Can be "AICc", "AIC", "BIC", "LOOCV" or none (NA). Use LOOCV for 12 hour data. Use something else for more frequent fixes. AIC is default.'),
        selectInput('informationCriteriaInput',
          label=NULL,
          c('AICc','AIC','BIC','LOOCV','NA')
        )
      ))
    )
  ),
  column(5,
    p("After processing completes, results and metadata will appear in the table below"),
    div(style = 'overflow-x: auto', tableOutput('resultsTable'))
    # tableOutput("resultsTable", style= "hoverflow-x: scroll;")
  ),
  )
)

appFourReload <- function(filePath){
  loadingScreenToggle('show','loading existing project')
  removeModal()
  rdsLocation<-paste0(filePath,'//workingFile.rds')
  if(file.exists(rdsLocation)){
    workingFile<<-readRDS(rdsLocation)    
    importedDatasetMaster<<-workingFile$importedDatasetMaster
    # masterWorkingDirectory<<-workingFile$masterWorkingDirectory
    workingFile$masterWorkingDirectory<<-filePath
    configOptions<<-readRDS(paste0(filePath,'//configOptions.rds'))
    masterWorkingDirectory<<-filePath
    dbConnection <<- dbConnect(RSQLite::SQLite(), paste0(masterWorkingDirectory,'//workingDb.db'))
    updateMasterTableFromDatabase()

    # -----------------------------
    # -----------------------------
    # DROPPING PROBLEMS AND MORTALITIES BY DEFAULT
    # -----------------------------
    # -----------------------------
    # importedDatasetMasterAsSf<<-st_as_sf(importedDatasetMaster[which(importedDatasetMaster@data$problem != 1 & importedDatasetMaster@data$mortality != 1),])
    importedDatasetMasterAsSf<<-importedDatasetMaster[which(importedDatasetMaster$problem != 1 & importedDatasetMaster$mortality != 1),]
    importedDatasetMasterAsSf<<-st_as_sf(importedDatasetMasterAsSf,coords = c("x", "y"), crs = configOptions$masterCrs)
    


    sessionInfo<-list()
    sessionInfo$masterWorkingDirectory<-masterWorkingDirectory
    sessionInfo$time<-Sys.time()
    saveTo<-paste0(dirname(getwd()),'//session.rds')
    saveRDS(sessionInfo,saveTo)
    getSequences()
  }else{
    modalMessager('Error',paste0('Data file from this session does not exist at ',filePath,'. Please try loading the data file manually using the "Reload Existing Project Folder" button.'))
    sessionCheckLocation<-paste0(dirname(getwd()),'//session.rds')
    file.remove(sessionCheckLocation)
  }
  loadingScreenToggle('hide','')
  loadConfig()
}

loadConfig<-function(){
  configOptions<<-readRDS(paste0(masterWorkingDirectory,'//configOptions.rds'))
  configOptions$masterWorkingDirectory<<-masterWorkingDirectory
  if('udConfigOptions'%in%names(configOptions)){
    reloadUdConfigOptions()
  }else{
    udConfigOptionsInit()
  }
}



server <- function(input, output, session) {
  checkForSession('app4')

  app4_init(input, output, session)
}

shiny::devmode(TRUE)
shinyApp(ui, server)
