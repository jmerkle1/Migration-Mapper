objs <- ls(pos = ".GlobalEnv")
rm(list = objs, pos = ".GlobalEnv")


source("scripts/app5_ui.R",local=TRUE)
source("scripts/app5_calcPopUseFootprints.R",local=TRUE)

source("../globalScripts/globalUiFunctions.R",local=TRUE)
source("../globalScripts/sqlLiteQueries.R",local=TRUE)


source("wmiScripts\\CalcPopUse.R")
source("wmiScripts\\CalcPopFootprint.R")


dependencies<-c("shiny","shinyjs","RSQLite",'sf', 'raster', 'stringr', 'move', 'smoothr', 'rgeos','shinyBS','shinyFiles')
loadDependencies(dependencies)

ui <- fluidPage(
  tags$head(tags$style("body{ overflow-x:hidden}")),
  HTML("<div id='loadingScreen' style='width:100%; display:none; height:200%; background-color:rgba(0, 0, 0,0.5); color:white; position:absolute; top:0px; left:0px; z-index:5000;'>
  <div id='loadingMessage' style='position:absolute; top:10%; text-align:center; font-size:15px; color:white; width:100%;'></div>
  <img src='spinner.gif' style='position:absolute; top:25%; left:45%;'>
  </div>"),
  useShinyjs(),
  column(12,
  HTML("<div style='width:110% !important; margin-left:-3rem !important; height:10rem !important; padding:4rem !important; background-color:black; color:white; text-align:center !important;'>
    <span style='text-align: center !important; font-size:3rem; width:100% !important; position:absolute !important; top:0px !important; left:0px !important; color:white;>Migration Mapper - Module 5</span>'>
    Migration Mapper 3.1 - App 5
    </div>"),
  actionButton("changeAppsButton", style = "width:15%; font-weight:bolder; position:absolute !important; top:5.5rem !important; left:42.5% !important; border:0px;", "Jump to another Module"),
  shinyDirButton("loadProjectButton", "Reload Existing Project Folder", "Please select a directory",style = "font-weight:bolder; position:absolute !important; top:5px !important; left:-5px !important;"),
  actionButton("closeMappButton", style = "font-weight:bolder; position:absolute !important; top:5px !important; right:5px !important;", "X - CLOSE MAPP")
  ),
  fluidRow(
  column(12,
    p('App 5 allows you to merge together your sequence-level outputs from App4 to create population level outputs that identify where the majority of a population moves or spends time.'),
    column(6,
      p("To start, choose the season or seasons from the dropdown below which you would like to merge together to create a final output"),
      selectInput('seasonsToMergeInput',
      # 'Information criteria used for model selection. Can be "AICc", "AIC", "BIC", "LOOCV" or none (NA). Use LOOCV for 12 hour data. Use something else for more frequent fixes. AIC is default.',
        label=NULL,
        multiple=T,
        c()
      ),
      p("There are often multiple hierarchies (e.g., season, individual, year) to deal with when merging, and in some cases the order of merging can matter, particularly for high use area calculation (e.g., merge by year first, and then by animal ID?). Thus, please specify the order that you would like to merge the products from App4. Merge order can have 1-2 of the following values c(year, id, seas)."),
      selectInput('mergeOrderInput',
        label=NULL,
        multiple=TRUE,
        c('year','id','seas'),
        selected=c('year')
      )
    ),
    hidden(
        fluidRow( id="dropConfigRow",
        column(6,
          selectInput('udFootprintsToDropSelect',
            label='Choose any UDs or footprints you would like dropped from merging here',
            multiple=TRUE,
            choices=c('')
          ),
        )
      )
    )
  ),
  column(12,
    actionButton("beginMerginButton", "Begin Merging", style="color: #ffffff; background-color: #15006f; border-color: #000000; padding:1rem;")
  ),
    column(12,
      h4('Configuration Parameters'),
      actionButton("resetConfigOptionsButton", "Reset all parameters to default")
    ),
    column(6,

      h4('Contour Levels - Population Use'),
      p('Used for population use merging. Percents (0 < 100) of contours. Do not include a 0. Must be ascending. Must be seperated with commas. Represents contours of volume or area of use.'),
      textInput('contourLevelsPopUseInput',
        label=NULL,
        value='5,10,15,20,30'),
      actionButton("processContoursPopUseButton", "Update Contour Values"),

      h4('Contour Type'),
      p("How should the contours be calculated? Applies only to population use contours. Contours with Area represent the % of individuals. Contours with volume represent the % of time spent in areas."),
      selectInput('contourTypeInput', label=NULL, c('Area','Volume'),selected='Area'),

      h4('Contour Levels - Population Footprints'),
      p('Used for population footprints merging. Percents (0 < 100) of contours. Do not include a 0. Must be ascending. Must be seperated with commas. Represents contours of volume or area of use.'),
      textInput('contourLevelsPopFootprintsInput',
        label=NULL,
        value='5,10,15,20,30'),
      actionButton("processContoursPopFootprintsButton", "Update Contour Values"),



      h4('Min Area Drop'),
      p('Do you want to remove some small polygons from the output? Numeric value in square meters.'),
      numericInput('minAreaDropInput',
      # 'The margin used for the behavioral change point analysis. This number has to be odd. See ??brownian.bridge.dyn for details.',
      label=NULL,
      value=20000,min = 1, max = 100000, step = 1,),

      h4('Min Area Fill'),
      p('Do you want to fill in holes with areas smaller than this value? Numeric value in square meters.'),
      numericInput('minAreaFillInput',
      # 'The margin used for the behavioral change point analysis. This number has to be odd. See ??brownian.bridge.dyn for details.',
      label=NULL,
      value=20000,min = 1, max = 100000, step = 1,)

    ),
    column(6,


          h4('Simplify Contours'),
          p("Do you want to smooth the contours so they aren't gridlooking? TRUE or FALSE"),
          selectInput('simplifyInput', label=NULL,c(TRUE,FALSE)),

          h4('KSmooth Smoothness'),
          p("If simplify is TRUE, smoothness value of the kernel smoothing (i.e., simplification). See smooth() function in smoothr package."),
          numericInput('kSmoothSmoothnessInput',
          # 'The margin used for the behavioral change point analysis. This number has to be odd. See ??brownian.bridge.dyn for details.',
          label=NULL,
          value=2,min = 0.01, max = 1000, step = 0.01,)

    )





  )
)

appFiveReload <- function(filePath){
  loadingScreenToggle('show','loading existing project')
  removeModal()
  rdsLocation<-paste0(filePath,'//workingFile.rds')
  if(file.exists(rdsLocation)){
    workingFile<<-readRDS(rdsLocation)
    importedDatasetMaster<<-workingFile$importedDatasetMaster
    # masterWorkingDirectory<<-workingFile$masterWorkingDirectory
    workingFile$masterWorkingDirectory<<-filePath
    masterWorkingDirectory<<-filePath
    sessionInfo<-list()
    sessionInfo$masterWorkingDirectory<-masterWorkingDirectory
    sessionInfo$time<-Sys.time()
    saveTo<-paste0(dirname(getwd()),'//session.rds')
    saveRDS(sessionInfo,saveTo)
    # getSequences()
  }else{
    modalMessager('Error',paste0('Data file from this session does not exist at ',filePath,'. Please try loading the data file manually using the "Reload Existing Project Folder" button.'))
    sessionCheckLocation<-paste0(dirname(getwd()),'//session.rds')
    file.remove(sessionCheckLocation)
  }
  loadingScreenToggle('hide','')
  loadConfig()

  buildSequencesButtons()
}

loadConfig<-function(){
  configOptions<<-readRDS(paste0(masterWorkingDirectory,'//configOptions.rds'))
  configOptions$masterWorkingDirectory<<-masterWorkingDirectory
  if('popConfigOption'%in%names(configOptions)){
    reloadUdPopConfigOptions()
  }else{
    udPopConfigOptionsInit()
  }
}



server <- function(input, output, session) {
  checkForSession('app5')

  app5_init(input, output, session)
}

shiny::devmode(FALSE)
shinyApp(ui, server)
