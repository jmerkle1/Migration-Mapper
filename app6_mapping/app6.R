objs <- ls(pos = ".GlobalEnv")
rm(list = objs, pos = ".GlobalEnv")


source("scripts/app6_ui.R",local=TRUE)
source("scripts/app6_mapping.R",local=TRUE)

source("../globalScripts/globalUiFunctions.R",local=TRUE)
source("../globalScripts/sqlLiteQueries.R",local=TRUE)



Sys.setenv(MAPBOX_API_TOKEN = "pk.eyJ1Ijoid21pLW1lcmtsZSIsImEiOiJja3RrYmluMnMxazRlMm9xbnN3bXluYjQzIn0.wOmx_vSC944YRdF8LSjZRQ")



dependencies<-c("shiny","shinyjs","RSQLite",'sf', 'raster', 'mapboxer')
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
    <span style='text-align: center !important; font-size:3rem; width:100% !important; position:absolute !important; top:0px !important; left:0px !important; color:white;>Migration Mapper - Module 6</span>'>
    Migration Mapper 3.1 - App 6
    </div>"),
  actionButton("changeAppsButton", style = "width:15%; font-weight:bolder; position:absolute !important; top:5.5rem !important; left:42.5% !important; border:0px;", "Jump to another Module"),
  actionButton("loadProjectButton", style = "font-weight:bolder; position:absolute !important; top:5px !important; left:-5px !important;", "Reload Existing Project Folder"),
  actionButton("closeMappButton", style = "font-weight:bolder; position:absolute !important; top:5px !important; right:5px !important;", "X - CLOSE MAPP"),
  ),
  # HTML("<div style='width:100%; height:3rem; padding:4rem; font-size:3rem; margin-bottom:2rem; padding-bottom:6rem; background-color:black; color:white; text-align:center !important;'>Migration Mapper 3.0</div>"),
  # actionButton("loadProjectButton", style = "font-weight:bolder; position:absolute !important; top:5px !important; left:25px !important;", "Reload Existing Project Folder"),
  # actionButton("closeMappButton", style = "font-weight:bolder; position:absolute !important; top:5px !important; right:25px !important;", "X - CLOSE MAPP"),
  fluidRow(
    column(12,
      actionButton('basemapButton',style="margin-top:23px !important;",'Toggle Basemap'),
      p('')
    ),
    column(9,
      mapboxerOutput('map', width = "100%",height='70vh')
    ),
    column(3,
      column(12,
        h3("Map Layers")
      ),
      column(12,
        uiOutput("popUseSelector")
      ),
      column(12,
        uiOutput("footPrintsSelector")
      ),
      column(12,uiOutput("sequencesSelector"))
    )
  )
)

appSixReload <- function(filePath){
  loadingScreenToggle('show','loading existing project')
  removeModal()
  rdsLocation<-paste0(filePath,'//workingFile.rds')
  if(file.exists(rdsLocation)){
    workingFile<<-readRDS(rdsLocation)
    importedDatasetMaster<<-workingFile$importedDatasetMaster
    workingFile$masterWorkingDirectory<<-filePath
    masterWorkingDirectory<<-filePath
    sessionInfo<-list()
    sessionInfo$masterWorkingDirectory<-masterWorkingDirectory
    sessionInfo$time<-Sys.time()
    saveTo<-paste0(dirname(getwd()),'//session.rds')
    saveRDS(sessionInfo,saveTo)
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
  mapInit()
}



server <- function(input, output, session) {
  checkForSession('app6')
  app6_init(input, output, session)
}

shiny::devmode(TRUE)
shinyApp(ui, server)
