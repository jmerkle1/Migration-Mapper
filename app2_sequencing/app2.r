objs <- ls(pos = ".GlobalEnv")
rm(list = objs, pos = ".GlobalEnv")
rm(list = ls())
gc()


source("scripts/app2_ui.R",local=TRUE)
source("scripts/app2_timeFunctions.R",local=TRUE)
source("scripts/app2_mapping.R",local=TRUE)
source("scripts/app2_plotting.R",local=TRUE)
source("../globalScripts/globalUiFunctions.R",local=TRUE)
source("../globalScripts/sqlLiteQueries.R",local=TRUE)

Sys.setenv(MAPBOX_API_TOKEN = "pk.eyJ1Ijoid21pLW1lcmtsZSIsImEiOiJja3RrYmluMnMxazRlMm9xbnN3bXluYjQzIn0.wOmx_vSC944YRdF8LSjZRQ")

dependencies<-c("shiny","sf","shinyjs","shinyBS","sp","ggplot2","mapboxer","rgdal","RSQLite","adehabitatLT")
loadDependencies(dependencies)


ui <- fluidPage(
  tags$head(tags$style("body{ overflow-x:hidden}")),
  HTML("<div id='loadingScreen' style='width:100%; display:none; height:200%; background-color:rgba(0, 0, 0,0.5); color:white; position:absolute; top:0px; left:0px; z-index:1000;'>
  <div id='loadingMessage' style='position:absolute; top:10%; text-align:center; font-size:15px; color:white; width:100%;'></div>
  <img src='spinner.gif' style='position:absolute; top:25%; left:45%;'>
  </div>"),
  useShinyjs(),
  bsModal("configModal", "Confiuration Options", NULL, size = "large",
        actionButton("rebuildMigtimeButton", "Rebuild Migtime Table"),
        tags$head(tags$style("#moreDataModal .modal-footer{ display:none}"))
  ),

  column(12,
  HTML("<div style='width:110% !important; margin-left:-3rem !important; height:10rem !important; padding:4rem !important; background-color:black; color:white; text-align:center !important;'>
    <span style='text-align: center !important; font-size:3rem; width:100% !important; position:absolute !important; top:0px !important; left:0px !important; color:white;>Migration Mapper - Module 2</span>'>
    Migration Mapper 3.1 - App 2
    </div>"),
  actionButton("changeAppsButton", style = "width:15%; font-weight:bolder; position:absolute !important; top:5.5rem !important; left:42.5% !important; border:0px;", "Jump to another Module"),
  actionButton("parametersButton", style = "font-weight:bolder; position:absolute !important; top:5px !important; left:-5px !important;", "Configuration Parameters"),
  actionButton("loadProjectButton", style = "font-weight:bolder; position:absolute !important; top:49px !important; left:-5px !important;", "Reload Existing Project Folder"),
  actionButton("closeMappButton", style = "font-weight:bolder; position:absolute !important; top:5px !important; right:5px !important;", "X - CLOSE MAPP"),
  actionButton("exportMigtimeButton", style = "font-weight:bolder; position:absolute !important; top:49px !important; right:5px !important;", "Export Migtime Table")
  ),

  # HTML("<div style='width:100%; height:3rem; padding:4rem; font-size:3rem; margin-bottom:2rem; padding-bottom:6rem; background-color:black; color:white; text-align:center !important;'>Migration Mapper 3.0</div>"),
  # actionButton("parametersButton", style = "font-weight:bolder; position:absolute !important; top:5px !important; left:25px !important;", "Configuration Parameters"),
  # actionButton("loadProjectButton", style = "font-weight:bolder; position:absolute !important; top:49px !important; left:25px !important;", "Reload Existing Project Folder"),
  # actionButton("closeMappButton", style = "font-weight:bolder; position:absolute !important; top:5px !important; right:25px !important;", "X - CLOSE MAPP"),
  # actionButton("exportMigtimeButton", style = "font-weight:bolder; position:absolute !important; top:49px !important; right:25px !important;", "Export Migtime Table"),
  fluidRow(
  column(12,
    p("It is now time to define the start and end dates for each season’s sequence. Using the sliders, you will need to do this for every sequence in your dataset. If there is no migration you can just ignore the sliders and not adjust them. Once you've finished defining all the sequence dates you can simply close MAPP or Jump to another Module.")
  ),
  fluidRow(
  column(4, align="center", style = "margin-top:18px;",
    actionButton('backwardHandlerButton','Previous Animal/Year ←')
  ),
  column(4, align="center",
    selectInput(
      "currentIndividualSelector",
      "",
      c(1,2),
      selected = NULL,
      multiple = FALSE
    )
  ),
  column(4,align="center", style = "margin-top:18px;",
    actionButton('forwardHandlerButton','Next Animal/Year →')
  )
),
column(12,
  br()
),
column(6,
  mapboxerOutput('sequencesMap', width = "100%",height='60vh'),
  column(6,
    actionButton('basemapButton',style="margin-top:23px !important;",'Toggle Basemap'),
  ),
  column(6,
    textInput('idYearNotesInput', 'Add notes for this individual below', value = "", width = NULL, placeholder = NULL)
  )
  ),

column(6, align="center",
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      .sequenceInputClass {
        position:relative !important;
        top:46px !important;
        margin-bottom: 15px !important;
        z-index:100 !important;
      }
      .sequenceSliderClass {
        position:relative !important;
        margin-bottom: -45px !important;
        padding-left:50px !important;
      }
      .irs--shiny {
        z-index: 100 !important;
      }
      .tabPanelClass {
        margin-top: 65px !important;
      }")
    )),
  textInput('sequenceName1', '', value = "", width = NULL, placeholder = 'sequence 1')%>%tagAppendAttributes(class = 'sequenceInputClass'),
  sliderInput("dateSlider1",
    "",
    min = as.Date("2020-01-01"), max = as.Date("2020-02-02"),
    value = c(as.Date("2020-01-01"),as.Date("2020-02-01")),
    width='95%'
  )%>%tagAppendAttributes(class = 'sequenceSliderClass'),
  textInput('sequenceName2', '', value = "", width = NULL, placeholder = 'sequence 2')%>%tagAppendAttributes(class = 'sequenceInputClass'),
  sliderInput("dateSlider2",
    "",
    min = as.Date("2020-01-01"), max = as.Date("2020-02-02"),
    value = c(as.Date("2020-01-01"),as.Date("2020-02-01")),
    width='95%'
  )%>%tagAppendAttributes(class = 'sequenceSliderClass'),
  hidden(
  textInput('sequenceName3', '', value = "", width = NULL, placeholder = 'sequence 3')%>%tagAppendAttributes(class = 'sequenceInputClass'),
  sliderInput("dateSlider3",
    "",
    min = as.Date("2020-01-01"), max = as.Date("2020-02-02"),
    value = c(as.Date("2020-01-01"),as.Date("2020-02-01")),
    width='95%'
  )%>%tagAppendAttributes(class = 'sequenceSliderClass')),
  hidden(
  textInput('sequenceName4', '', value = "", width = NULL, placeholder = 'sequence 4')%>%tagAppendAttributes(class = 'sequenceInputClass'),
  sliderInput("dateSlider4",
    "",
    min = as.Date("2020-01-01"), max = as.Date("2020-02-02"),
    value = c(as.Date("2020-01-01"),as.Date("2020-02-01")),
    width='95%'
  )%>%tagAppendAttributes(class = 'sequenceSliderClass')),
  hidden(
  textInput('sequenceName5', '', value = "", width = NULL, placeholder = 'sequence 5')%>%tagAppendAttributes(class = 'sequenceInputClass'),
  sliderInput("dateSlider5",
    "",
    min = as.Date("2020-01-01"), max = as.Date("2020-02-02"),
    value = c(as.Date("2020-01-01"),as.Date("2020-02-01")),
    width='95%'
  )%>%tagAppendAttributes(class = 'sequenceSliderClass')),
  hidden(
    textInput('sequenceName6', '', value = "", width = NULL, placeholder = 'sequence 6')%>%tagAppendAttributes(class = 'sequenceInputClass'),
  sliderInput("dateSlider6",
    "",
    min = as.Date("2020-01-01"), max = as.Date("2020-02-02"),
    value = c(as.Date("2020-01-01"),as.Date("2020-02-01")),
    width='95%'
  )%>%tagAppendAttributes(class = 'sequenceSliderClass')),
  hidden(
    textInput('sequenceName7', '', value = "", width = NULL, placeholder = 'sequence 7')%>%tagAppendAttributes(class = 'sequenceInputClass'),
  sliderInput("dateSlider7",
    "",
    min = as.Date("2020-01-01"), max = as.Date("2020-02-02"),
    value = c(as.Date("2020-01-01"),as.Date("2020-02-01")),
    width='95%'
  )%>%tagAppendAttributes(class = 'sequenceSliderClass')),
  hidden(
    textInput('sequenceName8', '', value = "", width = NULL, placeholder = 'sequence 8')%>%tagAppendAttributes(class = 'sequenceInputClass'),
  sliderInput("dateSlider8",
    "",
    min = as.Date("2020-01-01"), max = as.Date("2020-02-02"),
    value = c(as.Date("2020-01-01"),as.Date("2020-02-01")),
    width='95%'
  )%>%tagAppendAttributes(class = 'sequenceSliderClass')),
  tabsetPanel(id="graphPanels",
    tabPanel("NSD",value='graph1tab',
      plotOutput("plot1",width='95%')
      ),
    tabPanel("NSD +6 Months",value='graph7tab',
      plotOutput("plot7",width='95%')
      ),

    tabPanel("Sequence Periods NSD",value='graph0tab',
      plotOutput("plot0",width='95%')
      ),
    tabPanel("Speed",value='graph2tab',
      plotOutput("plot2",width='95%')
      ),
    tabPanel("Elevation",value='graph3tab',
      plotOutput("plot3",width='95%')
    ),
    tabPanel("FPT 50",value='graph4tab',
      plotOutput("plot4",width='95%')
      ),
    tabPanel("FPT 150",value='graph5tab',
      plotOutput("plot5",width='95%')
      ),
    tabPanel("FPT 300",value='graph6tab',
      plotOutput("plot6",width='95%')
      )

  )%>%tagAppendAttributes(class = 'tabPanelClass'),
    radioButtons(
    'nsdTypeSelect',
    'Would you like to view NSD or Displacement?',
    choices = c('NSD','DISPLACEMENT'),
    selected = 'NSD',
    inline = TRUE
  )
),
column(6,
  column(12,

  ),

),
column(12,
  br()
)


)
)




server <- function(input, output, session) {
  checkForSession('app2')
  app2_init(input, output, session)
  onStop(function() {
   stopApp()
 })
}

appTwoReload <- function(filePath){
  loadingScreenToggle('show','')
  rdsLocation<-paste0(filePath,'//workingFile.rds')
  if(file.exists(rdsLocation)){
    workingFile<<-readRDS(rdsLocation)
    importedDatasetMaster<<-workingFile$importedDatasetMaster
    masterWorkingDirectory<<-filePath
    dbConnection <<- dbConnect(RSQLite::SQLite(), paste0(masterWorkingDirectory,'//workingDb.db'))
    updateMasterTableFromDatabase()
    workingFile$importedDatasetMaster<<-importedDatasetMaster
    workingFile$masterWorkingDirectory<<-filePath
    loadConfig()
    getMigtime()
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
}

getMigtime<-function(){
  dbConnection <<- dbConnect(RSQLite::SQLite(), paste0(masterWorkingDirectory,'//workingDb.db'))
  tables<-dbListTables(dbConnection)
  if('migtime'%in%tables){
    migtime<<-dbGetQuery(dbConnection, "SELECT * FROM migtime")
    migtime$mig1start <<-as.Date(migtime$mig1start)
    migtime$mig1end <<-as.Date(migtime$mig1end)
    migtime$mig2start <<-as.Date(migtime$mig2start)
    migtime$mig2end <<-as.Date(migtime$mig2end)
    migtime$mig3start <<-as.Date(migtime$mig3start)
    migtime$mig3end <<-as.Date(migtime$mig3end)
    migtime$mig4start <<-as.Date(migtime$mig4start)
    migtime$mig4end <<-as.Date(migtime$mig4end)
    migtime$mig5start <<-as.Date(migtime$mig5start)
    migtime$mig5end <<-as.Date(migtime$mig5end)
    migtime$mig6start <<-as.Date(migtime$mig6start)
    migtime$mig6end <<-as.Date(migtime$mig6end)
    migtime$mig7start <<-as.Date(migtime$mig7start)
    migtime$mig7end <<-as.Date(migtime$mig7end)
    migtime$mig8start <<-as.Date(migtime$mig8start)
    migtime$mig8end <<-as.Date(migtime$mig8end)
    removeModal()
    mapInit()
  }else{
    chooseStartDate()
  }
}

chooseStartDate<-function(){
  if(exists('migtime')){
    showModal(modalDialog(
           title="BUILD MIGTIME TABLE",
           'We will now calculate a migtime table which will be used for defining all your sequence start & end dates. Choose the day of the year you would like to use as the start date of the biological year below. The year can be ignored',

                          dateInput("yearStartDateSelector",
                                       "",
                                       value = bioYearStartDate,
                                       min = "2021-01-01",
                                       max = "2021-12-31",
                                       format="mm-dd"
                                     ),
                             p('Choose the total number of migration sequences below.'),
                             selectInput(
                               "howManySequencesSelector",
                               "How many migrations would you like to identify?",
                               c(1,2,3,4,5,6,7,8),
                               selected = configOptions$totalSequences,
                               multiple = FALSE
                             ),
                            actionButton("calcMigtimeButton", "Build Migtime Table"),

         ))
  }else{
    showModal(modalDialog(
           title="BUILD MIGTIME TABLE",
           'We will now calculate a migtime table which will be used for defining all your sequence start & end dates. Choose the day of the year you would like to use as the start date of the biological year below. The year can be ignored',

                          dateInput("yearStartDateSelector",
                                       "",
                                       value = bioYearStartDate,
                                       min = "2021-01-01",
                                       max = "2021-12-31",
                                       format="mm-dd"
                                     ),
                             p('Choose the total number of migration sequences below.'),
                             selectInput(
                               "howManySequencesSelector",
                               "How many migrations would you like to identify?",
                               c(1,2,3,4,5,6,7,8),
                               selected = configOptions$totalSequences,
                               multiple = FALSE
                             ),
                            actionButton("calcMigtimeButton", "Build Migtime Table"),
                            tags$head(tags$style("#shiny-modal .modal-footer{ display:none}"))

         ))
  }


}




loadConfig<-function(){
  configOptions<<-readRDS(paste0(masterWorkingDirectory,'//configOptions.rds'))
  configOptions$masterWorkingDirectory<<-masterWorkingDirectory
  tempDate<-format(as.Date(configOptions$bioYearStartDate),'%m-%d')
  plotLabel<<-paste0('Displacement^2 km since ',tempDate)
  bioYearStartDate<<-configOptions$bioYearStartDate
  if(length(bioYearStartDate)==0){
    configOptions$bioYearStartDate<<-"2021-02-01"
    bioYearStartDate<<-"2021-02-01"
  }
  updateDateInput(session, 'yearStartDateSelector', value = bioYearStartDate)
  if('sequence1'%in%names(configOptions)){
    for(i in 1:8){
      updateTextInput(session, paste0('sequenceName',i), value = configOptions[paste0('sequence',i)][[1]])
    }
  }else{
    configOptions$sequence1<<-'sequence1'
    configOptions$sequence2<<-'sequence2'
    configOptions$sequence3<<-'sequence3'
    configOptions$sequence4<<-'sequence4'
    configOptions$sequence5<<-'sequence5'
    configOptions$sequence6<<-'sequence6'
    configOptions$sequence7<<-'sequence7'
    configOptions$sequence8<<-'sequence8'
    saveConfig()
  }
  if('totalSequences'%in%names(configOptions)){
    totalSequences<<-configOptions$totalSequences;
  }else{
    totalSequences<<-'2'
    configOptions$totalSequences<<-totalSequences;
    saveConfig()
  }
  if(totalSequences!='2'){
    updateSelectInput(session,'howManySequencesSelector',selected=totalSequences)
  }
  if('currentIndividual'%in%names(configOptions)){
    currentIndividual<<-configOptions$currentIndividual
    updateSelectInput(session,'currentIndividualSelector',selected=configOptions$currentIndividual)
  }
  saveConfig()
}



shinyApp(ui, server)
