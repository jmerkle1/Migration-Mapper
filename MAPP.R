objs <- ls(pos = ".GlobalEnv")
rm(list = objs, pos = ".GlobalEnv")

dependenciesAll<-c("shiny", "sf","circular","shinyjs","shinyBS","ggplot2","mapboxer","adehabitatHR","RSQLite","move","shinycssloaders","adehabitatLT","BBMM","R.utils",
  "lubridate","stringr","parallel","R.utils","dplyr","ctmm","fields","smoothr","rgeos","suncalc","terra","tcltk")

for(i in 1:length(dependenciesAll)){
  if(dependenciesAll[i] %in% installed.packages()==FALSE){
    if(dependenciesAll[i]=='BBMM'){
        install.packages("https://cran.r-project.org/src/contrib/Archive/BBMM/BBMM_3.0.tar.gz", repos=NULL)
      }else{
        install.packages(dependenciesAll[i])
      }
  }
}
dependencies<-c("shiny","shinyjs")

for(i in 1:length(dependencies)){
  if(dependencies[i] %in% installed.packages()==FALSE){
    install.packages(dependencies[i])
    require(dependencies[i],character.only=TRUE)
  } else{
    require(dependencies[i],character.only=TRUE)
  }
}


source("globalScripts/globalUiFunctions.R",local=TRUE)


ui <- fluidPage(
  useShinyjs(),
  fluidRow(id='importDataRow',
  column(12,
    HTML("<div style='width:100%; height:3rem; padding:4rem; font-size:3rem; margin-bottom:2rem; padding-bottom:6rem; background-color:black; color:white;'>Welcome to Migration Mapper v3.1!</div>"),
    column(12,
      column(4,
        HTML("<h3>Module 1 - Data Import and Preparation</h3><div style='height:3rem !important;'>Use this module to get started. This module will import your dataset(s), process date information, allow you to explore movement parameters and let you select problem points to drop from analysis.</div>"),
        actionButton("app1Button", style = "width:100%; font-weight:bolder; margin-top:15px !important;", "Open module 1")
      ),
      column(4,
        HTML("<h3>Module 2 - Extracting Sequences</h3><div style='height:3rem !important;'>This module is used to select seasonal periods of movement in the dataset. You will choose a biological year start date and define up to 8 sequences for each id-yr in your dataset.</div>"),
        actionButton("app2Button", style = "width:100%; font-weight:bolder; margin-top:15px !important;", "Open module 2")
      ),
      column(4,
        HTML("<h3>Module 3 - Exporting Sequences</h3><div style='height:3rem !important;'>This application is used to further define custom seasons and export sequences identified in Module 2.</div>"),
        actionButton("app3Button", style = "width:100%; font-weight:bolder; margin-top:15px !important;", "Open module 3")
      )
    ),
    column(12,
      p(),
    ),
    column(12,
      column(4,
        HTML("<h3>Module 4 - Applying movement models</h3><div style='height:3rem !important;'>This module applies movement models to your sequences and then exports occurrence distributions and footprints.</div>"),
        actionButton("app4Button", style = "width:100%; font-weight:bolder; margin-top:15px !important;", "Open module 4")
      ),
      column(4,
        HTML("<h3>Module 5 - Estimating population corridors and use</h3><div style='height:3rem !important;'>In this application you can merge together occurrence distributions and footprints from Module 5 into final products at the population level.</div>"),
        actionButton("app5Button", style = "width:100%; font-weight:bolder; margin-top:15px !important;", "Open module 5")
      ),
      column(4,
        HTML("<h3>Module 6 - Visualizing data and outputs</h3><div style='height:3rem !important;'>This module helps you visualize your data and results in a simple mapping interface.</div>"),
        actionButton("app6Button", style = "width:100%; font-weight:bolder; margin-top:15px !important;", "Open module 6")
      )
    )
  ),
  column(12,
    h3(''),
    p()
  ),
  column(8,
    h4('Migration mapper user guide'),
    HTML('To view the user guide, follow this link <a href="https://docs.google.com/document/d/1BV2-VAsxKRMaoTxPrVWC9v2YBhSAMg5WDl7Vok9narM/edit?usp=sharing
" target="_blank">https://docs.google.com/document/d/1BV2-VAsxKRMaoTxPrVWC9v2YBhSAMg5WDl7Vok9narM/edit?usp=sharing
</a>'),
    h4('Problems or bug reports?'),
    HTML('To report a problem, suggestion or bug, please fill out the form linked here <a href="https://forms.gle/hS5DgSLZd19uJLTg7" target="_blank">https://forms.gle/hS5DgSLZd19uJLTg7</a>')
  ),
  column(4,
    h3('PROJECT PARTNERS'),
    HTML(
      "
      <div style='text-align:center !important;'>
         <img src='wmiLogo.jpg' style='width:50rem; height:auto; margin-bottom:20px;'><br>
         <img src='gageCartoLogo.png' style='width:15rem; height:auto; margin-right:20px;margin-bottom:20px;'>
         <img src='usgsLogo.png' style='width:15rem; height:auto; margin-bottom:20px;'><br>
         <img src='uwLogo.png' style='height:20rem; width:auto; margin-righ:20px; margin-bottom:20px;'>
         <img src='wafwaLogo.png' style='height:20rem; width:auto; margin-bottom:20px;'>
       </div>
       "
    ),
  )

)
)

appToRun<<-NULL

server <- function(input, output, session) {

  session<<-session
  input<<-input
  output<<-output



  onStop(function() {
    startOther(appToRun)
  })


  appRoot<<-getwd()



  observeEvent(input$app1Button, {
    appToRun<<-'app1'
    runjs("window.close(); console.log('closing')")
    stopApp()
  },ignoreInit=TRUE)

  observeEvent(input$app2Button, {
    appToRun<<-'app2'
    runjs("window.close(); console.log('closing')")
    stopApp()
  },ignoreInit=TRUE)

  observeEvent(input$app3Button, {
    appToRun<<-'app3'
    runjs("window.close(); console.log('closing')")
    stopApp()
  },ignoreInit=TRUE)

  observeEvent(input$app4Button, {
    appToRun<<-'app4'
    runjs("window.close(); console.log('closing')")
    stopApp()
  },ignoreInit=TRUE)

  observeEvent(input$app5Button, {
    appToRun<<-'app5'
    runjs("window.close(); console.log('closing')")
    stopApp()
  },ignoreInit=TRUE)

  observeEvent(input$app6Button, {
    appToRun<<-'app6'
    runjs("window.close(); console.log('closing')")
    stopApp()
  },ignoreInit=TRUE)



}

shiny::devmode(FALSE)
shinyApp(ui, server)
