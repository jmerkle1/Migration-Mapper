hasMapRendered<<-FALSE
mortalityColor<<-'#dfff00'
problemsColor<<-'#dd00ff'
pointsInDrawBox<<-NULL
pointIdsInDrawBox<<-NULL

whichBaseMap<<-'default'

mapInit<-function(){

  hide('loadProjectButton')
  show('exportDataButton')

  observeEvent(input$basemapButton, {
    if(whichBaseMap=='default'){
      runjs("mapboxer._widget.importedDataMapBox.map.setPaintProperty('satlayer','raster-opacity',1)")
      whichBaseMap<<-'sat'
    }else{
      runjs("mapboxer._widget.importedDataMapBox.map.setPaintProperty('satlayer','raster-opacity',0)")
      whichBaseMap<<-'default'
    }
  },ignoreInit=TRUE)



  hasMapRendered<<-TRUE

  showElement(id = 'calcMoveParamsButton', anim = TRUE)
  showElement(id = 'recalcInstructions', anim = TRUE)


  if(exists('naDatesLength') && naDatesLength>0){
    modalMessager(
      "No points",
      paste0("You had ",naDatesLength," dates in your dataset that resulted in na values. These were removed from your dataset. These were saved to your working directory as nadates.csv if you'd like to inspect them")
    )
  }


  loadingScreenToggle('hide','')

  dummyPoint<<-importedDatasetMaster[1,]
  dummyPoint$lat<<-0
  dummyPoint$lon<<-0


  observeEvent(input$nextPointButton, {
    forwardBackHandler('forward')
  },ignoreInit=TRUE)

  observeEvent(input$previousPointButton, {
    forwardBackHandler('backward')
  },ignoreInit=TRUE)

  observeEvent(input$isMortalitySelector, {
    newValue<-FALSE
    if(input$isMortalitySelector=='yes'){
      newValue<-TRUE
    }
    importedDatasetMaster[which(importedDatasetMaster$rowIds==clickedId),'mortality']<<-newValue
    pointsForMap[which(pointsForMap$rowIds==clickedId),'mortality']<<-newValue
    updateTable('importedDatasetMaster','mortality',paste0('where rowIds = ',clickedId),newValue)
    updatePopupTable(clickedId)    
    updateProblemAndMortPoints()    
    mapboxer_proxy("importedDataMapBox") %>%
      set_data(pointsForMap,lat="lat",lng='lon','pointsSource')%>%
      update_mapboxer()
  },ignoreInit=TRUE)

  observeEvent(input$commentInput, {
    pointToChange<-clickedMapPoint$props$rowIds    
    importedDatasetMaster[which(importedDatasetMaster$rowIds==clickedId),'comments']<<-input$commentInput
    updateTable('importedDatasetMaster','comments',paste0('where rowIds = ',clickedId),paste0('"',input$commentInput,'"'))
    updatePopupTable(clickedId)    
    pointsForMap[which(pointsForMap$rowIds==clickedId),'comments']<<-input$commentInput

    mapboxer_proxy("importedDataMapBox") %>%      
      set_data(pointsForMap,lat="lat",lng='lon','pointsSource')%>%
      update_mapboxer()

  },ignoreInit=TRUE)

  observeEvent(input$isProblemSelector, {
    pointToChange<-clickedMapPoint$props$rowIds
    newValue<-FALSE
    if(input$isProblemSelector=='yes'){
      newValue<-TRUE
    }            
    importedDatasetMaster[which(importedDatasetMaster$rowIds==clickedId),'problem']<<-newValue
    pointsForMap[which(pointsForMap$rowIds==clickedId),'problem']<<-newValue    
    updateProblemAndMortPoints()
    updateTable('importedDatasetMaster','problem',paste0('where rowIds = ',clickedId),newValue)
    updatePopupTable(clickedId)    
    mapboxer_proxy("importedDataMapBox") %>%      
      set_data(pointsForMap,lat="lat",lng='lon','pointsSource')%>%
      update_mapboxer()


  },ignoreInit=TRUE)



  observeEvent(input$forwardHandlerButton, {    
    allAnimals<-unique(importedDatasetMaster$newUid)
    thisAnimalIndex<-which(allAnimals==selectedAnimal)
    if(thisAnimalIndex==length(allAnimals)){
      return()
    }
    newAnimalIndex=thisAnimalIndex+1
    selectedAnimal<<-allAnimals[newAnimalIndex]
    updateSelectInput(session, 'individualsSelector', selected=selectedAnimal)
  },ignoreInit=TRUE)

  observeEvent(input$backwardHandlerButton, {    
    allAnimals<-unique(importedDatasetMaster$newUid)
    thisAnimalIndex<-which(allAnimals==selectedAnimal)
    if(thisAnimalIndex==1){
      return()
    }
    newAnimalIndex=thisAnimalIndex-1
    selectedAnimal<<-allAnimals[newAnimalIndex]
    updateSelectInput(session, 'individualsSelector', selected=selectedAnimal)
  },ignoreInit=TRUE)





  observeEvent(input$allPointsSelector, {
    if(input$allPointsSelector=='yes, show all'){
      allPoints<<-TRUE;
    }else{
      allPoints<<-FALSE;
    }
    addPointsToMap()
  },ignoreInit=TRUE)

  plotClickObserver<<-observeEvent(input$plot_click, {
  },ignoreInit=TRUE)

  plotHoverObserver<<-observeEvent(input$plot_hover, {
  },ignoreInit=TRUE)



  observeEvent(input$variableSelector, {
    selectedGraphVariable <<- input$variableSelector
    plotData()
  })

  renderMap();
  delay(2500,showMortalityProblemBox())

}

showMortalityProblemBox<-function(){
  totalProblems<-length(which(importedDatasetMaster$problem==1))
  totalMortalities<-length(which(importedDatasetMaster$mortality==1))
  if(length(totalProblems)>0 | length(totalMortalities)>0){
    message<-"You had points which were flagged as mortalities or problems.<br>"
    if(length(totalProblems)>0){
      message<-paste0(message,'There were ',totalProblems,' points flagged as problems.<br>')
    }
    if(length(totalMortalities)>0){
      message<-paste0(message,'There were ',totalMortalities,' points flagged as mortalities.<br>')
    }
    message<-paste0(message,'<br>problem points are shown in <strong style="color:#dd00ff !important; background-color:black !important;  padding:3px !important;">magenta</strong> and those points classified as mortalities are shown in <strong style="color:#dfff00 !important; background-color:black !important; padding:3px !important;">yellow</strong>')
    modalMessager(
      "mortalities and problem points",
      HTML(message)
    )
  }

}

animalYearAverages<<-list()

getAnimalYearAverages=function(){
  allAnimals<-unique(importedDatasetMaster$newUid)
  for(i in 1:length(allAnimals)){
    thisAnimal<-allAnimals[i]    
    availableYears<-unique(importedDatasetMaster[which(importedDatasetMaster$newUid==thisAnimal),'year'])
    animalYearAverages[[thisAnimal]]<<-availableYears
  }

  selectedAnimal<<-allAnimals[1]
  selectedYear<<-animalYearAverages[[selectedAnimal]][[1]]

  updateSelectInput(session, 'individualsSelector', label = NULL, choices = allAnimals, selected=selectedAnimal)
  updateSelectInput(session, 'yearSelector', label = NULL, choices = c('All Years',animalYearAverages[[selectedAnimal]]), selected=animalYearAverages[[selectedAnimal]][2])



  observeEvent(input$individualsSelector, {
    selectedAnimal <<- input$individualsSelector
    thisAnimalsYears<-c('All Years',animalYearAverages[[selectedAnimal]])
    if(selectedYear!='All Years'){
      selectedYear <<- thisAnimalsYears[2]
    }
    updateSelectInput(session, 'yearSelector', label = NULL, choices = thisAnimalsYears, selected=selectedYear)
    addPointsToMap()
  },ignoreInit=TRUE)

  observeEvent(input$yearSelector, {
    selectedYear <<- input$yearSelector
    addPointsToMap()
  },ignoreInit=TRUE)

}







renderMap<-function(){

  hideElement(id = 'dateTimeElementsRow', anim = TRUE)
  showElement(id = 'importedDataMapRow', anim = TRUE)

  hideElement(id = 'loadProjectButton', anim = TRUE)
  showElement(id = 'exportDataButton', anim = TRUE)

  output$importedDataMapBox <- renderMapboxer({  
  mapboxer(center = c(importedDatasetMaster[1,'lon'],importedDatasetMaster[1,'lat']), style = 'mapbox://styles/wmi-merkle/ckxqg5r429gpr14sd3o6dlno4' ,zoom = 6) %>%
    add_navigation_control()
  })









    observeEvent(input$importedDataMapBox_onclick, {
      clickedMapPoint<-input$importedDataMapBox_onclick
      clickedMapPoint<<-clickedMapPoint
      clickedId<<-clickedMapPoint$props$rowIds
      pointClickEvent(clickedId,FALSE)
    },ignoreInit=TRUE)




    delay(2500,getAnimalYearAverages())
}

isSourceAdded<-FALSE
isDrawAdded<<-FALSE

drawInit<-function(){

  runjs('

  mapboxer._widget.importedDataMapBox.map.addControl(new mapboxgl.ScaleControl({position: "bottom-right", unit:"imperial"}));
  mapboxer._widget.importedDataMapBox.map.addControl(new mapboxgl.ScaleControl({position: "bottom-right", unit:"metric"}));



  draw = new MapboxDraw({
    displayControlsDefault: false,
    // Select which mapbox-gl-draw control buttons to add to the map.
    controls: {
    polygon: true,
    trash: true
    },
  });
  mapboxer._widget.importedDataMapBox.map.addControl(draw, "top-left");

  mapboxer._widget.importedDataMapBox.map.on("draw.update", function(){
    drawnData = draw.getAll();
    console.log(drawnData)
    Shiny.setInputValue("polygonHolder", drawnData.features[0]);
  });

  mapboxer._widget.importedDataMapBox.map.on("draw.create", function(){
    drawnData = draw.getAll();
    console.log(drawnData)
    Shiny.setInputValue("polygonHolder", drawnData.features[0]);
  });

  mapboxer._widget.importedDataMapBox.map.on("draw.delete", function(){
    console.log("draw delete")
    drawnData = draw.getAll();
    console.log(drawnData)
    Shiny.setInputValue("polygonHolder", 999);
  });

  $(".modal-footer > .btn-default").click(function(){
    clearDrawnPoly()
  })

  $("#manyPointsSelectedModal").on("hidden.bs.modal", function (event) {
    clearDrawnPoly()
  });

  $(".close").click(function(){
    clearDrawnPoly()
  })

  clearDrawnPoly=function(){
    draw.deleteAll()
    drawnData = draw.getAll();
    console.log(drawnData)
    Shiny.setInputValue("polygonHolder", 999);
  }

  mapboxer._widget.importedDataMapBox.map.on("draw.modechange", (e) => {
    const data = draw.getAll();
    if (draw.getMode() == "draw_polygon") {
      var pids = []
      // ID of the added template empty feature
      const lid = data.features[data.features.length - 1].id

      data.features.forEach((f) => {
        if (f.geometry.type === "Polygon" && f.id !== lid) {
          pids.push(f.id)
        }
      })
      draw.delete(pids)
      Shiny.setInputValue("polygonHolder", 999);
    }
  });


  drawChange=function(){
    drawnData = draw.getAll();
    console.log(drawnData)
  }


  mapboxer._widget.importedDataMapBox.map.boxZoom.enable();




  '
)

observeEvent(input$manyPointsIsProblemSelector, {

  if(input$manyPointsIsProblemSelector==''){
    return()
  }

  newValue<-FALSE
  if(input$manyPointsIsProblemSelector=='yes'){
    newValue<-TRUE
  }  
  whichRows<-which(importedDatasetMaster$rowIds%in%pointIdsInDrawBox)
  importedDatasetMaster[whichRows,'problem']<<-newValue
  whichRows<-which(pointsForMap$rowIds%in%pointIdsInDrawBox)
  pointsForMap[whichRows,'problem']<<-newValue
  updateTable('importedDatasetMaster','problem',paste0('where rowIds IN (',toString(pointIdsInDrawBox),') '),newValue)
  updateProblemAndMortPoints()
},ignoreInit=TRUE)

observeEvent(input$manyPointsIsMortalitySelector, {

  if(input$manyPointsIsMortalitySelector==''){
    return()
  }


  thisValue<-input$manyPointsIsMortalitySelector
  newValue<-FALSE
  if(thisValue=='yes'){
    newValue<-TRUE
  }  
  whichRows<-which(importedDatasetMaster$rowIds%in%pointIdsInDrawBox)
  importedDatasetMaster[whichRows,'mortality']<<-newValue  
  whichRows<-which(pointsForMap$rowIds%in%pointIdsInDrawBox)
  pointsForMap[whichRows,'mortality']<<-newValue
  updateTable('importedDatasetMaster','mortality',paste0('where rowIds IN (',toString(pointIdsInDrawBox),') '),newValue)
  updateProblemAndMortPoints()
},ignoreInit=TRUE)

observeEvent(input$manyPointsCommentInput, {

  if(input$manyPointsCommentInput=='no comments added'){
    return()
  }

  thisValue<-input$manyPointsCommentInput  
  whichRows<-which(importedDatasetMaster$rowIds%in%pointIdsInDrawBox)
  importedDatasetMaster[whichRows,'comments']<<-thisValue  
  whichRows<-which(pointsForMap$rowIds%in%pointIdsInDrawBox)
  pointsForMap[whichRows,'comments']<<-thisValue
  updateTable('importedDatasetMaster','comments',paste0('where rowIds IN (',toString(pointIdsInDrawBox),') '),paste0('"',thisValue,'"'))
},ignoreInit=TRUE)

observeEvent(input$polygonHolder, {
  drawnData<<-input$polygonHolder

  if(!typeof(drawnData)=='list'){
    mapboxer_proxy("importedDataMapBox") %>%
      set_data(dummyPoint,lat="lat",lng='lon','polygonSelectedSource')%>%
      update_mapboxer()

      pointsInDrawBox<<-NULL
      pointIdsInDrawBox<<-NULL

      return()
  }

  numberOfPoints<-length(drawnData$geometry[[1]][[1]])
  xCoords<-c()
  yCoords<-c()
  for(i in 1:numberOfPoints){
    thisPoint<-drawnData$geometry[[1]][[1]][[i]]
    thisX<-thisPoint[1][[1]]
    thisY<-thisPoint[2][[1]]
    xCoords<-c(xCoords,thisX)
    yCoords<-c(yCoords,thisY)
  }
  thisPolyCoords <- cbind(xCoords, yCoords)  
  thisPoly <- st_polygon(list(thisPolyCoords)) %>%
    st_sfc(crs = configOptions$masterCrs4326) %>%
    st_as_sf()

  if(!st_is_valid(thisPoly)){
    modalMessager("drawing error",'drawn polygon was invalid.. please try again')
    runjs('draw.deleteAll()
    drawnData = draw.getAll();
    console.log(drawnData)
    Shiny.setInputValue("polygonHolder", 999);')
    return()
  }   



  pointsInDrawBox<<-st_intersection(pointsForMap, thisPoly)  

  if(length(pointsInDrawBox)==0){
    mapboxer_proxy("importedDataMapBox") %>%
      set_data(dummyPoint,lat="lat",lng='lon','polygonSelectedSource')%>%
      update_mapboxer()
      return()
  }
  pointIdsInDrawBox<<-pointsInDrawBox[,'rowIds']
  pointIdsInDrawBox<<-st_drop_geometry(pointIdsInDrawBox)$rowIds


  mapboxer_proxy("importedDataMapBox") %>%
    set_data(pointsInDrawBox,lat="lat",lng='lon','polygonSelectedSource')%>%
    update_mapboxer()


    updateSelectInput(session, 'manyPointsIsMortalitySelector', selected='')
    updateSelectInput(session, 'manyPointsIsProblemSelector', selected='')
    updateTextInput(session, 'manyPointsCommentInput', value='no comments added')
    toggleModal(session,'manyPointsSelectedModal',toggle='open')
})

isDrawAdded<<-TRUE
}


addPointsToMap<-function(){

  if(!isDrawAdded){
    drawInit()
  }



  clearHoverPoint()

  if(selectedYear=='All Years'){
    if(selectedAnimal=='All Individuals'){
      pointsForMap<<-importedDatasetMaster
    }else{
      pointsForMap<<-importedDatasetMaster[which(importedDatasetMaster$newUid==selectedAnimal),]
    }
  }else{
    if(selectedAnimal=='All Individuals'){
      pointsForMap<<-importedDatasetMaster[which(importedDatasetMaster$year==selectedYear),]
    }else{
      pointsForMap<<-importedDatasetMaster[which(importedDatasetMaster$newUid==selectedAnimal & importedDatasetMaster$year==selectedYear),]
    }
  }



  if(nrow(pointsForMap)>0){
    pointsForMap$idDate<<-paste0(pointsForMap$year,pointsForMap$month,pointsForMap$day,pointsForMap$newUid)
    if(allPoints==FALSE){
      pointsForMap <<- pointsForMap[!duplicated(pointsForMap$idDate),]
    }    
    templat<-pointsForMap$lat
    templon<-pointsForMap$lon
    pointsForMap<<-st_as_sf(pointsForMap,coords = c("lon", "lat"), crs = configOptions$masterCrs4326)
    pointsForMap$lat<<-templat
    pointsForMap$lon<<-templon
    thisBbox<-st_bbox(pointsForMap)
    print(thisBbox)
    if(!exists('emptyLine')){
      emptyLinePoints<-pointsForMap[1,]      
      emptyLine<<-Points2Lines(emptyLinePoints)             
    }
    linesData<<-Points2Lines(pointsForMap)
    if(!isSourceAdded){
      mapboxer_proxy("importedDataMapBox") %>%
        add_source(as_mapbox_source(linesData),'linesUnderSource')%>%
        add_line_layer(
          source = 'linesUnderSource',
          line_color = '#632782',
          line_width = 1.5,
          line_opacity = 0.85,
          id='linesUnderLayer'
        )%>%
        update_mapboxer()

      mapboxer_proxy("importedDataMapBox") %>%
        add_source(as_mapbox_source(linesData),'linesSource')%>%
        add_line_layer(
          source = 'linesSource',
          line_color='#ffffff',
          line_width = 1,
          id='linesLayer'
        )%>%
        update_mapboxer()

        theseBounds<-c(thisBbox$xmin-0.01, thisBbox$ymin-0.01,thisBbox$xmax+0.01, thisBbox$ymax+0.01)
        names(theseBounds)<-NULL


      mapboxer_proxy("importedDataMapBox") %>%        
        add_source(as_mapbox_source(pointsForMap),'pointsSource')%>%
        add_circle_layer(
          source = 'pointsSource',
          circle_color = '#000cff',
          circle_radius = 5,
          id='pointLayer'
        )%>%        
        fit_bounds(theseBounds)%>%
        update_mapboxer()




      mapboxer_proxy("importedDataMapBox") %>%
        add_source(as_mapbox_source(dummyPoint,lat="lat",lng="lon"),'mortalitiesSource')%>%
        add_circle_layer(
          source = 'mortalitiesSource',
          circle_color = mortalityColor,
          circle_radius = 4.5,
          id='mortalityLayer'
        )%>%
        update_mapboxer()

        mapboxer_proxy("importedDataMapBox") %>%
          add_source(as_mapbox_source(dummyPoint,lat="lat",lng="lon"),'problemsSource')%>%
          add_circle_layer(
            source = 'problemsSource',
            circle_color = problemsColor,
            circle_radius = 4.5,
            id='problemsLayer'
          )%>%
          update_mapboxer()

          mapboxer_proxy("importedDataMapBox") %>%
            add_source(as_mapbox_source(dummyPoint,lat="lat",lng="lon"),'polygonSelectedSource')%>%
            add_circle_layer(
              source = 'polygonSelectedSource',
              circle_color = '#1dff00',
              circle_radius = 4.5,
              id='polygonsSelectedLayer'
            )%>%
            update_mapboxer()


      mapboxer_proxy("importedDataMapBox") %>%        
        # add_source(as_mapbox_source(pointsForMap[0,],lat="lat",lng="lon"),'hoverSource')%>%
        add_source(as_mapbox_source(dummyPoint,lat="lat",lng="lon"),'hoverSource')%>%
        add_circle_layer(
          source = 'hoverSource',
          circle_color = '#000000',
          circle_radius = 8,
          id='hoverLayer'
        )%>%
        update_mapboxer()

      isSourceAdded<<-TRUE;
    }else{

      mapboxer_proxy("importedDataMapBox") %>%
        set_data(linesData,'linesUnderSource')%>%
        update_mapboxer()

      mapboxer_proxy("importedDataMapBox") %>%
        set_data(linesData,'linesSource')%>%
        update_mapboxer()

        theseBounds<-c(thisBbox$xmin-0.01, thisBbox$ymin-0.01,thisBbox$xmax+0.01, thisBbox$ymax+0.01)        
        names(theseBounds)<-NULL


      mapboxer_proxy("importedDataMapBox") %>%        
        set_data(pointsForMap,lat="lat",lng='lon','pointsSource')%>%        
        fit_bounds(theseBounds)%>%
        update_mapboxer()
    }





    plotData()


    updateSummaryStats()


    updateProblemAndMortPoints();









  }else{



    mapboxer_proxy("importedDataMapBox") %>%      
      set_data(pointsForMap,lat="lat",lng='lon','pointsSource')%>%
      update_mapboxer()

    if(exists('emptyLine')){
      mapboxer_proxy("importedDataMapBox") %>%
        set_data(emptyLine,'linesSource')%>%
        update_mapboxer()

      mapboxer_proxy("importedDataMapBox") %>%
        set_data(emptyLine,'linesUnderSource')%>%
        update_mapboxer()
    }


    updateSummaryStats()

    plotData()


  }



}


updateProblemAndMortPoints<-function(){
  print('update problems morts')  
  if(any(pointsForMap$problem==1)){
    print('YES PROBS')    
    problemsToMap<-pointsForMap[which(pointsForMap$problem==1),]
    mapboxer_proxy("importedDataMapBox") %>%
      set_data(problemsToMap,lat="lat",lng='lon','problemsSource')%>%
      update_mapboxer()
  }else{
    print('no PROBS')
    mapboxer_proxy("importedDataMapBox") %>%
      set_data(dummyPoint,lat="lat",lng='lon','problemsSource')%>%
      update_mapboxer()
  }  
  if(any(pointsForMap$mortality==1)){
    mortalitiesToMap<-pointsForMap[which(pointsForMap$mortality==1),]
    mapboxer_proxy("importedDataMapBox") %>%
      set_data(mortalitiesToMap,lat="lat",lng='lon','mortalitiesSource')%>%
      update_mapboxer()
  }else{
    mapboxer_proxy("importedDataMapBox") %>%
      set_data(dummyPoint,lat="lat",lng='lon','mortalitiesSource')%>%
      update_mapboxer()
  }
}


hasAssignedPlotHandler=FALSE;

plotData=function(){

  print('******** plot ****************')  
  pointsForMap$idDate<<-paste0(pointsForMap$year,pointsForMap$month,pointsForMap$day,pointsForMap$newUid)
  if(allPoints==FALSE){
    pointsForMap <<- pointsForMap[!duplicated(pointsForMap$idDate),]
  }


  output$speedPlot <- renderPlot({      
      speedPlot<-ggplot(pointsForMap, aes(x=newMasterDate, y=speed))+
      geom_line(size= 0.5, color="black")+
      ylab('Speed (km/hr) ')+
      xlab("Date") +
      geom_point(size = 3, color="blue")+
      theme(axis.text=element_text(size=9),
        axis.title=element_text(size=9,face="bold"))
      speedPlot
  })

  output$fixRatePlot <- renderPlot({      
      fixRatePlot<-ggplot(pointsForMap, aes(x=newMasterDate, y=fixRateHours))+
      geom_line(size= 0.5, color="black")+
      ylab("Fix rate (hours)") +
      xlab("Date") +
      geom_point(size = 3, color="blue")+
      theme(axis.text=element_text(size=9),
        axis.title=element_text(size=9,face="bold"))
      fixRatePlot
  })

  output$nsdPlot <- renderPlot({      
      nsdPlot<-ggplot(pointsForMap, aes(x=newMasterDate, y=nsdYear))+
      geom_line(size= 0.5, color="black")+
      ylab('Squared Displacement (KM^2; since 1 Jan)')+
      xlab("Date") +
      geom_point(size = 3, color="blue")+
      theme(axis.text=element_text(size=9),
        axis.title=element_text(size=9,face="bold"))
      nsdPlot
  })


  plotClickObserver$destroy()
  plotHoverObserver$destroy()
  
  if(any(!is.na(pointsForMap$speed))){
  plotClickObserver<<-observeEvent(input$plot_click, {
    clickedPlotPoint<<-nearPoints(pointsForMap, input$plot_click, threshold = 10, maxpoints = 1, addDist = TRUE)
    if(nrow(clickedPlotPoint)>0){
      plotClickEvent(clickedPlotPoint)
    }
  },ignoreInit=TRUE)
  plotHoverObserver<<-observeEvent(input$plot_hover, {    
    hoveredPoint<<-nearPoints(pointsForMap, input$plot_hover, threshold = 5, maxpoints = 1, addDist = TRUE)    
    hoveredPoint<<-st_as_sf(hoveredPoint)
    if(nrow(hoveredPoint)>0){
      showHoverPoint(hoveredPoint)
    } else{
       clearHoverPoint()
     }
  },ignoreInit=TRUE)
  
  }
  
  
}

plotClickEvent=function(clickedPlotPoint){

  thisLon<<-clickedPlotPoint[1,'lon']
  thisLat<<-clickedPlotPoint[1,'lat']


  mapboxer_proxy("importedDataMapBox") %>%
    fit_bounds(c(c(thisLon-0.005,thisLat-0.005),c(thisLon+0.005,thisLat+0.005)))%>%
    update_mapboxer()
}

showHoverPoint=function(hoveredPoint){
  mapboxer_proxy("importedDataMapBox") %>%    
    set_data(hoveredPoint,lat="lat",lng='lon','hoverSource')%>%
    update_mapboxer()    
}

clearHoverPoint=function(){
  mapboxer_proxy("importedDataMapBox") %>%
    set_data(dummyPoint,lat="lat",lng='lon','hoverSource')%>%
    update_mapboxer()
}


forwardBackHandler=function(which){  
  currentRow<-which(pointsForMap$rowIds==clickedId)
  if(which=='forward'){
    thisRow<-currentRow+1
  }else{
    thisRow<-currentRow-1
  }
  if(length(currentRow)==0){
    return()
  }
  if(thisRow>nrow(pointsForMap)){
    modalMessager(
      "outside sequence",
      "this point is the last in this animal/year"
    )
    return()
  }
  if(thisRow==0){
    modalMessager(
      "outside sequence",
      "this point is the first in this animal/year"
    )
    return()
  }  
  clickedId<<-st_drop_geometry(pointsForMap[thisRow,'rowIds'])
  clickedId<<-clickedId$rowIds
  pointClickEvent(clickedId,TRUE)
}


pointClickEvent=function(clickedId,fromButton){



  if(is.null(clickedId)){
    return()
  }
  rowToMap<-importedDatasetMaster[which(importedDatasetMaster$rowIds==clickedId),]
  thisLon<-rowToMap$lon
  thisLat<-rowToMap$lat

  mapboxer_proxy("importedDataMapBox") %>%
    set_data(rowToMap,lat="lat",lng='lon','hoverSource')%>%
    update_mapboxer()

  mapboxer_proxy("importedDataMapBox") %>%
    fit_bounds(c(c(thisLon-0.01, thisLat-0.01),c(thisLon+0.01, thisLat+0.01)))%>%
    update_mapboxer()



  if(rowToMap$mortality==1){
    updateSelectInput(session, 'isMortalitySelector', selected='yes')
  }else{
    updateSelectInput(session, 'isMortalitySelector', selected='no')
  }

  if(rowToMap$problem==1){
    updateSelectInput(session, 'isProblemSelector', selected='yes')
  }else{
    updateSelectInput(session, 'isProblemSelector', selected='no')
  }



  if(rowToMap$comments!=""){
    updateTextInput(session, 'commentInput', value=rowToMap$comments)
  }else{
    updateTextInput(session, 'commentInput', value='')
  }  

  if(!fromButton){
    toggleModal(session,'pointClickModal',toggle='open')  
  }

  updatePopupTable(clickedId)
  
}



updatePopupTable<-function(clickedId){  
  print(clickedId)  

  rowToMap<-importedDatasetMaster[which(importedDatasetMaster$rowIds==clickedId),]
  allFields<-names(rowToMap)
  htmlToRender<-''
  mortalityPosition<-which(allFields=='mortality')
  allFields<-allFields[-mortalityPosition]
  problemPostion<-which(allFields=='problem')
  allFields<-allFields[-problemPostion]
  commentPostion<-which(allFields=='comments')
  allFields<-allFields[-commentPostion]
  allFields<-c(allFields,'mortality')
  allFields<-c(allFields,'problem')
  allFields<-c(allFields,'comments')
  for(i in 1:length(allFields)){    
    thisColumn<-allFields[i]
    thisValue<-rowToMap[1,thisColumn]
    if(is.numeric(thisValue)){
      thisValue<-round(thisValue,1)
    }
    thisValue<-as.character(thisValue)
    if(is.na(thisValue)){
      thisValue=' -  '
    }
    if(nchar(thisValue)==0){
      thisValue='  -  '
    }
    thisHtml<-paste0('<div style="display:inline-block !important; padding:10px; text-align:center !important;"><span style="font-weight:bold !important; font-size:14px !important;">',thisColumn,'</span><br><span style="">',thisValue,'</span></div>')
    # thisHtml<-paste0('<div style="font-size:14px !important;">',thisColumn,'</span><br><span style="">',thisValue,'</span></div>')
    htmlToRender<-paste0(htmlToRender,thisHtml)
  }

  shinyjs::html(
      id   = "pointClickData",
      html = htmlToRender
    )
}
