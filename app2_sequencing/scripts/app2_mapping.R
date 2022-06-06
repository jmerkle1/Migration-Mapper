hasMapRendered<<-FALSE

mapInit<-function(){

  dataSetExtent<-importedDatasetMaster@bbox
  if(!hasMapRendered){
    output$sequencesMap <- renderMapboxer({
    mapboxer(center = c(importedDatasetMaster@data[1,'lon'],importedDatasetMaster@data[1,'lat']), style = 'mapbox://styles/wmi-merkle/ckxqg5r429gpr14sd3o6dlno4' ,zoom = 6) %>%
      add_navigation_control()
    })
    observeEvent(input$sequencesMap_onclick, {
      print(input$sequencesMap_onclick)
    },ignoreInit=TRUE)
  }








  updateSelectMenus();
  adjustSequences()
  hasMapRendered<<-TRUE
}

isSourceAdded<-FALSE;
isScaleBarAdded<-FALSE;

mapCurrentIndividual<-function(){

  if(!isScaleBarAdded){
    runjs("mapboxer._widget.sequencesMap.map.addControl(new mapboxgl.ScaleControl({position: 'bottom-right', unit:'imperial'}));")
    isScaleBarAdded<<-TRUE;
  }


  pointsForMap<<-importedDatasetMaster[which(importedDatasetMaster$id_bioYear==currentIndividual),]
  pointsForMap<<-pointsForMap[which(pointsForMap@data$problem != 1),]
  pointsForMap<<-pointsForMap[which(pointsForMap@data$mortality != 1),]

  if(nrow(pointsForMap)==0){
    thereAreNoPointsToMap<<-TRUE
    pointsForMap<<-importedDatasetMaster[1,]
  }else{
    thereAreNoPointsToMap<<-FALSE
  }

  pointsForMap<<-spTransform(pointsForMap,CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0', SRS_string='EPSG:4326'))
  linesData<<-pointsToLines(pointsForMap)
  linesData<<-st_as_sf(linesData)
  thisBbox<-pointsForMap@bbox

  if(!isSourceAdded){

    mapboxer_proxy("sequencesMap") %>%
      add_source(as_mapbox_source(linesData),'linesUnderSource')%>%
      add_line_layer(
        source = 'linesUnderSource',
        line_color = '#632782',
        line_width = 1.5,
        line_opacity = 0.85,
        id='linesUnderLayer'
      )%>%
      update_mapboxer()

    mapboxer_proxy("sequencesMap") %>%
      add_source(as_mapbox_source(linesData),'linesSource')%>%
      add_line_layer(
        source = 'linesSource',
        line_color='#ffffff',
        line_width = 1,
      )%>%
      update_mapboxer()




    mapboxer_proxy("sequencesMap") %>%
      add_source(as_mapbox_source(pointsForMap@data,lat="lat",lng="lon"),'pointsSource')%>%
      add_circle_layer(
        source = 'pointsSource',
        circle_color = '#000cff',
        circle_radius = 5,
        id='pointLayer'
      )%>%
      fit_bounds(c(c(thisBbox[1,1]-0.01, thisBbox[2,1]-0.01),c(thisBbox[1,2]+0.01, thisBbox[2,2]+0.01)))%>%
      update_mapboxer()

    sequencePoints<-pointsForMap@data
    sequencePoints$alpha<-0
    sequencePoints$color<-'#ffffff'
    mapboxer_proxy("sequencesMap") %>%
      add_source(as_mapbox_source(sequencePoints,lat="lat",lng="lon"),'sequencePointsSource')%>%
      add_circle_layer(
        source = 'sequencePointsSource',
        circle_color = c("get", "color"),
        circle_opacity = c("get", "alpha"),
        circle_radius = 6,
        id='mapSequencesPoints'
      )%>%
      update_mapboxer()



    mapboxer_proxy("sequencesMap") %>%
      add_source(as_mapbox_source(pointsForMap@data[0,],lat="lat",lng="lon"),'hoverSource')%>%
      add_circle_layer(
        source = 'hoverSource',
        circle_color = '#000000',
        circle_radius = 8,
        id='hoverLayer'
      )%>%
      update_mapboxer()

      isSourceAdded<<-TRUE;
  }else{


    mapboxer_proxy("sequencesMap") %>%
      set_data(linesData,'linesUnderSource')%>%
      update_mapboxer()

    mapboxer_proxy("sequencesMap") %>%
      set_data(linesData,'linesSource')%>%
      update_mapboxer()

    mapboxer_proxy("sequencesMap") %>%
      set_data(pointsForMap@data,lat="lat",lng='lon','pointsSource')%>%
      fit_bounds(c(c(thisBbox[1,1]-0.01, thisBbox[2,1]-0.01),c(thisBbox[1,2]+0.01, thisBbox[2,2]+0.01)))%>%
      update_mapboxer()
  }

}



updateMapSequencePoints<-function(){

    thisRow<<-migtime[which(migtime$id_bioYear==currentIndividual),]

    seq1min<-thisRow$mig1start
    seq1max<-thisRow$mig1end
    seq2min<-thisRow$mig2start
    seq2max<-thisRow$mig2end
    seq3min<-thisRow$mig3start
    seq3max<-thisRow$mig3end
    seq4min<-thisRow$mig4start
    seq4max<-thisRow$mig4end
    seq5min<-thisRow$mig5start
    seq5max<-thisRow$mig5end
    seq6min<-thisRow$mig6start
    seq6max<-thisRow$mig6end
    seq7min<-thisRow$mig7start
    seq7max<-thisRow$mig7end
    seq8min<-thisRow$mig8start
    seq8max<-thisRow$mig8end



    sequencePoints<-pointsForMap@data
    sequencePoints$alpha<-0
    sequencePoints$color<-'#ffffff'

    for(i in 1:totalSequences){
      thisMin<-thisRow[1,paste0('mig',i,'start')]
      thisMax<-thisRow[1,paste0('mig',i,'end')]

      sequencePoints[as.Date(sequencePoints$newMasterDate)>=thisMin  &
      as.Date(sequencePoints$newMasterDate)<thisMax,'color']<-seqCols[i]

      sequencePoints[as.Date(sequencePoints$newMasterDate)>=thisMin  &
      as.Date(sequencePoints$newMasterDate)<thisMax,'alpha']<-1
    }




    if(nrow(sequencePoints)>0){
      mapboxer_proxy("sequencesMap") %>%
        set_data(sequencePoints,lat="lat",lng='lon','sequencePointsSource')%>%
        update_mapboxer()

    }



  }
