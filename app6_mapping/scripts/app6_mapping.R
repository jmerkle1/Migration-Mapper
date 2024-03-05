whichBaseMap<<-'default'

mapColors<-c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928','#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928','#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')
colorLookup<<-list()

# footPrintsColor<-'#e9e9e9'
footPrintsColor<-'rgba(255, 255, 255, 0.46)'

popUseColor<-'#eb8f22'


mapInit<-function(){


  observeEvent(input$basemapButton, {
    if(whichBaseMap=='default'){
      runjs("mapboxer._widget.map.map.setPaintProperty('satlayer','raster-opacity',1)")
      whichBaseMap<<-'sat'
    }else{
      runjs("mapboxer._widget.map.map.setPaintProperty('satlayer','raster-opacity',0)")
      whichBaseMap<<-'default'
    }
  },ignoreInit=TRUE)

  output$map <- renderMapboxer({
   mapboxer(center = c(importedDatasetMaster[1,'lon'],importedDatasetMaster[1,'lat']), style = 'mapbox://styles/wmi-merkle/ckxqg5r429gpr14sd3o6dlno4' ,zoom = 6) %>%
    add_navigation_control()
  })
  delay(2500,dataInit())
}

mapProjectFiles<-function(){
  loadedFileTypes<-names(filesHolder)


  if('footprints'%in%loadedFileTypes){
    theseFiles<-names(filesHolder$footprints)
    for(i in 1:length(theseFiles)){
      thisFileName<-theseFiles[i]
      addPolygonsToMap('footprints',thisFileName)
    }
  }


  if('popUse'%in%loadedFileTypes){
    theseFiles<-names(filesHolder$popUse)
    for(i in 1:length(theseFiles)){
      thisFileName<-theseFiles[i]
      addPolygonsToMap('popUse',thisFileName)
    }
  }

  if('sequences'%in%loadedFileTypes){
    theseFiles<-names(filesHolder$sequences)
    # build unique colors for each sequence



    for(i in 1:length(theseFiles)){
      thisFileName<-theseFiles[i]
      thisSequence<-strsplit(thisFileName,'_')[[1]][1]
      # thisIdYrMig<-filesHolder$sequences[thisFileName][[thisFileName]]$id_yrb_[1]
      # thisIdYrMig<-filesHolder$sequences[thisFileName][[thisFileName]]$mig[1]
      # thisSequence<-strsplit(thisIdYrMig,'_')
      # thisSequence<-thisSequence[[1]][length(thisSequence[[1]])]
      if(!thisSequence%in%names(colorLookup)){
        if(length(names(colorLookup))==0){
          colorLookup[[thisSequence]]<<-mapColors[1]
        }else{
          colorLookup[[thisSequence]]<<-mapColors[length(names(colorLookup))+1]
        }
      }
    }

    for(i in 1:length(theseFiles)){
      thisFileName<-theseFiles[i]
      if(grepl( '_points', thisFileName, fixed = TRUE)){
        addPointsToMap('sequences',thisFileName)
      }
      if(grepl( '_lines', thisFileName, fixed = TRUE)){
        addLinesToMap('sequences',thisFileName)
      }
    }
  }



}

addPolygonsToMap<-function(thisFileType,thisFileName){

  if(thisFileType=='popUse'){
    thisColor<-popUseColor
  }
  if(thisFileType=='footprints'){
    thisColor<-footPrintsColor
  }

  thisFile<-filesHolder[[thisFileType]][[thisFileName]]

  thisSourceName<-paste0(thisFileName,'Source')
  thisLayerName<-paste0(thisFileName,'Layer')


  mapboxer_proxy("map") %>%
    add_source(as_mapbox_source(thisFile),thisSourceName)%>%
    add_fill_layer(
      source = thisSourceName,
      visibility = FALSE,
      fill_color=thisColor,
      fill_outline_color='rgb(0, 0, 0)',
      fill_opacity=1,
      popup=thisLayerName,
      id=thisLayerName
    )%>%
    update_mapboxer()
}

addLinesToMap<-function(thisFileType,thisFileName){

  thisSeason<-strsplit(thisFileName,'_')[[1]][1]
  thisColor<-colorLookup[[thisSeason]]

  thisFile<-filesHolder[[thisFileType]][[thisFileName]]

  thisSourceName<-paste0(thisFileName,'Source')
  thisLayerName<-paste0(thisFileName,'Layer')

  mapboxer_proxy("map") %>%
    add_source(as_mapbox_source(thisFile),thisSourceName)%>%
    add_line_layer(
      source = thisSourceName,
      line_color = thisColor,
      line_width = 1.5,
      line_opacity = 0.85,
      visibility = FALSE,
      popup=thisLayerName,
      id=thisLayerName
    )%>%
    update_mapboxer()
}

addPointsToMap<-function(thisFileType,thisFileName){

  thisSeason<-strsplit(thisFileName,'_')[[1]][1]
  thisColor<-colorLookup[[thisSeason]]


  thisFile<-filesHolder[[thisFileType]][[thisFileName]]

  thisSourceName<-paste0(thisFileName,'Source')
  thisLayerName<-paste0(thisFileName,'Layer')

  mapboxer_proxy("map") %>%
    add_source(as_mapbox_source(thisFile),thisSourceName)%>%
    add_circle_layer(
      source = thisSourceName,
      circle_color = thisColor,
      visibility = FALSE,
      circle_radius = 5,
      popup=thisLayerName,
      id=thisLayerName
    )%>%
    update_mapboxer()
}


showMapLayer<-function(layerName){

  mapboxer_proxy("map") %>%
    set_layout_property(layerName, "visibility", TRUE) %>%
    update_mapboxer()

}

hideMapLayer<-function(layerName){

  mapboxer_proxy("map") %>%
    set_layout_property(layerName, "visibility", FALSE) %>%
    update_mapboxer()

}
