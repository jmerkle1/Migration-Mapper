mergeShapfilesHandler<-function(){   

    selectedUid<<-input$uniqueIdSelector

    if(selectedUid==""){
      modalMessager('Error','You need to select a field for the unique
      identifier or choose NaN if your dataset does not have a unique ID')
      return()
    }

    progressIndicator('Processing.... Please wait...','start')



    # if nan is chosen then the uid will just be the filename
    if(selectedUid=='NaN'){
      for(i in 1:length(importedShapefilesHolder)){        
        importedShapefilesHolder[[i]]$newUid<<-NULL
        thisNewUid<-names(importedShapefilesHolder[i])
        # remove these line because of user needs for underscores to persist
        # also cannot have underscores.. could cause errors later
        thisNewUid<-gsub("_", "-", thisNewUid)        
        importedShapefilesHolder[[i]]$newUid<<-thisNewUid
      }
    } else{ #otherwise the UID is the field that was selected for the UID
      for(i in 1:length(importedShapefilesHolder)){        
        importedShapefilesHolder[[i]]$newUid<<-NULL
        # also cannot have underscores.. could cause errors later        
        thisNewUid<-gsub("_","-",importedShapefilesHolder[[i]][[selectedUid]])                
        importedShapefilesHolder[[i]]$newUid<<-thisNewUid
      }
    }


    importedDatasetMaster <<- tryCatch({
        # Reduce(rbind,importedShapefilesHolder)
        # st_as_sf(rbindlist(importedShapefilesHolder))
        do.call(rbind, importedShapefilesHolder)
      },
      error = function(cond) {
      modalMessager(
        "DATASET MERGE ERROR",
        paste(
          "There was a fatal error merging
          your datasets. Please check the data and try again. Detailed error from
          R is : ",
          cond,
          sep = ""
        )
      )
      return()
      },
      warning = function(cond) {
      modalMessager(
        "DATASET MERGE WARNING",
        paste(
          "There was a fatal error merging
          your datasets. Please check the data and try again. Detailed error from
          R is : ",
          cond,
          sep = ""
        )
      )
      return()
      }
    )

  projectShapefilesHandler()
  progressIndicator('Processing.... Please wait...','stop')
}


projectShapefilesHandler<-function(){

  
    # importedDatasetMaster<<-spTransform(importedDatasetMaster,CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
  crs4326<-crs(elevation)
  configOptions$masterCrs4326<<-crs4326
  importedDatasetMaster<<-st_transform(importedDatasetMaster, crs = configOptions$masterCrs4326)



  #if there's a z dimension in the coords, drop it
  # if(ncol(importedDatasetMaster@coords)==3){
  #   print('---------------------------------------')
  #   print('-------- DROPPING Z DIM ---------------')
  #   print('---------------------------------------')
  #   importedDatasetMaster@coords <<- importedDatasetMaster@coords[, 1:2]
  # }

  coords<-st_coordinates(importedDatasetMaster)


  # add lat/lon for leaflet maps
  importedDatasetMaster[["lon"]]<<-coords[,1]
  importedDatasetMaster[["lat"]]<<-coords[,2]

  progressIndicator('Extracting Elevation Data','start')  
  # importedDatasetMaster@data[['elev']]<<-raster::extract(elevation,importedDatasetMaster)
  importedDatasetMaster$elev<<-extract(elevation,importedDatasetMaster)[,2]
  progressIndicator('Extracting Elevation Data','stop')
  
  importedDatasetMaster$comments<<-''

  midLatLong <- c(importedDatasetMaster[1,]$lat,importedDatasetMaster[1,]$lon)
  zone <- find_UTM_zone(midLatLong[2], midLatLong[1])
  UTMcrs <- paste0("+proj=utm +zone=", zone, " +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")  
  configOptions$masterCrs<<-UTMcrs
  importedDatasetMaster <<- st_transform(importedDatasetMaster, crs = UTMcrs)

  progressIndicator('adding x y columns','start')
  coords<-st_coordinates(importedDatasetMaster)
  importedDatasetMaster$x<<-coords[,1]
  importedDatasetMaster$y<<-coords[,2]  
  progressIndicator('adding x y columns','stop')


  # drop geometry.. will need to add this again sometime
  importedDatasetMaster<<-st_drop_geometry(importedDatasetMaster)

  


  
  rowIds<-c(1:nrow(importedDatasetMaster))
  importedDatasetMaster$rowIds<<-rowIds

  ###########################
  ###### TAB COMPLETED ######
  ###########################

  # this checks for null importedDatasetMaster.. this was occuring when users tried to import null UTM data
  if(is.null(importedDatasetMaster)){
    modalMessager(
      "DATASET IMPORT ERROR",'There was a fatal error while importing your dataset(s). Sometimes this occurs because of null values from imported points. Double check your dataset validity and try again.'
    )
    return()
  } else{    
    showDateTimeSelectionPanel()
  }
}
