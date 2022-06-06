mergeShapfilesHandler<-function(){

    if(!thisIsTestingRun){
      selectedUid<<-input$uniqueIdSelector
    }


    if(selectedUid==""){
      modalMessager('Error','You need to select a field for the unique
      identifier or choose NaN if your dataset does not have a unique ID')
      return()
    }

    progressIndicator('Processing.... Please wait...','start')



    # if nan is chosen then the uid will just be the filename
    if(selectedUid=='NaN'){
      for(i in 1:length(importedShapefilesHolder)){
        importedShapefilesHolder[[i]]@data['newUid']<<-NULL
        thisNewUid<-names(importedShapefilesHolder[i])
        # remove these line because of user needs for underscores to persist
        # also cannot have underscores.. could cause errors later
        thisNewUid<-gsub("_", "-", thisNewUid)
        importedShapefilesHolder[[i]]@data['newUid']<<-thisNewUid
      }
    } else{ #otherwise the UID is the field that was selected for the UID
      for(i in 1:length(importedShapefilesHolder)){
        importedShapefilesHolder[[i]]@data['newUid']<<-NULL
        # also cannot have underscores.. could cause errors later
        thisNewUid<-gsub("_","-",importedShapefilesHolder[[i]]@data[,selectedUid])
        # thisNewUid<-importedShapefilesHolder[[i]]@data[,selectedUid]
        importedShapefilesHolder[[i]]@data['newUid']<<-thisNewUid
      }
    }



    importedDatasetMaster <<- tryCatch({
        Reduce(rbind,importedShapefilesHolder)
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

  progressIndicator('Processing.... Please wait...','stop')

  projectShapefilesHandler()
}

projectShapefilesHandler<-function(){

  if(packageVersion("rgdal")>'1.5.7'){
  importedDatasetMaster <<- tryCatch({
      spTransform(importedDatasetMaster,CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0', SRS_string='EPSG:4326'))
    },
    error = function(cond) {
    modalMessager(
      "PROJECTION ERROR",
      paste(
        "There was a fatal error projecting
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
      "PROJECTION WARNING",
      paste(
        "There was a fatal error projecting
        your datasets. Please check the data and try again. Detailed error from
        R is : ",
        cond,
        sep = ""
      )
    )
    return()
    }
  )
}else{
  importedDatasetMaster <<- tryCatch({      
      importedDatasetMaster<<-spTransform(importedDatasetMaster,CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
    },
    error = function(cond) {
    modalMessager(
      "PROJECTION ERROR",
      paste(
        "There was a fatal error projecting
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
      "PROJECTION WARNING",
      paste(
        "There was a fatal error projecting
        your datasets. Please check the data and try again. Detailed error from
        R is : ",
        cond,
        sep = ""
      )
    )
    return()
    }
  )
}

  # add lat/lon for leaflet maps
  importedDatasetMaster@data[["lon"]]<<-importedDatasetMaster$coords.x1
  importedDatasetMaster@data[["lat"]]<<-importedDatasetMaster$coords.x2

  progressIndicator('Extracting Elevation Data','start')
  importedDatasetMaster@data[['elev']]<<-raster::extract(elevation,importedDatasetMaster)
  progressIndicator('Extracting Elevation Data','stop')

  importedDatasetMaster@data['comments']<<-''


  midLatLong <- c(importedDatasetMaster@data[1,'lat'],importedDatasetMaster@data[1,'lon'])
  zone <- find_UTM_zone(midLatLong[2], midLatLong[1])
  UTMcrs <- paste0("+proj=utm +zone=", zone, " +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  importedDatasetMaster <<- spTransform(importedDatasetMaster, CRS(UTMcrs))


  importedDatasetMaster@data$rowIds<<-row.names(importedDatasetMaster@data)

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
    # saveWorkingFile();
    showDateTimeSelectionPanel()
  }
}
