###########################################################################################################################

#                               Points to Clean Lines Function
#Created by Ben Robb
#Purpose:This function takes point data and converts using the Visvalingam algorithm (from rmapshaper) to make
#clean looking line files. Saved as the unique id plus the proportion of data used int he algorithm
#Last Update 1/17/2017 by jerod merkle

###########################################################################################################################

#Function. What we need to bring in is the input spatial points data frame,
# the unique ID as a character for each line file wanted,
# the date/timestamp column, also as a character
# the proportion of data you want to keep, 0-1. default is 0.05 (see ?mapshaper::ms_simplify)
# the method option for simplification, either NULL,vis or dp (see ?mapshaper::ms_simplify)
# and the destination folder

pts.to.lines.fx<-function(pts=pts,   # a SpatialPointsDataFrame of the GPS locations
                          id="mig",  #character that represents the name of the column for the id indicator
                          timestamp="TelemDate",  # character that represents the name of the column for the date/time indicator
                          proportion=0.05, # the proportion of data you want to keep, 0-1. default is 0.05 (see ?mapshaper::ms_simplify)
                          method="vis",   # the method option for simplification, either NULL,vis or dp (see ?mapshaper::ms_simplify)
                          write2file=FALSE,  #do you want to write the results to file as a shapefile?
                          dest=getwd()    # if you want to write to file, where should it go?
){
  
  if(all(c("raster","rgdal","rgeos","rmapshaper","dplyr") %in% installed.packages()[,1])==FALSE)
    stop("You must install the following packages: raster, rgdal, rgeos, rmapshaper, dplyr")
  require(raster)
  require(rgdal)
  require(rgeos)
  require(rmapshaper)
  require(dplyr)
  
  #First, make sure that pts is a spatial points data frame, and some other checks
  if(class(pts)!="SpatialPointsDataFrame")
    stop("pts must be a spatial points data frame")
  if(id %in% names(pts) == FALSE)
    stop("your id name is not in the columns of pts")
  if(timestamp %in% names(pts) == FALSE)
    stop("your timestamp name is not in the columns of pts")
  if("POSIXct" %in% class(pts@data[,timestamp]) == FALSE)
    stop("Please convert the timestamp column to POSIXct")
  
  #Now convert the id column to a factor if not already converted
  if(!is.factor(pts@data[,id])){
    pts$data[,id]<-as.factor(pts@data[,id])
  }
  
  #Now run a lapply for each unique id
  simp <- lapply(1:length(unique(pts@data[,id])), function(h){
    
    #Subset the unique id of the loop
    path <- pts[pts@data[,id] %in% unique(pts@data[,id])[h],]

    #Create a simple dataframe using dplyr to include with the attributes of the line file. Any of these rows can be
    #removed if not wanted/needed
    df<-data.frame(id=path@data[,id][1])%>%mutate(begin_coord_x=path@coords[1,1])%>%
      mutate(begin_coord_y=path@coords[1,2])%>%
      mutate(end_coord_x=path@coords[length(path),1])%>%
      mutate(end_coord_y=path@coords[length(path),2])%>%
      mutate(begin_timestamp=path@data[,timestamp][1])%>%
      mutate(end_timestamp=path@data[,timestamp][length(path)])%>%
      mutate(numb_points=length(path))
    
    #Convert everything into a spatial line with the attributes selected
    line<- spLines(path,attr=df)
    
    #Rmapshaper
    simp <- ms_simplify(line,keep=proportion,method=method,keep_shapes=FALSE)
    
    return(spChFIDs(simp, as.character(df$id)))   #fix the name of the ID slot
    
  })#End lapply loop
  
  #merge the lines together so they are all in the same file (but separate lines)
  simp <- do.call(rbind, simp)
  
  proj4string(simp) <- proj4string(pts)
  
  if(write2file == TRUE){
    #Save everything
    writeOGR(simp,dest,layer=paste0(simp$id[1],"_simp_",proportion),driver="ESRI Shapefile")
  }
  
  return(simp)
  
}#End function

