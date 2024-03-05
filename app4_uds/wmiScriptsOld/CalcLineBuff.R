#' Calculate line buffer method on a sequences of data. Note - this method does not provide a UD, only a footprint.
#'
#' @param seq.sf A sf dataframe of points representing a single sequence of data.
#' @param seq.name Character providing the name of the sequence. Typically includes underscores depicting animal id, year, and season name.
#' @param date.name character proding the name of the column representing times stamps for the seq.sf (must be in posixct).
#' @param Footprint.fldr Folder path to write out the footprint based on a contour around the UD or occurrence distribution.
#' @param Pop.grd Complete file path to the population's empty grid. Must be based on the output of CalcPopGrid(). Grid must be in a projection with meters as the unit.
#' @param buff Numeric value indicating the distance (in meters) to buffer lines (acts as radii).
#'
#' @return Returns a dataframe describing the results of the analysis, as well as writes out a footprint (as tif file).
#'
#' @examples
#' # To come.
#'
#' @export

CalcLineBuff <- function(
    seq.sf=tmp,
    seq.name="1_2020_spr",
    date.name="date",
    Footprint.fldr="./Footprints",
    Pop.grd="./PopGrid_empty.tif",
    buff=200
){
  
  #manage packages
  if(all(c("sf","raster") %in% installed.packages()[,1])==FALSE)
    stop("You must install the following packages: sf, raster")
  require(sf)
  require(raster)
  
  if("sf" %in% class(seq.sf)==FALSE)
    stop("seq.sf must be a sf data frame from package sf!")
  
  # load up the population grid
  grd <- raster(Pop.grd)
  
  # ensure data are in same projection as grid
  seq.sf <- st_transform(seq.sf, crs=projection(grd))
  
  # work on date and make sure it's ordered by date
  seq.sf$date1234 <- st_drop_geometry(seq.sf)[,date.name]  # make new column for date
  if("POSIXct" %in% class(seq.sf$date1234) == FALSE)
    stop("Your date.name column should be POSIXct!")
  if(any(is.na(seq.sf$date1234)))
    stop("You have NAs in your date.name column!")
  seq.sf <- seq.sf[order(seq.sf$date1234),]
  jul <- as.numeric(strftime(seq.sf$date1234, format = "%j", tz = attr(seq.sf$date1234,"tzone")))
  
  
  start.time <- Sys.time()
  
  if(nrow(seq.sf)<2){
    return(data.frame(seq.name=seq.name,
                      numb.cells=NA,
                      grid.cell.size=res(grd)[1],
                      date.created=Sys.time(),
                      execution_time=paste(round(difftime(Sys.time(), start.time, units="min"),2)," minutes",sep=""),
                      num.locs=nrow(seq.sf),
                      Start.Date=min(seq.sf$date1234),
                      End.Date=max(seq.sf$date1234),
                      num.days=length(unique(jul)),
                      errors="Only 1 point in sequence. No footprint was written out."))
  }
  
  seq.sf.xy1 <- sf::st_coordinates(seq.sf)
  seq.sf.xy2 <- seq.sf.xy1[2:nrow(seq.sf.xy1),]
  seq.sf.xy1 <- seq.sf.xy1[1:(nrow(seq.sf.xy1)-1), ]
  ln <- lapply(1:nrow(seq.sf.xy1), function(i) {
    return(sf::st_linestring(rbind(seq.sf.xy1[i,], seq.sf.xy2[i, ]), dim = "XY"))
  })
  ln <- sf::st_as_sfc(ln, crs = sf::st_crs(seq.sf))
  
  ln <- try(st_buffer(ln, dist=buff), silent=TRUE) # add buffer

  print('-------------')
  print('-------------')
  print('-------------')
  print('-------------')
  print(ln)
  dawg<<-ln
  
  if("try-error" %in% class(ln)){   # if buffering failed
    #gather summary info
    return(data.frame(seq.name=seq.name,
                      numb.cells=NA,
                      grid.cell.size=res(grd)[1],
                      date.created=Sys.time(),
                      execution_time=paste(round(difftime(Sys.time(), start.time, units="min"),2)," minutes",sep=""),
                      num.locs=nrow(seq.sf),
                      Start.Date=min(seq.sf$date1234),
                      End.Date=max(seq.sf$date1234),
                      num.days=length(unique(jul)),
                      errors="Buffering issue. No footprint was written out."))
  }
  
  ln <- st_union(ln)  # union up all the buffered line segments
  
  grd <- rasterize(as(ln, "Spatial"), grd, background=0) # rasterize onto the grid.
  
  writeRaster(grd, filename = paste0(Footprint.fldr,"/",seq.name,".tif"),
              format = "GTiff", overwrite = TRUE, datatype='INT1U')
  
  gc()
  
  #gather summary info
  return(data.frame(seq.name=seq.name,
                    numb.cells=cellStats(grd, sum),
                    grid.cell.size=res(grd)[1],
                    date.created=Sys.time(),
                    execution_time=paste(round(difftime(Sys.time(), start.time, units="min"),2)," minutes",sep=""),
                    num.locs=nrow(seq.sf),
                    Start.Date=min(seq.sf$date1234),
                    End.Date=max(seq.sf$date1234),
                    num.days=length(unique(jul)),
                    errors="None"))
  
} # end of function