#' Calculate dynamic Brownian Bridge on a sequence of data
#'
#' @param seq.sf A sf dataframe of points representing a single sequence of data.
#' @param seq.name Character providing the name of the sequence. Typically includes underscores depicting animal id, year, and season name.
#' @param date.name character providing the name of the column representing times stamps for the seq.sf (must be in posixct). 
#' @param UD.fldr Folder path to write out the UD or occurrence distribution.
#' @param Footprint.fldr Folder path to write out the footprint based on a contour around the UD or occurrence distribution.
#' @param Pop.grd Complete file path to the population's empty grid. Must be based on the output of CalcPopGrid(). Grid must be in a projection with meters as the unit.
#' @param location.error Location error of your GPS data (in meters).
#' @param max.lag Maximum amount of time (in hours) to allow any two sequential points to be connected when fitting BBs. Default is 8 hours.
#' @param contour Contour level used to create the footprint from the UD or occurrence distribution (numeric between 1 and 99.999). Typically 95, 99, or 99.9.
#' @param dbbmm.margin The margin used for the behavioral change point analysis. This number has to be odd. See ??brownian.bridge.dyn for details.
#' @param dbbmm.window The size of the moving window along the track. See ??brownian.bridge.dyn for details.
#' @param mult4buff Proportion of space around the extent of seq.sf (i.e., a buffer) that is used to subsample the population grid for analysis. Typically 0.2 or 0.3.
#' @param max.timeout # Max amount of time (in seconds) it should run before timing out. Default is 3600*12 (12 hours).
#'
#' @return Returns a dataframe describing the results of the analysis, as well as writes out a UD or occurrence distribution and a footprint (as tif file).
#'
#' @examples
#' # To come.
#'
#' @export

CalcDBBMM <- function(
  seq.sf=tmp,
  seq.name="1_2020_spr",
  date.name="date",
  UD.fldr="./UDs",
  Footprint.fldr="./Footprints",
  Pop.grd="./PopGrid_empty.tif",
  location.error=20,
  max.lag=8,
  contour=99,
  dbbmm.margin=11,   
  dbbmm.window=31,
  mult4buff=0.3,
  max.timeout=3600*12   
){
  
  #manage packages
  if(all(c("sf","raster","move","R.utils") %in% installed.packages()[,1])==FALSE)
    stop("You must install the following packages: sf, raster, move, R.utils")
  require(sf)
  require(raster)
  require(move)
  require(R.utils)
  
  if("sf" %in% class(seq.sf)==FALSE)
    stop("seq.sf must be a sf data frame from package sf!")
  
  # load up the population grid
  grd <- raster(Pop.grd)
  
  # ensure data are in same projection as grid
  seq.sf <- st_transform(seq.sf, crs=projection(grd))
  
  #prepare only the cells to run BB over
  ext2 <- raster::extent(seq.sf)
  multiplyers <- c((ext2[2]-ext2[1])*mult4buff, (ext2[4]-ext2[3])*mult4buff)   # add about mult4buff around the edges of your extent (you can adjust this if necessary)
  ext2 <- raster::extend(ext2, multiplyers)
  cels <- cellsFromExtent(grd, ext2)
  grd2 <- crop(grd, ext2)
  if(length(cels)!=ncell(grd2))
    stop("There is a problem with cropping down the population grid!")
  
  # take out of sf
  seq.sf$x <- st_coordinates(seq.sf)[,1]
  seq.sf$y <- st_coordinates(seq.sf)[,2]
  seq.sf <- st_drop_geometry(seq.sf) 
  
  if("POSIXct" %in% class(seq.sf[,date.name]) == FALSE)
    stop("Your date.name column should be POSIXct!")
  if(any(is.na(seq.sf[,date.name])))
    stop("You have NAs in your date.name column!")
  
  #order by date
  seq.sf <- seq.sf[order(seq.sf[,date.name]),]
  seq.sf$date1234 <- seq.sf[,date.name]  # make new column for date
  
  jul <- as.numeric(strftime(seq.sf$date1234, format = "%j", tz = attr(seq.sf$date1234,"tzone")))
  
  start.time <- Sys.time()
  
  # add information about max.lag
  seq.sf$connect <- c(as.numeric(diff(as.numeric(seq.sf$date1234))),0)/3600
  seq.sf$connect <- ifelse(seq.sf$connect > max.lag, "no", "yes")
  
  if(mean(diff(as.numeric(seq.sf$date1234))/3600 > max.lag) > 0.33){
    return(data.frame(seq.name=seq.name,
                      dbb.mean.motion.variance=NA,
                      grid.size=NA,
                      grid.cell.size=NA,
                      date.created=Sys.time(),
                      execution_time=paste(round(difftime(Sys.time(), start.time, units="min"),2)," minutes",sep=""),
                      num.locs=nrow(seq.sf),
                      Start.Date=min(seq.sf$date1234),
                      End.Date=max(seq.sf$date1234),
                      num.days=length(unique(strftime(seq.sf$date1234, format = "%j", tz = "GMT"))),
                      errors="More than 1/3 of your steps have time gaps greater than your max.lag! BBMM probably won't work."))
  }
  
  if(nrow(seq.sf) < 4){
    return(data.frame(seq.name=seq.name,
                      dbb.mean.motion.variance=NA,
                      grid.size=NA,
                      grid.cell.size=NA,
                      date.created=Sys.time(),
                      execution_time=paste(round(difftime(Sys.time(), start.time, units="min"),2)," minutes",sep=""),
                      num.locs=nrow(seq.sf),
                      Start.Date=min(seq.sf$date1234),
                      End.Date=max(seq.sf$date1234),
                      num.days=length(unique(strftime(seq.sf$date1234, format = "%j", tz = "GMT"))),
                      errors="Less than 4 points."))
  }
  
  # this is the function to calculate the dynamic BB
  
  # prep data for the dBB function
  mov <- move(x=seq.sf$x, y=seq.sf$y, time=seq.sf$date1234,
              animal=seq.name, proj=CRS(projection(grd)))   #create mov object
  mov <- burst(mov, seq.sf$connect[1:(nrow(seq.sf)-1)])   #this identifies the bad points (ie > than your MaxFixInterval)
  
  # this is the function to calculate the dynamics BB
  bb <- R.utils::withTimeout({
    try(move::brownian.bridge.dyn(mov,
                                  location.error=location.error, #this is the location error of your collars
                                  raster=grd2,
                                  margin=dbbmm.margin,    # margin and window.size are params of the dynBB. I have put the defaults
                                  window.size=dbbmm.window,
                                  burstType="yes"), 
        silent=TRUE)
  }, envir=environment(), timeout = max.timeout, onTimeout = "warning")
  
  #write out results to file too, so if there is an error you don't loose all your work!
  if("try-error" %in% class(bb)){
    return(data.frame(seq.name=seq.name,
                      dbb.mean.motion.variance=NA,
                      grid.size=NA,
                      grid.cell.size=NA,
                      date.created=Sys.time(),
                      execution_time=paste(round(difftime(Sys.time(), start.time, units="min"),2)," minutes",sep=""),
                      num.locs=nrow(seq.sf),
                      Start.Date=min(seq.sf$date1234),
                      End.Date=max(seq.sf$date1234),
                      num.days=length(unique(strftime(seq.sf$date1234, format = "%j", tz = "GMT"))),
                      errors=paste0("Error from brownian.bridge.dyn: ", attr(bb, "condition")$message, ". May have also ran too long.")))    
  }
  
  mean.motion.var <- mean(na.omit(bb@DBMvar@means))
  if(length(bb@layers)>1){   #check to see if it was a multi-part DBB
    bb <- sum(bb)
  }else{
    bb <- bb[[1]]
  }
  
  #set to 0 any values that are outside of the < 0.9999 contour
  bb <- values(bb)
  bb <- bb/sum(bb)  #rescale probabilities so they sum to equal 1
  cutoff <- sort(bb, decreasing=TRUE)
  vlscsum <- cumsum(cutoff)
  cutoff <- cutoff[vlscsum > .9999][1]
  bb[bb < cutoff] <- 0
  bb <- bb/sum(bb)  #rescale probabilities so they sum to equal 1
  
  # write to raster
  grd[cels] <- bb
  writeRaster(grd, filename = paste0(UD.fldr,"/",seq.name,".tif"),
              format = "GTiff", overwrite = TRUE, datatype='FLT4S')
  
  #output the footprint based on contour
  cutoff <- sort(bb, decreasing=TRUE)
  vlscsum <- cumsum(cutoff)
  cutoff <- cutoff[vlscsum > (contour/100)][1]
  bb <- ifelse(bb < cutoff, 0, 1)
  grd[cels] <- bb
  writeRaster(grd, filename = paste0(Footprint.fldr,"/",seq.name,".tif"),
              format = "GTiff", overwrite = TRUE, datatype='INT1U')
  
  toreturn <- data.frame(seq.name=seq.name,
                         dbb.mean.motion.variance=mean.motion.var,
                         grid.size=length(bb),
                         grid.cell.size=res(grd)[1],
                         date.created=Sys.time(),
                         execution_time=paste(round(difftime(Sys.time(), start.time, units="min"),2)," minutes",sep=""),
                         num.locs=nrow(seq.sf),
                         Start.Date=min(seq.sf$date1234),
                         End.Date=max(seq.sf$date1234),
                         num.days=length(unique(jul)),
                         errors="None")
  
  rm(bb, grd, grd2, mov, cels, contours, seq.sf, jul, cutoff, vlscsum) # remove some of the larger objects
  gc()
  
  #gather summary info
  return(toreturn)
  
} # end of function