#' Calculate regular Brownian Bridge and Fixed Motion Variance for regular Brownian Bridge on a sequence of data
#'
#' @param seq.sf A sf dataframe of points representing a single sequence of data
#' @param seq.name Character providing the name of the sequence. Typically includes underscores depicting animal id, year, and season name.
#' @param date.name character proding the name of the column representing times stamps for the seq.sf (must be in posixct) 
#' @param UD.fldr Folder path to write out the UD or occurrence distribution.
#' @param Footprint.fldr Folder path to write out the footprint based on a contour around the UD or occurrence distribution.
#' @param Pop.grd Complete file path to the population's empty grid. Must be based on the output of CalcPopGrid(). Grid must be in a projection with meters as the unit.
#' @param BMVar If Null, will run regular BB and calculate motion variance. If a number is specified, it will invoke the Forced motion variance method
#' @param location.error Location error of your GPS data (in meters).
#' @param max.lag Maximum amount of time (in hours) to allow any two sequential points to be connected when fitting BBs. Default is 8 hours.
#' @param contour Contour level used to create the footprint from the UD or occurrence distribution (numeric between 1 and 99.999). Typically 95, 99, or 99.9.
#' @param time.step How often (in minutes) that the BB integrates between sequential points.
#' @param mult4buff Proportion of space around the extent of seq.sf (i.e., a buffer) that is used to subsample the population grid for analysis. Typically 0.2 or 0.3.
#' @param max.timeout # Max amount of time (in seconds) it should run before timing out. Default is 3600*12 (12 hours)

#' @return Returns a dataframe describing the results of the analysis, as well as writes out a UD or occurrence distribution and a footprint (as tif file). 
#'
#' @examples
#' # To come.

#' @export

CalcBBMM <- function(
  seq.sf=tmp,
  seq.name="1_2020_spr",
  date.name="date",
  UD.fldr="./UDs",
  Footprint.fldr="./Footprints",
  Pop.grd="./PopGrid_empty.tif",
  BMVar=NULL,
  location.error=20,
  max.lag=8,
  contour=99,
  time.step=5,
  mult4buff=0.2,
  max.timeout=3600*12   # max number of seconds it should run before timing out. 
){
  
  #manage packages
  if(all(c("sf","raster","BBMM","R.utils") %in% installed.packages()[,1])==FALSE)
    stop("You must install the following packages: sf, raster, BBMM, R.utils")
  require(sf)
  require(raster)
  require(BBMM)
  require(R.utils)
  
  #make the custom brownian bridge function... FOR FMV
  BrownianBridgeCustom<-function (x, y, time.lag, location.error, area.grid = NULL, cell.size = NULL, 
                                  time.step = 10, max.lag = NULL, BMvar=BMVar) 
  {
    if (is.null(x) | is.null(y) | (length(x) != length(y))) {
      stop("data is missing or unequal number of x and y coordinates")
    }
    if (is.null(location.error)) 
      stop("must specify 'location.error'")
    if (is.null(area.grid) & is.null(cell.size)) {
      stop("'area.grid' or 'cell.size' must be specified")
    }
    if (!is.null(area.grid) & is.null(cell.size)) {
      cell.size <- abs(area.grid[1, 1] - area.grid[2, 1])
    }
    if (is.null(area.grid) & !is.null(cell.size)) {
      range.x <- range(x)
      range.y <- range(y)
      min.grid.x <- round(range.x[1] - 1 * sd(x))
      max.grid.x <- round(range.x[2] + 1 * sd(x))
      min.grid.y <- round(range.y[1] - 1 * sd(y))
      max.grid.y <- round(range.y[2] + 1 * sd(y))
      x. <- seq(min.grid.x, max.grid.x, cell.size)
      y. <- seq(min.grid.y, max.grid.y, cell.size)
      area.grid <- merge(x., y.)
    }
    if (is.null(max.lag)) {
      max.lag = max(time.lag) + 1
    }
    if (length(location.error) == 1) {
      location.error <- rep(location.error, length(x))
    }
    n.locs <- length(x)
    # BMvar <- brownian.motion.variance(n.locs, time.lag, location.error, 
    #                                   x, y, max.lag)
    BMvar <- rep(BMvar, times = length(x))
    if (is.null(time.step)) 
      time.step <- 10
    grid.size <- nrow(area.grid)
    probability <- rep(0, grid.size)
    T.Total <- sum(time.lag)
    bbmm <- vector("list", 4)
    names(bbmm) <- c("Brownian motion variance", "x", "y", "probability")
    class(bbmm) <- "bbmm"
    probability <- NULL
    int <- 0
    for (i in 1:(n.locs - 1)) {
      if (time.lag[i] <= max.lag) {
        theta <- NULL
        tm <- 0
        while (tm <= time.lag[i]) {
          alpha <- tm/time.lag[i]
          mu.x <- x[i] + alpha * (x[i + 1] - x[i])
          mu.y <- y[i] + alpha * (y[i + 1] - y[i])
          sigma.2 <- time.lag[i] * alpha * (1 - alpha) * 
            BMvar[i] + ((1 - alpha)^2) * (location.error[i]^2) + 
            (alpha^2) * (location.error[i + 1]^2)
          ZTZ <- (area.grid[, 1] - mu.x)^2 + (area.grid[, 
                                                        2] - mu.y)^2
          theta <- (1/(2 * pi * sigma.2)) * exp(-ZTZ/(2 * 
                                                        sigma.2))
          int <- int + theta
          tm <- tm + time.step
        }
      }
    }
    probability <- int/T.Total
    probability <- probability/sum(probability)
    bbmm[[4]] <- probability
    bbmm[[1]] <- BMvar[1]
    bbmm[[2]] <- area.grid[, 1]
    bbmm[[3]] <- area.grid[, 2]
    return(bbmm)
  }
  #----------- END OF CUSTOM BB FUNCTION (for FMV)
  
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
  
  if(mean(diff(as.numeric(seq.sf$date1234))/3600 > max.lag) > 0.33){
    return(data.frame(seq.name=seq.name,
                      brownian.motion.variance=NA,
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
                      brownian.motion.variance=NA,
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
    
    # this is the function to calculate the regular BB
    if(class(BMVar)=="NULL"){
      bb <- R.utils::withTimeout({
        try(BBMM::brownian.bridge(x=seq.sf$x,
                                  y=seq.sf$y,
                                  time.lag=diff(as.numeric(seq.sf$date1234)/60),
                                  area.grid=coordinates(grd)[cels,],
                                  max.lag=max.lag*60,
                                  time.step=time.step,
                                  location.error=location.error), #this is the location error of your collars
            silent=TRUE)
      }, envir=environment(), timeout = max.timeout, onTimeout = "warning")
      
    }else{ #THIS IS FMV code
      bb <- R.utils::withTimeout({
        try(BrownianBridgeCustom(x=seq.sf$x,
                                 y=seq.sf$y,
                                 time.lag=diff(as.numeric(seq.sf$date1234)/60),
                                 area.grid=coordinates(grd)[cels,],
                                 max.lag=max.lag*60,
                                 time.step=time.step,
                                 BMvar=BMVar,
                                 location.error=location.error), #this is the location error of your collars
            silent=TRUE)
      }, envir=environment(), timeout = max.timeout, onTimeout = "warning")
    }
    
    #write out results to file too, so if there is an error you don't loose all your work!
    if(class(bb)=="try-error" | class(bb)== "character"){
      return(data.frame(seq.name=seq.name,
                        brownian.motion.variance=NA,
                        grid.size=NA,
                        grid.cell.size=NA,
                        date.created=Sys.time(),
                        execution_time=paste(round(difftime(Sys.time(), start.time, units="min"),2)," minutes",sep=""),
                        num.locs=nrow(seq.sf),
                        Start.Date=min(seq.sf$date1234),
                        End.Date=max(seq.sf$date1234),
                        num.days=length(unique(strftime(seq.sf$date1234, format = "%j", tz = "GMT"))),
                        errors=ifelse(class(bb)=="try-error", attr(bb, "condition")$message, "Ran too long or other problem")))    
    }
    
    if(length(bb$probability) == 1 | all(is.na(bb$probability))==TRUE){
      return(data.frame(seq.name=seq.name,
                        brownian.motion.variance=NA,
                        grid.size=NA,
                        grid.cell.size=NA,
                        date.created=Sys.time(),
                        execution_time=paste(round(difftime(Sys.time(), start.time, units="min"),2)," minutes",sep=""),
                        num.locs=nrow(seq.sf),
                        Start.Date=min(seq.sf$date1234),
                        End.Date=max(seq.sf$date1234),
                        num.days=length(unique(strftime(seq.sf$date1234, format = "%j", tz = "GMT"))),
                        errors="BB Failed for unknown reason."))    
    }
    
    #set to 0 any values that are outside of the < 0.9999 contour
    cutoff <- sort(bb$probability, decreasing=TRUE)
    vlscsum <- cumsum(cutoff)
    cutoff <- cutoff[vlscsum > .9999][1]
    bb$probability[bb$probability < cutoff] <- 0
    
    #rescale probabilities so they sum to equal 1
    bb$probability <- bb$probability/sum(bb$probability)
    
    # write to raster
    grd[cels] <- bb$probability
    writeRaster(grd, filename = paste0(UD.fldr,"/",seq.name,".tif"),
                format = "GTiff", overwrite = TRUE, datatype='FLT4S')
    
    #output the footprint based on contour
    contours <- bbmm.contour(bb, levels=contour, plot=F)
    rcl <- matrix(c(-Inf, contours$Z, 0,
                    contours$Z, Inf, 1), ncol=3, byrow=TRUE)
    grd <- reclassify(grd, rcl = rcl)
    writeRaster(grd, filename = paste0(Footprint.fldr,"/",seq.name,".tif"),
                format = "GTiff", overwrite = TRUE, datatype='INT1U')
    
    toreturn <- data.frame(seq.name=seq.name,
                           brownian.motion.variance=round(bb[[1]],2),
                           grid.size=length(bb$x),
                           grid.cell.size=abs(bb$x[1]-bb$x[2]),
                           date.created=Sys.time(),
                           execution_time=paste(round(difftime(Sys.time(), start.time, units="min"),2)," minutes",sep=""),
                           num.locs=nrow(seq.sf),
                           Start.Date=min(seq.sf$date1234),
                           End.Date=max(seq.sf$date1234),
                           num.days=length(unique(jul)),
                           errors="None")
    
    rm(bb, grd, contours, seq.sf, jul, rcl, cutoff, vlscsum) # remove some of the larger objects
    gc()
    
    #gather summary info
    return(toreturn)
    
  } # end of function