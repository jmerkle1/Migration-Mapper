#' Calculate a kernel UD on a sequence of data, including an option to subsample data.
#'
#' @param seq.sf A sf dataframe of points representing a single sequence of data.
#' @param seq.name Character providing the name of the sequence. Typically includes underscores depicting animal id, year, and season name.
#' @param date.name character proding the name of the column representing times stamps for the seq.sf (must be in posixct).
#' @param UD.fldr Folder path to write out the UD or occurrence distribution.
#' @param Footprint.fldr Folder path to write out the footprint based on a contour around the UD or occurrence distribution.
#' @param Pop.grd Complete file path to the population's empty grid. Must be based on the output of CalcPopGrid(). Grid must be in a projection with meters as the unit.
#' @param smooth.param If NULL (the default), will evoke the 'href' method. If numeric, will force the given smoothing parameter.
#' @param contour Contour level used to create the footprint from the UD or occurrence distribution (numeric between 1 and 99.999). Typically 95, 99, or 99.9.
#' @param mult4buff Proportion of space around the extent of seq.sf (i.e., a buffer) that is used to subsample the population grid for analysis. Typically 0.2 or 0.3.
#' @param subsample If NULL (the default), no subsampling will take place. If numeric, will be used as the number of random locations to sample per julian day in seq.sf.
#' @param max.timeout # Max amount of time (in seconds) it should run before timing out. Default is 3600*12 (12 hours).
#'
#' @return Returns a dataframe describing the results of the analysis, as well as writes out a UD or occurrence distribution and a footprint (as tif file).
#'
#' @examples
#' # To come.
#'
#' @export

CalcKernel <- function(
  seq.sf=tmp,
  seq.name="1_2020_spr",
  date.name="date",
  UD.fldr="./UDs",
  Footprint.fldr="./Footprints",
  Pop.grd="./PopGrid_empty.tif",
  smooth.param=NULL,
  contour=99,
  mult4buff=0.3,
  subsample=NULL,
  max.timeout=3600*12
){

  #manage packages
  if(all(c("sf","raster","adehabitatHR","R.utils","dplyr") %in% installed.packages()[,1])==FALSE)
    stop("You must install the following packages: sf, raster, adehabitatHR, R.utils, dplyr")
  require(sf)
  require(raster)
  require(adehabitatHR)
  require(R.utils)
  require(dplyr)

  if("sf" %in% class(seq.sf)==FALSE)
    stop("seq.sf must be a sf data frame from package sf!")

  # load up the population grid
  grd <- raster(Pop.grd)

  # ensure data are in same projection as grid
  seq.sf <- st_transform(seq.sf, crs=projection(grd))

  # work on date
  seq.sf$date1234 <- st_drop_geometry(seq.sf)[,date.name]  # make new column for date
  if("POSIXct" %in% class(seq.sf$date1234) == FALSE)
    stop("Your date.name column should be POSIXct!")
  if(any(is.na(seq.sf$date1234)))
    stop("You have NAs in your date.name column!")

  # code to subsample the data, if necessary
  if(class(subsample) != "NULL"){
    seq.sf$year <- as.numeric(strftime(seq.sf$date1234, format = "%Y", tz = attr(seq.sf$date1234,"tzone")))
    seq.sf$jul <- as.numeric(strftime(seq.sf$date1234, format = "%j", tz = attr(seq.sf$date1234,"tzone")))
    seq.sf$yr_jul <- paste(seq.sf$year, seq.sf$jul, sep="_")
    # subsample the data using slice in dplyr
    seq.sf <- seq.sf %>%
      group_by(yr_jul) %>%
      slice_sample(n=subsample, replace=FALSE) %>% # I had to use some dplyr, sorry!
      ungroup() %>%
      as.data.frame() %>%
      st_set_geometry("geometry") %>%
      arrange(date1234)
  }

  jul <- as.numeric(strftime(seq.sf$date1234, format = "%j", tz = attr(seq.sf$date1234,"tzone")))


  #prepare only the cells to run kernel over
  ext2 <- raster::extent(seq.sf)
  multiplyers <- c((ext2[2]-ext2[1])*mult4buff, (ext2[4]-ext2[3])*mult4buff)   # add about mult4buff around the edges of your extent (you can adjust this if necessary)
  ext2 <- raster::extend(ext2, multiplyers)
  cels <- cellsFromExtent(grd, ext2)
  grd2 <- crop(grd, ext2)
  if(length(cels)!=ncell(grd2))
    stop("There is a problem with cropping down the population grid!")

  start.time <- Sys.time()

  if(nrow(seq.sf) < 5){
    return(data.frame(seq.name=seq.name,
                      kernel.smooth.param=NA,
                      grid.size=NA,
                      grid.cell.size=NA,
                      date.created=Sys.time(),
                      execution_time=paste(round(difftime(Sys.time(), start.time, units="min"),2)," minutes",sep=""),
                      num.locs=nrow(seq.sf),
                      Start.Date=min(seq.sf$date1234),
                      End.Date=max(seq.sf$date1234),
                      num.days=length(unique(jul)),
                      errors="Less than 5 points."))
  }

  # this is the function to calculate the regular BB
  if(class(smooth.param)=="NULL"){
    kern <- R.utils::withTimeout({
      try(kernelUD(as(seq.sf$geometry,"Spatial"),
                   h="href",
                   grid=as(grd2, "SpatialPixels"),
                   kern="bivnorm"),
          silent=TRUE)
    }, envir=environment(), timeout = max.timeout, onTimeout = "warning")

  }else{ #THIS IS forced smooth.param code

    kern <- R.utils::withTimeout({
      try(kernelUD(as(seq.sf$geometry,"Spatial"),
                   h=smooth.param,
                   grid=as(grd2, "SpatialPixels"),
                   kern="bivnorm"),
          silent=TRUE)
    }, envir=environment(), timeout = max.timeout, onTimeout = "warning")

  } # end of section for fixed smooth.param

  #write out results to file too, so if there is an error you don't loose all your work!
  if("try-error" %in% class(kern)){

    return(data.frame(seq.name=seq.name,
                      kernel.smooth.param=NA,
                      grid.size=NA,
                      grid.cell.size=NA,
                      date.created=Sys.time(),
                      execution_time=paste(round(difftime(Sys.time(), start.time, units="min"),2)," minutes",sep=""),
                      num.locs=nrow(seq.sf),
                      Start.Date=min(seq.sf$date1234),
                      End.Date=max(seq.sf$date1234),
                      num.days=length(unique(jul)),
                      errors=ifelse(class(kern)=="try-error", attr(kern, "condition")$message, "Ran too long or other kernel problem")))

  }

  h <- kern@h$h  # pull out the h smooth factor

  #set to 0 any values that are outside of the < 0.9999 contour
  kern <- values(raster(kern))  # turn the estUD object to a raster
  kern <- kern/sum(kern)  #rescale probabilities so they sum to equal 1
  cutoff <- sort(kern, decreasing=TRUE)
  vlscsum <- cumsum(cutoff)
  cutoff <- cutoff[vlscsum > .9999][1]
  kern[kern < cutoff] <- 0
  kern <- kern/sum(kern)  #rescale probabilities so they sum to equal 1

  # write to raster
  grd[cels] <- kern
  writeRaster(grd, filename = paste0(UD.fldr,"/",seq.name,".tif"),
              format = "GTiff", overwrite = TRUE, datatype='FLT4S')

  #output the footprint based on contour
  cutoff <- sort(kern, decreasing=TRUE)
  vlscsum <- cumsum(cutoff)
  cutoff <- cutoff[vlscsum > (contour/100)][1]
  kern <- ifelse(kern < cutoff, 0, 1)
  grd[cels] <- kern
  writeRaster(grd, filename = paste0(Footprint.fldr,"/",seq.name,".tif"),
              format = "GTiff", overwrite = TRUE, datatype='INT1U')

  toreturn <- data.frame(seq.name=seq.name,
                         kernel.smooth.param=h,
                         grid.size=length(kern),
                         grid.cell.size=res(grd)[1],
                         date.created=Sys.time(),
                         execution_time=paste(round(difftime(Sys.time(), start.time, units="min"),2)," minutes",sep=""),
                         num.locs=nrow(seq.sf),
                         Start.Date=min(seq.sf$date1234),
                         End.Date=max(seq.sf$date1234),
                         num.days=length(unique(jul)),
                         errors="None")

  rm(kern, cutoff, vlscsum, grd)  # remove some of the larger objects
  gc()

  #gather summary info
  return(toreturn)

} # end of function
