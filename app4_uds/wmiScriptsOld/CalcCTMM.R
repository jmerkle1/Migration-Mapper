#' Calculate a continuous time movement model on a sequence of data, and then calculate the occurrence distribution. 
#'
#' @param seq.sf A sf dataframe of points representing a single sequence of data.
#' @param seq.name Character providing the name of the sequence. Typically includes underscores depicting animal id, year, and season name.
#' @param date.name character providing the name of the column representing times stamps for the seq.sf (must be in posixct). 
#' @param UD.fldr Folder path to write out the UD or occurrence distribution.
#' @param Footprint.fldr Folder path to write out the footprint based on a contour around the UD or occurrence distribution.
#' @param Pop.grd Complete file path to the population's empty grid. Must be based on the output of CalcPopGrid(). Grid must be in a projection with meters as the unit. 
#' @param Information.Criteria Information criteria used for model selection. Can be "AICc", "AIC", "BIC", "LOOCV" or none (NA). Use LOOCV for 12 hour data. Use something else for more frequent fixes. AIC is default.
#' @param contour Contour level used to create the footprint from the UD or occurrence distribution (numeric between 1 and 99.999). Typically 95, 99, or 99.9.
#' @param mult4buff Proportion of space around the extent of seq.sf (i.e., a buffer) that is used to subsample the population grid for analysis. Typically 0.2 or 0.3.
#' @param max.timeout # Max amount of time (in seconds) it should run before timing out. Default is 3600*12 (12 hours)
#'
#' @return Returns a dataframe describing the results of the analysis, as well as writes out a UD or occurrence distribution and a footprint (as tif file).
#'
#' @examples
#' # To come

#' @export

CalcCTMM <- function(
  seq.sf=d[1:50,],
  seq.name="1_2020_spr",
  date.name="date",
  UD.fldr="./UDs",
  Footprint.fldr="./Footprints",
  Pop.grd="./PopGrid_empty.tif",
  Information.Criteria="AIC", 
  contour=99,
  mult4buff=0.3,
  max.timeout=3600*12   
){
  
  #manage packages
  if(all(c("sf","raster","ctmm","R.utils","move") %in% installed.packages()[,1])==FALSE)
    stop("You must install the following packages: sf, raster, ctmm, R.utils, move")
  require(sf)
  require(raster)
  require(ctmm)
  require(R.utils)
  require(move)
  
  if("sf" %in% class(seq.sf)==FALSE)
    stop("seq.sf must be a sf data frame from package sf!")
  
  # load up the population grid
  grd <- raster(Pop.grd)
  
  # ensure data are in same projection as grid
  seq.sf <- st_transform(seq.sf, crs=projection(grd))
  
  if(nrow(seq.sf) < 4){
    return(data.frame(seq.name=seq.name,
                      ctmm.mod=NA,    # model name
                      grid.size=NA,
                      grid.cell.size=NA,
                      date.created=Sys.time(),
                      execution_time=paste(round(difftime(Sys.time(), start.time, units="min"),2)," minutes",sep=""),
                      num.locs=nrow(seq.sf),
                      Start.Date=min(seq.sf$date1234),
                      End.Date=max(seq.sf$date1234),
                      num.days=length(unique(jul)),
                      errors="Less than 4 points"))
  }
  
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
  
  # create telem object with point data
  telem <- move(x=seq.sf$x, y=seq.sf$y,
                time=seq.sf$date1234, 
                proj=CRS(projection(grd)))
  telem <- as.telemetry(telem)
  
  # store guesstimated parameters of models (not including BM)
  GUESS <- ctmm.guess(telem, interactive = FALSE)
  
  # run model selection
  FITS <- R.utils::withTimeout({
    try(ctmm::ctmm.select(telem, CTMM = GUESS, IC = Information.Criteria, 
                          verbose = TRUE, method = "pHREML"), 
        silent=TRUE)
  }, envir=environment(), timeout = max.timeout, onTimeout = "warning")
  
  if("try-error" %in% class(FITS)){
    return(data.frame(seq.name=seq.name,
                      ctmm.mod=NA,    # model name
                      grid.size=NA,
                      grid.cell.size=NA,
                      date.created=Sys.time(),
                      execution_time=paste(round(difftime(Sys.time(), start.time, units="min"),2)," minutes",sep=""),
                      num.locs=nrow(seq.sf),
                      Start.Date=min(seq.sf$date1234),
                      End.Date=max(seq.sf$date1234),
                      num.days=length(unique(jul)),
                      errors=paste0("Error from ctmm.select: ", attr(FITS, "condition")$message, ". May have also ran too long.")))
  }
  
  # extract top model fit
  FITS <- FITS[[1]]
  
  # Krige the data given the top model
  rkrig <- R.utils::withTimeout({
    try(ctmm::occurrence(telem, 
                         CTMM = FITS, 
                         grid = list(x=coordinates(grd2)[,1],
                                     y=coordinates(grd2)[,2],
                                     dr=res(grd2),
                                     align.to.origin=FALSE,
                                     extent=extent(grd2))), 
        silent=TRUE)
  }, envir=environment(), timeout = max.timeout, onTimeout = "warning")
  
  if("try-error" %in% class(rkrig)){
    return(data.frame(seq.name=seq.name,
                      ctmm.mod=NA,    # model name
                      grid.size=NA,
                      grid.cell.size=NA,
                      date.created=Sys.time(),
                      execution_time=paste(round(difftime(Sys.time(), start.time, units="min"),2)," minutes",sep=""),
                      num.locs=nrow(seq.sf),
                      Start.Date=min(seq.sf$date1234),
                      End.Date=max(seq.sf$date1234),
                      num.days=length(unique(jul)),
                      errors=paste0("Error from occurrence(): ", attr(rkrig, "condition")$message, ". May have also ran too long.")))
  }
  
  # extract the vector of values for teh cells in grd2
  rkrig <- values(raster(rkrig, DF = "PDF", values = TRUE))
  rkrig[is.na(rkrig)] <- 0   # make sure there are no NAs
  
  #set to 0 any values that are outside of the < 0.9999 contour
  rkrig <- rkrig/sum(rkrig)  #rescale probabilities so they sum to equal 1
  cutoff <- sort(rkrig, decreasing=TRUE)
  vlscsum <- cumsum(cutoff)
  cutoff <- cutoff[vlscsum > .9999][1]
  rkrig[rkrig < cutoff] <- 0
  rkrig <- rkrig/sum(rkrig)  #rescale probabilities so they sum to equal 1
  
  # write to raster
  grd[cels] <- rkrig
  writeRaster(grd, filename = paste0(UD.fldr,"/",seq.name,".tif"),
              format = "GTiff", overwrite = TRUE, datatype='FLT4S')
  
  #output the footprint based on contour
  cutoff <- sort(rkrig, decreasing=TRUE)
  vlscsum <- cumsum(cutoff)
  cutoff <- cutoff[vlscsum > (contour/100)][1]
  rkrig <- ifelse(rkrig < cutoff, 0, 1)
  grd[cels] <- rkrig
  writeRaster(grd, filename = paste0(Footprint.fldr,"/",seq.name,".tif"),
              format = "GTiff", overwrite = TRUE, datatype='INT1U')
  
  # some inforation to spit out?
  # dofs <- t(as.data.frame(summary(FITS)$DOF)) # these are the degrees of freedom
  # colnames(dofs) <- paste0("ctmm.dof.",colnames(dofs))
  # ests <- t(as.data.frame(summary(FITS)$CI[,2])) # these are the estimates
  # nms <- colnames(ests)
  # nms <- sub(" ","", nms)
  # nms <- sub(" ","", nms)
  # nms <- sub("[[]",".", nms)
  # nms <- sub("[]]","", nms)
  # nms <- sub("[(]",".", nms)
  # nms <- sub("[)]","", nms)
  # nms <- sub("[/]",".", nms)
  # colnames(ests) <- paste0("ctmm.ests.",nms)
  
  toreturn <- data.frame(seq.name=seq.name,
                         ctmm.mod=summary(FITS)$name,    # model name
                         grid.size=length(rkrig),
                         grid.cell.size=res(grd)[1],
                         date.created=Sys.time(),
                         execution_time=paste(round(difftime(Sys.time(), start.time, units="min"),2)," minutes",sep=""),
                         num.locs=nrow(seq.sf),
                         Start.Date=min(seq.sf$date1234),
                         End.Date=max(seq.sf$date1234),
                         num.days=length(unique(jul)),
                         errors="None")
  
  rm(rkrig, FITS, telem, GUESS, grd, grd2, seq.sf, jul, cutoff, vlscsum) # remove some of the larger objects
  gc()
  
  return(toreturn)
  
} # end of function