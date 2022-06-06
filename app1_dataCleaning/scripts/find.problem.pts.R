############FUNCTION finds problem points####################
#It needs a dataframe with (at a minimum): x (Easting im meters; numeric),
#y (UTM Northing; numeric), date (POSIXct), and burst (where continuous
#bursts should be calculated).  You have the option to identify a vector
#for which are your bursts.  Must have your data ordered before you add in
# find.problem.pts <- function(data = data, burst = data$burst, speedlim=maxSpeedParameter) {
find.problem.pts <- function(data = data, burst = data$burst, speedlim=configOptions$maxSpeedParameter) {

  speedlim<-(speedlim*1000)/3600
  #some checks to start
  if(inherits(data, "data.frame") != TRUE) stop("data is not a dataframe")
  if(any(colnames(data) == "x") == FALSE) stop(print("You forgot an x column"))
  if(any(colnames(data) == "y") == FALSE) stop(print("You forgot an y column"))
  if(any(colnames(data) == "newMasterDate") == FALSE) stop(print("You forgot a date column"))
  if(!inherits(data$newMasterDate, "POSIXct")) stop(print("date column is not POSIXct"))
  key <- 1:nrow(data)
  key2 <- key[order(data$newUid, data$newMasterDate)]
  if(all(key==key2)==FALSE) stop(print("Your data are not ordered correctly"))
  rm(key, key2)
  if(length(burst) != nrow(data)) stop(print("data and burst do not have the same length"))
  if(any(duplicated(data[c("newUid", "newMasterDate")])) == TRUE) stop("You have duplicates in your database")
  if(any(is.na(data$newMasterDate) == TRUE)) stop ("You have NAs in your date column")
  if(any(is.na(data$newUid) == TRUE)) stop ("You have NAs in your id column")

  #create indicator where NAs should be put
  flags1 <- c(diff(burst),1)
  flags2 <- c(1, diff(burst))

  #create a column that calculates the distance (in m) of each move
  # for current point to next point
  xy2 <- data[2:nrow(data),c("x","y")]
  xy2 <- rbind(xy2, data.frame(x=NA,y=NA))
  dist1 <- sqrt((data$x-xy2$x)^2 + (data$y-xy2$y)^2)
  dist1[flags1==1] <- NA

  # for previous point to current point
  xy2 <- data[1:(nrow(data)-1),c("x","y")]
  xy2 <- rbind(data.frame(x=NA,y=NA), xy2)
  dist2 <- sqrt((data$x-xy2$x)^2 + (data$y-xy2$y)^2)
  dist2[flags2==1] <- NA

  #create a column that calculates difference in time of each move
  # for current point to next point
  tz <- attr(data$newMasterDate,"tzone")
  dt1 <- as.numeric(difftime(c(data$newMasterDate[2:nrow(data)],NA),data$newMasterDate,tz = tz, units="secs"))
  dt1[flags1==1] <- NA

  # for previous point to current point
  dt2 <- as.numeric(difftime(data$newMasterDate,c(data$newMasterDate[1],data$newMasterDate[1:(nrow(data)-1)]),tz = tz, units="secs"))
  dt2[flags2==1] <- NA

  #create a column that calculates speed (in m/sec)
  speed1 <- dist1/dt1
  speed2 <- dist2/dt2

  speed1[is.na(speed1)==TRUE] <- speedlim+5
  speed2[is.na(speed2)==TRUE] <- speedlim+5

  return(speed1 > speedlim & speed2 > speedlim)
} #end of function
