############FUNCTION CALCULATES MOVEMENT PARAMETERS####################
#It needs a dataframe with (at a minimum): x (UTM Easting; numeric),
#y (UTM Northing; numeric), date (POSIXct), and burst (where continuous
#bursts should be calculated).  You have the option to identify a vector
#for which are your bursts.  Must have your data ordered before you add in

mov.param <- function(data = data, burst = data$burst) {

  time <- Sys.time()
  #some checks to start
  if(inherits(data, "data.frame") != TRUE) stop("data is not a dataframe")
  if(any(colnames(data) == "x") == FALSE) stop(print("You forgot an x column"))
  if(any(colnames(data) == "y") == FALSE) stop(print("You forgot an y column"))
  if(any(colnames(data) == "newMasterDate") == FALSE) stop(print("You forgot a date column"))
  if(any(colnames(data) == "newUid") == FALSE) stop(print("You forgot a id column"))
  if(!inherits(data$newMasterDate, "POSIXct")) stop(print("date column is not POSIXct"))
  key <- 1:nrow(data)
  key2 <- key[order(data$newUid, data$newMasterDate)]
  if(all(key==key2)==FALSE) stop(print("Your data are not ordered correctly"))
  rm(key, key2)
  if(length(burst) != nrow(data)) stop(print("data and burst do not have the same length"))
  if(sum(as.numeric(strftime(data$newMasterDate, format = "%M"))) > 0) print("data$date is not rounded")
  if(any(duplicated(data[c("newUid", "newMasterDate")])) == TRUE) stop("You have duplicates in your database")
  if(any(is.na(data$newMasterDate) == TRUE)) stop ("You have NAs in your date column")
  if(any(is.na(data$newUid) == TRUE)) stop ("You have NAs in your id column")
    require(circular)

  #create indicator where NAs should be put
  flags <- c(diff(burst),1)

  #create a column that calculates the distance (in m) of each move
  xy2 <- data[2:nrow(data),c("x","y")]
  xy2 <- rbind(xy2, data.frame(x=NA,y=NA))
  dist <- sqrt((data$x-xy2$x)^2 + (data$y-xy2$y)^2)
  dist[flags==1] <- NA

  #create a column that calculates difference in time of each move
  tz <- attr(data$newMasterDate,"tzone")
  dt <- as.numeric(difftime(c(data$newMasterDate[2:nrow(data)],NA),data$newMasterDate,tz = tz, units="secs"))
  dt[flags==1] <- NA
  fixRateHours<-dt/3600


  #create a column that calculates speed (in m/sec)
  speed <- dist/dt

  #create column that calculates absolute turning angle (degrees), where
  # 0 correlates with North, and 180 with South.  East is 90, west is 270
  abs.angle <- atan2((xy2$y-data$y), (xy2$x-data$x))
  abs.angle[flags==1] <- NA
  abs.angle[dist <= 1e-07] <- NA
  abs.angle <- as.numeric(conversion.circular(circular(abs.angle,units = "radians"), units = "degrees",
                                               zero = pi/2, rotation = "clock", modulo = "2pi"))

  #create a column that calculates the relative turning angle for each
  #step, where 0 correlates with North, and 180 with South.
  #and East is 90, west is 270.
  rel.angle <- c(NA, abs.angle[2:length(abs.angle)]-abs.angle[1:(length(abs.angle)-1)])
  rel.angle[flags==1] <- NA
  flags2 <- c(1, diff(burst))
  rel.angle[flags2==1] <- NA
  rel.angle <- ifelse(rel.angle >= 0, rel.angle, 360+rel.angle)

  #Put all new vectors back together with origional data incorporated
  temp2 <- cbind(data, dist, dt, speed, abs.angle, rel.angle, fixRateHours)
  print(paste("it took: ", round(as.numeric(difftime(Sys.time(), time, units="mins")),2), " minutes", sep = ""))
  print(paste("there are ", length(unique(burst)), " bursts!"), sep = "")
  print(paste("there are ", length(temp2$dist[is.na(temp2$dist) == TRUE]),
              " NAs in dist.", sep = ""))
  print(paste("there are ", length(temp2$dt[is.na(temp2$dt) == TRUE]),
              " NAs in dt.", sep = ""))
  print(paste("there are ", length(temp2$speed[is.na(temp2$speed) == TRUE]),
              " NAs in speed.", sep = ""))
  print(paste("there are ", length(temp2$abs.angle[is.na(temp2$abs.angle) == TRUE]),
              " NAs in abs.angle", sep = ""))
  print(paste("there are ", length(temp2$rel.angle[is.na(temp2$rel.angle) == TRUE]),
              " NAs in rel.angle", sep = ""))
  if(any(temp2$dt[is.na(temp2$dt) == FALSE] > 86500) == TRUE) print("Warning: There are steps that are longer than 1 day!")
  if(any(temp2$speed[is.na(temp2$speed) == FALSE] > 3) == TRUE) print("Warning: There are unreasonable speeds in the data!")
  # if(any(temp2$speed[is.na(temp2$speed) == FALSE] > maxSpeedParameter) == TRUE) print("Warning: There are unreasonable speeds in the data!")
  return(temp2)
} #end of mov.param function
