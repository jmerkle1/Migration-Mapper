#' Identify problem points.
#'
#' A problem point is defined as a point where there is a high speed coming into the point and going away from the point. Written and updated by Jerod Merkle. Last updated January 2021.
#'
#' @param data A dataframe or sf POINT dataframe with a posix column and an animal id column (optional)
#' @param burst A vector of bursts, based on the CalcBurst function.
#' @param id_name A character specifying the name of the column representing animal ID.
#' @param date_name  A character specifying the name of the column representing date and time stamps of the locations.
#' @param speedlim  A character specifying the name of the column representing date and time stamps of the locations.
#'
#' @return Returns a logical vector indicating whether each row of dat is a problem point or not.
#'
#' @examples
#' #To come
#'
#' @export


############FUNCTION finds problem points####################
#It needs a sf_POINT dataframe with (at a minimum): date (POSIXct), and burst (where continuous
#bursts should be calculated).  You have the option to identify a vector
#for which are your bursts.  Must have your data ordered before you add in
# A problem point is defined as a point where there is a high speed coming
# into the point and going away from the point.

FindProblemPts <- function(data = data, date_name="date",
                           burst = data$burst, id_name="id", speedlim=3) {

  #some checks to start
  require(sf)
  if(inherits(data, "sf") != TRUE)
    stop("data is not a sfc_POINT dataframe")
  orig <- data
  data$xtmp <- st_coordinates(data)[,1]
  data$ytmp <- st_coordinates(data)[,2]
  data <- st_set_geometry(data, NULL)    #need to remove the

  if(any(colnames(data) == date_name) == FALSE) stop(print("Your date_name is not correct"))
  if(!inherits(data[,date_name], "POSIXct")) stop(print("date_name column is not POSIXct"))
  key <- 1:nrow(data)
  key2 <- key[order(data[,id_name], data[,date_name])]
  if(all(key==key2)==FALSE) stop(print("Your data are not ordered correctly"))
  rm(key, key2)
  if(length(burst) != nrow(data)) stop(print("dat and burst do not have the same length"))
  if(any(duplicated(data[c(id_name, date_name)])) == TRUE) stop("You have duplicates in your database")
  if(any(is.na(data[,date_name]) == TRUE)) stop ("You have NAs in your date column")
  if(any(is.na(data[,id_name]) == TRUE)) stop ("You have NAs in your id column")

  #create indicator where NAs should be put
  flags1 <- c(diff(burst),1)
  flags2 <- c(1, diff(burst))

  #create a column that calculates the distance (in m) of each move
  # for current point to next point
  xy2 <- data[2:nrow(data),c("xtmp","ytmp")]
  names(xy2) <- c("x","y")
  xy2 <- rbind(xy2, data.frame(x=NA,y=NA))
  dist1 <- sqrt((data$xtmp-xy2$x)^2 + (data$ytmp-xy2$y)^2)
  dist1[flags1==1] <- NA

  # for previous point to current point
  xy2 <- data[1:(nrow(data)-1),c("xtmp","ytmp")]
  names(xy2) <- c("x","y")
  xy2 <- rbind(data.frame(x=NA,y=NA), xy2)
  dist2 <- sqrt((data$xtmp-xy2$x)^2 + (data$ytmp-xy2$y)^2)
  dist2[flags2==1] <- NA

  #create a column that calculates difference in time of each move
  # for current point to next point
  tz <- attr(data[,date_name],"tzone")
  dt1 <- as.numeric(difftime(c(data[2:nrow(data),date_name],NA),data[,date_name],tz = tz, units="secs"))
  dt1[flags1==1] <- NA

  # for previous point to current point
  dt2 <- as.numeric(difftime(data[,date_name],c(data[1,date_name],data[1:(nrow(data)-1),date_name]),tz = tz, units="secs"))
  dt2[flags2==1] <- NA

  #create a column that calculates speed (in m/sec)
  speed1 <- dist1/dt1
  speed2 <- dist2/dt2

  speed1[is.na(speed1)==TRUE] <- 0   #replace the NAs with a small speed (so we keep them)
  speed2[is.na(speed2)==TRUE] <- 0

  return(speed1 > speedlim & speed2 > speedlim)

} #end of function