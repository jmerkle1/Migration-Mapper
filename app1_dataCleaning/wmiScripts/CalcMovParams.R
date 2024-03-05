#' Add movement parameters to a sf point dataframe based on x/y values (in meters), time( asPOSIXct), and a vector of bursts.
#'
#' movement params will only be calculated for sequential relocations denoted by the burst vector. Written by Jerod Merkle. Last updated January 2021.
#'
#' @param data A dataframe or sf POINT dataframe with a posix column and an animal id column
#' @param id_name A character specifying the name of the column representing animal ID.
#' @param date_name  A character specifying the name of the column representing date and time stamps of the locations.
#' @param burst A vector of bursts, based on the CalcBurst function.
#'
#' @return Returns the same data.frame entered with the addition of the following columns: dist (distance between steps in meters), dt (time elapsed bewteen steps in seconds), speed (meters/second), abs.angle (angle moved relative no north, in degrees), rel.angle (angle moved relative to the direction of the previous step, in degrees), and StepFlag (logical, denoting whether a step is connected and has all movement params calculated)
#'
#' @examples
#' # none

#' @export


CalcMovParams <- function(data = data, id_name="id", date_name="date", burst = data$burst) {

  time <- Sys.time()

  #manage packages
  if(all(c("circular") %in% installed.packages()[,1])==FALSE)
    stop("You must install the following packages: circular")
  require(circular)

  #some checks to start
  if(inherits(data, "sf") != TRUE) stop("data is not a sf_POINT dataframe")

  if("rel.angle" %in% colnames(data) == TRUE){
    warning("You have movement params for this database. Those params will be removed and replaced.")
    data$dist <- NULL
    data$dt <- NULL
    data$speed <- NULL
    data$abs.angle <- NULL
    data$rel.angle <- NULL
    data$StepFlag <- NULL
  }

  orig <- data   # save original database for later
  data$xtmp <- st_coordinates(data)[,1]
  data$ytmp <- st_coordinates(data)[,2]
  data <- st_set_geometry(data, NULL)    #need to remove the geometry so easier to work with
  if(inherits(data, "tbl_df") == TRUE){  # take out of dumb tibble!
    data <- data.frame(data)
  }
  if(any(colnames(data) == date_name) == FALSE)
    stop(print("Your date_name is not correct."))
  if(any(colnames(data) == id_name) == FALSE)
    stop(print("Your id_name is not correct."))
  if(!inherits(data[,date_name], "POSIXct"))
    stop(print("date column is not POSIXct"))

  key <- 1:nrow(data)
  key2 <- key[order(data[,id_name], data[,date_name])]
  if(all(key==key2)==FALSE) stop(print("Your data are not ordered correctly"))
  rm(key, key2)
  if(length(burst) != nrow(data))
    stop(print("data and burst do not have the same length"))
  if(sum(as.numeric(strftime(data[,date_name], format = "%M"))) > 0)
    print("Warning: data[,date_name] is not rounded")
  if(any(duplicated(data[c(id_name, date_name)])) == TRUE)
    stop("You have duplicates in your database")
  if(any(is.na(data[,date_name]) == TRUE))
    stop("You have NAs in your date column")
  if(any(is.na(data[,id_name]) == TRUE))
    stop("You have NAs in your id column")
  if(max(unique(diff(burst))) > 1)
    stop("Your burst vector does not have sequantil integers, or is out of order.")

  #create indicator where NAs should be put
  flags <- c(diff(burst),1)

  #create a column that calculates the distance (in m) of each move
  xy2 <- data[2:nrow(data),c("xtmp","ytmp")]
  xy2 <- rbind(xy2, data.frame(xtmp=NA,ytmp=NA))
  dist <- sqrt((data$xtmp-xy2$xtmp)^2 + (data$ytmp-xy2$ytmp)^2)
  dist[flags==1] <- NA

  #create a column that calculates difference in time of each move
  tz <- attr(data[,date_name],"tzone")
  dt <- as.numeric(difftime(c(data[2:nrow(data),date_name],NA),data[,date_name],tz = tz, units="secs"))
  dt[flags==1] <- NA

  #create a column that calculates speed (in m/sec)
  speed <- dist/dt

  #create column that calculates absolute turning angle (degrees), where
  # 0 correlates with North, and 180 with South.  East is 90, west is 270
  abs.angle <- atan2((xy2$ytmp-data$ytmp), (xy2$xtmp-data$xtmp))
  abs.angle[flags==1] <- NA
  # abs.angle[dist <= 1e-07] <- NA
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

  #Put all new vectors back together with original data incorporated
  temp2 <- cbind(orig, dist, dt, speed, abs.angle, rel.angle)
  temp2$StepFlag <- ifelse(is.na(temp2$rel.angle), FALSE, TRUE)
  print(paste("it took: ", round(as.numeric(difftime(Sys.time(), time, units="mins")),2), " minutes", sep = ""))
  print(paste("there are ", length(unique(burst)), " bursts!"), sep = "")
  print(paste("there are ", sum(temp2$StepFlag), " steps in your database with all the movement params calulcated.", sep = ""))
  if(any(temp2$dt[is.na(temp2$dt) == FALSE] > 86500) == TRUE) print("Warning: There are steps that are longer than 1 day!")
  if(any(temp2$speed[is.na(temp2$speed) == FALSE] > 3) == TRUE) print("Warning: There are potentially unreasonable speeds in the data!")
  return(temp2)
} #end of CalcMovParams function