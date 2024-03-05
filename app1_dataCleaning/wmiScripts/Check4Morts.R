#' Function to identify if and when you have mortalies, or periods of time when you animal was not moving, given a dist (in m) and time (hrs) threshold.
#'
#' Data must be ordered and have columns dist, id, date, from mov.param(). Written and updated by Jerod Merkle. Last updated January 2021.
#'
#' @param data A dataframe or sf POINT dataframe with a posix column and an animal id column (optional)
#' @param dist_thresh Distance threshold (in meters) for how far the animal can move without being considered alive.
#' @param time_thresh Time threshold (in hrs) for how long the animal needs to move less than the dist_thresh without being considered alive.
#' @param id_name A character specifying the name of the column representing animal ID.
#' @param date_name  A character specifying the name of the column representing date and time stamps of the locations.
#'
#' @return Returns a datafame denoting all mortlities, including a column for animal ID, start time and end time of the mortality event.
#'
#' @examples
#' #To come
#'
#' @export

Check4Morts <- function(data=data,
                        dist_thresh = 50,
                        time_thresh = 24,
                        id_name="id",
                        date_name="date"){

  require(sf)

  # if(inherits(data, "sf") != TRUE) stop("data is not a dataframe")
  if(any(colnames(data) == date_name) == FALSE) stop(print("Your date_name is incorrect."))
  if(any(colnames(data) == id_name) == FALSE) stop(print("Your id_name is incorrect."))

  orig <- data
  data$xtmp <- st_coordinates(data)[,1]
  data$ytmp <- st_coordinates(data)[,2]
  data <- st_set_geometry(data, NULL)    #need to remove the


  if(!inherits(data[,date_name], "POSIXct")) stop(print("date column is not POSIXct"))
  key <- 1:nrow(data)
  key2 <- key[order(data[,id_name], data[,date_name])]
  if(all(key==key2)==FALSE) stop(print("Your data are not ordered correctly"))
  if(any(duplicated(data[c(id_name, date_name)])) == TRUE) stop("You have duplicates in your database")
  if(any(is.na(data[,date_name]) == TRUE)) stop ("You have NAs in your date column")
  if(any(is.na(data[,id_name]) == TRUE)) stop ("You have NAs in your id column")

  #efficiently figure out the consecutive times where animals aren't moving much
  dst <- ifelse(data$dist <= dist_thresh, 1, 0)
  dst[is.na(dst)==TRUE] <- 0
  flag <- c(diff(dst),1)
  flag2 <- c(1, diff(dst))
  dst[flag==-1 & flag2==1] <- 0
  flag3 <- c(1, diff(flag))
  dst2 <- ifelse(flag==0 & flag3 == -1, 1, 0)
  dst2 <- cumsum(dst2)
  dst2[dst==0] <- NA
  data$dst2 <- dst2
  data$key <- 1:nrow(data)
  data <- data[is.na(data$dst2)==FALSE,]
  rwsmin <- as.numeric(tapply(data$key, data$dst2, min))
  rwsmax <- as.numeric(tapply(data$key, data$dst2, max))

  result <- data[data$key %in% rwsmin, c(id_name, date_name)]
  names(result) <- c("id","date_start")
  result <- cbind(result, data[data$key %in% rwsmax, date_name])
  names(result) <- c("id","date_start","date_end")
  result$mort_time_hrs <- as.numeric(difftime(result$date_end,result$date_start,
                                              tz=attr(data[,date_name],"tzone"), units="hours"))
  result <- result[result$mort_time_hrs > time_thresh,]
  if(nrow(result)==0){
    print("Your data look good given your parameters!")
    return(NULL)
  }else{
    return(result)
  }
}