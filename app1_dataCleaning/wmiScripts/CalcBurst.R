#' Create a burst vector based on relocations of one or multiple animals given a max time between intervals
#'
#' It is based on two points connected in time, where row i is connected with row i+1. Written and updated by Jerod Merkle. Last updated January 2021.
#'
#' @param data A dataframe or sf POINT dataframe with a posix column and an animal id column (optional)
#' @param id Logical. If TRUE (the default), look for and use an animal ID column to create bursts. If FALSE, assumes a single animal in the dataset.
#' @param id_name A character specifying the name of the column representing animal ID.
#' @param date_name  A character specifying the name of the column representing date and time stamps of the locations.
#' @param Tmax An integer representing the max time (in seconds) between two relocations where those two relocations should be considered a connected step.
#'
#' @return Returns a vector with integers of the same value denoting the same burst. If the integers change their value, a new burst has begun.
#'
#' @examples
#' # none
#'
#' @export

CalcBurst <- function(data = data, id = TRUE, id_name="id", date_name="date", Tmax = 176400){

  require(sf)
  if(inherits(data, "data.frame") != TRUE)
    stop("data is not a dataframe!")
  if(inherits(data, "sf") == TRUE){
    data <- st_set_geometry(data, NULL)
  }
  if(inherits(data, "tbl_df") == TRUE){
    data <- data.frame(data)
  }

  if(!inherits(data[,date_name], "POSIXct"))
    stop(print("date column is not POSIXct"))
  key <- 1:nrow(data)
  key2 <- key[order(data[,id_name], data[,date_name])]
  if(all(key==key2)==FALSE) stop(print("Your data are not ordered correctly"))
  rm(key, key2)
  if(any(duplicated(data[,c(id_name, date_name)])) == TRUE)
    stop("You have duplicates in your database")
  if(any(is.na(data[,date_name]) == TRUE))
    stop ("You have NAs in your date column")
  if(any(is.na(data[,id_name]) == TRUE))
    stop ("You have NAs in your id column")
  d <- c(1, as.numeric(difftime(data[2:nrow(data),date_name], data[1:nrow(data)-1, date_name], units = "secs")))
  ids <- c(0, diff(as.numeric(as.factor(data[,id_name]))))
  if(id == TRUE){
    burst <- cumsum(ifelse(ids != 0, 1,
                           ifelse(d > Tmax, 1, 0))) + 1
  } else {
    burst <- cumsum(ifelse(d > Tmax, 1, 0)) + 1
  }
  return(burst)
}