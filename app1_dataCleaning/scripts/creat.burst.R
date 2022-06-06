# This function creates a burst vector, based on ID and a max time between intervals.
# It is based on two points connected in time, where row i is connected with row i+1.
# You must have a dataframe, with id column and date (in POSIXct).  You cannot have any NAs.
# Your database must be ordered like: data[order(data$id, data$date),]
# Written by Jerod Merkle, 2013

creat.burst <- function(data = data, id = TRUE, Tmax = 500000){
  if(inherits(data, "data.frame") != TRUE) stop("data is not a dataframe")
  if(any(colnames(data) == "newMasterDate") == FALSE) stop(print("You forgot an date column"))
  if(any(colnames(data) == "newUid") == FALSE) stop(print("You forgot a id column"))
  if(!inherits(data$newMasterDate, "POSIXct")) stop(print("date column is not POSIXct"))
  key <- 1:nrow(data)
  key2 <- key[order(data$newUid, data$newMasterDate)]
  if(all(key==key2)==FALSE) stop(print("Your data are not ordered correctly"))
  rm(key, key2)
  if(any(duplicated(data[c("newUid", "newMasterDate")])) == TRUE) stop("You have duplicates in your database")
  if(any(is.na(data$newMasterDate) == TRUE)) stop ("You have NAs in your date column")
  if(any(is.na(data$newUid) == TRUE)) stop ("You have NAs in your id column")
  d <- c(1, as.numeric(difftime(data$newMasterDate[2:nrow(data)], data$newMasterDate[1:nrow(data)-1], units = "secs")))
  ids <- c(0, diff(as.numeric(as.factor(data$newUid))))
  if(id == TRUE){
    burst <- cumsum(ifelse(ids != 0, 1,
                           ifelse(d > Tmax, 1, 0))) + 1
  } else {
    burst <- cumsum(ifelse(d > Tmax, 1, 0)) + 1
  }
  return(burst)
}
