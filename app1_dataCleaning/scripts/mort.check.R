# Function to identify if and when you have mortalies, or
# periods of time when you animal was not moving, given
# a dist (in m) and time (hrs) threshold.
# data must be ordered and have columns dist, id, date, from mov.param()...
mort.check <<- function(data=data, dist_thresh = configOptions$mortDistance, time_thresh = configOptions$mortTime){
  if(inherits(data, "data.frame") != TRUE) stop("data is not a dataframe")
  if(any(colnames(data) == "newMasterDate") == FALSE) stop(print("You forgot a date column"))
  if(any(colnames(data) == "newUid") == FALSE) stop(print("You forgot a id column"))
  if(!inherits(data$newMasterDate, "POSIXct")) stop(print("date column is not POSIXct"))
  key <- 1:nrow(data)
  key2 <- key[order(data$newUid, data$newMasterDate)]
  if(all(key==key2)==FALSE) stop(print("Your data are not ordered correctly"))
  if(any(duplicated(data[c("newUid", "newMasterDate")])) == TRUE) stop("You have duplicates in your database")
  if(any(is.na(data$newMasterDate) == TRUE)) stop ("You have NAs in your date column")
  if(any(is.na(data$newUid) == TRUE)) stop ("You have NAs in your id column")
  if(nrow(data[is.na(data$dist)==TRUE,])> length(unique(data$newUid))) warning("You have a lot of NAs in your dist column!!!!!")


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

  result <- data[data$key %in% rwsmin, c("newUid", "newMasterDate")]

  names(result) <- c("newUid","date_start")
  result <- cbind(result, data[data$key %in% rwsmax, c("newMasterDate")])
  names(result) <- c("newUid","date_start","date_end")
  result$mort_time <- as.numeric(difftime(result$date_end,result$date_start,
                               tz=attr(data$newMasterDate,"tzone"), units="hours"))
  result <- result[result$mort_time > time_thresh,]

  print(result)

  if(nrow(result)==0){
    print("Your data look good given your parameters!")
    return(NULL)
  }else{
    return(result)
  }
}
