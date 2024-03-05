#' Calculate max distance among relocations based on an id column
#'
#' @param datasf A sf dataframe of points with at least 1 column representing a id column
#' @param id.name A character specifying the name of the column representing unique sequences of points.
#'
#' @return Returns a data frame with a max distance in km for each sequence in id.name, including some other derived summaries. 
#'
#' @examples
#' # it's simple!
#' result <- CalSeqDistances(datasf=d, id.name="mig")
#' head(result)

#' @export

CalcSeqDistances <- function(datasf=d, 
                            id.name="mig" 
){
  
  #manage packages
  if(all(c("sf","fields") %in% installed.packages()[,1])==FALSE)
    stop("You must install the following packages: sf, fields.")
  require(sf)
  require(fields)
  
  if("sf" %in% class(datasf)==FALSE)
    stop("datasf must be a sf data frame from package sf!")
  if(grepl("units=m", st_crs(datasf)$proj4string)==FALSE)
    stop("Your datasf is not in a projection with units=m!")
  # take out of sf
  datasf$x <- st_coordinates(datasf)[,1]
  datasf$y <- st_coordinates(datasf)[,2]
  datasf <- st_drop_geometry(datasf)  
  if(any(is.na(datasf[,id.name])))
    stop("You have NAs in your id.name column!")
  
  u <- unique(datasf[,id.name])
  
  mig_dists <- do.call(rbind, lapply(1:length(u), function(i){
    dsts <- datasf[datasf[,id.name] == u[i],c("x","y")]
    dsts <- rdist(dsts)
    return(data.frame(id.name=u[i], max_dist_km=max(dsts)/1000))
  }))
  # add some other meta information
  mig_dists$mean_max_dist <- mean(mig_dists$max_dist_km)
  mig_dists$sd_max_dist <- sd(mig_dists$max_dist_km)
  mig_dists$min_max_dist <- min(mig_dists$max_dist_km)
  mig_dists$max_max_dist <- max(mig_dists$max_dist_km)
  
  rm(datasf) # remove objects
  gc()
  
  return(mig_dists)
  
}