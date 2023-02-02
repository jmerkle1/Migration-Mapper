#' Create a population grid to build all UDs and footprints over.
#'
#' @param datasf A sf dataframe of points with at least 1 column representing an id column. Projection must be in meters
#' @param out.fldr A character specifying the folder path where the grid should be saved.
#' @param mult4buff Proportion of current extent or bbox to add for building the grid. Typically 0.2 or 0.3.
#' @param cell.size Numeric providing the cell or grid size (in meters) for output.
#'
#' @return Returns a data frame with a max distance in km for each sequence in id_name, including some other derived summaries.
#'
#' @examples
#' # it's simple!
#' CalcPopGrid(datasf=d, out.fldr=getwd(), mult4buff=0.3, cell.size=500)

#' @export

CalcPopGrid <- function(datasf=d,
                        out.fldr=getwd(),
                        mult4buff=0.3,
                        cell.size=500){

  #manage packages
  if(all(c("sf","raster") %in% installed.packages()[,1])==FALSE)
    stop("You must install the following packages: sf, raster")
  require(sf)
  require(raster)

  if("sf" %in% class(datasf)==FALSE)
    stop("datasf must be a sf data frame from package sf!")
  if(grepl("units=m", st_crs(datasf)$proj4string)==FALSE)
    stop("Your datasf is not in a projection with units=m!")

  ext2 <- raster::extent(datasf)
  multiplyers <- c((ext2[2]-ext2[1])*mult4buff, (ext2[4]-ext2[3])*mult4buff)   # add about 20% around the edges of your extent (you can adjust this if necessary)
  ext2 <<- raster::extend(ext2, multiplyers)
  grd <- raster(ext2)
  res(grd) <- cell.size
  projection(grd) <- st_crs(datasf)$proj4string
  grd[] <- 0  # put some 0s in there so it isn't empty

  rm(ext2, multiplyers, datasf) #remove objects
  gc()

  #write out grid
  writeRaster(grd, filename=paste0(out.fldr,"/PopGrid_empty.tif"),
              format = "GTiff", overwrite = TRUE)

  return(paste0("Your population grid has ", ncell(grd), " cells! It has been written to your out.fldr"))
}
