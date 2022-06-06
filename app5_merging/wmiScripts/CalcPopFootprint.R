#' Calculate Population level Footprints
#'
#' @param Foot.fldr This is the location of the folder where the season folders are located for footprints.
#' @param out.fldr Folder to store outputs.
#' @param seas2merge Character vector of the seasons to merge together. These values must match folder names in Foot.fldr. If vector of length 1, no seasons will be merged.
#' @param contour.levels Percents (ranging from 1 to 90, in whole numbers) of contours. Do not include a 0. Max value is 90. Must be ascending. Represents % of animals using an area.
#' @param min_area_drop Do you want to remove some small polygons from the output? Numeric value in square meters.
#' @param min_area_fill Do you want to fill in holes with areas smaller than this value? Numeric value in square meters.
#' @param simplify Do you want to smooth the contours so they aren't gridlooking? TRUE or FALSE
#' @param ksmooth_smoothness If simplify is TRUE, smoothness value of the kernel smoothing (i.e., simplification). See smooth() function in smoothr package.
#' @param out.proj  Specify the projection of the out shapefile, as a proj4string.
#'
#' @return Returns tif files (merged UDs) and a shapefile (of population contours) of the resultant merging of UDs
#'
#' @examples
#' # To come.
#'
#' @export

CalcPopFootprint <- function(
  Foot.fldr = "C:/Users/jmerkle_local/Desktop/Corridor_test/Footprints_Test",
  out.fldr = "C:/Users/jmerkle_local/Desktop/Corridor_test/Footprints_out",
  udFootprintsToDrop = NULL,
  seas2merge = c("spring", "fall"),
  contour.levels = c(5,10,15,20,30),
  min_area_drop = 20000,
  min_area_fill = 20000,
  simplify = TRUE,
  ksmooth_smoothness = 2,
  out.proj = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
){

  #manage packages
  if(all(c("sf","raster","stringr","smoothr", "rgeos") %in% installed.packages()[,1])==FALSE)
    stop("You must install the following packages: sf, raster, stringr, smoothr, rgeos")
  require(sf)
  require(raster)
  require(stringr)
  require(smoothr)
  require(rgeos)  

  # some checks
  if(length(dir(out.fldr))> 0)
    stop("Your out.fldr has something in it. It should be empty!")
  if(length(dir(Foot.fldr))< 1)
    stop("There are no Footprints in your Foot.fldr!")
  if(any(!seas2merge %in% dir(Foot.fldr)))
    stop("One or more of your seas2merge values do not represent season folders inside your Foot.fldr!")
  if(any(!contour.levels %in% 1:90))
    stop("contour.levels must be integers between 1 and 100!!")
  if(any(!contour.levels == sort(contour.levels)))
    stop("contour.levels must be written in ascending order!")

  # -------------------#
  # File management ####
  # -------------------#

  fls <- list()  # open up a list to store all the file names with directory
  nms <- list()  # open up a list to store all the file names only without the tif


  # if there is more than one season being used, bring those files together
  for(i in 1:length(seas2merge)){
    if(!is.null(udFootprintsToDrop)){
      # add tif extension
      udFootprintsToDropFileNames<-paste0(udFootprintsToDrop,'.tif')
      theseFiles<-list.files(paste0(Foot.fldr, "/", seas2merge[i]), ".tif$", full.names=TRUE)
      # drop and files that are in the drop vector
      theseFiles<-theseFiles[!grepl(paste0(udFootprintsToDropFileNames, collapse = "|"), theseFiles)]
      fls[[i]] <- theseFiles
      theseNames<-sub(".tif", "", list.files(paste0(Foot.fldr, "/", seas2merge[i]), ".tif$", full.names=FALSE))
      theseNames<-theseNames[!grepl(paste0(udFootprintsToDrop, collapse = "|"), theseNames)]
      nms[[i]]<-theseNames
    }else{
      fls[[i]] <- list.files(paste0(Foot.fldr, "/", seas2merge[i]), ".tif$", full.names=TRUE)
      nms[[i]] <- sub(".tif", "", list.files(paste0(Foot.fldr, "/", seas2merge[i]), ".tif$", full.names=FALSE))
    }
  }

  # if there is more than one season being used, bring those files together
  # for(i in 1:length(seas2merge)){
  #   fls[[i]] <- list.files(paste0(UD.fldr, "/", seas2merge[i]), ".tif$", full.names=TRUE)
  #   nms[[i]] <- sub(".tif", "", list.files(paste0(UD.fldr, "/", seas2merge[i]), ".tif$", full.names=FALSE))
  # }








  fls <- unlist(fls)
  nms <- unlist(nms)

  # get unique ids
  ids <- str_split_fixed(nms, "_", 3)[,1]
  ids.unique <- unique(ids)
  numb_ids <- length(ids.unique)
  print(paste0("You have ", numb_ids, " uniqe ids."))

  # get unique years
  years <- str_split_fixed(nms, "_", 3)[,2]
  yrs.unique <- unique(years)
  print(paste0("You have ", length(yrs.unique), " uniqe years. They are: ", paste(yrs.unique, collapse = ", "), "."))

  # get unique seasons
  seasons <- str_split_fixed(nms, "_", 3)[,3]
  seas.unique <- unique(seasons)
  print(paste0("You have ", length(seas.unique), " unique seasons. They are: ", paste(seas.unique, collapse = ", "), "."))

  # -----------#
  # Merging ####
  # -----------#
  dir.create(paste0(out.fldr, "/ids"))

  # i=1
  for(i in 1:numb_ids){
    print(i)
    sub1.fls <- fls[ids %in% ids.unique[i]]
    sub1.nms <- nms[ids %in% ids.unique[i]]

    s <- stack(sub1.fls)
    if(nlayers(s)>1){
      s <- sum(s)
      s <- reclassify(s, rcl=matrix(c(0.9,Inf, 1), ncol=3))
    }  # end of work if there are multiple layers in s

    # write out file. Will be named by first merge underscore second merge.
    writeRaster(s, filename = paste0(out.fldr, "/ids/", ids.unique[i], ".tif"),
                format = "GTiff", overwrite = TRUE, datatype='INT1U')
  } # end of loop over opt1
  rm(s)
  gc()

  # Create final population UD
  Pop.Foot <- stack(list.files(paste0(out.fldr, "/ids"), ".tif$", full.names = TRUE))  # bring them back in to do mean value. Maybe do need upper level for year so these can go in there!
  if(nlayers(Pop.Foot)>1){
    Pop.Foot <- sum(Pop.Foot)  # sum up values
  }
  # calculate the percent of individuals using an area
  Pop.Foot.perc <- Pop.Foot/numb_ids

  writeRaster(Pop.Foot, filename = paste0(out.fldr, "/Pop_Footprint_NumbIds.tif"),  # Number of ids
              format = "GTiff", overwrite = TRUE, datatype='INT2U')
  writeRaster(Pop.Foot.perc, filename = paste0(out.fldr, "/Pop_Footprint_PropIds.tif"),  # proportion of ids
              format = "GTiff", overwrite = TRUE, datatype='FLT4S')

  rm(Pop.Foot, Pop.Foot.perc)
  gc()

  print("Done with merging the Footprints! Now calculating final contours.")

  # ------------------#
  # Final Contours ####
  # ------------------#

  Pop.Foot.perc <- raster(paste0(out.fldr, "/Pop_Footprint_PropIds.tif"))

  contour.levels2 <- contour.levels[(contour.levels/100) > (2/numb_ids)]
  contour.levels2 = c(0, c(2/numb_ids)-0.0001, (contour.levels2/100)-0.0001, .99)  #this starts with low 1 or more, then low 2 or more, then the other percents
  contour.levels2_names <- c(1,2,contour.levels[(contour.levels/100) > (2/numb_ids)])

  print(paste0("contours of ", paste(contour.levels2_names,collapse = ", "),
               " represent the following threshold number of individuals: ",
               paste(floor(contour.levels2[1:(length(contour.levels2)-1)]*numb_ids)+1,collapse=" - "),"."))

  meta <- data.frame(Footprint_info=c(paste0("You have ", numb_ids, " uniqe ids."),
                                      paste0("You have a total of ", length(fls), " sequences."),
                                      paste0("You have ", length(yrs.unique), " uniqe years. They are: ", paste(yrs.unique, collapse = ", "), "."),
                                      paste0("You have ", length(seas.unique), " unique seasons. They are: ", paste(seas.unique, collapse = ", "), "."),
                                      paste0("contours of ", paste(contour.levels2_names,collapse = ", "),
                                             " represent the following threshold number of individuals: ",
                                             paste(floor(contour.levels2[1:(length(contour.levels2)-1)]*numb_ids)+1,collapse=" - "),".")))

  # break up the raster into its contours
  classifiedRaster <- cut(Pop.Foot.perc, breaks=contour.levels2)

  # extract the contours as polygons
  classifiedPoly <- rasterToPolygons(classifiedRaster,dissolve=T)
  classifiedPoly <- as(classifiedPoly, "sf")

  # add proper labels
  classifiedPoly <- classifiedPoly[order(classifiedPoly$layer),]
  classifiedPoly$contour <- contour.levels2_names[classifiedPoly$layer]  # put only the names of the layers that were identified
  classifiedPoly$layer <- NULL

  # plot(classifiedPoly, color=classifiedPoly)
  # mapview(classifiedPoly, zcol="contour")

  # aggregate the different corridor levels
  # make it so the top level includes the ones below it!
  codes <- sort(unique(classifiedPoly$contour))
  classifiedPoly <- do.call(rbind, lapply(codes, function(i){

    tmp <- st_union(classifiedPoly[classifiedPoly$contour <= i,])

    # this removes polygon segments smaller than a specific threshold
    tmp <- drop_crumbs(tmp, threshold = min_area_drop)

    # this fills holes smaller than a specific threshold
    tmp <- fill_holes(tmp, threshold = min_area_fill)

    tmp <- st_cast(tmp, "POLYGON")

    return(st_as_sf(data.frame(contour = i),
                    geometry=tmp))
  }))

  # reorder for plotting
  classifiedPoly <- classifiedPoly[order(classifiedPoly$contour, decreasing=TRUE),]

  if(simplify == TRUE){
    # do something here so if it fails, it doesn't kill the whole thing!
    classifiedPoly2 <- try(smooth(classifiedPoly, method="ksmooth", smoothness=ksmooth_smoothness), silent=TRUE)
    # mapview(classifiedPoly2, zcol="contour")

    if("try-error" %in% class(classifiedPoly2)){
      print("Your k smoothing didn't work! Try changing the ksmooth_smoothness parameter and read about the smooth() function!")
    }else{
      classifiedPoly <- classifiedPoly2
      rm(classifiedPoly2)
    }
  } # end of if simplify equals TRUE

  # reproject to the projection of interest
  classifiedPoly <- st_transform(classifiedPoly, crs=out.proj)

  # write to a shapefile
  st_write(classifiedPoly, out.fldr, "Footprint_contours", driver="ESRI Shapefile", quiet=TRUE, append=FALSE)

  # calculate some areas
  codes <- sort(unique(classifiedPoly$contour))
  areas <- do.call(rbind, lapply(codes, function(i){
    data.frame(contour=i,
               area_SqKm=as.numeric(st_area(st_union(classifiedPoly[classifiedPoly$contour == i,], is_coverage = TRUE)))/1000000)
  }))

  write.csv(areas, paste0(out.fldr, "/Footprint_contour_areas.csv"), row.names = FALSE)
  write.csv(meta, paste0(out.fldr, "/Footprint_metadata.csv"), row.names = FALSE)

} # End of function
