#' Calculate Population level UDs and contours
#'
#' @param UD.fldr This is the location of the folder where the season folders are located for UDs.
#' @param out.fldr Folder to store outputs.
#' @param seas2merge Character vector of the seasons to merge together. These values must match folder names in UD.fldr. If vector of length 1, no seasons will be merged.
#' @param merge.order Specify the order of operations for merging UDs. Character vector of length 1 or 2. Can only be from these values: id, year, or seas.
#' @param contour How much of the volume of the UD to keep each time there is a merge? If 99, the lowest 1% of the volume will be removed from the UD. if 100, nothing will be removed at each merge.
#' @param contour.type How should the contours be calculated? Must either be Area or Volume. Contours with Area represent the % of the total area used. Contours with volume represent the % of time spent in areas.
#' @param contour.levels Percents (ranging from 1 to 100, in whole numbers) of contours to identify. Do not include a 0. Max value is 100. Must be ascending.
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

CalcPopUse <- function(
  UD.fldr = "",
  out.fldr = "",
  udFootprintsToDrop = NULL,
  seas2merge = c("spring", "fall"),
  merge.order = c("year", "id"),
  contour = 90,
  contour.type = "Area",
  contour.levels = c(5,10,15,20,30,40,50,60,70,80,90),
  min_area_drop = 20000,
  min_area_fill = 20000,
  simplify = TRUE,
  ksmooth_smoothness = 2,
  out.proj = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
){

  #manage packages
  if(all(c("sf","raster","stringr","move","smoothr", "rgeos") %in% installed.packages()[,1])==FALSE)
    stop("You must install the following packages: sf, raster, stringr, move, smoothr, rgeos")
  require(sf)
  require(raster)
  require(stringr)
  require(move)
  require(smoothr)
  require(rgeos)

  # some checks
  if(length(dir(out.fldr))> 0)
    stop("Your out.fldr has something in it. It should be empty!")
  if(length(dir(UD.fldr))< 1)
    stop("There are no UDs in your UD.fldr!")
  if(length(merge.order) > 2)
    stop("merge.order cannot have more than 2 values!")
  if(any(merge.order %in% c("id","year","seas") == FALSE))
    stop("merge.order can only contain 2 of the folloing character values: id, year, seas.")
  if(any(!seas2merge %in% dir(UD.fldr)))
    stop("One or more of your seas2merge values do not represent season folders inside your UD.fldr!")
  if(any(!contour.type %in% c("Area","Volume")))
    stop("contour.type must be either Area or Volume!!")
  if(any(!contour.levels %in% 1:100))
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
      theseFiles<-list.files(paste0(UD.fldr, "/", seas2merge[i]), ".tif$", full.names=TRUE)
      # drop and files that are in the drop vector
      theseFiles<-theseFiles[!grepl(paste0(udFootprintsToDropFileNames, collapse = "|"), theseFiles)]
      fls[[i]] <- theseFiles
      theseNames<-sub(".tif", "", list.files(paste0(UD.fldr, "/", seas2merge[i]), ".tif$", full.names=FALSE))
      theseNames<-theseNames[!grepl(paste0(udFootprintsToDrop, collapse = "|"), theseNames)]
      nms[[i]]<-theseNames
    }else{
      fls[[i]] <- list.files(paste0(UD.fldr, "/", seas2merge[i]), ".tif$", full.names=TRUE)
      nms[[i]] <- sub(".tif", "", list.files(paste0(UD.fldr, "/", seas2merge[i]), ".tif$", full.names=FALSE))
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
  print(paste0("You have ", length(ids.unique), " uniqe ids."))

  # get unique years
  years <- str_split_fixed(nms, "_", 3)[,2]
  yrs.unique <- unique(years)
  print(paste0("You have ", length(yrs.unique), " uniqe years. They are: ", paste(yrs.unique, collapse = ", "), "."))

  # get unique seasons
  seasons <- str_split_fixed(nms, "_", 3)[,3]
  seas.unique <- unique(seasons)
  print(paste0("You have ", length(seas.unique), " unique seasons. They are: ", paste(seas.unique, collapse = ", "), "."))

  meta <- data.frame(UD_info=c(paste0("You have ", length(ids.unique), " uniqe ids."),
                               paste0("You have a total of ", length(fls), " sequences."),
                               paste0("You have ", length(yrs.unique), " uniqe years. They are: ", paste(yrs.unique, collapse = ", "), "."),
                               paste0("You have ", length(seas.unique), " unique seasons. They are: ", paste(seas.unique, collapse = ", "), "."),
                               paste0("Your merge order was ", paste(merge.order, collapse = " and then "), "."),
                               paste0("Your contour type was ", contour.type, ".")))

  # create list to manage order of operations
  opts.order <- list()
  for(i in 1:length(merge.order)){
    if(merge.order[i] == "id"){
      opts.order[[i]] <- ids.unique
      names(opts.order)[i] <- "ids"
    }
    if(merge.order[i] == "seas"){
      opts.order[[i]] <- seas.unique
      names(opts.order)[i] <- "seasons"
    }
    if(merge.order[i] == "year"){
      opts.order[[i]] <- yrs.unique
      names(opts.order)[i] <- "years"
    }
  }

  # -----------#
  # Merging ####
  # -----------#
  # If there is only 1 level to merge ####
  if(length(merge.order)==1){
    dir.create(paste0(out.fldr, "/", names(opts.order)[1]))

    # i=1
    for(i in 1:length(opts.order[[1]])){
      print(i)
      sub1.fls <- fls[get(names(opts.order)[1]) %in% opts.order[[1]][i]]
      sub1.nms <- nms[get(names(opts.order)[1]) %in% opts.order[[1]][i]]

      s <- stack(sub1.fls)
      if(nlayers(s)>1){
        s <- sum(s)
        s <- s/sum(values(s))
        # remove cutoff, and restandardize to sum to equal 1
        cutoff <- sort(values(s), decreasing=TRUE)
        vlscsum <- cumsum(cutoff)
        cutoff <- cutoff[vlscsum > contour/100][1]
        s <- reclassify(s, rcl=matrix(c(-1,cutoff,0),2,3, byrow=T))
        s <- s/sum(values(s))   #verify that they add up to 1
      }  # end of work if there are multiple layers in s

      # write out file. Will be named by first merge underscore second merge.
      writeRaster(s, filename = paste0(out.fldr, "/", names(opts.order)[1], "/", opts.order[[1]][i], ".tif"),
                  format = "GTiff", overwrite = TRUE, datatype='FLT4S')
    } # end of loop over opt1
    rm(s)
    gc()

    # Create final population UD
    Pop.UD <- stack(list.files(paste0(out.fldr, "/", names(opts.order)[1]), ".tif$", full.names = TRUE))  # bring them back in to do mean value. Maybe do need upper level for year so these can go in there!
    if(nlayers(Pop.UD)>1){
      Pop.UD <- sum(Pop.UD)  # sum up values
      Pop.UD <- Pop.UD/sum(values(Pop.UD))
      # remove cutoff, and restandardize to sum to equal 1
      cutoff <- sort(values(Pop.UD), decreasing=TRUE)
      vlscsum <- cumsum(cutoff)
      cutoff <- cutoff[vlscsum > contour/100][1]
      Pop.UD <- reclassify(Pop.UD, rcl=matrix(c(-1,cutoff,0),2,3, byrow=T))
      Pop.UD <- Pop.UD/sum(values(Pop.UD))   #verify that they add up to 1
    }  # end of work if there are multiple layers in opt1.stack

    writeRaster(Pop.UD, filename = paste0(out.fldr, "/Pop_UD.tif"),
                format = "GTiff", overwrite = TRUE, datatype='FLT4S')

  } # end of section with only 1 level for merging

  # If there are 2 levels for merging ####
  if(length(merge.order)==2){

    # make initial level folders
    dir.create(paste0(out.fldr, "/", names(opts.order)[1]))
    for(i in 1:length(opts.order[[1]])){
      dir.create(paste0(out.fldr, "/",names(opts.order)[1], "/", opts.order[[1]][i]))
    }
    # i=1
    for(i in 1:length(opts.order[[1]])){
      print(i)
      sub1.fls <- fls[get(names(opts.order)[1]) %in% opts.order[[1]][i]]
      sub1.nms <- nms[get(names(opts.order)[1]) %in% opts.order[[1]][i]]

      opt2 <- get(names(opts.order)[2])[get(names(opts.order)[1]) %in% opts.order[[1]][i]]
      opt2.unique <- unique(opt2)

      # e=1
      for(e in 1:length(opt2.unique)){
        sub2.fls <- sub1.fls[opt2 %in% opt2.unique[e]]
        sub2.nms <- sub1.nms[opt2 %in% opt2.unique[e]]

        s <- stack(sub2.fls)
        if(nlayers(s)>1){
          s <- sum(s)  # sum up values
          s <- s/sum(values(s))
          # remove cutoff, and restandardize to sum to equal 1
          cutoff <- sort(values(s), decreasing=TRUE)
          vlscsum <- cumsum(cutoff)
          cutoff <- cutoff[vlscsum > contour/100][1]
          s <- reclassify(s, rcl=matrix(c(-1,cutoff,0),2,3, byrow=T))
          s <- s/sum(values(s))   #verify that they add up to 1
        }  # end of work if there are multiple layers in s

        # write out file. Will be named by first merge underscore second merge.
        writeRaster(s, filename = paste0(out.fldr, "/", names(opts.order)[1], "/", opts.order[[1]][i],"/", opts.order[[1]][i], "_", opt2.unique[e], ".tif"),
                    format = "GTiff", overwrite = TRUE, datatype='FLT4S')

      }  # end of loop over opt2
      rm(s)
      gc()

      # bring in opt2 layers to merge
      opt2.stack <- stack(list.files(paste0(out.fldr, "/", names(opts.order)[1], "/", opts.order[[1]][i]), ".tif$", full.names = TRUE))  # bring them back in to do mean value. Maybe do need upper level for year so these can go in there!
      if(nlayers(opt2.stack)>1){
        opt2.stack <- sum(opt2.stack)  # sum up values
        opt2.stack <- opt2.stack/sum(values(opt2.stack))
        # remove cutoff, and restandardize to sum to equal 1
        cutoff <- sort(values(opt2.stack), decreasing=TRUE)
        vlscsum <- cumsum(cutoff)
        cutoff <- cutoff[vlscsum > contour/100][1]
        opt2.stack <- reclassify(opt2.stack, rcl=matrix(c(-1,cutoff,0),2,3, byrow=T))
        opt2.stack <- opt2.stack/sum(values(opt2.stack))   #verify that they add up to 1
      }  # end of work if there are multiple layers in opt2.stack

      writeRaster(opt2.stack, filename = paste0(out.fldr, "/", names(opts.order)[1], "/", opts.order[[1]][i], ".tif"),
                  format = "GTiff", overwrite = TRUE, datatype='FLT4S')

    } # end of loop over opt1
    rm(opt2.stack)
    gc()

    # Create final population UD
    Pop.UD <- stack(list.files(paste0(out.fldr, "/", names(opts.order)[1]), ".tif$", full.names = TRUE))  # bring them back in to do mean value. Maybe do need upper level for year so these can go in there!
    if(nlayers(Pop.UD)>1){
      Pop.UD <- sum(Pop.UD)  # sum up values
      Pop.UD <- Pop.UD/sum(values(Pop.UD))
      # remove cutoff, and restandardize to sum to equal 1
      cutoff <- sort(values(Pop.UD), decreasing=TRUE)
      vlscsum <- cumsum(cutoff)
      cutoff <- cutoff[vlscsum > contour/100][1]
      Pop.UD <- reclassify(Pop.UD, rcl=matrix(c(-1,cutoff,0),2,3, byrow=T))
      Pop.UD <- Pop.UD/sum(values(Pop.UD))   #verify that they add up to 1
    }  # end of work if there are multiple layers in opt1.stack

    writeRaster(Pop.UD, filename = paste0(out.fldr, "/Pop_UD.tif"),
                format = "GTiff", overwrite = TRUE, datatype='FLT4S')

  } # end of section with 2 levels for merging

  rm(Pop.UD)
  gc()

  print("Done with merging the UDs! Now calculating final contours.")

  # ------------------#
  # Final Contours ####
  # ------------------#

  popUDVol <- raster(paste0(out.fldr, "/Pop_UD.tif"))
  #identify the contour
  popUDVol <- getVolumeUD(as(popUDVol, Class = "DBBMM"))

  #identify the contour
  contour.levels2 <- c(0, contour.levels)  # add the 0 to make the cuts correct

  # This denotes whether we want contours based on area or volume
  if(contour.type == "Area"){
    breakValues <- raster::quantile(popUDVol[popUDVol != 1], probs = contour.levels2/100)
    # compute the contours
    classifiedRaster = raster::cut(popUDVol, breaks=breakValues)
  }else{  # if contour.type == "Volume"
    # compute the contours
    classifiedRaster = raster::cut(popUDVol, breaks=contour.levels2/100)
  }

  # extract the contours as polygons
  classifiedPoly <- rasterToPolygons(classifiedRaster,dissolve=T)
  classifiedPoly <- as(classifiedPoly, "sf")

  # add proper labels
  classifiedPoly <- classifiedPoly[order(classifiedPoly$layer),]
  contour.levels2 <- contour.levels2[2:length(contour.levels2)]  # reomve the 0
  classifiedPoly$contour <- contour.levels2[classifiedPoly$layer]  # put only the names of the layers that were identified
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
  st_write(classifiedPoly, out.fldr, "Pop_use_contours", driver="ESRI Shapefile", quiet=TRUE, append=FALSE)

  # calculate some areas
  codes <- sort(unique(classifiedPoly$contour))
  areas <- do.call(rbind, lapply(codes, function(i){
    data.frame(contour=i,
               area_SqKm=as.numeric(st_area(st_union(classifiedPoly[classifiedPoly$contour == i,], is_coverage = TRUE)))/1000000)
  }))

  write.csv(areas, paste0(out.fldr, "/PopUse_contour_areas.csv"), row.names = FALSE)
  write.csv(meta, paste0(out.fldr, "/PopUse_metadata.csv"), row.names = FALSE)

} # End of function
