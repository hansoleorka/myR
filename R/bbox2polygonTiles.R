#' Create polygon from bounding box for each object
#' 
#' Create a square polygon from a the bounding box of each object in a SpatialPolygonsDataFrame.
#' 
#' @param obj SpatialPolygonsDataFrame
#' @return a object of class SpatialPolygonsDataFrame
#' @details Check function...  
#' @author Hans Ole {\O}rka \email{hans.ole.orka@@gmail.org}
bbox2polygonTiles <- function(obj){
  ID <- sapply(slot(obj, "polygons"), function(x) slot(x, "ID"))
  l <- list()
  for(i in 1:length(obj)){
    bb <- bbox(obj[i,])
    x <- c(bb[1,1],bb[1,2],bb[1,2],bb[1,1],bb[1,1])
    y <- c(bb[2,1],bb[2,1],bb[2,2],bb[2,2],bb[2,1]) 
    l[[i]] <- Polygons(list(Polygon(cbind(x,y))),ID[i])
  }
  SpP <- SpatialPolygons(l)#,proj4string=crs)
  SpPD <- SpatialPolygonsDataFrame(SpP,data=obj@data)
  proj4string(SpPD) <- proj4string(obj)
  return(SpPD)
}
