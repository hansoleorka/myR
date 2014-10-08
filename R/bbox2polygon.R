#' Create polygon from bounding box
#' 
#' Create a square polygon from a the bounding box of input geometry. 
#' 
#' @param obj spatial object or object with existing bbox method.
#' @param ID unique identifier of polygon, defaults to name of first column
#' @return a object of class SpatialPolygons
#' @details Check function...  
#' @author Hans Ole {\O}rka \email{hans.ole.orka@@gmail.org}
bbox2polygon <- function(obj,ID=names(obj)[1]){
  bb <- bbox(obj)
  x <- c(bb[1,1],bb[1,2],bb[1,2],bb[1,1],bb[1,1])
  y <- c(bb[2,1],bb[2,1],bb[2,2],bb[2,2],bb[2,1]) 
  SpatialPolygons(list(Polygons(list(Polygon(cbind(x,y))),ID=ID)),proj4string=CRS(proj4string(obj)))
}

library(devtools)
install_github("hansoleorka/myR")
