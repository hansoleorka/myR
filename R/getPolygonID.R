#' Polygons ids
#' 
#' Extraxt the ID of a SpatialPolygon or SpatialPolgyonsDataFrame. See \code{sp} 
#' 
#' @param obj a sp object
#' @return returns an vector with IDs
#' @author Hans Ole &Oslashrka \email{hans.ole.orka@@gmail.com}
getPolygonID <- function(obj){
	id <- sapply(slot(obj, "polygons"), function(x) slot(x, "ID"))
	return(id)
}
