#' Moprpholgogical operators : erosion
#' 
#' Perform a erosion
#' 
#' @inheritParams myR::dilation
#' @rdname morphology
erosion <- function(r){
	require("raster")
	r@data@values <-ifelse(r@data@values==1,0,1) 
	r1 <- focal(r,w=filterCircularOnes(3),max,pad=TRUE)
	r1@data@values <-ifelse(r1@data@values==1,0,1)
	return(r1)
}