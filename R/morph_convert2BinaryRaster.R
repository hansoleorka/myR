#' Moprpholgogical operators
#' 
#' Convert a raster to a binary raster 
#' 
#' @inheritParams myR::dilation
#' @rdname morphology
convert2BinaryRaster <- function(r){
	require("raster")
	r@data$bin <- ifelse(r@data$layer>0,1,0)
	r <- r[,grepl("bin",names(r))]
	r <- raster(r)
	return(r)
}
