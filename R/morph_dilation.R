#' Moprpholgogical operators
#' 
#' Perform a dilation
#' 
#' @param r a raster object
#' @return returns an object of class raster. See \code{raster}
#' @author Hans Ole &Oslashrka \email{hans.ole.orka@@gmail.com}
#' @seealso \link[raster]{focal}
#' @rdname morphology
dilation <- function(r){
	require("raster")
	r1 <- focal(r,w=filterCircularOnes(3),max,pad=TRUE)
	return(r1)
}
