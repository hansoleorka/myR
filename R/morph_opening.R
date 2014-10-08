#' Moprpholgogical operators : opening
#' 
#' Perform a opening
#' 
#' @inheritParams myR::dilation
#' @rdname morphology
opening <- function(r){
	err <- erosion(r)
	dil <- dilation(err)
	return(dil)
}