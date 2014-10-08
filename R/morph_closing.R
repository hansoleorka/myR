#' Moprpholgogical operators : closing
#' 
#' 
#' @description Perform a closing
#' @inheritParams myR::dilation
#' @rdname morphology
closing <- function(r){
	dil <- dilation(err)
	err <- erosion(r)
	return(err)
}