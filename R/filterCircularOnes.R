#' Creates a circular filter 
#' 
#' Create a circular filter of ones 
#' 
#' @param w filter size 
#' @param CircularRadius How to define the radius of the circular filter, either in a euclidian distance space (w-1)/2 or by using the pixels w/2. Defaults to "euclidian"
#' @details The fitler should be used toghter with the focal function in the raster-package.
#' @seealso \link[raster]{focal}
#' @return a matrix zeros and ones
#' @author Hans Ole Orka \email{hans.ole.orka@@gmail.org}
#' @examples
#' windows()
#' (f <- filterCircularOnes(w=3))
#' image(f)
#' (f<- filterCircularOnes(w=5))
#' image(f)
#' (f <- filterCircularOnes(w=9))
#' (f <- filterCircularOnes(w=19))
#' image(f)
filterCircularOnes <- function(w=3,CircularRadius="euclidan"){
	onesfilter <- matrix(rep(1,w^2),w,w)
	g <- expand.grid(c(1:w) - ((w-1)/2+1), c(1:w) - ((w-1)/2+1))
	if(CircularRadius == "euclidan") {th <- (w-1)/2}
	if(CircularRadius == "pixels") {th <- w/2}
	
	d <- sqrt(g[,1]^2 + g[,2]^2) > th
	circ <- matrix(d,w,w)
	onesfilter[circ] <- 0
	return(onesfilter)
}
