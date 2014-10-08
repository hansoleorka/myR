#' Creates a circular binomail filter 
#' 
#' Create a circular low-pass filter using binomial coefincient and filter weights obtained from  Pascal's triangle. Used for filtering Canopy height model before local maxima detection   
#' 
#' @param w filter size 
#' @param CircularRadius How to define the radius of the circular filter, either in a euclidian distance space (w-1)/2 or by using the pixels w/2. Defaults to "euclidian"
#' @details The filter should be used toghter with the focal function in the raster-package.
#' @seealso \link[raster]{focal} and \link{filterLocalMaxima}
#' @return a matrix of filter weights
#' @author Hans Ole Orka \email{hans.ole.orka@@gmail.org}
#' @examples
#' windows()
#' par(mfcol=c(2,2))
#' (f <- filterBinomial(w=3))
#' image(f)
#' (f <- filterBinomial(w=5))
#' image(f)
#' (f <- filterBinomial(w=9))
#' (f <- filterBinomial(w=19))
#' image(f)


filterBinomial <- function(w=3,CircularRadius="euclidan"){
	require(raster)
	pascalTriangle <- function(h) {
		lapply(0:h, function(i) choose(i, 0:i))
	}
	
	
	bc <- 0.5*0.5^w # Binomial coefisient
	h <- w -1 
	p <- matrix(pascalTriangle(h)[[w]],1,w)
	weights <- matrix(rep(p,w),w,w) * matrix(rep(p,w),w,w,byrow=TRUE) #Weights
	
	#Make circular filter
	g <- expand.grid(c(1:w) - ((w-1)/2+1), c(1:w) - ((w-1)/2+1))
	if(CircularRadius == "euclidan") {th <- (w-1)/2}
	if(CircularRadius == "pixels") {th <- w/2}
	
	d <- sqrt(g[,1]^2 + g[,2]^2) > th
	circ <- matrix(d,w,w)
	weights[circ] <- 0
	binfilter <- matrix(weights/sum(weights,na.rm=TRUE), ncol=w, nrow=w)#filter
	return(binfilter)
	}
