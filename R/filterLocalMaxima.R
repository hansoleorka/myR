#' Local Maxima Filtering 
#' 
#' Find local maxima's in a local neighborhod    
#' 
#' @param CHM a object of class SpatialGridDataFrame  
#' @param w matrix of weights e.g. a 3 by 3 matrix; see Details. The matrix can also be expressed as the number of cells in a single direction or in two directions from the focal cell, in which case the weights are all set to 1. I.e. w=3 refers to a 3 by 3 matrix: 2 cells at each side of the focal cell, queen's case, 9 cells in total. This is equivalent to w=c(3,3). You can also specify a rectangular neighborhood, e.g. w=c(3,5); but the sides must be odd numbers. If you need even sides, you can add a column or row with weights of zero.
#' @param ov threshold of local maximas to consider
#' @return object of class SpatialPointsDataFrame-class
#' @author Hans Ole Orka \email{hans.ole.orka@@gmail.org}

filterLocalMaxima <- function(CHM,w=3,ov=2){
	require("raster")
	require("sp")
	r <- raster(CHM)
	#The extended maxima transformation is the regional maxima computation of the corresponding hmaxima transformation.
	#finds extended maxima where the range of values is not greater than h.
	LM <- focal(r,w=w,fun=max)

	
	#ELM <- focal(LM,w=3,fun=max)
	
	LM@data@values <- (r@data@values - LM@data@values) 
	LM@data@values <- ifelse(abs(LM@data@values) == 0,1,NA)
	
	#adj <- adjacent(LM,cell=c(1:length(LM))[LM@data@values==1])
	
	#Convert to points
	LM <- as(LM,"SpatialPointsDataFrame")
	
	o <- overlay(CHM,LM)
	#LM@data$dz <- CHM@data[o,1] old update retention okt13
	LM@data$dz <- o@data[,1]	
	LM <- LM[LM@data$dz>ov,]
	
#	ELM <- CHM.sm*LM 
#	ELM@data@values <- ifelse(ELM@data@values >= ov,ELM@data@values,NA) 
#	ELM <- as(ELM,"SpatialGridDataFrame")
	return(LM)
	
}