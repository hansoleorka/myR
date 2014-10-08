#' Root Mean Square Error
#' 
#' Compute the root means square error
#' 
#' @param y the reference y value
#' @param yhat the estimated y valye
#' @return a scalar value 
#' @author Hans Ole Orka \email{hans.ole.orka@@gmail.org}
rmse <-function(y,yhat){(sqrt(mean((yhat-y)^2)))}

#rmsd <- function(y,yhat){
#	rms <- sqrt(mean((y-yhat)^2))
#	pr <- rms/mean(y)
#	return(paste("rmsd = ",round(rms,2)," - ","rmsd% = ",round(pr,2),sep=""))
#	
#}