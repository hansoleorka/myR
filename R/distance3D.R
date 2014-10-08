#' Compute distance 3D
#' 
#' Compute distance 3D
#' 
#' @param x1 the first x value
#' @param y1 the first y value
#' @param z1 the first z value
#' @param x2 the second x value
#' @param y2 the second y value
#' @param z2 the second z value
#' @return a scalar value 
#' @details Sjekk funksjonen.. 
#' @author Hans Ole Orka \email{hans.ole.orka@@gmail.org}
distance3D <- function(x1,y1,z1,x2,y2,z2){
	xd = x2-x1
	yd = y2-y1
	zd = z2-z1
	Distance = sqrt(xd*xd + yd*yd + zd*zd)
	return(Distance)
}

