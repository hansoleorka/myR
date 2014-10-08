#' Compute distance
#' 
#' Compute distance
#' 
#' @param x1 the first x value
#' @param y1 the first y value
#' @param x2 the second x value
#' @param y2 the second y value
#' @return a scalar value 
#' @details Sjekk funksjonen.. 
#' @author Hans Ole Orka \email{hans.ole.orka@@gmail.org}
distance <- function(x1,y1,x2,y2){sqrt(abs(x1-x2)^2+abs(y1-y2)^2)}
