#' Gini coefficient
#' 
#' Compute the Gini coefficient (Gini index or Gini ratio) which is a mesure of statistical dispersion  
#' 
#' @param x a vector
#' @return a scalar
#' @author Hans Ole Orka \email{hans.ole.orka@@gmail.org}
#' @references Gini, C (1909) Concentration and dependency ratios (in Italian). English translation in Rivista di Politica Economica, 87 (1997), 769-789.
gini <- function(x){
	j <- rank(x)
	N <- length(x)
	GC <- sum((2 * j - N - 1) * x) / 	sum(x*(N-1))
	return(GC)
}

