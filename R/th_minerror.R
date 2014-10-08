#' Minimum error thresholding
#' 
#' Find a global threshold for a grayscale image using the minimum error thresholding method.   
#' 
#' @param h histogram
#' @return threshold 
#' @details Functions based on matlab function by 2004 Antti Niemist&#246 http://www.dentistry.bham.ac.uk/landinig/software/autothreshold/autothreshold.html
#' @author Hans Ole Orka \email{hans.ole.orka@@gmail.org}
#' @references J. Kittler and J. Illingworth, "Minimum error thresholding," Pattern Recognition, vol. 19, pp. 41-47, 1986.
#' @references C. A. Glasbey, "An analysis of histogram-based thresholding algorithms,"CVGIP: Graphical Models and Image Processing, vol. 55, pp. 532-537, 1993.
#' @examples
#' x <- c(rnorm(100,10),rnorm(50,20))
#' h <- hist(x)
#' T <- minError(h)
#' hist(x)
#' abline(v=T)
minError <- function(h) {
y <- h$counts
n = length(y)-1 

A <- function(y,j) {#The partial sum A from C
	x = sum(y[1:(j+1)])
	return(x)}  

B <- function(y,j){	# The partial sum B from C
		ind <- 0:j
		x <- ind*y[1:(j+1)]
		x <- sum(x)
		return(x)}
		
C <- function(y,j){ #The partial sum C from C
		ind = 0:j
		x = ind^2*y[1:(j+1)]
		x <- sum(x)
		return(x)}
			

# The threshold is chosen such that the following expression is minimized.
	vec <- c()
	for(threshold in 0:n){
		mu = B(y,threshold)/A(y,threshold)
		nu = (B(y,n)-B(y,threshold))/(A(y,n)-A(y,threshold))
		p = A(y,threshold)/A(y,n)
		q = (A(y,n)-A(y,threshold)) / A(y,n)
		sigma2 = C(y,threshold)/A(y,threshold)-mu^2
		tau2 = (C(y,n)-C(y,threshold)) / (A(y,n)-A(y,threshold)) - nu^2
		vec[threshold+1] = p*log10(sqrt(sigma2)/p) + q*log10(sqrt(tau2)/q)
	}


vec[is.infinite(vec)] <- NaN
T <- h$mids[which.min(vec)-1]
return(T)

}