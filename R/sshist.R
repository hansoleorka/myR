#' Optimal number of bins of a histogram  
#' 
#' Function `sshist' returns the optimal number of bins of a histogram. The optimization principle is L2 loss function. See ref. for details.   
#' 
#' @param x Sample data.
##' x_min The data range within which the optimization applies to. Default value is min(x). 
##'  x_max The data range within which the optimization applies to. Default value is max(x).
##' A vector that specifies number of divisions (bins). The optimal number of bins is selected from the elements of N.  Default value is N = 2:50.   
#' @return 
##' Output argument
#optN: Optimal number of bins.
#N:    Bin numbers searched.
#C:    Cost function for N.
#		#
#' @author Author Hideaki Shimazaki \email{shimazaki at jhu.edu}
##' 2006 Author Hideaki Shimazaki
#Department of Physics, Kyoto University
#shimazaki at jhu.edu
#http://2000.jukuin.keio.ac.jp/shimazaki
#' @references Shimazaki and Shinomoto, Neural Comput. 19(6), 1503-1527, 2007

sshist <- function(x){
		
		N <- c(2:100)
		C <- numeric(length(N))
		D <- C
		
		#Computation of the Cost Function
		for (i in 1:length(N)) {
			D[i] <- diff(range(x))/N[i]
			
			edges = seq(min(x),max(x),length=N[i])
			hp <- hist(x, breaks = edges, plot=FALSE )
			ki <- hp$counts
			
			k <- mean(ki)
			v <- sum((ki-k)^2)/N[i]
			
			C[i] <- (2*k-v)/D[i]^2	#Cost Function
		}
		
		idx <- which.min(C)
		optD <- D[idx]
		
		edges <- seq(min(x),max(x),length=N[idx])
		h = hist(x, breaks = edges, plot=FALSE )
		
		return(h)
	}