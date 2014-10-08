#' Otsu thresholding
#' 
#' Find otsu threshold   
#' 
#' @param h histogram created by \code{hist} or \code{sshist}
#' @return threshold 
#' @details to be written an other time... 
#' @author Hans Ole &Oslashrka \email{hans.ole.orka@@gmail.org}
#' @examples
#' x <- c(rnorm(100,10),rnorm(50,20))
#' h <- hist(x)
#' T <- otsuModif(h)
#' hist(x)
#' abline(v=T)

otsuModif <- function(h){
	counts <- h$counts
	if(sum(counts)<2 | max(h$intensities == 1)){
		th <- NA
	}
	else {
		
		num_bins <- length(counts)
		bins <- h$mids
		
		#num_bins = 100
		#zsegm = z_laser_plot
		
		# Freedman-Diaconis' rule:
		# iqr = prctile(zsegm, [25 75])
		# num_bins = ceil(range(zsegm) / ( 2 * range(iqr)/ numel(zsegm)^(1/3)))
		# 
		# 
		# # Scott's rule:
		# num_bins = ceil(range(zsegm) / (3.5*std(zsegm)/ numel(zsegm)^(1/3)))
		# num_bins = sshist(zsegm, range(zsegm) / numel(zsegm))
		#[counts bins] <- hist(zsegm, num_bins)
		
		# Variables names are chosen to be similar to the formulas in
		# the Otsu paper.
		p <- counts / sum(counts)
		omega <- cumsum(p)
		mu <- cumsum(p * (1:num_bins))
		mu_t <- mu[length(mu)]
		
		sigma_b_squared <- (mu_t * omega - mu)^2 / (omega * (1 - omega))
		
		# Find the location of the maximum value of sigma_b_squared.
		# The maximum may extend over several bins, so average together the
		# locations.  If maxval is NaN, meaning that sigma_b_squared is all NaN,
		# then return 0.
		maxval <- max(sigma_b_squared,na.rm=TRUE)
		isfinite_maxval <- is.finite(maxval)
		if(isfinite_maxval){
			idx <- mean(c(1:num_bins)[sigma_b_squared == maxval],na.rm=TRUE)
			# Normalize the threshold to the range [0, 1].
			#level = (idx - 1) / (num_bins - 1)
			locs <- max(c(ceiling(idx - 1), 1))
			th <- bins[locs]
		}
		if(isfinite_maxval==FALSE){
			th <- 0.0
		}
	}
	return(th)
}

