#' Area Under The Curve (AUC)
#' 
#' Compute the area under the receiver operating curve (AUC) based on the trapezoid method.
#' 
#' @param y a vector of ones and zeros descirbing presense/absense 
#' @param probs predicted probabilities
#' @param plot logical value indicating if the ROC should be ploted.
#' @return a scalar value
#' @details See s. 38-42 in reference and Rune's excel ark. 
#' @author Hans Ole &Oslashrka \email{hans.ole.orka@@gmail.org}
#' @references Halvorsen, R. 2012. A maximum likelihood explanation of MaxEnt, and some 
#' implications for distribution modelling. Sommerfeltia 36: 1-XXX. Oslo. ISBN 82-7420-0XXX. 
#' ISSN 0800-6865.
# @examples 
# y <- c(0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,1,0,0,0,0,0,1,0,0,1,0,0,0,0,1,0,0,0,0,1,0,1,1,0,0,0,
# 1,1,1,0,1,0,0,1,0,1,0,1,1,0,0,1,0,1,1,0,1,1,0,1,1,0,0,1,1,1,0,0,1,1,1,1,0,1,1,0,1,1,1,1,0,0,1,1,1,1,1,1,1,0,1,1)
# probs <- c(0.02,0.05,0.06,0.08,0.11,0.12,0.15,0.16,0.16,0.17,0.19,0.21,0.22,0.22,0.24,0.25,0.26,
# 0.28,0.28,0.31,0.32,0.33,0.33,0.33,0.36,0.36,0.38,0.39,0.4,0.4,0.42,0.44,0.45,0.47,0.47,0.48,0.48,
# 0.5,0.51,0.52,0.53,0.56,0.56,0.56,0.58,0.59,0.6,0.6,0.62,0.62,0.63,0.64,0.66,0.67,0.67,0.68,0.69,0.7,
# 0.7,0.71,0.72,0.74,0.74,0.74,0.75,0.76,0.76,0.77,0.77,0.79,0.8,0.81,0.81,0.82,0.82,0.83,0.84,0.86,0.87,
# 0.87,0.87,0.88,0.88,0.89,0.9,0.9,0.92,0.93,0.93,0.93,0.94,0.94,0.94,0.95,0.96,0.97,0.97,0.97,0.99,0.99)
# areaUnderCurve(y,probs)
# areaUnderCurve(y,probs,plot=TRUE)

areaUnderCurve <- function(y,probs,plot=FALSE){
	y <- y[order(probs)]
	probs <- probs[order(probs)]
	
	y1 <- 1 - y
	tpr <- fpr <- c()
	tpr[1] <- fpr[1] <- 1
	sel <- rep(TRUE,length(probs))
	for(i in 1:length(probs)){
		sel[i] <- FALSE
		tpr[i+1] <-sum(y[sel]) / sum(y)
		fpr[i+1] <-sum(y1[sel]) / sum(y1)
	}
	tpr <- c(tpr,0)
	fpr <- c(fpr,0)
#	
#	y2 <- c(NA,y,NA)
#	probs2 <- c(NA,probs,NA)
#	round(cbind(y2,probs2,tpr,fpr),2)
	
	
	AUCcontr <- (fpr[-length(fpr)]-fpr[-1]) * (tpr[-length(tpr)] + tpr[-1])/2
	
	if(plot){
		windows()
		plot(fpr,tpr,asp=1,type="o")
	}
	auc <- sum(AUCcontr)
	return(auc)
}




