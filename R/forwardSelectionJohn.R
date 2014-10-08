#' Forward variable selection
#' 
#' Perform a forward variable selection procedure as described by John in a \code{glm} setting
#' 
#' @param y dependent variable
#' @param X a data.frame of x variables 
#' @param family a description of the error distribution and link function to be used in the model. This can be a character string naming a family function, a family function or the result of a call to a family function. (See \code{family} for details of family functions.)
#' @param Crit Criterion to be used for variable selection "p.value" or "aic" implemented.
#' @param pValueThreshold a p-value threshold for single term models 
#' @param pValueLikelihoodRatio a p-value threshold for the likelihood ratio test
#' @param trace logical printing model selection 
#' @return returns an object of class inheriting from "glm". See \code{glm}
#' @author Hans Ole &Oslashrka \email{hans.ole.orka@@gmail.com}
#y <- Ostmarka.Birds[,10]
#X <- cbind(X.fs,X.rel)
#mod <- forwardSelectionJohn(y,X,family=binomial(),Crit="p.value",pValueThreshold=0.05)
#mod2 <- forwardSelectionJohn(y,X,family=binomial(),Crit="aic",pValueThreshold=0.05,pValueLikelihoodRatio=0.01)
forwardSelectionJohn <- function(y,X,family=gaussian(),Crit="p.value",pValueThreshold=NULL,pValueLikelihoodRatio=0.05,trace = TRUE){
	res <- c()
	for (i in 1:ncol(X)){
		mod <- glm(y~X[,i], family = family)
		p.value <- summary(mod)$coefficients[2,4]
		aic <- AIC(mod)
		res <- rbind(res,c(p.value=p.value,aic=aic,colid=i))
		
	}
	res <- data.frame(res)
	res$VarName <- names(X)
	
	if(is.null(pValueThreshold)==FALSE){
		print(paste("Remove variables with p>",pValueThreshold))
		X <- X[,res$p.value < pValueThreshold]
		res <- res[res$p.value < pValueThreshold,]
		res$colid <- 1:nrow(res)
	}
	
	print(paste("Rank variables according to",Crit))
	crit <- res[,grepl(Crit,names(res))]
	ord <- res$colid[order(crit)]
	
	sel <- ord[1]
	for(i in 2:ncol(X)){
		null.mod <- glm(y~.,data=data.frame(X[,sel]), family = family)
		mod <- glm(y~.,data=X[,c(sel,ord[i])], family = family)
		ms <- anova(null.mod,mod,test="Chi")
		p.value <- ms[[5]][2]
		if(trace){print(paste("Model:",as.character(formula(mod)[3]),round(p.value,6),sep=" "))}
		if(p.value < pValueLikelihoodRatio) {sel <- c(sel,ord[i])}
		
	}
	
	X2 <- data.frame(X[,sel])
	names(X2) <- names(X)[sel]
	mod <- glm(y~.,data=X2, family = family)
	return(mod)
}



