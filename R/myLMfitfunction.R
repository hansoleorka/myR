###############################################################################
#' BEST SUBSET VARIABLE SELECTION WITH PENALIZATION FOR COLLINEARITY
#' 
#' Model selection using the Bayesian information criterion (BIC) and a branc and bound 
#' search for the best subset implemented in package leaps.Among the alternative subsets, the subset with lowest BIC are preferred. 
#' Furthermore, the selection procedure penalized for collinearity using the 
#' variance inflation factor (VIF). The subset with one variable less are iteratively 
#' selected if any of the variables in the current subset has a large VIF greater than 5.
#' TODO: implement posibility to change VIF value 
#' 
#' @param Xy data.frame with X variables and the y variable named Xy$y
#' @param Nvmax maximum size of subsets to examine
#' @return a object of type lm
#' @details Check function...  
#' @author Hans Ole {\O}rka \email{hans.ole.orka@@gmail.org}

myLMfitfunction <- function(Xy,Nvmax=5){
  require(leaps)
  require(car)
  VIF <- 100
  lc <- 100
  reg <- regsubsets(y~.,data=Xy,nvmax=Nvmax,really.big=TRUE)
  summary.out <- summary(reg)
  a <- which.min(summary.out$bic[1:Nvmax])
  while(VIF>5 & a>0){
    b <- summary.out$which[a,]
    Xsub <- subset(Xy,select=c(names(b)[b][-1],"y"))
    lm1 <- lm(y~.,data=Xsub)
    if(length(coef(lm1))>2){VIF <- max(vif(lm1))} else {VIF <- 0}
    a<- a-1
  }
  return(lm1)
}
