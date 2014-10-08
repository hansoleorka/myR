#' Compute errormatrix
#' 
#' Algorithm to find peak values 
#' 
#' @details Last update:29.10.2008  ?? 
#' @param class a classified vector
#' @param reference reference vector
#' @return em errormatrix ,oa=oa,ua=ua,pa=pa,EM=EM,kappa=kappa)
#' @return oa overall accuracy
#' @return ua user's accuracy
#' @return pa producer's accuracy
#' @return kappa kappa-coefficient
#' @return EM errormatric printable 
#' @author Hans Ole &Oslashrka \email{hans.ole.orka@@gmail.com}
#' @references Congalton, R.G. (1991). A Review of Assessing the Accuracy of Classifications of Remotely Sensed Data. Remote Sensing of Environment, 37, 35-46
#' @references Story, M., & Congalton, R.G. (1986). Accuracy Assessment - a Users Perspective. Photogrammetric Engineering and Remote Sensing, 52, 397-399
#' @examples 
#' #Example: Congalton, R.G. (1991). A Review of Assessing the Accuracy of Classifications of Remotely Sensed Data. Remote Sensing of Environment, 37, 35-46:
#' x<-matrix(c(65,6,0,4,4,81,11,7,22,5,85,3,24,8,19,90),4,4)
#' xsup<-matrix(c(68,12,3,0,7,112,9,2,3,15,89,5,0,10,0,56),4,4)
#' xunsup<-matrix(c(60,15,6,2,11,102,13,4,3,14,90,5,4,8,2,52),4,4)
#' xmod<-matrix(c(75,4,3,1,6,116,7,1,1,11,96,4,0,3,2,61),4,4)
errormatrix <-function(class,reference){
  	class <- as.character(class)
  	reference <- as.character(reference)
  
	lev <- sort(unique(c(class,reference)))
	reference <- factor(reference,levels=lev)
	class <- factor(class,levels=lev)
	#Error matrix (x)
	x<-table(class, reference)
	
	#Defining classification accuracy function 
	#--------------------------------------------------------------------------------
	n <- sum(x)
	ni <- apply(x, 1, sum)
	nj <- apply(x, 2, sum)
	m <- min(length(ni), length(nj))
	p0 <- sum(diag(x[1:m, 1:m]))/n
	pc <- sum(ni[1:m] * nj[1:m])/n^2
	
	oa<-p0 #Overall accuracy
	kappa <- (p0 - pc)/(1 - pc) #Kappa coef.
	pa<-diag(x)/nj #Produser accuracy a.k.a. omission error
	ua<-diag(x)/ni #User accuracy a.k.a. commission errror
	
	#Errormatrix
	EM<-rbind(cbind(x,"Sum"=ni,"Com/UA"=ua*100),"Sum"=c(nj,n,0),"Om/PA"=c(pa*100,0,oa*100))
	list(em=x,oa=oa,ua=ua,pa=pa,EM=EM,kappa=kappa)
}



#
#
##' @export
#print.errormatrix <- function(em){
#	print(em$EM)
#	print(em$kappa)
#}