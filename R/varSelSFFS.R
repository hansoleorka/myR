#
# TITLE: Sequential forward floating feature selection with Jeffries-Matusita Distance
#
# Reference: Pudil, P.; Novovicová, J. & Kittler, J. Floating search methods in feature selection Pattern recognition letters, Elsevier, 1994, 15, 1119-1125
#
# Date: Nov 1, 2011 
# Author: Michele Dalponte & Hans Ole Ørka
###############################################################################
varSelSSFS <- function(g,X,strategy="mean"){
	nfeat <- ncol(X)
	nclass <- length(unique(g))	
	#mu <- by(X,g,mean)
	mu <- by(X,g,function(x){apply(x,2,mean)}) #Updated Nov 2013 - Hans Ole
	Cov <- by(X,g,cov)

	#Jeffries-Matusita (JM) distance
	JM1<-function(fe){
		f<-fe
		ncomb <- t(combn(1:nclass,2))
		Bhat <- c()
		jm <- c()
		for(j in 1:nrow(ncomb)){
			#print(paste(f,j,sep="-"))
			mu.i <- mu[[ncomb[j,1]]]
			cov.i <- Cov[[ncomb[j,1]]]
			mu.j <- mu[[ncomb[j,2]]]
			cov.j <- Cov[[ncomb[j,2]]]
			Bhat[j]<-(1/8)*t(mu.i[f]-mu.j[f]) %*% (solve((cov.i[f,f]+cov.j[f,f])/2)) %*% (mu.i[f]-mu.j[f]) + 0.5*log((((cov.i[f,f]+cov.j[f,f])/2))/(sqrt(((cov.i[f,f]))*((cov.j[f,f])))),base=exp(1))
			jm[j] <- sqrt(2*(1-exp(-Bhat[j])))
			}
		jm.dist<-c(min(jm),mean(jm))
		return(jm.dist)}

	JM<-function(fe){
		f<-fe
		ncomb <- t(combn(1:nclass,2))
		Bhat <- c()
		jm <- c()
		for(j in 1:nrow(ncomb)){
			#print(paste(f,j,sep="-"))
			mu.i <- mu[[ncomb[j,1]]]
			cov.i <- Cov[[ncomb[j,1]]]
			mu.j <- mu[[ncomb[j,2]]]
			cov.j <- Cov[[ncomb[j,2]]]
			Bhat[j]<-(1/8)*t(mu.i[f]-mu.j[f]) %*% (solve((cov.i[f,f]+cov.j[f,f])/2)) %*% (mu.i[f]-mu.j[f]) + 0.5*log(det(((cov.i[f,f]+cov.j[f,f])/2))/(sqrt((det(cov.i[f,f]))*(det(cov.j[f,f])))),base=exp(1))
			jm[j] <- sqrt(2*(1-exp(-Bhat[j])))
		}
		jm.dist<-c(min(jm),mean(jm))
		return(jm.dist)}
	
	
	#Backword function
	backword<-function(fs){
		
		distance<-1:length(fs)
		
		for (j in 1:length(fs)){
			distance[j]<-JM(fs[-j])[1]
		}
		
		if (which.max(distance)==length(fs)){
			return<--1
		}
		else{
			return<-which.max(distance)
		}
	}

	
	features<-matrix(nfeat-1,nfeat-1,data=NA)
	distances<-matrix(1,nfeat-1,data=NA)

	
	###########################################################################################
	#STRART
	###########################################################################################
	#computing JM for single features
	JMsingle<-matrix(nfeat,2,data=0)
	for(f in 1:nfeat){
		JMsingle[f,] <- JM1(f)
	}
	JMsingle
	#strategy <- "mean"
	if(strategy == "mean"){ref <- 1}
	if(strategy == "minimum"){ref <- 2}
	
	features[1,1]<-which.max(JMsingle[,ref])
	distances[1] <- JMsingle[which.max(JMsingle[,ref]),1]
	#Two features
	i = 2
	C<-combn(nfeat,2)
	dist<-apply(C,2,FUN=JM)
	
#	print("Feature to select: ",i)
#	print(paste("Maximum minimum distance: ",max(dist[1,])))
#	print("Features selected: ")
#	print(C[,which.max(dist[ref,])])
	
	FS<-C[,which.max(dist[ref,])]
	
	features[2,c(1,2)]<-C[,which.max(dist[ref,])]
	distances[2]<-dist[ref,which.max(dist[ref,])]
	
	#More than two features
	i<-2
	
	while (i<nfeat-1){
		#print(i)
		C<-matrix(i+1,(nfeat-i),data=NA)
		C[c(1:i),c(1:(nfeat-i))]<-replicate((nfeat-i),FS)
		ff<-1:nfeat
		ff<-ff[-FS]
		C[i+1,]<-ff
		
		tryCatch(dist<-apply(C,2,FUN=JM),error=dist <- NA)
		if(is.na(dist)[1]){
			return(list(features=features,distances=distances,var.names = names(X),strategy=strategy, distance = "Jeffries-Matusita distance"))
			stop("KABOOMMMMM")}
		
#		print(paste("Feature to select: ",i+1))
#		print(paste("Maximum minimum distance: ",max(dist[ref,])))
#		print("Features selected: ")
#		print(C[,which.max(dist[ref,])])
#		
		FS<-C[,which.max(dist[ref,])]
		
		bw<-backword(FS)
		
		if (bw!=-1)
		{
			FS<-FS[-bw]
			features[i,c(1:(i))]<-FS
			distances[i+1]<-JM(FS)[ref]
		}
		else
		{
			features[i+1,c(1:(i+1))]<-C[,which.max(dist[ref,])]
			distances[i+1]<-dist[ref,which.max(dist[ref,])]
			i<-i+1
		}
	}

	return(list(features=features,distances=distances,var.names = names(X) ,strategy=strategy, distance = "Jeffries-Matusita distance"))

}



varSelSFFS.vars <- function(sel,p=0.01){
	increase <- (sel$distances[-1] - sel$distances[-length(sel$distances)]) / sel$distances[-1]   
	selected.vars <- sel$var.names[na.omit(sel$features[length(na.omit(increase[increase > p])),])]
	return(selected.vars)
}


