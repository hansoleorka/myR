# TODO: Add comment
# TITLE: Variable selection using anova and correlation treshold
# used by e.g. Holmgren et al. 2004
#
# Date: Nov 19, 2010 
# Author: Hans Ole Ørka
###############################################################################

fsel.ftest <- function(g,X){
	#ANOVA F-test
	nf <- dim(X)[2]
	f.values <- matrix(NaN,nf,1)
	for(i in 1:nf){
		aov1 <- aov(X[,i]~g)
		f.values[i,] <- anova(aov1)[1,4]}
	res <- data.frame(Var.name = names(X),f.value = f.values)[rev(order(c(f.values))),]
	res$Var.name <- as.character(res$Var.name)
	df1 <- length(table(g))- 1
	df2 <- length(g)-length(table(g))
	res <- subset(res,f.value>qf(0.95,df1,df2))
	return (res)}

cor.rm <- function(res,X,r.level = 0.50){
	vars <- res[1,1]
	for (i in 1:dim(res)[1]){
		cor1 <- cor(X[,names(X)==res[i,1]],X[,vars])
		if(max(abs(cor1)) < r.level){vars <- c(vars,res[i,1])}
	}
	return(vars)}

varSelAOV <- function(g,X,r.level = 0.50){
	res <- fsel.ftest(g,X)
	vars <- cor.rm(res,X,r.level)
	varSel <- list(f.values = res, selected.vars = vars,r.level = r.level)
	return(varSel)
}

