# TODO: Add comment
# 
# Author: hanso
###############################################################################


accuracy.ass <- function(y,yhat){
	diff <- y - yhat
	r2 <- cor(y,yhat)^2
	RMSE <- rmse(y,yhat)
	RRMSE <- (rmse(y,yhat)/mean(y))*100
	MD <- mean(diff)
	SDD <- sd(diff)
	RSDD <- (sd(diff)/mean(y))*100
	tt <- t.test(diff)
	tt$p.value
	tt$conf.int
	D <- c(r2=r2,RMSE=RMSE, RRMSE=RRMSE, MD=MD,SDD=SDD,RSDD=RSDD,p.value=tt$p.value,LW.CI=tt$conf.int[1],UP.CI=tt$conf.int[2])
	D <- round(D,2)
	return(D)
}


