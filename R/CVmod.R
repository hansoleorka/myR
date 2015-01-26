#' Crossvalidated prediktions 
#' 
#' Leave-one-out crossvalidated prediktions only considering obsercations not variables.
#' 
#' @param mod a fitted model that work with funtion update and predict
#' @return crossvalidated prediktions 
#' @details Sjekk funksjonen.. 
#' @author Hans Ole Orka \email{hans.ole.orka@@gmail.org}
CVmod <- function(mod){
  Xy <- mod$data
  pred <- c()
  for(j in 1:nrow(Xy)){
    g1 <- update(mod,data=Xy[-j,])
    pred[j] <- predict(g1,newdata=Xy[j,],type="response")
  }
  return(pred)
}