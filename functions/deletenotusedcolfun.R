#--------------------------------------------------
#function for deleting not useed variables
#--------------------------------------------------

deletenotusedcolfun <- function(data,forsqsd) {
  
  varstock    <- colnames(data)
  vartoremain <- c()
  
  deletecol <- forsqsd[match(varstock,tabulation[,4]),2]
  
  for(i in 1:length(varstock))
  {
    if (deletecol[i]==0) (vartoremain <- cbind(vartoremain,i))
  }
  
  if (is.null(vartoremain)) (return(data)) else (return(data[,-vartoremain]))
}
