#-----------------------------------------------
#function for trimming colormat
#-----------------------------------------------

trimcolormatfun <- function(oldheaders,tabulation,colormat) {
  
  varintabulation <- unique(tabulation[,4])
  
  return(colormat[oldheaders %in% varintabulation,])
}