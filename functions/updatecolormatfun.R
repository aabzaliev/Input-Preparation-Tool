#--------------------------------------------------------------------------------------------------------
#Function for updating colormat
#--------------------------------------------------------------------------------------------------------
#var=-999 --> Change color in all variables

updatecolormatfun <- function(colormat,variable,actionbutton, undo = FALSE) {
  if (undo==FALSE) (newcolor <- "color: #2E2E2E; background-color: #E55A00; border-color: #D8D8D8")
  if (undo==TRUE)  (newcolor <- "color: #2E2E2E; background-color: #FFFFFF; border-color: #D8D8D8")
  col <- match(actionbutton,colnames(colormat))
  
  if(variable==-999)
  {
    colormat[,col] <- newcolor
  } else {
    var <- colnames(workingrawfile.withoutlevels)[variable]
    tempcolormat <- colormat[,col]
    tempcolormat[colormat[,1]==var] <- newcolor
    colormat[,col] <- tempcolormat
  }
  return(colormat)
}