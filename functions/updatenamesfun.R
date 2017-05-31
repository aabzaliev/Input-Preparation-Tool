#--------------------------------------------------------------------------------------------------------
#Function for updating colnames
#--------------------------------------------------------------------------------------------------------

updatenamesfun <- function(rawfile,updatedrawfile,names) {
  
  coltodelete <- NULL
  
  for (i in 1:length(colnames(rawfile)))
  {
    if ((colnames(rawfile)[i] %in% colnames(updatedrawfile))==FALSE)
    {
      coltodelete <- cbind(coltodelete,i)
    }
  }
  if(is.null(coltodelete)) (return(names)) else (return(names[-coltodelete]))
}