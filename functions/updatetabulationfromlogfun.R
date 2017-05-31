#--------------------------------------------------------------------------------------------------------
#Function for updating tabulation from logfile
#--------------------------------------------------------------------------------------------------------

updatetabulationfromlogfun <- function(logfile,tabulation) {
  vartoupdate <- unique(logfile[,1])
  for (i in 1:length(vartoupdate))
  {
    tempvar <- vartoupdate[i]
    updated=FALSE
    
    for (j in 1:length(logfile[,1]))
    {
      if (tempvar==tabulation[j,4] & logfile[j,2]==tabulation[j,1]) {
        tabulation[j,2] <- logfile[,3]
        updated <- TRUE}
    }
    
    if (updated==FALSE) (tabulation <- rbind(tabulation,cbind(logfile[logfile[,1]==tempvar,2],logfile[logfile[,1]==tempvar,4],matrix(0,length(logfile[logfile[,1]==tempvar,2]),1),logfile[logfile[,1]==tempvar,1])))
  }
  return(tabulation)
}