#--------------------------------------------------------------------------------------------------------
#Function for updating inittabulation from logfile
#--------------------------------------------------------------------------------------------------------

updateinittabulationfromlogfun <- function(logfile,inittabulation) {
  vartoupdate <- unique(logfile[,1])
  for (i in 1:length(vartoupdate))
  {
    tempvar <- vartoupdate[i]
    updated=FALSE
    
    for (j in 1:length(logfile[,1]))
    {
      if (tempvar==inittabulation[j,4] & logfile[j,2]==inittabulation[j,1]) {
        inittabulation[j,2] <- logfile[,3]
        updated <- TRUE}
    }
    
    if (updated==FALSE) (inittabulation <- rbind(tabulation,cbind(logfile[logfile[,1]==tempvar,2],logfile[logfile[,1]==tempvar,3],matrix(0,length(logfile[logfile[,1]==tempvar,2]),1),logfile[logfile[,1]==tempvar,1])))
  }
  return(inittabulation)
}