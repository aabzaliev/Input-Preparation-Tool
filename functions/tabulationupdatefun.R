#--------------------------------------------------------------------------------------------------------
#Function for updating tabulation
#--------------------------------------------------------------------------------------------------------

tabulationupdatefun <- function(tabulation, recoding) {
  
  for (i in 1:length(tabulation[,1]))
  {
    for (j in 1:length(recoding[,1]))
    {
      if (is.na(tabulation[i,4])==FALSE & is.na(tabulation[i,2])==FALSE) 
      {
        if (tabulation[i,4]==recoding[j,1] & tabulation[i,2]==recoding[j,2]) (tabulation[i,2] <- recoding[j,3])
      }
    }
  }
  return(tabulation)
}
