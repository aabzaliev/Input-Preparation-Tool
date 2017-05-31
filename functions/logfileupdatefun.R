#--------------------------------------------------------------------------------------------------------
#Function for updating uploaded logfile 
#--------------------------------------------------------------------------------------------------------
logfileupdatefun <- function(logfile, recoding) {
  vartorecode <- unique(recoding[,1])
  for (i in 1:length(vartorecode))
  {
    j=0
    repeat
    {
      j=j+1
      if (j>length(logfile[,1])) break
      if (logfile[j,1]==vartorecode[i]){
        k=j-1
        l=0
        repeat
        {
          k=k+1
          l=l+1
          
          if(k>length(logfile[,1])) break
          if(logfile[k,1]!=vartorecode[i]) break
          
          logfile[k,3] <- recoding[l,2]
          logfile[k,4] <- recoding[l,3]
        }
      }
    }
  }
}