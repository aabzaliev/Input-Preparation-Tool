#--------------------------------------------------------------------------------------------------------
#Function for check if cells will be merged by recoding
#--------------------------------------------------------------------------------------------------------
#If merging will appear, the function returns a vector with the row numbers
#If merging will not appear, the function returns FALSE

checkmergefun <- function(recodemat) {
  len <- length(recodemat[,1])
  merging <- FALSE
  
  for (i in 1:(len-1))
  {
    for (j in (i+1):len)
    {
      if (recodemat[i,3]==recodemat[j,2]) (merging <- c(i,j))
    }
  }  
  return(merging)
}