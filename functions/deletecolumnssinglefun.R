#--------------------------------------------------------------------------------------------------------
#define function: delete single-entry columns
#--------------------------------------------------------------------------------------------------------

deletecolumnssinglefun <- function(data) {
  len=length(data[1,])
  for (i in len:1)
  {
    if (length(unique(data[,i]))==1) 
    {
      data <- data[,-i]
    }
  }
  return(data)
}
