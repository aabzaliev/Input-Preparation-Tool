#--------------------------------------------------------------------------------------------------------
#define function: delete unique columns
#--------------------------------------------------------------------------------------------------------

deletecolumnsuniquefun <- function(data) {
  len=length(data[1,])
  for (i in len:1)
  {
    if (length(unique(data[,i]))==length(data[,i])) 
    {
      data <- data[,-i]
    }
  }
  return(data)
}