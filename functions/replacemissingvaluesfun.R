#--------------------------------------------------------------------------------------------------------
#define function: replace missing values other than NA with NA
#--------------------------------------------------------------------------------------------------------

replacemissingvaluesfun <- function(data,oldmissingvalue,newmissingvalue) {
  if (length(oldmissingvalue)==length(newmissingvalue)) {
    for (j in 1:length(data[1,]))
    {
      for (k in 1:length(data[,1]))
      {
        for (i in 1:length(newmissingvalue))
        {
          if (is.na(data[k,j])==FALSE & data[k,j]==oldmissingvalue[i]) (data[k,j]=newmissingvalue[i])
        }
      }
    }
    return(data)
  } else {
    return(NULL)
  }
}