#--------------------------------------------------------------------------------------------------------
#Function for levels, values and frequencies in data
#--------------------------------------------------------------------------------------------------------

overviewfun <- function(datawithoutlevels, datawithlevels) {
  for (k in 1:length(colnames(datawithoutlevels)))
  {
    tempcoltable    <- table(Label = datawithlevels[,colnames(datawithlevels)[k]], Value = datawithoutlevels[,colnames(datawithoutlevels)[k]],useNA = "ifany")
    tempcolmat      <- matrix(NA,length(rownames(tempcoltable)),4)
    tempcolmat[,4]  <- colnames(datawithoutlevels)[k]
    
    #reorder levels
    for (j in 1:length(rownames(tempcoltable)))
    {
      for (l in 1:length(colnames(tempcoltable)))
      {
        if (tempcoltable[j,l]>0) 
        {
          tempcolmat[j,1] <- rownames(tempcoltable)[j]
          tempcolmat[j,2] <- colnames(tempcoltable)[l]
          tempcolmat[j,3] <- tempcoltable[j,l]
        }
      }
    }
    
    if(k==1)
    {
      tabulation <- tempcolmat
    }else{
      tabulation <- rbind(tabulation,tempcolmat)
    }
  }
  colnames(tabulation) <- c("level","value","freq","var")
  return(tabulation)
}