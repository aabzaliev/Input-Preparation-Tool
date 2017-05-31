#--------------------------------------------------------------------------------------------------------
#Function for levels, values and frequencies in data
#--------------------------------------------------------------------------------------------------------

overviewfun <- function(datawithoutlevels, datawithlevels) {
  for (k in 1:length(colnames(datawithoutlevels)))
  {
    tempcoltable    <- table(Label = datawithlevels[,colnames(datawithlevels)[k]], Value = datawithoutlevels[,colnames(datawithoutlevels)[k]],useNA = "ifany")
    tempcolmat      <- matrix(NA,length(rownames(tempcoltable)),4)
    tempcolmat[,4]  <- colnames(datawithoutlevels)[k]
    tempcolmat[,1]  <- rownames(tempcoltable)
    for (i in 1:(length(colnames(tempcoltable))))
    {
      tempcolmat[i,2]=colnames(tempcoltable)[i]
      tempcolmat[i,3]=tempcoltable[i,i]
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