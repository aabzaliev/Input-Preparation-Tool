#--------------------------------------------------------------------------------------------------------
#Function for saving current recode imput
#--------------------------------------------------------------------------------------------------------

#1. Check if recoding object exist
#2. Check if current variable was previously recoded
#   --> No: rbind currecoding for this variable
#   --> Yes:Update existing recoding
#3. If this variable is the first to recoded, define current recoding as recodematrix 

savecurrecodingfun <- function(recoding,currecoding) {
  if (is.null(recoding)==FALSE) {
    i=0
    repeat
    {
      i=i+1
      if (i > length(recoding[,1])) {
        recodingmat <- rbind(recoding,currecoding)
        break
      }
      if (recoding[i,1]==currecoding[1,1]) {
        j=i-1
        repeat
        {
          j=j+1
          if (j > length(currecoding[,1])) break
          if (recoding[j,1]!=currecoding[1,1]) break
        } 
        for (k in i:(j-1))
        {
          for (l in 1:length(currecoding[,1]))
            if (recoding[k,3]==currecoding[l,2]) (recoding[k,3]<-currecoding[l,3])
        }
        recodingmat <- recoding
        break
      }
    }
    
  } else {
    recodingmat <- currecoding
  }
  
  return(recodingmat)
}