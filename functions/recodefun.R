#--------------------------------------------------------------------------------------------------------
#Function for recoding
#--------------------------------------------------------------------------------------------------------

#recodetable: Tabelle mit 11 Spalten und n Zeilen
#Spalte 1 gibt den Variablennamen, Spalte 2 den zu 
#ersetzenden Wert und Zeile 3 den entsprechenden neuen Wert
#Die restlichen Spalten geben MV an 

library(dplyr)

recodefun <- function(data, recodemat, defaultmissingvalue = NULL) {
  
  vartorecode <- unique(recodemat[,1])
  
  for (j in 1:length(vartorecode))
  {
    currecoding      <- recodemat[recodemat[,1]==vartorecode[j],2:3]
    curmissingvalues <- recodemat[recodemat[,1]==vartorecode[j],4:11]
    
    if(is.null(dim(currecoding))) 
    {
      currecoding <- matrix(currecoding,1,2)
      curmissingvalues <- matrix(curmissingvalues,1,8)
    }
    
    #do recoding only if changes saved in recodemat
    if(!(identical(currecoding[,1],currecoding[,2]) &
         identical(curmissingvalues[,1],curmissingvalues[,2]) &
         identical(curmissingvalues[,3],curmissingvalues[,4]) & 
         identical(curmissingvalues[,5],curmissingvalues[,6]) & 
         identical(curmissingvalues[,7],curmissingvalues[,8])
         )
       )
    {
      temprecoding <- list()
      
      #append recodings
      for (i in 1:length(currecoding[,1])) 
      {
        if (!is.na(currecoding[i,1]))
        {
          temprecoding[[currecoding[i,1]]] <- as.character(currecoding[i,2])
        } else {
          if(!is.na(currecoding[i,2]))
          {
            defaultmissingvalue <- currecoding[i,2] 
          }
        }
      }
      
      #append missing values
      for (k in 1:length(curmissingvalues[,1])) {
        for (l in c(1,3,5,7))
        {
          if (!is.na(curmissingvalues[k,l]))
          {
            temprecoding[[curmissingvalues[k,l]]] <- curmissingvalues[k,l+1]
          } else {
            if(!is.na(curmissingvalues[k,l+1]))
            {
              defaultmissingvalue <- curmissingvalues[k,l+1]
            }
          }
        }
      }
      
      #append default missing value
      if (!is.null(defaultmissingvalue))
      {
        temprecoding[[".missing"]] <- defaultmissingvalue
      }
      
      #append data
      tempdata <- data[[as.character(vartorecode[j])]]
      temprecoding[['.x']] <- as.name('tempdata')
      
      
      #recode 
      data[[as.character(vartorecode[j])]] <- do.call(dplyr::recode, args = temprecoding)
    } 
  }
  
  return(data)
}


