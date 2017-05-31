################################################################################
################################################################################
# create_dummies                                                               #
# function for creating dummy variables for categorial variables               #
#                                                                              #
# date: 2014-05-07                                                             #
# author: Monia Mahling                                                        #
#                                                                              #
# params: - dataset: a data.frame object containing the variable for which     #
#           dummies are to be created                                          #
#         - varname: a character string giving the name of this variable;      #
#           note that this variable absolutely has to be a factor - otherwise, #
#           the procedure will not work!                                       #
#         - exclude: logical value indicating if the original categorial       #
#           variable should be excluded from the resulting data.frame object   #
#           (defaults to FALSE)                                                #
#         - processNA: logical value indicating if NAs (such as empty          #
#           character strings or blanks) should be processed                   #
#           (defaults to TRUE)                                                 #
#           TRUE: corresponding values in the dummies are set to NA            #
#                 and the result does not contain a dummy for NA               #
#           FALSE: the result contains a dummy for NA                          #
#                  which is set to 1, accordingly                              #
#         - concat: logical value indicating if the original name of the       #
#           categorial variable should be part of the new names                #
#           (defaults to FALSE)                                                #
#                                                                              #
# value: a data.frame object containing (additional) dummies                   #
################################################################################
################################################################################


create_dummies <- function(dataset, varname, exclude=FALSE, processNA=TRUE, concat=FALSE){
  
  catvar <- dataset[, which(names(dataset)==varname)]
  
  if(!is.factor(catvar)){catvar <- factor(catvar)}
  
  lev <- levels(catvar)
  
  NAflag <- FALSE
  
  dummyma <- matrix(NA, nrow=dim(dataset)[1], ncol=length(lev))
  
  for (i in 1:length(lev)){
    
    dummyma[,i] <- ifelse(catvar==lev[i], 1, 0)
    
  }
  
  dummydf <- data.frame(dummyma, row.names=rownames(dataset))
  
  trimlev <- sub("[[:blank:]]+$", "", lev)
  
  if (any(trimlev=="")){
    
    NAlev <- paste("NA_", varname, sep="")
    trimlev[trimlev==""] <- NAlev
    NAflag <- TRUE
    
  }
  
  if (concat) {
    names(dummydf) <- paste(tolower(varname),                      # FIXME: here it crashes when newly created dummy has no level at all; thus catch it where appropriate
                            gsub(pattern=" ",          # replace spaces
                                 replacement="_",
                                 gsub(pattern=",",     # replace commas
                                      replacement="",
                                      tolower(trimlev))),
                            sep="___")
  } else {
    
    names(dummydf) <- gsub(pattern=" ", replacement="_", gsub(pattern=",", replacement="", tolower(trimlev)))
    
  }
  
  if (processNA && NAflag) {
    
    if (concat) {
      
      NAlev <- paste(varname, gsub(pattern=" ", replacement="_", gsub(pattern=",", replacement="", NAlev)), sep=".")
      
    } else {
      
      NAlev <- gsub(pattern=" ", replacement="_", gsub(pattern=",", replacement="", NAlev))
      
    }
    
    NAvar <- dummydf[, which(names(dummydf)==NAlev)]
    
    allnames <- names(dummydf)
    namesselect <- allnames[allnames!=NAlev]
    dummydf <- subset(dummydf, select=namesselect)
    
    dummydf[NAvar==1, ] <- NA 
    
  }
  
  dfres <- cbind(dataset, dummydf)
  
  if(exclude){
    
    allnames <- names(dfres)
    namesselect <- allnames[allnames!=varname]
    dfres <- subset(dfres, select=namesselect)
    
  }
  
  return(dfres)
  
}


################################################################################
################################################################################
# create_all_dummies                                                           #
# function for creating dummy variables for all categorial variables in a      #
# given data set                                                               #
# note that these categorial variables absolutely have to be factors -         #
# otherwise, the procedure will not work!                                      #
#                                                                              #
# date: 2014-05-07                                                             #
# author: Monia Mahling                                                        #
#                                                                              #
# params: - dataset: a data.frame object containing the variables for which    #
#           dummies are to be created                                          #
#         - exclude: logical value indicating if the original categorial       #
#           variable should be excluded from the resulting data.frame object   #
#           (defaults to FALSE)                                                #
#         - processNA: logical value indicating if NAs (such as empty          #
#           character strings or blanks) should be processed                   #
#           (defaults to TRUE)                                                 #
#           TRUE: corresponding values in the dummies are set to NA            #
#                 and the result does not contain a dummy for NA               #
#           FALSE: the result contains a dummy for NA                          #
#                  which is set to 1, accordingly                              #
#         - concat: logical value indicating if the original name of the       #
#           categorial variable should be part of the new names                #
#           (defaults to FALSE)                                                #
#                                                                              #
# value: a data.frame object containing all (additional) dummies               #
################################################################################
################################################################################


create_all_dummies <- function(dataset, exclude=FALSE, processNA=TRUE, concat=FALSE){
  
  classvec <- rep(NA, times=dim(dataset)[2])
  
  for (i in 1:dim(dataset)[2]){
    
    classvec[i] <- class(dataset[, i])
    
  } 
  
  varlist <- names(dataset)[classvec=="factor"]
  dset <- dataset
  
  for (k in 1:length(varlist)){
    
    catvar <- varlist[k]
    dset <- create_dummies(dataset=dset, varname=catvar, exclude=exclude, processNA=processNA, concat=concat)
    
  }
  
  return(dset)
  
}




################################################################################
################################################################################
# create_sel_dummies                                                           #
# function for creating dummy variables for all categorial variables in a      #
# given selection of variables from a data set                                 #
#                                                                              #
# date: 2014-05-08                                                             #
# author: Monia Mahling                                                        #
#                                                                              #
# params: - dataset: a data.frame object containing the variables for which    #
#           dummies are to be created                                          #
#         - varlist: vector containing the names of the variables for which    #
#           dummies are to be created; note that these variables absolutely    #
#           have to be factors - otherwise, the procedure will not work!       #
#         - exclude: logical value indicating if the original categorial       #
#           variable should be excluded from the resulting data.frame object   #
#           (defaults to FALSE)                                                #
#         - processNA: logical value indicating if NAs (such as empty          #
#           character strings or blanks) should be processed                   #
#           (defaults to TRUE)                                                 #
#           TRUE: corresponding values in the dummies are set to NA            #
#                 and the result does not contain a dummy for NA               #
#           FALSE: the result contains a dummy for NA                          #
#                  which is set to 1, accordingly                              #
#         - concat: logical value indicating if the original name of the       #
#           categorial variable should be part of the new names                #
#           (defaults to FALSE)                                                #
#                                                                              #
# value: a data.frame object containing all (additional) dummies               #
################################################################################
################################################################################


create_sel_dummies <- function(dataset, varlist, exclude=FALSE, processNA=TRUE, concat=FALSE){
  
  dset <- dataset
  
  for (k in 1:length(varlist)){
    
    catvar <- varlist[k]
    dset <- create_dummies(dataset=dset, varname=catvar, exclude=exclude, processNA=processNA, concat=concat)
    
  }
  
  return(dset)
  
}
