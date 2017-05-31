library(shiny)
library(foreign)
library(stringr)

#set max size of data
options(shiny.maxRequestSize=100*1024^2)
#load functions
file.sources = list.files("C:/Users/arsteink/Desktop/Shiny/01 Data Preparation Tool/functions/", 
                          pattern="*.R$", full.names=TRUE, 
                          ignore.case=TRUE)

sapply(file.sources,sys.source,.GlobalEnv)

#--------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------
# CORE SERVER # CORE SERVER # CORE SERVER # CORE SERVER # CORE SERVER # CORE SERVER # CORE SERVER # CORE 
#--------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------

#init trigger
trigger.coldeleted <<- matrix(FALSE,2,1)
rownames(trigger.coldeleted) <- c("unique","single")


function(input, output, session) {
  
  #delete objects from last session
  #onSessionEnded(function(){
    #manual list of objects to be deleted. When create while app is running, shony objects will be deleted, too.
    objectstodelete <- c( "colormat",
                          "dummy",                                             
                          "forsqsd",
                          "IDvariable.name",
                          "inittabulation",                
                          "mvlogmat",                                                        
                          "prioritymat",                                          
                          "rawfile.withlevels",
                          "rawfile.withoutlevels",
                          "tabulation",            
                          "trigger.coldeleted",                              
                          "workingrawfile.withlevels",     
                          "workingrawfile.withoutlevels")
    rm(list = objectstodelete); gc()
    rm("objectstodelete")
  #})
  
  #------------------------------------------------------------
  #load rawfile
  #------------------------------------------------------------
  
  #create manipualte UI
  output$manipulatevariables <- renderUI({
    titlePanel(h3("Please upload SPSS data first!", align = "center"))
  })
    
  #create logfile UI
  output$logoutput <- renderUI({
    titlePanel(h3("Please upload SPSS data first!", align = "center"))
  })  
  
  output$rawfile <- DT::renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$SAV
    
    if (is.null(inFile))
      return(NULL)
    
    # read data with values
    rawfile.withoutlevels <<- read.spss(file = inFile$datapath, to.data.frame = TRUE, use.value.labels = FALSE, reencode = "UTF-8")
    rawfile.withlevels    <<- read.spss(file = inFile$datapath, to.data.frame = TRUE, use.value.labels = TRUE, reencode = "UTF-8")    

    workingrawfile.withoutlevels  <<- rawfile.withoutlevels
    workingrawfile.withlevels     <<- rawfile.withlevels
    
    #Gives levels, values and freq of variables contained in original file
    tempinittabulation  <- overviewfun(workingrawfile.withoutlevels,workingrawfile.withlevels)
    colnames(tempinittabulation) <- c("level", "oldvalue", "freq", "variable")
    inittabulation  <<- tempinittabulation
    tabulation      <<- inittabulation
    
    #Missing-value matrix for logfile
    tempmvlogmat  <- tabulation[,4]
    tempmvlogmat  <- cbind(tempmvlogmat,matrix(NA,length(tabulation[,1]),8))
    colnames(tempmvlogmat) <- c("variable", "MV1_old", "MV1_new","MV2_old", "MV2_new","MV3_old", "MV3_new","MV4_old", "MV4_new")
    mvlogmat <<- tempmvlogmat
    
    #matrix with styles for action buttons
    if(!exists("colormat"))
    {
      colormat <<- matrix("color: #2E2E2E; background-color: #FFFFFF; border-color: #D8D8D8",length(colnames(workingrawfile.withoutlevels)),14)
      colormat[,1] <- colnames(workingrawfile.withoutlevels)
      colnames(colormat) <- c("variable","MV1_recode","MV2_recode","MV3_recode","MV4_recode","deletecolsingle","deletecolunique","mvinallcolumns","useforSQSD","usenotforSQSD","dorecode","applyall","yesdummy","nodummy")  
    }
    
    #variable header and names
    varheader <- names(attributes(workingrawfile.withoutlevels)$variable.labels)
    varnames  <- (attributes(workingrawfile.withoutlevels)$variable.labels)
          
    #identify free missing values
    defaultmv <- c(-66,-77,-88,-99)
    for (j in 1:length(workingrawfile.withlevels[1,]))
    {
      for (i in 1:4)
      {
        if (defaultmv[i] %in% workingrawfile.withoutlevels[,j])
        {
          repeat
          {
            defaultmv[i] <- as.numeric(paste0(as.character(defaultmv[i]),str_sub(as.character(defaultmv[i]), start= -1)))
            if (!(defaultmv[i] %in%  workingrawfile.withoutlevels[,j])) break
          }
        }
      }
    }
    
    #set default for creating dummies
    #Default: Do not create dummys
    #Create matrix only, if it not exists. Otherwise it will be overwritten before saveing
    if (!exists("dummy"))
    {
      dummy <<- matrix(0,length(inittabulation[,1]),2)
    }
    
    #set default use for sqsd
    #Default: Use all variables for SQSD
    #Create matrix only, if it not exists. Otherwise it will be overwritten before saveing
    if (!exists("forsqsd"))
    {
      forsqsd <<- matrix(1,length(inittabulation[,1]),2)
    }
    
    #set default priority
    #Create matrix only, if it not exists. Otherwise it will be overwritten before saveing
    if (!exists("prioritymat"))
    {
      prioritymat <<- matrix(1,length(inittabulation[,1]),2)
    }
    
    #create choice list for select response ID
    varlist <- list()
    count <- -1
    for (i in c("no ID variable",colnames(workingrawfile.withoutlevels)))
    {
      count <- count+1
      varlist[[i]] <- count
    }
    
    #select response ID in modal dialog
    showModal(modalDialog(
      title = 'ID variable',
      selectInput("selectID", label = h3("Please select ID variable"), 
                  choices = varlist, 
                  selected = 0),
      actionButton("closeidselection","continue"),
      easyClose = FALSE,
      footer = NULL))
    
    #save ID variable and close modal dialog
    observeEvent(input$closeidselection, {
        IDvariable.name    <<- names(varlist[as.numeric(input$selectID)+1])
        if(IDvariable.name!="no ID variable") (IDvariable <<- workingrawfile.withoutlevels[[IDvariable.name]])
      removeModal()
    })
   
    
    #-----------------------------------------------------------------------------------------
    #active manipulation
    #-----------------------------------------------------------------------------------------
    
    #delete single-entry columns
    observeEvent(input$deletecolsingle, {
      
      showModal(modalDialog(
        title = 'Delete columns with uniform entries"',
        "deleting...",
        easyClose = TRUE,
        footer = NULL))
      
      #update names and header
      tempold.workingrawfile.withoutlevels <- workingrawfile.withoutlevels
      
      workingrawfile.withoutlevels  <<- deletecolumnssinglefun(workingrawfile.withoutlevels)
      workingrawfile.withlevels     <<- deletecolumnssinglefun(workingrawfile.withlevels)
      tabulation <<- overviewfun(workingrawfile.withoutlevels,workingrawfile.withlevels)
      mvlogmat <<- mvlogmat[(mvlogmat[,1] %in% tabulation[,4]),]

      varheader <<- updatenamesfun(tempold.workingrawfile.withoutlevels,workingrawfile.withoutlevels,varheader)
      varnames  <<- updatenamesfun(tempold.workingrawfile.withoutlevels,workingrawfile.withoutlevels,varnames)
      
      colormat <<- trimcolormatfun(colnames(tempold.workingrawfile.withoutlevels),tabulation,colormat)
      trigger.coldeleted[2,1] <- TRUE
      
      if (trigger.coldeleted[1,1]) {colormat <<- updatecolormatfun(colormat,-999,"deletecolunique")}
      colormat <<- updatecolormatfun(colormat,-999,"deletecolsingle")
      
      #save for logfile
      tempforsqsd <- forsqsd
      tempforsqsd[!(inittabulation[,4] %in% colnames(workingrawfile.withoutlevels)),2] <- 0
      forsqsd <<- tempforsqsd
      forsqsd[,1] <- inittabulation[,4]
      
      showModal(modalDialog(
        title = 'Recode!',
        "FINISHED - deleted all columns with uniform entries!",
        easyClose = TRUE,
        footer = NULL))
    })
    
    #delete unique-entry columns
    observeEvent(input$deletecolunique, {
      
      showModal(modalDialog(
        title = 'Delete columns with unique entries"',
        "deleting...",
        easyClose = TRUE,
        footer = NULL))
      
      #update names and header
      tempold.workingrawfile.withoutlevels <- workingrawfile.withoutlevels
      
      workingrawfile.withoutlevels  <<- deletecolumnsuniquefun(workingrawfile.withoutlevels)
      workingrawfile.withlevels     <<- deletecolumnsuniquefun(workingrawfile.withlevels)
      tabulation <<- overviewfun(workingrawfile.withoutlevels,workingrawfile.withlevels)
      mvlogmat <<- mvlogmat[(mvlogmat[,1] %in% tabulation[,4]),]
      
      varheader <<- updatenamesfun(tempold.workingrawfile.withoutlevels,workingrawfile.withoutlevels,varheader)
      varnames  <<- updatenamesfun(tempold.workingrawfile.withoutlevels,workingrawfile.withoutlevels,varnames)
      
      colormat <<- trimcolormatfun(colnames(tempold.workingrawfile.withoutlevels),tabulation,colormat)
      trigger.coldeleted[1,1] <- TRUE
      
      if (trigger.coldeleted[2,1]) {colormat <<- updatecolormatfun(colormat,-999,"deletecolsingle")}
      colormat <<- updatecolormatfun(colormat,-999,"deletecolunique")
      
      #save for logfile
      tempforsqsd <- forsqsd
      tempforsqsd[!(inittabulation[,4] %in% colnames(workingrawfile.withoutlevels)),2] <- 0
      forsqsd <<- tempforsqsd
      forsqsd[,1] <- inittabulation[,4]
      
      showModal(modalDialog(
        title = 'Recode!',
        "FINISHED - deleted all columns with unique entries!",
        easyClose = TRUE,
        footer = NULL))
    })
    
    #set current variable number
    output$curcolnum <- renderUI({
      return(numericInput("curcolnuminput", label = "Go to variable", min = 1, max = length(colnames(workingrawfile.withoutlevels)), value = 1))
    })
    
    #header of current variable
    output$curvariableheader <- renderText(varheader[input$curcolnuminput])
    output$curvariablename   <- renderText(varnames[input$curcolnuminput])
        
    #Define new missing values
    output$MV_new <- renderUI({
      curcol <- input$curcolnuminput
      MV_new <- lapply(1:4, function(i) {
        inputname <- paste0("MV",i,"_new")
        tempnewvalue=defaultmv[i]
        textInput(inputname, label = "new", value = tempnewvalue)
      })
      MV_new
    })
    
    #Write new missing values in specific column
    observeEvent(input$MV1_recode, {
      
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"MV1_recode")
      
      showModal(modalDialog(
        title = '',
        "recoding...",
        easyClose = TRUE,
        footer = NULL))
      
      curcol    <- input$curcolnuminput
      tempoldmv <- input$MV1_old
      tempnewmv <- input$MV1_new
      currecodemat <- cbind(tabulation[tabulation[,4]==colnames(workingrawfile.withoutlevels)[curcol],c(4,2,2)], NA,NA,NA,NA,NA,NA,NA,NA)
      currecodemat[1,4] <- tempoldmv 
      currecodemat[1,5] <- tempnewmv
      
      #check if mv is one of the old values. If yes, replace it
      for (i in 1:length(currecodemat[,1]))
      {
        if (!is.na(currecodemat[i,2]) & !is.na(currecodemat[1,4]))
        {
          if (currecodemat[i,2]==currecodemat[1,4]) (currecodemat[i,3] <- currecodemat[1,5])
        }
      }
      
      #do recoding
      workingrawfile.withoutlevels <<- recodefun(workingrawfile.withoutlevels, currecodemat)
      
      #save changes in tabulation
      tabulation <<- overviewfun(workingrawfile.withoutlevels,workingrawfile.withlevels)
      
      #save mv for log
      tempmvlogmat <- mvlogmat
      i=0
      repeat {
        i=i+1
        if (i>length(tempmvlogmat[,1])) break
        if (tempmvlogmat[i,1]==colnames(workingrawfile.withoutlevels[curcol])) 
        {
          tempmvlogmat[i,2] <- tempoldmv
          tempmvlogmat[i,3] <- tempnewmv
          break
        }
      }
      mvlogmat <<- tempmvlogmat
      
      finishedtext <- paste("FINISHED - recoded",tempoldmv,"to",tempnewmv, "in", colnames(workingrawfile.withoutlevels[curcol]))
      showModal(modalDialog(
        title = 'FINISHED',
        finishedtext,
        easyClose = TRUE,
        footer = NULL))
    })
    
    observeEvent(input$MV2_recode, {
      
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"MV2_recode")
      
      showModal(modalDialog(
        title = '',
        "recoding...",
        easyClose = TRUE,
        footer = NULL))
      
      curcol    <- input$curcolnuminput
      tempoldmv <- input$MV2_old
      tempnewmv <- input$MV2_new
      currecodemat <- cbind(tabulation[tabulation[,4]==colnames(workingrawfile.withoutlevels)[curcol],c(4,2,2)], NA,NA,NA,NA,NA,NA,NA,NA)
      currecodemat[1,4] <- tempoldmv 
      currecodemat[1,5] <- tempnewmv
      
      #check if mv is one of the old values. If yes, replace it
      for (i in 1:length(currecodemat[,1]))
      {
        if (!is.na(currecodemat[i,2]) & !is.na(currecodemat[1,4]))
        {
          if (currecodemat[i,2]==currecodemat[1,4]) (currecodemat[i,3] <- currecodemat[1,5])
        }
      }
      
      #do recoding
      workingrawfile.withoutlevels <<- recodefun(workingrawfile.withoutlevels, currecodemat)
      
      #save changes in tabulation
      tabulation <<- overviewfun(workingrawfile.withoutlevels,workingrawfile.withlevels)
      
      #save mv for log
      tempmvlogmat <- mvlogmat
      i=0
      repeat {
        i=i+1
        if (i>length(tempmvlogmat[,1])) break
        if (tempmvlogmat[i,1]==colnames(workingrawfile.withoutlevels[curcol])) 
        {
          tempmvlogmat[i,4] <- tempoldmv
          tempmvlogmat[i,5] <- tempnewmv
          break
        }
      }
      mvlogmat <<- tempmvlogmat
      
      finishedtext <- paste("FINISHED - recoded",tempoldmv,"to",tempnewmv, "in", colnames(workingrawfile.withoutlevels[curcol]))
      showModal(modalDialog(
        title = 'FINISHED',
        finishedtext,
        easyClose = TRUE,
        footer = NULL))
    })
    
    observeEvent(input$MV3_recode, {
      
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"MV3_recode")
      
      showModal(modalDialog(
        title = '',
        "recoding...",
        easyClose = TRUE,
        footer = NULL))
      
      curcol    <- input$curcolnuminput
      tempoldmv <- input$MV3_old
      tempnewmv <- input$MV3_new
      currecodemat <- cbind(tabulation[tabulation[,4]==colnames(workingrawfile.withoutlevels)[curcol],c(4,2,2)], NA,NA,NA,NA,NA,NA,NA,NA)
      currecodemat[1,4] <- tempoldmv 
      currecodemat[1,5] <- tempnewmv
      
      #check if mv is one of the old values. If yes, replace it
      for (i in 1:length(currecodemat[,1]))
      {
        if (!is.na(currecodemat[i,2]) & !is.na(currecodemat[1,4]))
        {
          if (currecodemat[i,2]==currecodemat[1,4]) (currecodemat[i,3] <- currecodemat[1,5])
        }
      }
      
      #do recoding
      workingrawfile.withoutlevels <<- recodefun(workingrawfile.withoutlevels, currecodemat)
      
      #save changes in tabulation
      tabulation <<- overviewfun(workingrawfile.withoutlevels,workingrawfile.withlevels)
      
      #save mv for log
      tempmvlogmat <- mvlogmat
      i=0
      repeat {
        i=i+1
        if (i>length(tempmvlogmat[,1])) break
        if (tempmvlogmat[i,1]==colnames(workingrawfile.withoutlevels[curcol])) 
        {
          tempmvlogmat[i,6] <- tempoldmv
          tempmvlogmat[i,7] <- tempnewmv
          break
        }
      }
      mvlogmat <<- tempmvlogmat 
      
      finishedtext <- paste("FINISHED - recoded",tempoldmv,"to",tempnewmv, "in", colnames(workingrawfile.withoutlevels[curcol]))
      showModal(modalDialog(
        title = 'FINISHED',
        finishedtext,
        easyClose = TRUE,
        footer = NULL))
    })
    
    
    observeEvent(input$MV4_recode, {
      
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"MV4_recode")
      
      showModal(modalDialog(
        title = '',
        "recoding...",
        easyClose = TRUE,
        footer = NULL))
      
      curcol    <- input$curcolnuminput
      tempoldmv <- input$MV4_old
      tempnewmv <- input$MV4_new
      currecodemat <- cbind(tabulation[tabulation[,4]==colnames(workingrawfile.withoutlevels)[curcol],c(4,2,2)], NA,NA,NA,NA,NA,NA,NA,NA)
      currecodemat[1,4] <- tempoldmv 
      currecodemat[1,5] <- tempnewmv
      
      #check if mv is one of the old values. If yes, replace it
      for (i in 1:length(currecodemat[,1]))
      {
        if (!is.na(currecodemat[i,2]) & !is.na(currecodemat[1,4]))
        {
          if (currecodemat[i,2]==currecodemat[1,4]) (currecodemat[i,3] <- currecodemat[1,5])
        }
      }
      
      #do recoding
      workingrawfile.withoutlevels <<- recodefun(workingrawfile.withoutlevels, currecodemat)
      
      #save changes in tabulation
      tabulation <<- overviewfun(workingrawfile.withoutlevels,workingrawfile.withlevels)
      
      #save mv for log
      tempmvlogmat <- mvlogmat
      i=0
      repeat {
        i=i+1
        if (i>length(tempmvlogmat[,1])) break
        if (tempmvlogmat[i,1]==colnames(workingrawfile.withoutlevels[curcol])) 
        {
          tempmvlogmat[i,6] <- tempoldmv
          tempmvlogmat[i,7] <- tempnewmv
          break
        }
      }
      mvlogmat <<- tempmvlogmat 
      
      finishedtext <- paste("FINISHED - recoded",tempoldmv,"to",tempnewmv, "in", colnames(workingrawfile.withoutlevels[curcol]))
      showModal(modalDialog(
        title = 'FINISHED',
        finishedtext,
        easyClose = TRUE,
        footer = NULL))
    })
      
      
    #Recode missing values in all cells
    observeEvent(input$MVall_recode, {
      
      colormat <<- updatecolormatfun(colormat,-999,"mvinallcolumns")
      
      showModal(modalDialog(
        title = 'Recode all mising values',
        "recoding...",
        easyClose = TRUE,
        footer = NULL))
      
      #write log matrix for missing values
      mvvector <- c(input$MV1_old,input$MV1_new,input$MV2_old,input$MV2_new,input$MV3_old,input$MV3_new,input$MV4_old,input$MV4_new)
      
      tempmvlogmat <- mvlogmat
      
      for (i in 1:length(tempmvlogmat[,1])) {
        if (i==1) 
        {
          tempmvlogmat[1,2:9] <- mvvector
        } else {
          if (tempmvlogmat[i,1]!=tempmvlogmat[i-1,1]) 
          {
            tempmvlogmat[i,2:9] <- mvvector
          }
        }
      }
     
      #write recodeing matrix for function recodefun
      currecoding <- cbind(tabulation[,c(4,2,2)],tempmvlogmat[,2:9])
      
      #do recoding
      workingrawfile.withoutlevels <<- recodefun(workingrawfile.withoutlevels,currecoding)
      tabulation <<- overviewfun(workingrawfile.withoutlevels,workingrawfile.withlevels)
      mvlogmat <<- tempmvlogmat
        
      finishedtext <- paste("FINISHED - recoded",input$MV1_old,"to",input$MV1_new,",",input$MV2_old,"to",input$MV2_new,",",input$MV3_old,"to",input$MV3_new,"and",input$MV4_old,"to",input$MV4_new)
      showModal(modalDialog(
        title = 'FINISHED',
        finishedtext,
        easyClose = TRUE,
        footer = NULL))
    })
    
    #Plot freq for each value in presented variable
    output$recodeplot <- renderPlot({
      curcol          <- input$curcolnuminput
      curcolstattemp  <- tabulation[tabulation[,4]==colnames(workingrawfile.withoutlevels)[curcol],]

      if (is.null(dim(curcolstattemp))) 
      {
        return(list(h3("All cells have the same entry!")))
      } else {
        curcolstat <- curcolstattemp[!is.na(curcolstattemp[,1]),]
        if (!is.null(dim(curcolstat))) 
        {
          recodeplot <- barplot(as.numeric(curcolstat[,3]), col="#E55A00", ylab="Frequencies", xlab="Values", names.arg=(curcolstat[,2]))
          return(recodeplot)
        }
      }
    })
    
    #create dummy?
    observeEvent(input$yesdummy, {
      
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"yesdummy")
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"nodummy",undo = TRUE)
      
      tempdummy <- dummy
      tempdummy[inittabulation[,4]==colnames(workingrawfile.withoutlevels)[input$curcolnuminput],2] <- 1
      dummy <<- tempdummy
      dummy[,1] <- inittabulation[,4]
    })
    
    observeEvent(input$nodummy, {
      
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"nodummy")
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"yesdummy",undo = TRUE)
      
      tempdummy <- dummy
      tempdummy[inittabulation[,4]==colnames(workingrawfile.withoutlevels)[input$curcolnuminput],2] <- 0
      dummy <<- tempdummy
      dummy[,1] <- inittabulation[,4]
    })
    
    #Use variable for SQSD?
    observeEvent(input$usenotforSQSD, {
      
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"usenotforSQSD")
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"useforSQSD",undo = TRUE)
      
      tempforsqsd <- forsqsd
      tempforsqsd[inittabulation[,4]==colnames(workingrawfile.withoutlevels)[input$curcolnuminput],2] <- 0
      forsqsd <<- tempforsqsd
      forsqsd[,1] <- inittabulation[,4]
    })
    
    observeEvent(input$useforSQSD, {
      
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"useforSQSD")
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"usenotforSQSD",undo = TRUE)
      
      tempforsqsd <- forsqsd
      tempforsqsd[inittabulation[,4]==colnames(workingrawfile.withoutlevels)[input$curcolnuminput],2] <- 1
      forsqsd <<- tempforsqsd
      forsqsd[,1] <- inittabulation[,4]
    })
    
    #set priority of variable
    observeEvent(input$priority, {
      tempprioritymat <<- prioritymat
      tempprioritymat[tabulation[,4]==colnames(workingrawfile.withoutlevels)[input$curcolnuminput],2] <<- input$priority
      prioritymat <<- tempprioritymat
      prioritymat[,1] <<- inittabulation[,4]
    })
    
    
    #do recoding and write log
    observeEvent(input$dorecode, {
      
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"dorecode")
      
      tempdata    <- workingrawfile.withoutlevels
      curcol      <- input$curcolnuminput
      
      tempmvlogmat <- mvlogmat[mvlogmat[,1]==colnames(workingrawfile.withoutlevels)[curcol],2:9]
      if (is.null(dim(tempmvlogmat))) (tempmvlogmat <- matrix(tempmvlogmat,1,8))
      
      currecoding <- cbind(tabulation[tabulation[,4]==colnames(workingrawfile.withoutlevels)[curcol],c(4,2,2)],tempmvlogmat)
      curvaluenum <- length(currecoding[,1])
      
      curcolstattemp  <- tabulation[tabulation[,4]==colnames(workingrawfile.withoutlevels)[curcol],]
      curcolstat <- curcolstattemp[!is.na(curcolstattemp[,1]),]
      
      for (i in 1:curvaluenum) 
      {
        for (j in 1:length(curcolstat[,1]))
        {
          if (!is.na(currecoding[i,2]) & !is.na(curcolstat[j,2]))
          {
            if (currecoding[i,2]==curcolstat[j,2]) 
            {
              temp <- paste("value_",j,sep="")
              currecoding[i,3] <- c(input[[temp]])
            }
          }
        }
      }
      
      curmerging <- checkmergefun(currecoding[!is.na(currecoding[,2]),])
      if (curmerging[1]!=FALSE)
      {

        
        showModal(modalDialog(
          title = 'WARNING',
          paste(curcolstat[curmerging[1],2],"(",curcolstat[curmerging[1],1],")","will be merged with",curcolstat[curmerging[2],2],"(",curcolstat[curmerging[2],1],")"),
          easyClose = FALSE,
          footer = NULL,
          size = "l",
          actionButton("continuerecoding","continue"),
          actionButton("escaperecoding","escape")))
        
          observeEvent(input$continuerecoding, {
            
            showModal(modalDialog(
              title = 'Recode!',
              "recoding...",
              easyClose = TRUE,
              footer = NULL))
            
            workingrawfile.withoutlevels <<- recodefun(workingrawfile.withoutlevels,currecoding)
            tabulation <<- overviewfun(workingrawfile.withoutlevels,workingrawfile.withlevels)
            
            showModal(modalDialog(
              title = 'Recode!',
              paste("FINISHED - recoded variable",colnames(tempdata[curcol])),
              easyClose = TRUE,
              footer = NULL))
          })
          
          observeEvent(input$escaperecoding, {
            showModal(modalDialog(
              title = 'Recode!',
              "Recoding canceled",
              easyClose = TRUE,
              footer = NULL))
          })
            
          } else {
            
            showModal(modalDialog(
              title = 'Recode!',
              "recoding...",
              easyClose = TRUE,
              footer = NULL))
 
            workingrawfile.withoutlevels <<- recodefun(workingrawfile.withoutlevels,currecoding)
            tabulation <<- overviewfun(workingrawfile.withoutlevels,workingrawfile.withlevels)
            
            showModal(modalDialog(
              title = 'Recode!',
              paste("FINISHED - recoded variable",colnames(tempdata[curcol])),
              easyClose = TRUE,
              footer = NULL))
          }
    })
    
    #Radio buttons for priority
    output$radiopriority <- renderUI({
      radioButtons("priority", label = h3("Priority"),choices = list("low" = 1, "mid" = 2, "high" = 3), selected = prioritymat[match(colnames(workingrawfile.withoutlevels)[input$curcolnuminput],prioritymat[,1]),2], inline = TRUE)
    })
        
    
    #Table for "check variables"
    output$col <- DT::renderDataTable({
      workingrawfile.withoutlevels[,c(input$num,input$num+1)]
    })
    
    #Output trimdata
    output$trimdata <<- renderUI({
      list(h4("Trim data?"),
           br(),
           actionButton('deletecolsingle',"Delete columns: uniform cells",style = colormat[1,6]),
           br(),br(),
           actionButton('deletecolunique',"Delete columns: unique cells ",style = colormat[1,7]),
           br(),br())
    })
    
    #Show numericInput for each value in presented variable
    observeEvent({c(input$dorecode,input$curcolnuminput,input$yesdummy,input$nodummy)},{
      output$recodenumericinput <- renderUI({
        dummy[,1] <- inittabulation[,4]
        if (dummy[match(colnames(workingrawfile.withlevels)[input$curcolnuminput],dummy[,1]),2]==0)
        {
          tabulation      <<- overviewfun(workingrawfile.withoutlevels,workingrawfile.withlevels)
          curcol          <-  input$curcolnuminput
          curcolstattemp  <-  tabulation[tabulation[,4]==colnames(workingrawfile.withoutlevels)[curcol],]
          
          if (is.null(dim(curcolstattemp))) 
          {
            return(list(h3("All cells have the same entry!")))
          } else {
            curcolstat <- curcolstattemp[!is.na(curcolstattemp[,1]),]
            if (is.null(dim(curcolstat))) 
            {
              return(list(h3("All cells have the same entry!")))
            } else {
              recodeinputlist <- lapply(1:(length(curcolstat[,1])), function(i) {
                inputname <- paste0("value_",i)
                if (is.na(curcolstat[i,2])) (tempnewvalue=NA) else (tempnewvalue=i)
                numericInput(inputname, label = paste('recode ',curcolstat[i,2],' (',curcolstat[i,1],') to'), value = tempnewvalue)
              })
              return(list(recodeinputlist,uiOutput("recodevariables")))
            }
          }
        } else {
          return(list(h4("Dummy will be created!")))
        }
      })
    })

    
    #Update color of action buttons
    observeEvent({c(input$curcolnuminput,input$MV1_recode,input$MV2_recode,input$MV3_recode,input$MV4_recode,input$deletecolsingle,input$deletecolunique,input$MVall_recode,input$useforSQSD,input$usenotforSQSD,input$dorecode,input$yesdummy,input$nodummy,input$applyall)} ,{
      output$MV_recode_token <<- renderUI(list(actionButton('MV1_recode','Recode!',style = colormat[input$curcolnuminput,2]), br(),br(),br(), 
                                              actionButton('MV2_recode','Recode!',style = colormat[input$curcolnuminput,3]), br(),br(),br(),
                                              actionButton('MV3_recode','Recode!',style = colormat[input$curcolnuminput,4]), br(),br(),br(), 
                                              actionButton('MV4_recode','Recode!',style = colormat[input$curcolnuminput,5])))
      output$mvinallcolumns  <<- renderUI(list(actionButton('MVall_recode','Recode missing values in all columns!',style = colormat[input$curcolnuminput,8])))
      output$useinSQSD       <<- renderUI(list(column(2,actionButton("usenotforSQSD",label = "Yes",style = colormat[input$curcolnuminput,10])),
                                               column(10,actionButton("useforSQSD",label = "No",style = colormat[input$curcolnuminput,9]))))
      output$createdummy     <<- renderUI(list(column(2,actionButton("yesdummy",label = "Yes",style = colormat[input$curcolnuminput,13])),
                                               column(10,actionButton("nodummy",label = "No",style = colormat[input$curcolnuminput,14]))))
      output$recodevariables <<- renderUI(list(actionButton('dorecode','Recode!', style = colormat[input$curcolnuminput,11])))
      output$trimdata <- renderUI({
        list(h4("Trim data?"),
           br(),
           actionButton('deletecolsingle',"Delete columns: uniform cells",style = colormat[1,6]),
           br(),br(),
           actionButton('deletecolunique',"Delete columns: unique cells ",style = colormat[1,7]),
           br(),br())
      })
    })
    
    #create UI: Manipulate variables tab
    if (exists("rawfile.withoutlevels"))
    {
      output$manipulatevariables <- renderUI({
        list(
          sidebarLayout(
            sidebarPanel( width	= 7,
              titlePanel("Manipulate Variables"),
              uiOutput("curcolnum"),
              br(),
              h4 ("Delete variable:"),
              uiOutput('useinSQSD'),
              br(),br(),br(),
              h4 ("Create dummy variable:"),
              uiOutput('createdummy'),
              br(),
              uiOutput('radiopriority'),
              br(),
              h4 ("Missing Values"),
              column(5, textInput("MV1_old", label = 'Non-Responder', value = NA),textInput("MV2_old", label = 'Filter', value = NA),textInput("MV3_old", label = 'SQSD', value = NA),textInput("MV4_old", label = 'Other', value = NA),uiOutput('mvinallcolumns')),
              column(5, uiOutput('MV_new')),
              column(2, br(), uiOutput('MV_recode_token')),
              br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
              #plotOutput('recodeplot')
            ),
          mainPanel( width = 5,
            br(),
            h1 (textOutput("curvariableheader")),
            h3 (textOutput('curvariablename')),
            br(),br(),
            uiOutput("recodenumericinput")
            )
          )
        )
      })
    } 
    
    #If variable is not numeric, force recoding
    #observeEvent(input$curcolnuminput, {
    #  if (!is.numeric(workingrawfile.withoutlevels[,input$curcolnuminput])) 
    #  {
    #    if (is.factor(workingrawfile.withoutlevels[,input$curcolnuminput]))
    #    {
    #      workingrawfile.withoutlevels[,input$curcolnuminput] <- as.numeric(workingrawfile.withoutlevels[,input$curcolnuminput])
    #      tabulation <<- overviewfun(workingrawfile.withoutlevels,workingrawfile.withlevels)
    #    } else {
    #      
    #      curcolstattemp  <- tabulation[tabulation[,4]==colnames(workingrawfile.withoutlevels)[input$curcolnuminput],]
    #      if(is.null(dim(curcolstattemp)))
    #      {
    #        curcolstat <- matrix(c(curcolstattemp[1],curcolstattemp[4]),1,2)
    #      } else {
    #        curcolstat <- curcolstattemp[!is.na(curcolstattemp[,1]),]
    #      }
    #    
    #      forcedrecodinglist <- lapply(1:(length(curcolstat[,1])), function(i) {
    #        inputname <- paste0("forced_value_",i)
    #        if (is.na(curcolstat[i,2])) (tempnewvalue=NA) else (tempnewvalue=i)
    #        return(numericInput(inputname, label = paste('recode ',curcolstat[i,2],' (',curcolstat[i,1],') to'), value = tempnewvalue))
    #      })
    #    
    #      output$forcerecoding <- renderUI(list(
    #        h4 ("Please recode variable to numeric:"),
    #        br(),
    #        h4 (varheader[input$curcolnuminput]),
    #        h5 (varnames[input$curcolnuminput]),
    #        tags$div(print(forcedrecodinglist)),
    #        actionButton("forcedrecode","Recode!")
    #      ))
    #      
    #      showModal(modalDialog(
    #        title = 'Recode!',
    #        uiOutput("forcerecoding"),
    #        easyClose = FALSE,
    #        footer = NULL))
    #      
    #      observeEvent(input$forcedrecode, {
    #        tempdata    <- workingrawfile.withoutlevels
    #        curcol      <- input$curcolnuminput
    #        curvaluenum <- length(unique(workingrawfile.withoutlevels[,curcol]))
    #        curvalue    <- sort(unique(workingrawfile.withoutlevels[,curcol]))
    #        currecoding <<- matrix(NA,curvaluenum,3)
    #        currecoding[,1] <- colnames(workingrawfile.withoutlevels)[curcol]
    #     
    #        for (i in 1:curvaluenum) {
    #          temp <- paste("forced_value_",i,sep="")
    #          currecoding[i,2:3] <- c(curvalue[i],input[[temp]])
    #        }
    #      
    #        if (length(currecoding[,1])>1)
    #        {
    #          curmerging <- checkmergefun(currecoding)
    #          if (curmerging[1]!=FALSE)
    #          {
    #            curcolstattemp  <- tabulation[tabulation[,4]==colnames(workingrawfile.withoutlevels)[curcol],]
    #            curcolstat <- curcolstattemp[!is.na(curcolstattemp[,1]),]
    #          
    #            showModal(modalDialog(
    #              title = 'WARNING',
    #              paste(curcolstat[curmerging[1],2],"(",curcolstat[curmerging[1],1],")","will be merged with",curcolstat[curmerging[2],2],"(",curcolstat[curmerging[2],1],")"),
    #              easyClose = FALSE,
    #              footer = NULL,
    #              size = "l",
    #              actionButton("continuerecoding","continue"),
    #              actionButton("escaperecoding","escape")))
    #          
    #            observeEvent(input$continuerecoding, {
    #              workingrawfile.withoutlevels <<- recodefun(workingrawfile.withoutlevels,currecoding)
    #              tabulation <<- tabulationupdatefun(tabulation,currecoding)
    #              workingrawfile.withoutlevels[,curcol] <- as.numeric(workingrawfile.withoutlevels[,curcol])
    #              if(is.numeric(workingrawfile.withoutlevels[,curcol])) (removeModal())
    #            })
    #          } else {
    #            workingrawfile.withoutlevels <<- recodefun(workingrawfile.withoutlevels,currecoding)
    #            tabulation <<- tabulationupdatefun(tabulation,currecoding)
    #            workingrawfile.withoutlevels[,curcol] <- as.numeric(workingrawfile.withoutlevels[,curcol])
    #            if(is.numeric(workingrawfile.withoutlevels[,curcol])) (removeModal())
    #          }
    #        } else {
    #          workingrawfile.withoutlevels <<- recodefun(workingrawfile.withoutlevels,currecoding)
    #          tabulation <<- tabulationupdatefun(tabulation,currecoding)
    #          workingrawfile.withoutlevels[,curcol] <- as.numeric(workingrawfile.withoutlevels[,curcol])
    #          if(is.numeric(workingrawfile.withoutlevels[,curcol])) (removeModal())
    #       }
    #     })
    #    }
    #  }
    #})
    
    
    #------------------------------------------------------------
    #load logfile
    #------------------------------------------------------------
    
    #create logfileUI
    
    output$logoutput <- renderUI({
      if (exists("workingrawfile.withoutlevels"))
      {
        list(
          titlePanel("Select Logfile"),
          sidebarLayout(
            sidebarPanel(
              fileInput('LOG', 'Upload Logfile', accept=c('.csv')),
              downloadButton('downloadlogfile', 'Download Logfile'),
              br(),br(),
              uiOutput('applylogfile')
            ),
            mainPanel(
              wellPanel(htmlOutput("log"))
            )
          )
        )
      }
    })
    
    
    output$log <- renderUI({
      
      colormat <<- updatecolormatfun(colormat,-999,"applyall", undo = TRUE)
      
      inFileLog <- input$LOG
      
      if (is.null(inFileLog))
        return(NULL)
      
      # read data with values
      uploadedLogfile <<- read.table(file = inFileLog$datapath, header = TRUE, sep=";",quote = '"/', fill = TRUE, colClasses = "character")
      
      #actionbutton apply all changes
      output$applylogfile <- renderUI({
        list(actionButton('applyall','Apply all changes from logfile', style = colormat[1,12]))
      })
      
      observeEvent(input$applyall, {
        output$applylogfile <- renderUI({
          list(actionButton('applyall','Apply all changes from logfile', style = colormat[1,12]))
        })  
      })
      
      #create summary of logfile for output
      logfilesumrecoding <- uploadedLogfile[(!is.na(uploadedLogfile[,3]) & !is.na(uploadedLogfile[,4]) & uploadedLogfile[,3]!=uploadedLogfile[,4]),]
      logfilesummv       <- uploadedLogfile[(!is.na(uploadedLogfile[,5]) | !is.na(uploadedLogfile[,6]) | !is.na(uploadedLogfile[,7]) | !is.na(uploadedLogfile[,8]) | !is.na(uploadedLogfile[,9]) | !is.na(uploadedLogfile[,10]) | !is.na(uploadedLogfile[,11]) | !is.na(uploadedLogfile[,12])),]
      
      logfilemvsummary <<- lapply(1:(length(logfilesummv[,1])), function(i) {
        if (i==1)
        {
          return(HTML(paste0("<br> Recode missing values ",logfilesummv[i,5],", ",logfilesummv[i,7],", ",logfilesummv[i,9],", ",logfilesummv[i,11]," to  ",logfilesummv[i,6],", ",logfilesummv[i,8],", ",logfilesummv[i,10],", ",logfilesummv[i,12],"<br/>")))
        }
        if (i>1) 
        {
          if (!(identical(logfilesummv[i-1,5:12],logfilesummv[i,5:12])) & all(logfilesummv[i,5:12]!="") & any(!is.na(logfilesummv[i,5:12])))
          {
            return(HTML(paste0("<br> Recode missing values ",logfilesummv[i,5],", ",logfilesummv[i,7],", ",logfilesummv[i,9],", ",logfilesummv[i,11]," to  ",logfilesummv[i,6],", ",logfilesummv[i,8],", ",logfilesummv[i,10],", ",logfilesummv[i,12],"<br/>")))
          }
        }
      })
      
      if (dim(logfilesumrecoding)[1]==0)
      {
        logfilesummary <- list(HTML("<br> No recodings saved in logfile! <br/>"))
      } else {
        logfilesummary <- lapply(1:length(logfilesumrecoding[,1]), function(i) {
          HTML(paste0("<br> Recode ",logfilesumrecoding[i,1],": ", logfilesumrecoding[i,3]," (",logfilesumrecoding[i,2],") to ", logfilesumrecoding[i,4],"<br/>"))
        })
      }
      
      #Which variables will be deleted?
      logfilevartoremain <- HTML(paste0("<br> Delete variable(s): ",unique(uploadedLogfile[uploadedLogfile$Remain==0,1]),"<br/>"))
      
      #Which variables will be dummycoded?
      logfiledummy <- HTML(paste0("<br> Create dummy: ",unique(uploadedLogfile[uploadedLogfile$Create.Dummy==1,1]),"<br/>"))
      
      return(list(logfilesummary,logfilemvsummary,logfilevartoremain,logfiledummy))
    })
    
    #recode all from logfile
    observeEvent(input$applyall, {
      
      showModal(modalDialog(
        title = 'Apply all changes from logfile',
        "recoding...",
        easyClose = TRUE,
        footer = NULL))
      
      tempcolormat <<- colormat
      templogfile  <<- uploadedLogfile
      tempworkingrawfile.withoutlevels <<- workingrawfile.withoutlevels
      
      colnames(tempcolormat) <<- c("variable","MV1_recode","MV2_recode","MV3_recode","MV4_recode","deletecolsingle","deletecolunique","mvinallcolumns","useforSQSD","usenotforSQSD","dorecode","applyall","yesdummy","nodummy")
      tempcolormat <<- updatecolormatfun(tempcolormat,-999,"applyall")
      
      #use only those variables from logfile that occure in data
      for (var in unique(templogfile[,1]))
      {
        if (!(var %in% colnames(workingrawfile.withoutlevels)))
        {
          templogfile <- templogfile[templogfile[,1]!=var,]
        }
      }
      
      #recoding
      tempworkingrawfile.withoutlevels <<- recodefun(tempworkingrawfile.withoutlevels, cbind(uploadedLogfile[,1],uploadedLogfile[,3:12]))
      tabulation <<- overviewfun(workingrawfile.withoutlevels,workingrawfile.withlevels)
      
      #label unused variables. Variables will be deleted when data will be downloaded.
      tempforsqsd     <<- forsqsd
      tempforsqsd[,1] <<- inittabulation[,4]
      for (var in colnames(tempworkingrawfile.withoutlevels))
      {
        if (templogfile$Remain[match(var,templogfile$variable)]==0)
        {
          tempforsqsd[(tempforsqsd[,1]==var),2] <- 0
          tempcolormat <<- updatecolormatfun(tempcolormat,match(var,colnames(tempworkingrawfile.withoutlevels)),"usenotforSQSD")
          tempcolormat <<- updatecolormatfun(tempcolormat,match(var,colnames(tempworkingrawfile.withoutlevels)),"useforSQSD",undo = TRUE)
        } else {
          tempforsqsd[(tempforsqsd[,1]==var),2] <- 1
          tempcolormat <<- updatecolormatfun(tempcolormat,match(var,colnames(tempworkingrawfile.withoutlevels)),"useforSQSD")
          tempcolormat <<- updatecolormatfun(tempcolormat,match(var,colnames(tempworkingrawfile.withoutlevels)),"usenotforSQSD",undo = TRUE)
        }
      }
      forsqsd <<- tempforsqsd
      
      #dummy variables
      tempdummy     <<- dummy
      tempdummy[,1] <<- inittabulation[,4]
      for (var in colnames(tempworkingrawfile.withoutlevels))
      {
        if (templogfile$Create.Dummy[match(var,templogfile$variable)]==0)
        {
          tempdummy[(tempdummy[,1]==var),2] <- 0
          tempcolormat <<- updatecolormatfun(tempcolormat,match(var,colnames(tempworkingrawfile.withoutlevels)),"nodummy")
          tempcolormat <<- updatecolormatfun(tempcolormat,match(var,colnames(tempworkingrawfile.withoutlevels)),"yesdummy",undo = TRUE)
        } else {
          tempdummy[(tempdummy[,1]==var),2] <- 1
          tempcolormat <<- updatecolormatfun(tempcolormat,match(var,colnames(tempworkingrawfile.withoutlevels)),"yesdummy")
          tempcolormat <<- updatecolormatfun(tempcolormat,match(var,colnames(tempworkingrawfile.withoutlevels)),"nodummy",undo = TRUE)
        }
      }
      dummy <<- tempdummy
      
      #priotity
      tempprioritymat     <<- prioritymat
      tempprioritymat[,1] <<- inittabulation[,4]
      for (var in colnames(tempworkingrawfile.withoutlevels))
      {
        for (prio in 1:3)
        {
          if (templogfile$Priority[match(var,templogfile$variable)]==prio)
          {
            tempprioritymat[(tempprioritymat[,1]==var),2] <- prio
          }
        }
      }
      
      prioritymat <<- tempprioritymat
      
      #update missing values matrix
      tempmvlogmat <<- mvlogmat
      for (var in unique(uploadedLogfile[,1]))
      {
        if (var %in% tempmvlogmat[,1])
        {
          tempmvlogmat <<- tempmvlogmat[(tempmvlogmat[,1]!=var),]
          tempmvlogmat <<- rbind(tempmvlogmat,uploadedLogfile[(uploadedLogfile[,1]==var),c(1,5:12)])
        }
      }
      mvlogmat <<- tempmvlogmat
      
      #save changes in global environment
      colormat   <<- tempcolormat
      tabulation <<- overviewfun(workingrawfile.withoutlevels,workingrawfile.withlevels)
      workingrawfile.withoutlevels <<- tempworkingrawfile.withoutlevels
      
      showModal(modalDialog(
        title = 'Apply all changes from logfile',
        "FINISHED",
        easyClose = TRUE,
        footer = NULL))
    })
    
    
    workingrawfile.withoutlevels
  })
  
  
  #------------------------------------------------------------
  #savings
  #------------------------------------------------------------
  
  #download manual
  output$downloadManual <- downloadHandler(
    filename = "Manual Data Input Preparation Tool.pdf",
    content = function(file) {
      file.copy(from = "C:/Users/arsteink/Desktop/Shiny/01 Data Preparation Tool/www/Manual Data Input Preparation Tool.pdf", to = file, overwrite = TRUE)
    },
    contentType = "pdf"
  )
  
  #download new dataset
  output$downloadData <- downloadHandler(
    filename <- function() {
      paste("reviewed_", Sys.Date(),"_",".csv", sep="")
    },
    content <- function(file) {
      
      workingrawfile.withoutlevels <- deletenotusedcolfun(workingrawfile.withoutlevels,forsqsd)
      
      dummy[,1] <- inittabulation[,4]
      for(tempcolumn in colnames(workingrawfile.withoutlevels))
      {
        if (dummy[match(tempcolumn,dummy[,1]),2]==1) (workingrawfile.withoutlevels <- create_sel_dummies(workingrawfile.withoutlevels, list(tempcolumn), exclude = TRUE, processNA = TRUE, concat = TRUE))
      }
      
      if(IDvariable.name %in% colnames(workingrawfile.withoutlevels)) 
      {
        workingrawfile.withoutlevels <- workingrawfile.withoutlevels[,-(match(IDvariable.name,colnames(workingrawfile.withoutlevels)))]
      }
      
      if(IDvariable.name!="no ID variable") (workingrawfile.withoutlevels <- cbind(IDvariable,workingrawfile.withoutlevels))
      
      return(write.csv(workingrawfile.withoutlevels, file, sep=""))
    }
  )
  
  #download new logfile
  output$downloadlogfile <- downloadHandler(
    filename = function() {paste0(input$SAV,'_logfile_',Sys.Date(),'.csv')},
    content = function(file) {(
      write.table(
      cbind(inittabulation[,4],inittabulation[,1],inittabulation[,2],tabulation[,2],mvlogmat[,2:9],forsqsd[,2],dummy[,2],prioritymat[,2]),
      col.names = c("variable", "level", "oldvalue", "newvalue", "MV1_old", "MV1_new","MV2_old", "MV2_new","MV3_old", "MV3_new","MV4_old", "MV4_new","Remain","Create Dummy","Priority"),
      row.names = FALSE,
      sep = ";",
      eol = "\r\n",
      file))}
  )
}