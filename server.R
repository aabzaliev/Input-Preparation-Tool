library(shiny)
library(foreign)
library(stringr)

#set path to app
path2app <- "S:/4Alex/Praktikum Alex/01 Shiny App/01 Data Preparation Tool/"

#set max size of data
options(shiny.maxRequestSize=100*1024^2)

#load functions
file.sources = list.files(paste0(path2app,"functions/"), 
                          pattern="*.R$", full.names=TRUE, 
                          ignore.case=TRUE)

sapply(file.sources,sys.source,.GlobalEnv)

#--------------------------------------------------------------------------------------------------------
# CORE SERVER # CORE SERVER # CORE SERVER # CORE SERVER # CORE SERVER # CORE SERVER # CORE SERVER # CORE 
#--------------------------------------------------------------------------------------------------------

#init trigger for column deletion in tab 'Upload File'
#This is necessary for updating the color of action buttons after they have been pressed.
trigger.coldeleted <<- matrix(FALSE,2,1)
rownames(trigger.coldeleted) <- c("unique","single")

#Shiny server function
function(input, output, session) {
  
  #Delete objects from last session
    objectstodelete <- list("colormat",
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
    rm("objectstodelete")
    gc()
  
  #------------------------------------------------------------
  #load data
  #------------------------------------------------------------
  
  #Create UI for manipulation tab
  #When no data is uploaded, this information will be presented
  output$manipulatevariables <- renderUI({
    titlePanel(h3("Please upload SPSS data first!", align = "center"))
  })
    
  #Create UI for logfile tab
  #When no data is uploaded, this information will be presented
  output$logoutput <- renderUI({
    titlePanel(h3("Please upload SPSS data first!", align = "center"))
  })  
  
  #Everything in this function will only be executed when data was uploaded
  output$rawfile <- DT::renderDataTable({
    
    #Uploaded file
    inFile <- input$SAV
    
    #If no file was uploaded, function returns null
    if (is.null(inFile))
      return(NULL)
    
    #Read data without and with levels
    rawfile.withoutlevels <<- read.spss(file = inFile$datapath, to.data.frame = TRUE, use.value.labels = FALSE, reencode = "UTF-8")
    rawfile.withlevels    <<- read.spss(file = inFile$datapath, to.data.frame = TRUE, use.value.labels = TRUE, reencode = "UTF-8")    

    #Save data in working files
    workingrawfile.withoutlevels  <<- rawfile.withoutlevels
    workingrawfile.withlevels     <<- rawfile.withlevels
    
    #This gives levels, values and frequencies of variables contained in original file
    #Inittabulation should not be edited by the app
    #All changes that are done by the user are stored in the tabulation object
    #Therefore, tabulation must be updated after each manipulation
    #Note that both objects are saved globally so that they can be shared between functions
    tempinittabulation  <- overviewfun(workingrawfile.withoutlevels,workingrawfile.withlevels)
    colnames(tempinittabulation) <- c("level", "oldvalue", "freq", "variable")
    inittabulation  <<- tempinittabulation
    tabulation      <<- inittabulation
    
    #Missing value matrix for logfile
    tempmvlogmat  <- tabulation[,4]
    tempmvlogmat  <- cbind(tempmvlogmat,matrix(NA,length(tabulation[,1]),8))
    colnames(tempmvlogmat) <- c("variable", "MV1_old", "MV1_new","MV2_old", "MV2_new","MV3_old", "MV3_new","MV4_old", "MV4_new")
    mvlogmat <<- tempmvlogmat
    
    #Matrix with styles for action buttons
    #When a action button was pressed, the regarding cell is updated with a different style. The action button will then be presented in this style.
    if(!exists("colormat"))
    {
      colormat <<- matrix("color: #2E2E2E; background-color: #FFFFFF; border-color: #D8D8D8",length(colnames(workingrawfile.withoutlevels)),14)
      colormat[,1] <- colnames(workingrawfile.withoutlevels)
      colnames(colormat) <- c("variable","MV1_recode","MV2_recode","MV3_recode","MV4_recode","deletecolsingle","deletecolunique","mvinallcolumns","useforSQSD","usenotforSQSD","dorecode","applyall","yesdummy","nodummy")  
    }
    
    #Variable headers and names
    varheader <- names(attributes(workingrawfile.withoutlevels)$variable.labels)
    varnames  <- (attributes(workingrawfile.withoutlevels)$variable.labels)
          
    #Identify free missing values for missing values recoding suggestions 
    #Start with -66, -77, -88 and -99. When one of those is already existing in the data, another value is used (e.g. when -99 exists, -999 is used as a suggestion)
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
    
    #Create default matrix for creating dummies
    #Default value = 0 (do not create dummys)
    if (!exists("dummy"))
    {
      dummy <<- matrix(0,length(inittabulation[,1]),2)
    }
    
    #Create default matrix for single variable deletion 
    #Default value = 1 (do not delete)
    if (!exists("forsqsd"))
    {
      forsqsd <<- matrix(1,length(inittabulation[,1]),2)
    }
    
    #Create default priority matrix
    #Default value = 1 (low priority)
    if (!exists("prioritymat"))
    {
      prioritymat <<- matrix(1,length(inittabulation[,1]),2)
    }
    
    #Define a ID-Variable
    #This variable will not be changed by any manipulations
    #It is saved here and binded to the data when it will be downloaded 
    #Create list of variables for selecting a ID-variable
    varlist <- list()
    count <- -1
    for (i in c("no ID variable",colnames(workingrawfile.withoutlevels)))
    {
      count <- count+1
      varlist[[i]] <- count
    }
    
    #Select ID-variable in modal dialog
    showModal(modalDialog(
      title = 'ID variable',
      selectInput("selectID", label = h3("Please select ID variable"), 
                  choices = varlist, 
                  selected = 0),
      actionButton("closeidselection","continue"),
      easyClose = FALSE,
      footer = NULL))
    
    #Save ID variable and close modal dialog
    observeEvent(input$closeidselection, {
        IDvariable.name    <<- names(varlist[as.numeric(input$selectID)+1])
        if(IDvariable.name!="no ID variable") 
          {
          IDvariable <<- workingrawfile.withoutlevels[[IDvariable.name]]
        }
      removeModal()
    })
   
    #Delete single-entry columns
    #Do if action button is pressed
    observeEvent(input$deletecolsingle, {
      
      #Present processing status
      showModal(modalDialog(
        title = 'Delete columns with uniform entries"',
        "deleting...",
        easyClose = TRUE,
        footer = NULL))
      
      #create temp workfile
      tempold.workingrawfile.withoutlevels <- workingrawfile.withoutlevels
      
      #delete variables
      workingrawfile.withoutlevels  <<- deletecolumnssinglefun(workingrawfile.withoutlevels)
      workingrawfile.withlevels     <<- deletecolumnssinglefun(workingrawfile.withlevels)
      
      #update tabulation and matrix for missing values
      tabulation <<- overviewfun(workingrawfile.withoutlevels,workingrawfile.withlevels)
      mvlogmat <<- mvlogmat[(mvlogmat[,1] %in% tabulation[,4]),]
      
      #update variabel names and headers
      varheader <<- updatenamesfun(tempold.workingrawfile.withoutlevels,workingrawfile.withoutlevels,varheader)
      varnames  <<- updatenamesfun(tempold.workingrawfile.withoutlevels,workingrawfile.withoutlevels,varnames)
      
      #update matrix with styles for action buttons and change trigger
      colormat <<- trimcolormatfun(colnames(tempold.workingrawfile.withoutlevels),tabulation,colormat)
      trigger.coldeleted[2,1] <- TRUE
      
      #Change style of the pressed action button
      if (trigger.coldeleted[1,1]) {colormat <<- updatecolormatfun(colormat,-999,"deletecolunique")}
      colormat <<- updatecolormatfun(colormat,-999,"deletecolsingle")
      
      #save for logfile
      #Which variables were deleted?
      tempforsqsd <- forsqsd
      tempforsqsd[!(inittabulation[,4] %in% colnames(workingrawfile.withoutlevels)),2] <- 0
      forsqsd <<- tempforsqsd
      forsqsd[,1] <- inittabulation[,4]
      
      #Present processing status
      showModal(modalDialog(
        title = 'Recode!',
        "FINISHED - deleted all columns with uniform entries!",
        easyClose = TRUE,
        footer = NULL))
    })
    
    #delete unique-entry columns
    observeEvent(input$deletecolunique, {
      
      #Present processing status
      showModal(modalDialog(
        title = 'Delete columns with unique entries"',
        "deleting...",
        easyClose = TRUE,
        footer = NULL))
      
      #create temp workfile
      tempold.workingrawfile.withoutlevels <- workingrawfile.withoutlevels
      
      #delete variables
      workingrawfile.withoutlevels  <<- deletecolumnsuniquefun(workingrawfile.withoutlevels)
      workingrawfile.withlevels     <<- deletecolumnsuniquefun(workingrawfile.withlevels)
      
      #update tabulation and matrix for missing values
      tabulation <<- overviewfun(workingrawfile.withoutlevels,workingrawfile.withlevels)
      mvlogmat <<- mvlogmat[(mvlogmat[,1] %in% tabulation[,4]),]
      
      #update variabel names and headers
      varheader <<- updatenamesfun(tempold.workingrawfile.withoutlevels,workingrawfile.withoutlevels,varheader)
      varnames  <<- updatenamesfun(tempold.workingrawfile.withoutlevels,workingrawfile.withoutlevels,varnames)
      
      #update matrix with styles for action buttons and change trigger
      colormat <<- trimcolormatfun(colnames(tempold.workingrawfile.withoutlevels),tabulation,colormat)
      trigger.coldeleted[1,1] <- TRUE
      
      #Change style of the pressed action button
      if (trigger.coldeleted[2,1]) {colormat <<- updatecolormatfun(colormat,-999,"deletecolsingle")}
      colormat <<- updatecolormatfun(colormat,-999,"deletecolunique")
      
      #save for logfile
      tempforsqsd <- forsqsd
      tempforsqsd[!(inittabulation[,4] %in% colnames(workingrawfile.withoutlevels)),2] <- 0
      
      #Which variables were deleted?
      forsqsd <<- tempforsqsd
      forsqsd[,1] <- inittabulation[,4]
      
      #Present processing status
      showModal(modalDialog(
        title = 'Recode!',
        "FINISHED - deleted all columns with unique entries!",
        easyClose = TRUE,
        footer = NULL))
    })
    
    #Set current variable number in manipualte variables tab
    output$curcolnum <- renderUI({
      return(numericInput("curcolnuminput", label = "Go to variable", min = 1, max = length(colnames(workingrawfile.withoutlevels)), value = 1))
    })
    
    #Header of current variable
    output$curvariableheader <- renderText(varheader[input$curcolnuminput])
    output$curvariablename   <- renderText(varnames[input$curcolnuminput])
        
    #Define UI for recoding missing values with suggested missing values
    output$MV_new <- renderUI({
      curcol <- input$curcolnuminput
      MV_new <- lapply(1:4, function(i) {
        inputname <- paste0("MV",i,"_new")
        tempnewvalue=defaultmv[i]
        textInput(inputname, label = "new", value = tempnewvalue)
      })
      MV_new
    })
    
    #Recode missing value 1
    #Do if action button is pressed
    observeEvent(input$MV1_recode, {
      
      #Update style of action button
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"MV1_recode")
      
      #Present process status
      showModal(modalDialog(
        title = '',
        "recoding...",
        easyClose = TRUE,
        footer = NULL))
      
      #prepare recode matrix for function recodefun
      curcol    <- input$curcolnuminput
      if (input$MV1_old == "") (tempoldmv <- NA) else (tempoldmv <- input$MV1_old)
      if (input$MV1_new == "") (tempnewmv <- NA) else (tempnewmv <- input$MV1_new)
      currecodemat <- cbind(tabulation[tabulation[,4]==colnames(workingrawfile.withoutlevels)[curcol],c(4,2,2)], NA,NA,NA,NA,NA,NA,NA,NA)
      currecodemat[1,4] <- tempoldmv 
      currecodemat[1,5] <- tempnewmv
      
      #Check if missing value is one of the old values in the recode matrix. If yes, replace it
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
      
      #save missing values for log
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
      
      #Present process status
      finishedtext <- paste("FINISHED - recoded",tempoldmv,"to",tempnewmv, "in", colnames(workingrawfile.withoutlevels[curcol]))
      showModal(modalDialog(
        title = 'FINISHED',
        finishedtext,
        easyClose = TRUE,
        footer = NULL))
    })
    
    #Recode missing value 2
    #Do if action button is pressed
    observeEvent(input$MV2_recode, {
      
      #Update style of action button
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"MV2_recode")
      
      #Present process status
      showModal(modalDialog(
        title = '',
        "recoding...",
        easyClose = TRUE,
        footer = NULL))
      
      #prepare recode matrix for function recodefun
      curcol    <- input$curcolnuminput
      if (input$MV2_old == "") (tempoldmv <- NA) else (tempoldmv <- input$MV2_old)
      if (input$MV2_new == "") (tempnewmv <- NA) else (tempnewmv <- input$MV2_new)
      currecodemat <- cbind(tabulation[tabulation[,4]==colnames(workingrawfile.withoutlevels)[curcol],c(4,2,2)], NA,NA,NA,NA,NA,NA,NA,NA)
      currecodemat[1,4] <- tempoldmv 
      currecodemat[1,5] <- tempnewmv
      
      #Check if missing value is one of the old values in the recode matrix. If yes, replace it
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
      
      #save missing values for log
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
      
      #Present process status
      finishedtext <- paste("FINISHED - recoded",tempoldmv,"to",tempnewmv, "in", colnames(workingrawfile.withoutlevels[curcol]))
      showModal(modalDialog(
        title = 'FINISHED',
        finishedtext,
        easyClose = TRUE,
        footer = NULL))
    })
    
    #Recode missing value 3
    #Do if action button is pressed
    observeEvent(input$MV3_recode, {
      
      #Update style of action button
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"MV3_recode")
      
      #Present process status
      showModal(modalDialog(
        title = '',
        "recoding...",
        easyClose = TRUE,
        footer = NULL))
      
      #prepare recode matrix for function recodefun
      curcol    <- input$curcolnuminput
      if (input$MV3_old == "") (tempoldmv <- NA) else (tempoldmv <- input$MV3_old)
      if (input$MV3_new == "") (tempnewmv <- NA) else (tempnewmv <- input$MV3_new)
      currecodemat <- cbind(tabulation[tabulation[,4]==colnames(workingrawfile.withoutlevels)[curcol],c(4,2,2)], NA,NA,NA,NA,NA,NA,NA,NA)
      currecodemat[1,4] <- tempoldmv 
      currecodemat[1,5] <- tempnewmv
      
      #Check if missing value is one of the old values in the recode matrix. If yes, replace it
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
      
      #save missing values for log
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
      
      #Present process status
      finishedtext <- paste("FINISHED - recoded",tempoldmv,"to",tempnewmv, "in", colnames(workingrawfile.withoutlevels[curcol]))
      showModal(modalDialog(
        title = 'FINISHED',
        finishedtext,
        easyClose = TRUE,
        footer = NULL))
    })
    
    #Recode missing value 4
    #Do if action button is pressed
    observeEvent(input$MV4_recode, {
      
      #Update style of action button
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"MV4_recode")
      
      #Present process status
      showModal(modalDialog(
        title = '',
        "recoding...",
        easyClose = TRUE,
        footer = NULL))
      
      #prepare recode matrix for function recodefun
      curcol    <- input$curcolnuminput
      if (input$MV4_old == "") (tempoldmv <- NA) else (tempoldmv <- input$MV4_old)
      if (input$MV4_new == "") (tempnewmv <- NA) else (tempnewmv <- input$MV4_new)
      currecodemat <- cbind(tabulation[tabulation[,4]==colnames(workingrawfile.withoutlevels)[curcol],c(4,2,2)], NA,NA,NA,NA,NA,NA,NA,NA)
      currecodemat[1,4] <- tempoldmv 
      currecodemat[1,5] <- tempnewmv
      
      #Check if missing value is one of the old values in the recode matrix. If yes, replace it
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
      
      #save missing values for log
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
      
      #Present process status
      finishedtext <- paste("FINISHED - recoded",tempoldmv,"to",tempnewmv, "in", colnames(workingrawfile.withoutlevels[curcol]))
      showModal(modalDialog(
        title = 'FINISHED',
        finishedtext,
        easyClose = TRUE,
        footer = NULL))
    })
      
      
    #Recode missing values in all variables
    #Do if action button is pressed
    observeEvent(input$MVall_recode, {
      
      #Update style of action button
      colormat <<- updatecolormatfun(colormat,-999,"mvinallcolumns")
      
      #Present process status
      showModal(modalDialog(
        title = 'Recode all mising values',
        "recoding...",
        easyClose = TRUE,
        footer = NULL))
      
      #Get old and new missinf values
      mvvector <- c(input$MV1_old,input$MV1_new,input$MV2_old,input$MV2_new,input$MV3_old,input$MV3_new,input$MV4_old,input$MV4_new)
      
      #write log matrix for missing values
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
     
      #Write recoding matrix for function recodefun
      currecoding <- cbind(tabulation[,c(4,2,2)],tempmvlogmat[,2:9])
      
      #replace "" in currecoding with NA
      for (i in 1:length(currecoding[1,]))
      {
        for (j in 1:length(currecoding[,1]))
        {
          if (!is.na(currecoding[j,i]))
          {
            if (currecoding[j,i]=="") (currecoding[j,i] <- NA)
          }
        }
      }
      
      #do recoding
      workingrawfile.withoutlevels <<- recodefun(workingrawfile.withoutlevels,currecoding)
      
      #Update tabulation and missing values matrix for log
      tabulation <<- overviewfun(workingrawfile.withoutlevels,workingrawfile.withlevels)
      mvlogmat <<- tempmvlogmat
      
      #Present process status
      finishedtext <- paste("FINISHED - recoded",input$MV1_old,"to",input$MV1_new,",",input$MV2_old,"to",input$MV2_new,",",input$MV3_old,"to",input$MV3_new,"and",input$MV4_old,"to",input$MV4_new)
      showModal(modalDialog(
        title = 'FINISHED',
        finishedtext,
        easyClose = TRUE,
        footer = NULL))
    })
    
    #Should dummy variables be created for the current variable?
    #If 'yes' is pressed, do this
    observeEvent(input$yesdummy, {
      
      #Update style of the action buttons
      #First, change pressed 'yes'-button to orange
      #Second, change not pressed 'no'-button to white
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"yesdummy")
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"nodummy",undo = TRUE)
      
      #Update dummy vector for logfile
      tempdummy <- dummy
      tempdummy[inittabulation[,4]==colnames(workingrawfile.withoutlevels)[input$curcolnuminput],2] <- 1
      dummy <<- tempdummy
      dummy[,1] <- inittabulation[,4]
    })
    
    #Should dummy variables be created for the current variable?
    #If 'no' is pressed, do this
    observeEvent(input$nodummy, {
      
      #Update style of the action buttons
      #First, change pressed 'no'-button to orange
      #Second, change not pressed 'yes'-button to white
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"nodummy")
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"yesdummy",undo = TRUE)
      
      #Update dummy vector for logfile
      tempdummy <- dummy
      tempdummy[inittabulation[,4]==colnames(workingrawfile.withoutlevels)[input$curcolnuminput],2] <- 0
      dummy <<- tempdummy
      dummy[,1] <- inittabulation[,4]
    })
    
    #Delete variable?
    #If 'no' is pressed, do this
    observeEvent(input$usenotforSQSD, {
      
      #Update style of the action buttons
      #First, change pressed 'no'-button to orange
      #Second, change not pressed 'yes'-button to white
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"usenotforSQSD")
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"useforSQSD",undo = TRUE)
      
      #Update dummy vector for logfile
      tempforsqsd <- forsqsd
      tempforsqsd[inittabulation[,4]==colnames(workingrawfile.withoutlevels)[input$curcolnuminput],2] <- 0
      forsqsd <<- tempforsqsd
      forsqsd[,1] <<- inittabulation[,4]
    })
    
    #Delete variable?
    #If 'yes' is pressed, do this
    observeEvent(input$useforSQSD, {
      
      #Update style of the action buttons
      #First, change pressed 'yes'-button to orange
      #Second, change not pressed 'no'-button to white
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"useforSQSD")
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"usenotforSQSD",undo = TRUE)
      
      #Update dummy vector for logfile
      tempforsqsd <- forsqsd
      tempforsqsd[inittabulation[,4]==colnames(workingrawfile.withoutlevels)[input$curcolnuminput],2] <- 1
      forsqsd <<- tempforsqsd
      forsqsd[,1] <<- inittabulation[,4]
    })
    
    #Set priority of the current variable
    observeEvent(input$priority, {
      tempprioritymat <<- prioritymat
      tempprioritymat[tabulation[,4]==colnames(workingrawfile.withoutlevels)[input$curcolnuminput],2] <<- input$priority
      prioritymat <<- tempprioritymat
      prioritymat[,1] <<- inittabulation[,4]
    })
    
    #Recoding
    #Do if action button is pressed
    observeEvent(input$dorecode, {
      
      #Update style of action button
      colormat <<- updatecolormatfun(colormat,input$curcolnuminput,"dorecode")
      
      #Get input
      tempdata    <- workingrawfile.withoutlevels
      curcol      <- input$curcolnuminput
      
      #Create recode matrix (currecoding)
      tempmvlogmat <- mvlogmat[mvlogmat[,1]==colnames(workingrawfile.withoutlevels)[curcol],2:9]
      if (is.null(dim(tempmvlogmat))) (tempmvlogmat <- matrix(tempmvlogmat,1,8))
      currecoding <- cbind(tabulation[tabulation[,4]==colnames(workingrawfile.withoutlevels)[curcol],c(4,2,2)],tempmvlogmat)
      curvaluenum <- length(currecoding[,1])
      
      #Get information for modal dialog
      curcolstattemp  <- tabulation[tabulation[,4]==colnames(workingrawfile.withoutlevels)[curcol],]
      curcolstat <- curcolstattemp[!is.na(curcolstattemp[,1]),]
      
      #Write new values in recode matrix
      for (i in 1:curvaluenum) 
      {
        for (j in 1:length(curcolstat[,1]))
        {
          if (!is.na(currecoding[i,2]) & !is.na(curcolstat[j,2]))
          {
            if (currecoding[i,2]==curcolstat[j,2]) 
            {
              temp <- paste0("value_",j)
              currecoding[i,3] <- c(input[[temp]])
            }
          }
        }
      }
      
      #replace "" in currecoding with NA
      for (i in 1:length(currecoding[1,]))
      {
        for (j in 1:length(currecoding[,1]))
        {
          if (!is.na(currecoding[j,i]))
          {
            if (currecoding[j,i]=="") (currecoding[j,i] <- NA)
          }
        }
      }
      
      #Will cells be merged?
      if (length(currecoding[!is.na(currecoding[,2]),1])>1) 
      {
        curmerging <- checkmergefun(currecoding[!is.na(currecoding[,2]),])
      } else {
        #If there is only one level to recode, no merging can happen.
        curmerging <- matrix(FALSE,1,1)
      }
        
      #If merging will happen, show warning
      if (curmerging[1]!=FALSE)
      {
        
        #Show merge warning
        showModal(modalDialog(
          title = 'WARNING',
          paste(curcolstat[curmerging[1],2],"(",curcolstat[curmerging[1],1],")","will be merged with",curcolstat[curmerging[2],2],"(",curcolstat[curmerging[2],1],")"),
          easyClose = FALSE,
          footer = NULL,
          size = "l",
          actionButton("continuerecoding","continue"),
          actionButton("escaperecoding","escape")))
          
          #If user decides to do recoding altough merging will happen
          observeEvent(input$continuerecoding, {
            
            #Present process status
            showModal(modalDialog(
              title = 'Recode!',
              "recoding...",
              easyClose = TRUE,
              footer = NULL))
            
            #Do recoding and update tabulation
            workingrawfile.withoutlevels <<- recodefun(workingrawfile.withoutlevels,currecoding)
            tabulation <<- overviewfun(workingrawfile.withoutlevels,workingrawfile.withlevels)
            
            #Present process status
            showModal(modalDialog(
              title = 'Recode!',
              paste("FINISHED - recoded variable",colnames(tempdata[curcol])),
              easyClose = TRUE,
              footer = NULL))
          })
          
          #If user decides to do no recoding
          observeEvent(input$escaperecoding, {
            showModal(modalDialog(
              title = 'Recode!',
              "Recoding canceled",
              easyClose = TRUE,
              footer = NULL))
          })
          
          #If no merging will happen at all
          } else {
            
            #Present process status
            showModal(modalDialog(
              title = 'Recode!',
              "recoding...",
              easyClose = TRUE,
              footer = NULL))
            
            #Do recoding and update tabulation
            workingrawfile.withoutlevels <<- recodefun(workingrawfile.withoutlevels,currecoding)
            tabulation <<- overviewfun(workingrawfile.withoutlevels,workingrawfile.withlevels)
            
            #Present process status
            showModal(modalDialog(
              title = 'Recode!',
              paste("FINISHED - recoded variable",colnames(tempdata[curcol])),
              easyClose = TRUE,
              footer = NULL))
          }
    })
    
    #Create UI for priority
    #This will create three radio buttons with labels "low", "mid" and "high"
    output$radiopriority <- renderUI({
      radioButtons("priority", label = h4("Priority"),choices = list("low" = 1, "mid" = 2, "high" = 3), selected = prioritymat[match(colnames(workingrawfile.withoutlevels)[input$curcolnuminput],prioritymat[,1]),2], inline = TRUE)
    })
    
    #Create UI for delete columns in the upload file tab
    #Gives two action buttons with header 'Trim data?'
    output$trimdata <<- renderUI({
      list(h4("Trim data?"),
           br(),
           actionButton('deletecolsingle',"Delete columns: uniform cells",style = colormat[1,6]),
           br(),br(),
           actionButton('deletecolunique',"Delete columns: unique cells ",style = colormat[1,7]),
           br(),br())
    })
    
    #Create UI for recodings
    #This will be updated when recoding is done, variable number is changed, dummy settings are changed, delete columns settings are changed or missing values are recoded.
    observeEvent({c(input$dorecode,input$curcolnuminput,input$yesdummy,input$nodummy,input$usenotforSQSD,input$useforSQSD,input$MV1_recode,input$MV2_recode,input$MV3_recode,input$MV4_recode,input$MVall_recode)},{
      output$recodenumericinput <- renderUI({
        
        #get default settings
        forsqsd[,1] <- inittabulation[,4]
        dummy[,1]   <- inittabulation[,4]
        
        #If variable will be deleted, return 'Variable will be deleted'
        if (forsqsd[match(colnames(workingrawfile.withlevels)[input$curcolnuminput],forsqsd[,1]),2]==0)
        {
          return(list(h3("Variable will be deleted!")))
        } else {
          #If variable will not be deleted, check wheather dummy will not be created
          if (dummy[match(colnames(workingrawfile.withlevels)[input$curcolnuminput],dummy[,1]),2]==0)
          {
            
            #Get current tabulation and additonal information for recoding
            tabulation      <<- overviewfun(workingrawfile.withoutlevels,workingrawfile.withlevels)
            curcol          <-  input$curcolnuminput
            curcolstattemp  <-  tabulation[tabulation[,4]==colnames(workingrawfile.withoutlevels)[curcol],]
            
            #Have all cells of the current variable the same entry?
            if (is.null(dim(curcolstattemp))) 
            {
              #If yes, return "All cells have the same entry!"
              return(list(h3("All cells have the same entry!")))
            } else {
              curcolstat <- curcolstattemp[!is.na(curcolstattemp[,1]),]
              if (is.null(dim(curcolstat))) 
              {
                return(list(h3("All cells have the same entry!")))
              } else {
                #If at least two cells differ, present recode options
                recodeinputlist <- lapply(1:(length(curcolstat[,1])), function(i) {
                  inputname <- paste0("value_",i)
                  if (is.na(curcolstat[i,2])) (tempnewvalue=NA) else (tempnewvalue=i)
                  numericInput(inputname, label = paste('recode ',curcolstat[i,2],' (',curcolstat[i,1],') to'), value = tempnewvalue)
                })
                return(list(recodeinputlist,uiOutput("recodevariables")))
              }
            }
          } else {
            # If dummy variables will be created, present "Dummy variables will be created!"
            return(list(h3("Dummy variables will be created!")))
          }
        }
      })
    })
  
    
    #Update and execute style of action buttons
    #The specified styles (object colormat) of all action buttons are implemented
    #Do if any action button is pressed
    observeEvent({c(input$curcolnuminput,input$MV1_recode,input$MV2_recode,input$MV3_recode,input$MV4_recode,input$deletecolsingle,input$deletecolunique,input$MVall_recode,input$useforSQSD,input$usenotforSQSD,input$dorecode,input$yesdummy,input$nodummy,input$applyall)} ,{
      output$MV_recode_token <<- renderUI(list(actionButton('MV1_recode','Recode!',style = colormat[input$curcolnuminput,2]), br(),br(),br(), 
                                              actionButton('MV2_recode','Recode!',style = colormat[input$curcolnuminput,3]), br(),br(),br(),
                                              actionButton('MV3_recode','Recode!',style = colormat[input$curcolnuminput,4]), br(),br(),br(), 
                                              actionButton('MV4_recode','Recode!',style = colormat[input$curcolnuminput,5])))
      output$mvinallcolumns  <<- renderUI(list(actionButton('MVall_recode','Recode missing values in all variables!',style = colormat[input$curcolnuminput,8])))
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
    
    #create UI for the manipulate variables tab
    #Do if data was successfully uploaded
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
              h4 ("Create dummy variables:"),
              uiOutput('createdummy'),
              br(), br(), br(),
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

    #------------------------------------------------------------
    #load logfile
    #------------------------------------------------------------
    
    #create UI for logfile tab
    
    output$logoutput <- renderUI({
      #Do if data was successfully uploaded
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
    
    #Do if logfile is uploaded
    output$log <- renderUI({
      
      #Reset styles of all action buttons. These will be updated when the logfile is apllied
      colormat <<- updatecolormatfun(colormat,-999,"applyall", undo = TRUE)
      
      #Logfile path and name
      inFileLog <- input$LOG
      
      #If no logfile was chosen, return NULL
      if (is.null(inFileLog))
        return(NULL)
      
      # read data with values
      uploadedLogfile <<- read.table(file = inFileLog$datapath, header = TRUE, sep=";",quote = '"/', fill = TRUE, colClasses = "character")
      
      #Create actionbutton that applies all changes from the logfile
      output$applylogfile <- renderUI({
        list(actionButton('applyall','Apply all changes from logfile', style = colormat[1,12]))
      })
      
      #Update style of action button when pressed 
      observeEvent(input$applyall, {
        output$applylogfile <- renderUI({
          list(actionButton('applyall','Apply all changes from logfile', style = colormat[1,12]))
        })  
      })
      
      #Create and present a summary of all changes that are saved in the logfile
      #Get those recodings that are not NAs and the old value is not the new value
      logfilesumrecoding <- uploadedLogfile[(!is.na(uploadedLogfile[,3]) & !is.na(uploadedLogfile[,4]) & uploadedLogfile[,3]!=uploadedLogfile[,4]),]
      #Get those recodings of missing values that are not NAs
      logfilesummv       <- uploadedLogfile[(!is.na(uploadedLogfile[,5]) | !is.na(uploadedLogfile[,6]) | !is.na(uploadedLogfile[,7]) | !is.na(uploadedLogfile[,8]) | !is.na(uploadedLogfile[,9]) | !is.na(uploadedLogfile[,10]) | !is.na(uploadedLogfile[,11]) | !is.na(uploadedLogfile[,12])),]
      
      #Create list of changes from logfile for recodings and missing values
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
      
      #If no changes are saved in the logfile, present 'No recodings saved in logfile!'
      if (dim(logfilesumrecoding)[1]==0)
      {
        logfilesummary <- list(HTML("<br> No recodings saved in logfile! <br/>"))
      } else {
        #If changes are saved in the logfile, create list with changes
        logfilesummary <- lapply(1:length(logfilesumrecoding[,1]), function(i) {
          HTML(paste0("<br> Recode ",logfilesumrecoding[i,1],": ", logfilesumrecoding[i,3]," (",logfilesumrecoding[i,2],") to ", logfilesumrecoding[i,4],"<br/>"))
        })
      }
      
      #Which variables will be deleted?
      logfilevartoremain <- HTML(paste0("<br> Delete variable(s): ",unique(uploadedLogfile[uploadedLogfile$Remain==0,1]),"<br/>"))
      
      #Which variables will be dummy coded?
      logfiledummy <- HTML(paste0("<br> Create dummy: ",unique(uploadedLogfile[uploadedLogfile$Create.Dummy==1,1]),"<br/>"))
      
      #Return list of all changes saved in logfile
      return(list(logfilesummary,logfilemvsummary,logfilevartoremain,logfiledummy))
    })
    
    #Apply all changes from logfile
    #Do if action button 'applyall' is pressed
    observeEvent(input$applyall, {
      
      #Present process status
      showModal(modalDialog(
        title = 'Apply all changes from logfile',
        "recoding...",
        easyClose = TRUE,
        footer = NULL))
      
      #Get current styles of action button, logfile and data
      tempcolormat <<- colormat
      templogfile  <<- uploadedLogfile
      tempworkingrawfile.withoutlevels <<- workingrawfile.withoutlevels
      
      #Change style of action button 'applyall'
      colnames(tempcolormat) <<- c("variable","MV1_recode","MV2_recode","MV3_recode","MV4_recode","deletecolsingle","deletecolunique","mvinallcolumns","useforSQSD","usenotforSQSD","dorecode","applyall","yesdummy","nodummy")
      tempcolormat <<- updatecolormatfun(tempcolormat,-999,"applyall")
      
      #use only those variables from logfile that exist in data
      for (var in unique(templogfile[,1]))
      {
        if (!(var %in% colnames(workingrawfile.withoutlevels)))
        {
          templogfile <- templogfile[templogfile[,1]!=var,]
        }
      }
      
      #Do recoding and update tabulation
      tempworkingrawfile.withoutlevels <<- recodefun(tempworkingrawfile.withoutlevels, cbind(uploadedLogfile[,1],uploadedLogfile[,3:12]))
      tabulation <<- overviewfun(workingrawfile.withoutlevels,workingrawfile.withlevels)
      
      #Mark variables that will be deleted. Variables will be deleted when data will be downloaded.
      #Additionally, update styles of action buttons for manipulation tab. This gives user information on current deletion settings
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
      #Save deletion settings in global environment
      forsqsd <<- tempforsqsd
      
      #Mark variables that will be dummy coded. Variables will be dummy coded when data will be downloaded.
      #Additionally, update styles of action buttons for manipulation tab. This gives user information on current deletion settings
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
      #Save dummy settings in global environment
      dummy <<- tempdummy
      
      #Get information of priority
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
      #Save priority settings in global environment
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
      
      #save objects in global environment
      colormat   <<- tempcolormat
      tabulation <<- overviewfun(workingrawfile.withoutlevels,workingrawfile.withlevels)
      workingrawfile.withoutlevels <<- tempworkingrawfile.withoutlevels
      
      #Present process status
      showModal(modalDialog(
        title = 'Apply all changes from logfile',
        "FINISHED",
        easyClose = TRUE,
        footer = NULL))
    })
    
    #End of output$rawfile <- DT::renderDataTable({
    #Function gives back workingrawfile.withoutlevels
    workingrawfile.withoutlevels
  })
  
  
  #------------------------------------------------------------
  #Downloads
  #------------------------------------------------------------
  
  #download manual
  output$downloadManual <- downloadHandler(
    filename = "Manual Data Input Preparation Tool.pdf",
    content = function(file) {
      file.copy(from = paste0(path2app,"www/Manual Data Input Preparation Tool.pdf"), to = file, overwrite = TRUE)
    },
    contentType = "pdf"
  )
  
  #download example data
  output$downloadExampleData <- downloadHandler(
    filename = "Example",
    content = function(file) {
      file.copy(from = paste0(path2app,"www/example_data.sav"), to = file, overwrite = TRUE)
    },
    contentType = ".sav"
  )
  
  #download example logfile
  output$downloadExampleLogfile <- downloadHandler(
    filename = "Example",
    content = function(file) {
      file.copy(from = paste0(path2app,"www/example_logfile"), to = file, overwrite = TRUE)
    },
    contentType = ".csv"
  )
  
  #download manipulated dataset
  #Single variable deletion and dummy coding will be executed here
  output$downloadData <- downloadHandler(
    filename <- function() {
      paste("reviewed_", Sys.Date(),"_",".csv", sep="")
    },
    content <- function(file) {
      
      #Create data for download
      #Delete single variables
      workingrawfile.withoutlevels <- deletenotusedcolfun(workingrawfile.withoutlevels,forsqsd)
      
      #Do dummy coding
      dummy[,1] <- inittabulation[,4]
      for(tempcolumn in colnames(workingrawfile.withoutlevels))
      {
        if (dummy[match(tempcolumn,dummy[,1]),2]==1) (workingrawfile.withoutlevels <- create_sel_dummies(workingrawfile.withoutlevels, list(tempcolumn), exclude = TRUE, processNA = TRUE, concat = TRUE))
      }
      
      #Delete old ID-variable and append initial ID-variable
      if(IDvariable.name %in% colnames(workingrawfile.withoutlevels)) 
      {
        workingrawfile.withoutlevels <- workingrawfile.withoutlevels[,-(match(IDvariable.name,colnames(workingrawfile.withoutlevels)))]
      }
      if(IDvariable.name!="no ID variable") (workingrawfile.withoutlevels <- cbind(IDvariable,workingrawfile.withoutlevels))
      
      #Save data
      return(write.csv(workingrawfile.withoutlevels, file, sep=""))
    }
  )
  
  #download new logfile
  output$downloadlogfile <- downloadHandler(
    filename = function() {paste0(input$SAV,'_logfile_',Sys.Date(),'.csv')},
    content = function(file) {(
      #Create logfile by adding columns with the required information
      write.table(
      cbind(inittabulation[,4],inittabulation[,1],inittabulation[,2],tabulation[,2],mvlogmat[,2:9],forsqsd[,2],dummy[,2],prioritymat[,2]),
      col.names = c("variable", "level", "oldvalue", "newvalue", "MV1_old", "MV1_new","MV2_old", "MV2_new","MV3_old", "MV3_new","MV4_old", "MV4_new","Remain","Create Dummy","Priority"),
      row.names = FALSE,
      sep = ";",
      eol = "\r\n",
      file))}
  )
}