library(shiny)

navbarPage("",
          
          tabPanel(
             "Start",
             column(10, h2("Data Input Preparation Tool")),
             column(2,img(src="GfK_logo_online.png",height=100,width=100, allign = "right")),
             downloadButton("downloadManual","Download Manual"),
             downloadButton("downloadExampleData","Download Example Data"),
             downloadButton("downloadExampleLogfile","Download Example Logfile"),
             br(),br(),
             img(src="Figure_Workflow.png",height=422,width=682, allign = "center")
          ),
          
          tabPanel(
            "Upload File",
            titlePanel("Select sav-File"),
            sidebarLayout(
              sidebarPanel(
                fileInput('SAV', 'Choose SPSS File', accept=c('.sav')),
                downloadButton('downloadData', 'Download'),
                br(),br(),
                uiOutput("trimdata")
              ),
              mainPanel(
                wellPanel(DT::dataTableOutput("rawfile"))
              )
            )
          ),
          
          tabPanel(
            "Manipulate Variables",
            fluidRow(
              uiOutput("manipulatevariables")
              )
            ),
          
          tabPanel(
            "Logfile",
            fluidRow(
              uiOutput("logoutput")
            )
          ) # Insert comma here when adding tabs
          
          
          #-----------------------------------------------------------------------------
          #Add tab
          #-----------------------------------------------------------------------------
          #In order to add a tab, please delete the following hashtags and insert your
          #UI code in the denoted column. 
          #Note that a additional comma must be placed right after the previous tabPanel
          #object.
          #For further information on this app, see the extension manual.
          #For further information on the UI of Shiny, see shiny.rstudio.com/
          #-----------------------------------------------------------------------------
          
          #tabPanel(
          #  "tab name",
          #  fluidRow(
          #    # >> insert your code here <<
          #  )
          #)
          
      )