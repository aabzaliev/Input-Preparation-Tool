library(shiny)

navbarPage("",
          
          tabPanel(
             "Start",
             column(10, h2("Data Input Preparation Tool")),
             column(2,img(src="GfK_logo_online.png",height=100,width=100, allign = "right")),
             downloadButton("downloadManual","Download Manual"),
             br(),br(),
             img(src="Figure_Workflow.jpg",height=422,width=682, allign = "center")
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
          )
      )