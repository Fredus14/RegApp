# Load libraries
library(shiny)


shinyUI(
    pageWithSidebar(
        headerPanel("Regression Model Analyser"),
        
        # Select input dataset
        sidebarPanel(
           # User selects dataset from drop down menu. 
           selectInput(
                inputId = "dataset", label = "Select Dataset", 
                choices = c("airquality", "mtcars", "father.son", 
                            "faithful", "Your own data set")
                ),
           
           conditionalPanel("input.dataset == 'Your own data set'",
                            fileInput(inputId = "uploadFile", 
                            label = "Select a csv file to upload")
                            ),  
                      
                
            # Select menu for independent variable, available once
            # the user has selected a data set
            uiOutput(outputId = "yVarSelector"),
            # Checkbox group for independent variables, available once
            # the user has selected a valid data.frame, and includes
            # all variables in the data.frame EXCEPT the dependent variable.
            uiOutput(outputId = "xVarSelector"),
            
            # Instructions for th user
            h5("Application instructions:"),
            helpText("1. Select a dataset",br(),  
                     "2. Analyse the 'Correlation Matrix Plot'.",br(),
                     "3. Select the outcome variable.",br(),
                     "4. Select the regressors among remaining variables.",br(),
                     "5. The 'Model Summary' tab displays model details",br(),
                     "6. The 'Diagnostic Plots' tab provies residuals analysis"), 
        
           h5("Obs.: "),
           helpText("Help me support The Royal Brompton Hospital, London.",
                    "Please check out this link to our fundraising page."),
           
           div(
                   # Include fundraising link.
                   includeHTML("fundraising.html")
           ),
#            helpText(   a("Support The Royal Brompton Hospital, London",     href='http://www.justgiving.com/FredericoAmaral', target="_blank")
#            ),


           
            width = 4
        ),
        
        # Regression output goes here
        mainPanel(
            # Reminder of the model being evaluated.
            p(htmlOutput(outputId = "regHeader")),
            # Include an HTML horizontal line above tab panel.
            tabsetPanel(
                tabPanel("Correlation Matrix Plot",
                plotOutput(outputId = "regCorPlot")),    
                
                
                tabPanel("Model Summary",
                    verbatimTextOutput(outputId = "regSummary")),
                
                tabPanel("Diagnostic Plots",
                    selectInput(
                        inputId = "regPlotType",
                        label   = "Select diagnostic plot",
                        choices = list(
                            "Residuals vs Fitted"   = 1,
                            "Normal Q-Q"            = 2,
                            "Scale-Location"        = 3,
                            "Cook's Distance"       = 4,
                            "Residuals vs Leverage" = 5,
                            "Cook's Dist vs Leverage" = 6)
                    ),
                    plotOutput(outputId = "regPlot")),
                
                tabPanel("Data",
                         dataTableOutput("dataTable"))
            ),
            width = 8   
        )
    )
)
