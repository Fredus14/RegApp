# Load libraries.
library(shiny)
library(datasets)
library(UsingR)
library(PerformanceAnalytics)

shinyServer(
    # Function takes input from ui.R and returns output objects.
    function(input, output) {
        
        # Returns the data set name chosen in the drop down menu, updates
        # every time that a new data set is chosen.
        getdataset <- reactive({
            input$dataset
        })
        
        # Returns data.frame selected by the user or uploaded
        getData <- reactive({
                
                if (getdataset() != "Your own data set") {
                        out <- tryCatch(
                                get(getdataset()), 
                                error = function(e) return(NULL))
                }else{
                        uploadFilePath <- input$uploadFile$datapath
                        
                        if(is.null(input$uploadFile)) out <- NULL
                        else {
                        dataFile <- read.csv(uploadFilePath, 
                                             header=TRUE,
                                             stringsAsFactors=FALSE) 
                        out <- dataFile}
                        }

                return(out)
                })       

        
        # Returns the colnames() of the data.frame named in getdataset(),
        # if it's a valid data.frame, otherwise returns NULL.
        dataVarNames <- reactive({
            dataFrame <- getData()
            if (class(dataFrame) == "data.frame") out <- colnames(dataFrame)
            else out <- NULL
            return(out)
        })
        
        # If dataVarNames() returns valid column names, then output
        # a select input where the user can select a dependent variable.
        output$yVarSelector <- renderUI({
            varNames <- dataVarNames()
            if (is.null(varNames)) varNames <- "N/A"
        
            out <- selectInput(
                inputId = "yVarSelector",
                label   = "Select dependent (y) variable",
                choices = as.list(varNames)
            )
            return(out)
        })
        
        # Assuming the data.frame name entered is valid, return the current
        # selected value for the dependent variable.
        selectedYvar <- reactive({
            input$yVarSelector
        })
        
        # If dataVarNames() returns valid column names, then output
        # a group of checkboxes where the user can select one or more
        # independent variables.
        output$xVarSelector <- renderUI({
            varNames <- dataVarNames()
            if (is.null(varNames)) xVars <- "N/A"
            else {
                yVar     <- selectedYvar()
                # Exclude the currently selected y-variable from
                # the list of choices for x-variables.n
                xVars    <- setdiff(varNames, yVar)
            }
            
            out <- checkboxGroupInput(
                inputId = "xVarSelector",
                label   = "Select independent (x) variable(s)",
                choices = as.list(xVars)
            )
            return(out)
        })

        # Assuming the data.frame name entered is valid, return the currently
        # checked independent variables.
        checkedXvars <- reactive({
            input$xVarSelector
        })
        
        # Read input values of y- and x-variables, and if they're both
        # valid, return a basic regression formula object of the form
        # yVar ~ xVar1 + xVar2 + xVar3 ...
        # Otherwise return NULL.
        regFormulaText <- reactive({                
            yVar  <- selectedYvar()
            xVars <- checkedXvars()
            if (yVar != "N/A" && length(xVars) > 0) {
                out <- sprintf("%s ~ %s", 
                        yVar,
                        paste(xVars, collapse=" + ")
                    )
            } else out <- NULL
            return(out)
        })
        
        # If regFormulaText() returns a valid formula (although
        # as a character string, return a header to show what's being
        # modeled, otherwise return a header saying there's nothing to
        # be modeled yet.
        regHeader <- reactive({
            if (class(regFormulaText()) == "character") {
                out <- HTML(sprintf("Modeling <strong>%s</strong> in data.frame <strong>%s</strong>", 
                    regFormulaText(), 
                    getdataset()))
            } else out <- HTML("Nothing to model yet.")
            return(out)
        })
        
        # If regFormulaText() returns a valid formula, run lm() using that 
        # formula and getData() as the data.frame, otherwise return NULL.
        regObject <- reactive({
            if (class(regFormulaText()) == "character") {
                dataFrame <- getData()
                out <- lm(regFormulaText(), data = dataFrame)
            } else out <- NULL
            return(out)
        })
        
        # If regObject() returns a valid lm() object, return the summary
        # of it, otherwise return NULL.
        regSummary <- reactive({
            if (class(regObject()) == "lm") out <- summary(regObject())
            else out <- "Select the variables to be modelled."
            return(out)
        })
        
        # Update the plot type to show based on the select input on the
        # Diagnostic Plots tab
        regPlotType <- reactive({
            as.integer(input$regPlotType)
        })
        
        # If regObject() returns a valid lm() object, return the plot
        # given by the current value of the plot selection input.
        regPlot <- reactive({
            if (class(regObject()) == "lm") out <- plot(regObject(), 
                which = regPlotType())
            else out <- NULL
            return(out)
        })
        
        # Correlation Matrix Plot
        regCorPlot <- reactive({
           if (is.null(getData())) out <- NULL
           else out <- chart.Correlation(getData(), 
                                    las = 1,
                                    cex.axis = 1.2, 
                                    mgp = c(0.3, 0.6, 0.3), 
                                    col = "blue", 
                                    hist.col="cyan")     
           return(out)     
        })
        

        #####
        #
        # Output objects
        #
        #####
        
        # If regFormula() returns a valid formula, return a text string
        # showing the formula modeled above the regression summary area,
        # otherwise return a string saying there's nothing to model yet.
        output$regHeader <- renderText({
            regHeader()
        })
        
        # If regObject() returns a valid lm() object, return its summary, 
        # otherwise return NULL.
        output$regSummary <- renderPrint({
            regSummary()
        })
        
        
        # If regObject returns a valid lm() object, return its plot,
        # otherwise return NULL.
        output$regPlot <- renderPlot({
            regPlot()
        })
        
        # Correlation Matrix plot Output
        output$regCorPlot <- renderPlot({
                regCorPlot()
        }, height = 570)
        
        ## Data Table ####################
        
        output$dataTable <- renderDataTable(getData(), options = list(
                pageLength = 10))
        
    }
)
