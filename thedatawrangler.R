#------------------------------------------------------------------#
# R-Projekt: Interactive Piping with an RStudio Add-In (PIRA)
# Authors: Stanislaus Stadlmann
# Module: Statistical Programming with R (University of Göttingen)
#------------------------------------------------------------------#

#.............................
# PRELIMINARIES
#.............................
library(rstudioapi)
library(dplyr)
library(miniUI)
library(shiny)

#.............................
# HELPER FUNCTIONS
#.............................

# Function that scans the working space for dataframes
search_df <- function() {
  # Container
  c <- c()
  
  # Function to tell which place an object has in the workspace 
  w <- function(x) {
    ls <- ls(envir = .GlobalEnv)
    return(which(ls == x))
  }
  
  # Which object is a dataframe?
  for (data in ls(envir = .GlobalEnv)) {
    if (any(class(eval(parse(text = data))) == "data.frame")) {
      c[w(data)] <- data
    }
  }
  
  # Return all non-NA values
  return(c[!is.na(c)])
  
  # Delete the rest
  rm(w)
  rm(c)
}

ui <- miniPage(
  gadgetTitleBar("The Data Wrangler", left = miniTitleBarButton("reset", "Reset")),
  miniTabstripPanel(
    miniTabPanel("Data", icon = icon("folder-open"),
                 miniContentPanel(
                       selectInput(label = "Select your dataset:",
                                   inputId = "dataset",
                                   choices = c("", search_df())),
                       actionButton("update_df", "Update dataframe",
                                    class = "btn btn-primary btn-sm action-button",
                                    style = "float:left"),
                       br(),
                       br(),
                       br(),
                       h5("Perform grouping after selecting dataframe"),
                       uiOutput("variables"),
                       actionButton("update_group", "Perform grouping",
                                    class = "btn btn-primary btn-sm action-button",
                                    style = "float:left")
                 )
    ),
    miniTabPanel("Reshape", icon = icon("exchange")
                 # Content Tab 2
    ),
    miniTabPanel("Subset", icon = icon("scissors")
                 # Content Tab 3
    ),
    miniTabPanel("Summarise", icon = icon("compress")
                 # Content Tab 4
    ),
    miniTabPanel("Preview", icon = icon("table"),
                 DT::dataTableOutput("previewtable")
                 #textOutput("tableprint")
    )
  )
)

server <- function(input, output, session) {
  
  # Start with iris DF
  values <- reactiveValues()
  
  # If df button is pressed update dataframe
  observeEvent(input$update_df, {
    values$preview <- tbl_df(get(input$dataset))
    cat("Dataset button was pressed\n")
  })
  
  # If group_by button is pressed group and update dataframe
  observeEvent(input$update_group, {
    values$preview <- group_by_(values$preview, input$group_variable)
    cat("Group_by button was pressed \n")
  })
  
  # Render the UI button with variable selections, if dataset is selected
  output$variables <- renderUI({
    col.names <- colnames(values$preview)
    selectInput(inputId = "group_variable",
                label = "Select your variable",
                choices = col.names)
  })
  
  # Render Preview table
  output$previewtable <-  DT::renderDataTable({
    as.data.frame(values$preview)
    }, options = list(autoWidth = FALSE, 
                      paging = TRUE,
                      searching = FALSE,
                      info = TRUE,
                      ordering = FALSE,
                      processing = FALSE,
                      scrollX = TRUE),
    class = "cell-border stripe")
  
  # Do this when pressing "Close"
  observeEvent(input$done, {
    stopApp()
  })
}

# We'll use a pane viwer, and set the minimum height at
# 300px to ensure we get enough screen space to display the clock.
viewer <- dialogViewer(dialogName = "The Data Wrangler", width = 700, height = 700)
runGadget(ui, server, viewer = viewer)


