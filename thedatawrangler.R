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
                   selectInput("group",
                               "Select variable for grouping",
                               choices = c("A categorial variable")),
                   actionButton("update_group", "Perform grouping",
                                class = "btn btn-primary btn-sm action-button")
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
    miniTabPanel("Preview", icon = icon("table")
                 # Content Tab 3
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$done, {
    
    stopApp()
  })
}

# We'll use a pane viwer, and set the minimum height at
# 300px to ensure we get enough screen space to display the clock.
viewer <- dialogViewer(dialogName = "The Data Wrangler", width = 600, height = 600)
runGadget(ui, server, viewer = viewer)


