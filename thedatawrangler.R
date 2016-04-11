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

ui <- miniPage(
  gadgetTitleBar("The Data Wrangler"),
  miniTabstripPanel(
    miniTabPanel("Input", icon = icon("folder-open")
      # Content Tab 1
    ),
    miniTabPanel("Preview", icon = icon("table")
                 # Content Tab 1
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


