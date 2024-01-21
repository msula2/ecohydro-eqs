library(shiny)
library(shinythemes)
library(katex)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(dplyr)
library(kableExtra)
library(knitr)
library(shinyjs)
library(DT)
library(shinyWidgets)
source("ui/ui_penman.R")
source("ui/ui_vapor.R")
source("ui/ui_open.R")
source("ui/ui_criddle.R")
source("ui/ui_pan.R")
# Define UI for application
ui <- navbarPage(
  "Evapotranspiration",
  id = "navbar",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "fontawesome/all.min.css"),
    tags$link(rel="stylesheet", type="text/css" , href="katex/katex.min.css"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jquery.inputmask/5.0.6/jquery.inputmask.min.js"),
    tags$script(src="katex/katex.min.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style("body{background-color: rgba(139, 69, 19, 0.4);}
               .about .katex { font: normal 1.25vw KaTeX_Main, Times New Roman, serif !important; }
               #results_tab a{color: #2c3e50;}
               .fa-droplet{color: #89CFF0;}
               .navbar-inverse .navbar-brand:hover{color: white;}
               .shiny-notification {position:fixed;top: 0;right: 0;}
               .fa-calculator, .fa-circle-question {transition all 1s;}
               .fa-calculator:hover, .fa-circle-question:hover{transform: scale(1.2);}
               p {line-height: 1.7;}")
  ),
  collapsible = TRUE,
  inverse = TRUE,
  theme = shinytheme("flatly"),
  tabPanel(
    "Penman's Method",
    icon = icon("droplet"),
    ui_penman
  ),
  tabPanel(
    "Vapor Pressure Deficit",
    icon = icon("sun", class="fas"),
    ui_vapor
  ),
  tabPanel(
    "Meyer's Open Water Evaporation",
    icon = icon("water", class="fas"),
    ui_open
  ),
  tabPanel(
    "Blaney Criddle Method",
    icon = icon("seedling", class="fas"),
    ui_criddle
  ),
  tabPanel(
    "Pan Evaporation",
    icon = icon("drum-steelpan", class="fas"),
    ui_pan
  )
)

# server.R

# Source the server functions for each tab
source("server/server_penman.R")
source("server/server_open.R")
source("server/server_vapor.R")
source("server/server_criddle.R")
source("server/server_pan.R")

# Define the main server function
server <- function(input, output, session) {
  
  observe({
    # Get the currently selected tab
    current_tab <- input$navbar
    
    # Call the corresponding server function based on the selected tab
    if (current_tab == "Penman's Method") {
      penman_server(input, output, session)
    } else if (current_tab == "Meyer's Open Water Evaporation") {
      open_server(input, output, session)
    } else if (current_tab == "Vapor Pressure Deficit") {
      vapor_server(input, output, session)
    } else if (current_tab == "Blaney Criddle Method"){
      criddle_server(input, output, session)
    }
    else if (current_tab == "Pan Evaporation"){
      pan_server(input, output, session)
    }
  })
}

shinyApp(ui, server)

