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
source("ui_penman.R")
source("ui_vapor.R")
source("ui_open.R")
source("ui_criddle.R")
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
  )
  
  
)
