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
    fluidPage(
      useShinyjs(),
      column(
        width = 7,
        uiOutput("set_temperatures"),
        uiOutput("plot_box"),
        uiOutput("set_arguments")
      ),
      column(
        width = 5,
        box(
          width = NULL, title = "About", class = "custom-box about",
          div(
            p(
              "Penman (1948) first combined factors to account for a supply of energy and a mechanism to remove the water
              vapor near the evaporating surface. We should recognize these two factors as the essential ingredients for evapo-
              ration. Penman derived an equation for a well-watered grass reference crop:"
            ),
            HTML(katex_html(
              "ET_{0} = \\frac{\\Delta}{\\Delta + \\gamma}\\left(R_n - G\\right) + \\frac{\\left(\\frac{\\gamma}{\\Delta + \\gamma}\\right)6.43(1.0 + 0.53u_2)(e_s - e_d)}{\\lambda}",
              displayMode = TRUE, 
              preview = FALSE,
              include_css = TRUE,
              output = "html"
            )
            )
          ),
          uiOutput("plot_by")
          
        )
      )
    )
  )
)