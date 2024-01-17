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
          )
        )
        
      ),
      column(
        width = 7,
        uiOutput("set_temperatures"),
        uiOutput("plot_box"),
        uiOutput("set_arguments")
      ),
      
    )
  ),
  tabPanel(
    "Vapor Pressure Deficit",
    icon = icon("sun", class="fas"),
    fluidPage(
      useShinyjs(),
      column(
        width = 12,
        box(
          width = NULL, title = "About", class = "custom-box about",
          div(
            p(
              "The vapor pressure deficit (Jensen 1990) can be calculated using several methods, here
              we will explore the method which subtracts the saturation vapor pressure at mean temperature 
              from saturation vapor pressure at dewpoint temperature. This can be written as:"
            ),
            div(
              style = "display: flex; align-items: center; justify-content: center;",
              HTML(paste0(
                katex_html(
                  "(e_{s} - e_{d}) = e_{s(T_{avg})} - e_{s(T_{d})}",
                  displayMode = TRUE, 
                  preview = FALSE,
                  include_css = TRUE,
                  output = "html"
                ),
                img(src = "img/circle-1.png", alt = "Equation 1", style = "margin-left: 10px;")
              ))),
            p(
              HTML(paste(
                "To compute saturation vapor pressure ",
                katex_html("e_{s}", displayMode = FALSE),
                ", in kilopascal if temperature ",
                katex_html("T", displayMode = FALSE),
                " is in degrees Celsius, we can use the following
                equation which is valid for temperatures ranging 
                from 0°C to 50°C: "
              ))
            ),
            div(
              style = "display: flex; align-items: center; justify-content: center;",
              HTML(paste0(
                katex_html(
                  "e_{s} = exp^{\\frac{16.78T - 116.9}{T + 237.3}}",
                  displayMode = TRUE, 
                  preview = FALSE,
                  include_css = TRUE,
                  output = "html"
                ),
                img(src = "img/circle-2.png", alt = "Equation 2", style = "margin-left: 10px;")
              ))),
            p(
              HTML(paste(
                "Actual vapor pressure,",
                katex_html("e_{d}", displayMode = FALSE),
                ", is the vapor pressure of the air. Unlike saturation vapor pressure, 
                actual vapor pressure cannot be determined simply by knowing the temperature of the air.
                To determine ",
                katex_html("e_{d}", displayMode = FALSE),
                ", we need to know the air temperature and either the relative humidity or the 
                dewpoint temperature of the air. The following equation can be used to find
                actual vapor pressure: "
              ))
              
            ),
            div(
              style = "display: flex; align-items: center; justify-content: center;",
              HTML(paste0(
                katex_html(
                  "e_{d} = e_{s}\\frac{RH}{100}",
                  displayMode = TRUE, 
                  preview = FALSE,
                  include_css = TRUE,
                  output = "html"
                ),
                img(src = "img/circle-3.png", alt = "Equation 3", style = "margin-left: 10px;")
              ))),
            p(
              HTML(paste(
                "where ",
                katex_html("e_{d}", displayMode = FALSE),
                " is the actual vapor pressure, ",
                katex_html("e_{s}", displayMode = FALSE),
                " is the saturation vapor pressure, and ",
                katex_html("RH", displayMode = FALSE),
                " is the relative humidity in percentage"
              ))
            ),
            div(
              style = "display: flex; justify-content: center;",
              div(
                class = "choices_box",
                radioButtons("vpd_calc_choice", label = "",
                             choiceNames = c("Enter arguments", "Enter tabular data"),
                             choiceValues = c("arguments", "table"),
                             selected = character(0), inline = TRUE)
              )
              
            )
            
            
          ),
          uiOutput("vpd_results")
        )
        
      )
      
    )
  ),
  tabPanel(
    "Meyer's Open Water Evaporation",
    icon = icon("water", class="fas"),
    fluidPage(
      useShinyjs(),
      column(
        width = 5,
        box(
          width = NULL, title = "About", class = "custom-box about",
          div(
            p(
              "Monthly evaporation from lakes or reservoirs can be computed using the empirical formula developed by Meyer
              (1915), but based on Dalton’s law using the vapor deficit (1802) (Harrold et al. 1986)."
            ),
            HTML(katex_html(
              "E = C(e_{s} - e_{d})(1 + \\frac{u_{25}}{10})",
              displayMode = TRUE, 
              preview = FALSE,
              include_css = TRUE,
              output = "html"
            )
            ),
            p(
              HTML(paste(
                "where ",
                katex_html("E", displayMode = FALSE),
                "is the evaporation in inches/month,",
                katex_html("e_{s}", displayMode = FALSE),
                "is the saturation vapor pressure (inches of mercury) of air at the water
                temperature at 1 ft deep,",
                katex_html("e_{d}", displayMode = FALSE),
                "is the actual vapor pressure (inches of mercury) of air calculated using:  "
              ))
            ),
            HTML(katex_html(
              "e_{d} = e_{s_{(airT)}} \\cdot RH",
              displayMode = TRUE, 
              preview = FALSE,
              include_css = TRUE,
              output = "html"
            )
            ),
            p(
              HTML(
                paste(
                  katex_html("u_{25}", displayMode = FALSE),
                  "is the average wind velocity (miles/hour) at a height of 25 ft above the 
                  lake or surrounding land areas, and",
                  katex_html("C", displayMode = FALSE),
                  " is the coefficient that equals 11 ft for small lakes and reservoirs and 15 ft for shallow ponds."
                )
              )
              
            )
          )
        )
        
      ),
      column(
        width = 7,
        uiOutput("plot_box_open"),
        uiOutput("set_arguments_open")
        
      )
      
    )
  )
  
  
)