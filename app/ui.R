library(shiny)
library(shinythemes)
library(katex)
library(shinydashboard)
# Define UI for application
ui <- navbarPage(
  "Equations in Ecohydrology",
  id = "navbar",
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel="stylesheet", href="https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.css", integrity="sha384-9tPv11A+glH/on/wEu99NVwDPwkMQESOocs/ZGXPoIiLE8MU/qkqUcZ3zzL+6DuH", crossorigin="anonymous"),
    tags$script(src="https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.js", integrity="sha384-U8Vrjwb8fuHMt6ewaCy8uqeUXv4oitYACKdB0VziCerzt011iQ/0TqlSlv8MReCm", crossorigin="anonymous"),
    tags$style("body{background-color: rgba(139, 69, 19, 0.4);}")
  ),
  collapsible = TRUE,
  inverse = TRUE,
  theme = shinytheme("flatly"),
  tabPanel(
    "Penman-Monteith",
    icon = icon("droplet"),
    fluidPage(
      column(
        width = 7,
        uiOutput("set_arguments")
      ),
      column(
        width = 5,
        box(
          width = NULL, title = "About", class = "custom-box",
          p(
            "The Penman-Monteith equation is a widely used method for calculating 
              the evapotranspiration (ET) of water from the land surface into the atmosphere."
          ),
          p(
            " Evapotranspiration is the combined process of evaporation, which is the loss 
              of water from the land surface directly into the atmosphere, and transpiration, 
              which is the release of water vapor from plants through their leaves."
          ),
          div(
              HTML(katex_html(
              "ET_{0} = \\frac{\\Delta}{\\Delta + \\gamma}\\left(R_n - G\\right) + \\frac{\\left(\\frac{\\gamma}{\\Delta + \\gamma}\\right)6.43(1.0 + 0.53u_2)(e_s - e_d)}{\\lambda}",
              displayMode = TRUE, 
              preview = FALSE,
              include_css = TRUE,
              output = "html")
            )
          ),
          uiOutput("plot_by")
          
        )
      )
    )
  )
)