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
        width = 8,
        uiOutput("set_arguments")
      ),
      column(
        width = 4,
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
          HTML(katex_html(
            "\\lambda{ET} = \\frac{ \\Delta (R_n - G) + \\rho_a c_p \\frac{(e_s - e_a)}{r_a} }{\\Delta + \\gamma(1 + \\frac{r_s}{r_a})}",
            displayMode = TRUE, 
            preview = FALSE,
            include_css = TRUE,
            output = "html")
          ),
          uiOutput("plot_by")
          
        )
      )
    )
  )
)