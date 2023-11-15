library(shiny)
library(shinythemes)
library(shinydashboard)

# Define UI for application
ui <- navbarPage(
  "Equations in Ecohydrology",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
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
        box(
          width = NULL, title = "Graph", class = "custom-box", height = 500,
          p("hello")
        )
        
      )
    )
  )
)