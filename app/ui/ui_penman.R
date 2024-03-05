ui_penman <-   fluidPage(
  useShinyjs(),
  div(
    id = "loader_background_p"
  ),
  div(
    id = "loader_p",
    icon("droplet", class="fas", style = "font-size: 125px;"),
    p(
      "Loading . . .",
      style = "padding-top: 5px;"
    )
    
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
