ui_monteith <- fluidPage(
  useShinyjs(),
  div(
    id = "loader_background_pm"
  ),
  div(
    id = "loader_pm",
    icon("wheat-awn", class="fas", style = "font-size: 125px;"),
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
          vapor near the evaporating surface. Penman did not, however, have much theoretical basis for this equation. 
          It did not include an aerodynamic resistance function (to quantify boundary layer resistance), and it did
          not include surface resistance to vapor transfer (to account for stomatal resistance). Several investigators have
          proposed equations to remedy these omissions, but the one most often cited is that of Monteith (1981)."
        ),
        HTML(katex_html(
          "ET_{0} = \\frac{0.408\\Delta(R_{n} - G) + \\gamma(\\frac{900}{T + 273})u_{2}(e_{s} - e_{a})}{\\Delta + \\gamma(1 + 0.34u_{2})}",
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
    uiOutput("set_monthly_data_pm"),
    uiOutput("plot_box_pm"),
    uiOutput("set_arguments_pm")
  ),
  
)
