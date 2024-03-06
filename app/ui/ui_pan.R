ui_pan <- fluidPage(
  useShinyjs(),
  div(
    id = "loader_background_pn",
    style="display: block;"
  ),
  div(
    id = "loader_pn",
    icon("drum-steelpan", class="fas", style = "font-size: 125px;"),
    p(
      "Loading . . .",
      style = "padding-top: 5px;"
    ),
    style="display: block;"
    
  ),
  column(
    width = 5,
    box(
      width = NULL, title = "About", class = "custom-box about",
      div(
        p(
          "One of the oldest and simplest ways to measure evaporation is with a pan. This involves placing a pan of water out-
          side and recording how much water evaporates during a specific time. Pan evaporation data can be used to estimate 
          ET of a reference crop using the following equation (Jensen et al. 1990):"
        ),
        HTML(katex_html(
          "ET_{o} = k_{p}E_{pan}",
          displayMode = TRUE, 
          preview = FALSE,
          include_css = TRUE,
          output = "html"
        )
        ),
        p(
          HTML(paste(
            "where ",
            katex_html("k_{p}", displayMode = FALSE),
            "is a crop-specific pan coefficient.The coefficients can be used to convert pan measurements to an alfalfa reference crop ",
            katex_html("ET_{o}", displayMode = FALSE),
            " estimate."
          ))
        )
      )
    )
    
  ),
  column(
    width = 7,
    uiOutput("plot_box_pan"),
    uiOutput("set_arguments_pan")
  )
  
)