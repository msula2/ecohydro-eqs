ui_criddle <- fluidPage(
  useShinyjs(),
  div(
    id = "loader_background_cr",
    style="display: block;"
  ),
  div(
    id = "loader_cr",
    icon("seedling", class="fas", style = "font-size: 125px;"),
    p(
      "Loading . . .",
      style = "padding-top: 5px;"
    ),
    style="display: block;"
  ),
  column(
    width = 12,
    box(
      width = NULL, title = "About", class = "custom-box about",
      div(
        p(
          "The SCS Blaney–Criddle method estimates seasonal (either growing season or irrigation season) actual evapo-
          transpiration. This is the standard method recommended by the USDA-NRCS, is well known in the western United
          States, and is used extensively throughout the world (Jensen et al. 1990). The original relationships were developed
          around 1945 and were intended for seasonal estimates. This method may be used to obtain monthly estimates if
          monthly crop coefficients are locally available."
        ),
        p(
          HTML(paste(
            "We know that the amount of ",
            katex_html("ET", displayMode = FALSE),
            " is related to how much energy is available for vaporizing water. The energy is
            provided by solar radiation, but measuring solar radiation requires instrumentation not available at most field sites.
            Blaney and Criddle assumed that mean monthly air temperature and monthly percentage of annual daytime hours
            could be used instead of solar radiation to provide an estimate of the energy received by the crop. They defined a
            monthly consumptive use factor ",
            katex_html("f", displayMode = FALSE),
            " as: "
          ))
        ),
        div(
          style = "display: flex; align-items: center; justify-content: center;",
          HTML(paste0(
            katex_html(
              "f = ET_{o} = \\frac{tP}{100}",
              displayMode = TRUE, 
              preview = FALSE,
              include_css = TRUE,
              output = "html"
            ),
            img(src = "img/circle-1.png", alt = "Equation 1", style = "margin-left: 10px;")
          ))),
        p(
          HTML(paste(
            katex_html("t", displayMode = FALSE),
            " is the mean monthly air temperature in °F (average of daily maximum and minimum) and ",
            katex_html("P", displayMode = FALSE),
            " is the mean monthly percentage of annual daytime hours.",
            "Once ",
            katex_html("ET_{o}", displayMode = FALSE),
            " is computed for each month, then the actual crop ",
            katex_html("ET_{c}", displayMode = FALSE),
            " for the season is computed by: "
          ))
        ),
        div(
          style = "display: flex; align-items: center; justify-content: center;",
          HTML(paste0(
            katex_html(
              "ET_{c} = K_{c}\\sum_{i=1}^{n} ET_{o}",
              displayMode = TRUE, 
              preview = FALSE,
              include_css = TRUE,
              output = "html"
            ),
            img(src = "img/circle-2.png", alt = "Equation 2", style = "margin-left: 10px;")
          ))),
        p(
          HTML(paste(
            "where ",
            katex_html("K_{c}", displayMode = FALSE),
            " is the seasonal crop coefficient for a crop with a normal growing season ",
            katex_html("n", displayMode = FALSE),
            " is the number of months in the season, and ",
            katex_html("ET_{c}", displayMode = FALSE),
            " is the seasonal consumptive use in inches/season. The method can be applied on a
            monthly basis, by calculating ",
            katex_html("ET_{o}", displayMode = FALSE),
            " , and then multiplying it by ",
            katex_html("K_{c}", displayMode = FALSE)
          ))
        ),
        div(
          style = "display: flex; justify-content: center;",
          div(
            class = "choices_box",
            radioButtons("criddle_calc_choice", label = "",
                         choiceNames = c("Enter tabular data"),
                         choiceValues = c("table"),
                         selected = character(0), inline = TRUE)
          )
          
        ),
        uiOutput("criddle_results")
        
      )

    )
    
  )
  
)
