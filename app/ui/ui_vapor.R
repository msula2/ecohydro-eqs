ui_vapor <- fluidPage(
  useShinyjs(),
  div(
    id = "loader_background_vp",
    style="display: block;"
  ),
  div(
    id = "loader_vp",
    icon("sun", class="fas", style = "font-size: 125px;"),
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