ui_open <- fluidPage(
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