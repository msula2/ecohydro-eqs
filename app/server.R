server <- function(input, output, session) {
  v <- reactiveValues(
    vars_plot = NULL,
    vars_def = NULL,
    output_var = NULL,
    vars_eqs = NULL,
    data = NULL
  )
  
  observeEvent(input$navbar,{
    tab <- input$navbar
    vars_eqs <- NULL
    if (tab == "Penman-Monteith") {
      v$vars_plot <- list("\\Delta" = "slope_saturation","R_n"="net_radiation", "G"="ground_heat_flux", "\\lambda" = "latent_heat", "\\gamma" = "psy_constant", "u_2" = "wind_speed", "e_s - e_d" = "vapor_pressure_deficit")
      v$vars_def <- list("slope_saturation" = "Slope of the saturation vapor pressure curve with respect to temperature","net_radiation" = "Net Radiation", "ground_heat_flux"="Ground Heat Flux", "latent_heat"="Latent Heat of Vaporation", "psy_constant"="Psychometric Constant", "wind_speed"="Wind Speed 2m above ground", "vapor_pressure_deficit" = "Vapor Pressure Deficit")
      v$output_var <- list(id = "evapo_transpiration", value = "ET_o")  
      v$vars_eqs <- c(names(v$vars_def), v$output_var$id)
    }
    
    
    
    updateSelectizeInput(
      session, "plot_type",
      choices = v$vars_plot,
      options = list(
        render = I("
          {
            item: function(item, escape) { 
              var html = katex.renderToString(item.label);
              return '<div>' + html + '</div>'; 
            },
            option: function(item, escape) { 
              var html = katex.renderToString(item.label);
              return '<div>' + html + '</div>'; 
            }
          }
        ")
      )
    )
  })
  
  output$plot_by <- renderUI({
    fluidRow(
      column(6,
             selectizeInput("plot_type", label = "Plot by:", choices = NULL)
      ),
      column(6,
             uiOutput("definition"),
             style = "display: flex; align-items: center; height: 85px;"
      )
    )
    
  })
  
  observeEvent(input$plot_type,{
    output$definition <- renderUI({
      def <- v$vars_def[[input$plot_type]]
      label_html <- sprintf('<i class="fa-regular fa-circle-question info" data-toggle="tooltip" data-placement="right" title="%s"></i>', def)
      div(
        HTML(label_html)
      )
      
    })
    
    output$set_arguments <- renderUI({
      box(
        width = 12,
        class = "custom-box",
        title = "Set Arguments",
        uiOutput("range_x"),
        uiOutput("default_values"),
        fluidRow(
          actionButton(
            inputId = "submit",
            icon = icon("check"),
            label = "Submit",
            style = " float: right; border-radius: 15px; background-color:  rgb(111, 78, 55);"
          )  
        )
        
        
      )
      
    })
    
    output$range_x <- renderUI({
      def <- v$vars_def[[input$plot_type]]
      mapping <- setNames(names(v$vars_plot), v$vars_plot)
      x_value <- mapping[input$plot_type]
      if (!is.null(x_value)){
        label_html <- paste0(
          katex_html(x_value, displayMode = FALSE),
          sprintf('<i class="fa-regular fa-circle-question info" style="margin-left: 5px;" data-toggle="tooltip" data-placement="right" title="%s"></i>', def)
        )
        div(
          style = "display: flex; align-items: center; justify-content: center;",
          textInput("min", label = "", value = "", width = "10%"),
          div(HTML(katex_html("\\leq", displayMode = FALSE)),  style = "margin-left: 15px;"),
          div(HTML(label_html), style = "margin-left: 15px;"),
          div(HTML(katex_html("\\leq", displayMode = FALSE)),  style = "margin-left: 15px; margin-right: 15px;"),
          textInput("max", label = "", value = "", width = "10%")
        )
        
      }
      
    })
    
    output$default_values <- renderUI({
      mapping <- setNames(names(v$vars_plot), v$vars_plot)
      inputs <- lapply(names(mapping), function(var_id) {
        if (var_id != input$plot_type) {
          def <- v$vars_def[[var_id]]
          label_html <- paste0(
            katex_html(mapping[var_id], displayMode = FALSE),
            sprintf('<i class="fa-regular fa-circle-question info" style="margin-left: 5px;" data-toggle="tooltip" data-placement="right" title="%s"></i>', def)
          )
          return(
            column(
              width = 4,
              textInput(var_id,
                        label = HTML(label_html),
                        value = "",
                        width = "80%"
              )    
            )
          )
        }
      })
      
      fluidRow(inputs)
    })
    
  })

  observeEvent(input$submit, {
    x_axis_values <- seq(input$min, input$max, length.out = 10)
    dependant_var <- v$output_var$id
    
    default_vars <- setdiff(names(v$vars_def), input$plot_type)
    for (dv in default_vars){
      v[[dv]] <- as.numeric(input[[dv]])
    }
    independant_var <- setdiff(names(v$vars_def), default_vars)
    
    for (indp in independant_var) {
      v[[indp]] <- x_axis_values
    }
    
    # Initialize an empty data frame to store the results
    result_df <- data.frame(matrix(ncol = length(v$vars_eqs), nrow = length(x_axis_values)))
    colnames(result_df) <- v$vars_eqs
    
    if (dependant_var == "evapo_transpiration"){
      y_axis_values <- (v$slope_saturation / (v$slope_saturation + v$psy_constant)) * (v$net_radiation - v$ground_heat_flux) + (((v$psy_constant / (v$slope_saturation + v$psy_constant)) * 6.43 * (1.0 + 0.53 * v$wind_speed) * (v$vapor_pressure_deficit)) / v$latent_heat)
    }
   
    for (j in seq_along(v$vars_eqs)) {
      if (v$vars_eqs[j] == input$plot_type) {
        result_df[, v$vars_eqs[j]] <- x_axis_values
      } else if (v$vars_eqs[j] == dependant_var) {
        result_df[, v$vars_eqs[j]] <- y_axis_values
      } else {
        result_df[, v$vars_eqs[j]] <- input[[v$vars_eqs[j]]]
      }
    }
    
    v$data <- result_df
    
  })  
  
  
  
}
