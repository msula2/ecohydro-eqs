server <- function(input, output, session) {
  
  v <- reactiveValues(
    vars_plot = NULL,
    vars_def = NULL,
    output_var = NULL,
    vars_eqs = NULL,
    vars_axis = NULL,
    data = NULL,
    enable_submit = TRUE
  )
  
  observe({
    v$enable_submit = TRUE
    if (!is.null(v$vars_axis)){
      for (val in names(v$vars_def)){
        if (!is.null(input$plot_type) && val != input$plot_type){
          value_entered <- input[[val]]
          if (!is.null(value_entered)){
            if (value_entered != ""){
              num <- as.numeric(value_entered)
              if (is.na(num)) {
                showNotification(paste("Error:", "Only numbers are allowed"), type = "error")
                v$enable_submit = FALSE
              }   
            }
            else{
              v$enable_submit = FALSE
            }
            
          }   
        }
        
      }
    }
    if (is.null(input$min) && is.null(input$min)){
      v$enable_submit = FALSE
    }
    else{
      if (input$max != "" && input$min != ""){
        num_max <- as.numeric(input$max)
        num_min <- as.numeric(input$min)
        if (!is.na(num_max) && !is.na(num_min)){
          if (input$max <= input$min){
            v$enable_submit = FALSE 
            showNotification(paste("Error:", "Maximum value must be greater than minimum value"), type = "error")
            
          }  
        }
        else{
          v$enable_submit = FALSE 
          showNotification(paste("Error:", "Only numbers are allowed"), type = "error")
          
        }
        
        
      }
      else{
        v$enable_submit = FALSE 
      }
      
    }
    if(v$enable_submit){
      enable("submit")
    }
    else{
      disable("submit")
    }
    
  })  
  
  observeEvent(input$navbar,{
    tab <- input$navbar
    vars_eqs <- NULL
    if (tab == "Penman's Method") {
      v$vars_plot <- list("\\Delta" = "slope_saturation","R_n"="net_radiation", "G"="ground_heat_flux", "\\lambda" = "latent_heat", "\\gamma" = "psy_constant", "u_2" = "wind_speed", "e_s - e_d" = "vapor_pressure_deficit")
      v$vars_def <- list("slope_saturation" = "Slope of the saturation vapor pressure curve with respect to temperature","net_radiation" = "Net Radiation", "ground_heat_flux"="Ground Heat Flux", "latent_heat"="Latent Heat of Vaporation", "psy_constant"="Psychometric Constant", "wind_speed"="Wind Speed 2m above ground", "vapor_pressure_deficit" = "Vapor Pressure Deficit")
      v$output_var <- list(id = "evapo_transpiration", value = "ET_o")  
      v$vars_eqs <- c(names(v$vars_def), v$output_var$id)
      v$vars_axis <- list("slope_saturation" = "Slope saturation (kPa/°C)","net_radiation" = "Net Radiation (MJ/m²/d )", "ground_heat_flux"="Ground Heat Flux (MJ/m²/d )", "latent_heat"="Latent Heat of Vaporation (MJ/kg)", "psy_constant"="Psychometric Constant (kPa/°C)", "wind_speed"="Wind Speed 2m above ground (m/s)", "vapor_pressure_deficit" = "Vapor Pressure Deficit", "evapo_transpiration" = "Evapotranspiration (mm/day)")
    }
    
  })
  
  output$set_temperatures <- renderUI({
    box(
      width = 12,
      class = "custom-box",
      title = "Set Altitude & Temperatures",
      div(
        style = "display: flex; align-items: center;",
        div(HTML(katex_html("H", displayMode = FALSE))),
        div(textInput("H", label = "", value = "", width = "60%"), style="margin-left: 37px;")
      ),
      tags$table( 
        tags$thead(
          tags$tr(
            tags$th(
              style = "text-align: center;"
            ),
            tags$th(
              HTML(katex_html("i-1", displayMode = FALSE)),
              style = "text-align: center;"
              
            ),
            tags$th(
              HTML(katex_html("i", displayMode = FALSE)),
              style = "text-align: center;"
            ),
            tags$th(
              HTML(katex_html("i+1", displayMode = FALSE)),
              style = "text-align: center;"
            )
          )
        ), 
        tags$tbody(
          tags$tr(
            tags$td(align="center", HTML(katex_html("T", displayMode = FALSE))),
            tags$td(align="center", textInput("t_prev", label = "", value = "", width = "60%")),
            tags$td(align="center", textInput("t", label = "", value = "", width = "60%")),
            tags$td(align="center", textInput("t_aft", label = "", value = "", width = "60%"))
            
          )
        )
      ),
      fluidRow(
        actionButton(
          inputId = "submit_temperatures",
          label = HTML("Submit <i class='fas fa-check' style='margin-left: 2px'></i>"),
          class = "submit-btn"
        )  
      )
      
      
    )
  })
  
  observeEvent(input$submit_temperatures,{
    t_aft <- as.numeric(input$t_aft)
    t_prev <- as.numeric(input$t_prev)
    t <- as.numeric(input$t)
    H <- as.numeric(input$H)
    
    v[["slope_saturation"]] <- 0.200 * (0.00738 * t + 0.8072)^7 - 0.000116
    v[["ground_heat_flux"]] <- 4.2 * ((t_aft - t_prev) / 2)
    v[["latent_heat"]] <- 2.501 - 2.361 * (10^(-3)) * t
    v[["atmospheric_pressure"]] <- 101.3 - (0.01055 * H)
    v[["psy_constant"]] <- (0.001013 * v[["atmospheric_pressure"]]) / (0.622 * v[["latent_heat"]])
    
    
  })
  
  
  
  output$plot_by <- renderUI({
    
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
    
    fluidRow(
      div(
        selectizeInput("plot_type", label = "Plot by:", choices = NULL, width = "20%"),
        style = "display: flex; align-items: center; margin-left: 20px;"
      )
      
    )
    
  })
  
  observeEvent(input$slope_saturation_eqs, {
    mean_temperature <- as.character(input$t)
    slope_saturation <- as.character(v$slope_saturation)

    showModal(
      modalDialog(
        title = "Calculation",
        div(
          p(
            "The slope of the saturation vapor pressure–temperature curve Δ can be computed if the mean temperature is
         known using: "
          ),
          HTML(katex_html(
            "\\Delta = 0.200[0.00738 T + 0.8072]^{7} - 0.000116",
            displayMode = TRUE, 
            preview = FALSE,
            include_css = TRUE,
            output = "html"
          )
          ),
          p(
            "where Δ is in kilopascals per degree Celsius and T is the mean temperature in degree Celsius."
          ),
          p(
            "Given that: "
          )
        ),
        div(
          HTML(katex_html(paste("T", " = ", mean_temperature, "°C", sep = "~"), displayMode = TRUE)),
          HTML(katex_html(paste0("\\Delta", " = ", slope_saturation, "~kPa/°C", sep = "~"), displayMode = TRUE))
        ),
        footer = tagList(
          actionButton(inputId = "close", label = "Close", class = "submit-btn")
        )
      )
    )
  })
  observeEvent(input$psy_constant_eqs, {
    H <- as.character(input$H)
    P <- as.character(v$atmospheric_pressure)
    mean_temperature <- as.character(input$t)
    lambda <- as.character(v$latent_heat)
    gamma <- as.character(v$psy_constant)
    
    
    
    showModal(
      modalDialog(
        title = "Calculation",
        div(
          p(
            HTML(paste(
              "To calculate the psychrometric constant, we must first calculate ",
              katex_html("P", displayMode = FALSE),
              ", the atmospheric pressure that Doorenbos and Pruitt (1977) suggested could be calculated using:"
            ))
          ),
          HTML(katex_html(
            "P = 101.3−0.01055H",
            displayMode = TRUE, 
            preview = FALSE,
            include_css = TRUE,
            output = "html"
          )
          ),
          p(
            HTML(paste(
              "where ",
              katex_html("P", displayMode = FALSE),
              " is in kilopascals and ",
              katex_html("H", displayMode = FALSE),
              " is the elevation above sea level in meters."
            ))
          ),
          p(
            "Given that: "
          )
        ),
        div(
          HTML(katex_html(paste("H", " = ", H, "m", sep = "~"), displayMode = TRUE)),
          HTML(katex_html(paste0("P", " = ", P, "~kPa", sep = "~"), displayMode = TRUE))
        ),
        div(
          p("Next, we need to determine delta, the latent heat of vaporization (MJ/kg) using the following equation: "),
          HTML(katex_html(
            "\\lambda = 2.501 - 2.361 × 10^{-3}T",
            displayMode = TRUE, 
            preview = FALSE,
            include_css = TRUE,
            output = "html"
          )
          ),
          p("Given that: "),
          
        ),
        div(
          HTML(katex_html(paste("T", " = ", mean_temperature, "°C", sep = "~"), displayMode = TRUE)),
          HTML(katex_html(paste0("\\lambda", " = ", lambda, "~MJ/kg", sep = "~"), displayMode = TRUE))
        ),
        div(
          p(
            HTML(paste(
              "Using ",
              katex_html(paste("P", "\\lambda", "c_{p}", sep = ","), displayMode = FALSE),
              " the specific heat of water at constant pressure (0.001013 kJ/kg/°C),",
              " the psychrometric constant (in kPa/°C) can be calculated using:"
            ))
          ),
          HTML(katex_html(
            paste("\\gamma = \\frac{c_pP}{0.622\\lambda} = ", gamma , "~kPa/°C"),
            displayMode = TRUE, 
            preview = FALSE,
            include_css = TRUE,
            output = "html"
          )
          )
        ),
        footer = tagList(
          actionButton(inputId = "close", label = "Close", class = "submit-btn")
        )
      )
    )
  })
  observeEvent(input$plot_type,{
    
    output$definition <- renderUI({
      def <- v$vars_def[[input$plot_type]]
      label_html <- sprintf('<i class="fa-regular fa-circle-question info" data-toggle="tooltip" data-placement="right" title="%s"></i>', def)
      div(
        HTML(label_html),
        style = "margin-left: 10px; margin-bottom: 5px;"
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
        if (input$plot_type == "slope_saturation" || input$plot_type == "psy_constant" || input$plot_type == "ground_heat_flux"){
          min_box <- textInput("min", label = "", value = v[[input$plot_type]], width = "10%")
          
        }
        else{
          min_box <- textInput("min", label = "", value = "", width = "10%")
        }
        div(
          style = "display: flex; align-items: center; justify-content: center;",
          min_box,
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
          if (var_id == "slope_saturation" || var_id == "psy_constant" || var_id == "ground_heat_flux"){
            return(
              column(
                width = 4,
                fluidRow(
                  column(
                    width = 10,
                    textInput(var_id,
                              label = HTML(label_html),
                              value = v[[var_id]],
                              width = "100%"
                    )
                  ),
                  column(
                    width = 2,
                    actionButton(inputId = paste0(var_id, "_eqs"), label = HTML("<i class='fa-solid fa-calculator' style = 'color: #2c3e50;'></i>"), style = "background: none; border: none;margin-left: -60px; margin-top: -10px;")
                  )
                )
              )
            )
          }
          else{
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
          
        }
      })
      
      fluidRow(inputs)
    })
    
  })
  output$set_arguments <- renderUI({
    box(
      width = 12,
      class = "custom-box",
      title = "Set Arguments",
      uiOutput("plot_by"),
      uiOutput("range_x"),
      uiOutput("default_values"),
      fluidRow(
        actionButton(
          inputId = "submit",
          label = HTML("Submit <i class='fas fa-check' style='margin-left: 2px'></i>"),
          class = "submit-btn"
        )  
      )
      
      
    )
    
  })
  
  
  observeEvent(input$submit, {
    v$data <- NULL
    x_axis_values <- seq(input$min, input$max, length.out = 10)
    dependent_var <- v$output_var$id
    
    default_vars <- setdiff(names(v$vars_def), input$plot_type)
    for (dv in default_vars){
      v[[dv]] <- as.numeric(input[[dv]])
    }
    independent_var <- input$plot_type
    
    v[[independent_var]] <- x_axis_values
    
    # Initialize an empty data frame to store the results
    result_df <- data.frame(matrix(ncol = length(v$vars_eqs), nrow = length(x_axis_values)))
    colnames(result_df) <- v$vars_eqs
    
    if (dependent_var == "evapo_transpiration"){
      y_axis_values <- (v$slope_saturation / (v$slope_saturation + v$psy_constant)) * (v$net_radiation - v$ground_heat_flux) + (((v$psy_constant / (v$slope_saturation + v$psy_constant)) * 6.43 * (1.0 + 0.53 * v$wind_speed) * (v$vapor_pressure_deficit)) / v$latent_heat)
      
    }
    
    for (j in seq_along(v$vars_eqs)) {
      if (v$vars_eqs[j] == input$plot_type) {
        result_df[, v$vars_eqs[j]] <- x_axis_values
      } else if (v$vars_eqs[j] == dependent_var) {
        result_df[, v$vars_eqs[j]] <- y_axis_values
      } else {
        result_df[, v$vars_eqs[j]] <- input[[v$vars_eqs[j]]]
      }
    }
    
    v$data <- result_df
    
    
    output$plot_box <- renderUI({
      box(
        width = 12, title = "Results", class = "custom-box",
        actionButton(inputId = "reset", label = "Reset", class = "reset-btn"),
        tabBox(
          width = NULL,
          id = "results_tab",
          title = "",
          tabPanel(
            title = "Graph",
            plotlyOutput("plot_relationship")
          ),
          tabPanel(
            title = "Summary",
            tags$div(
              uiOutput("args_table"),
              style = "overflow-x: auto; overflow-y: auto; width: 100%; background-color: white !important; padding: 15px;"
            )
          )
        )
      )
      
      
    })
    
    output$args_table <- renderUI(
      {
        default_table <- v$data %>% 
          select(all_of(default_vars)) %>% 
          summarise_all(max)
        
        select_vals <- c(independent_var, dependent_var)
        results_table <- v$data %>% 
          select(all_of(select_vals)) %>% 
          summarise_all(~sprintf("%s to %s", round(min(.), digits = 3), round(max(.), digits = 3)))
        
        args_table <- bind_cols(default_table, results_table)
        
        mapping <- setNames(names(v$vars_plot), v$vars_plot)
        
        symbols_latex_col <- lapply(colnames(args_table), function(colname) {
          symbol <- NULL
          if (colname != v$output_var$id){
            symbol <- mapping[colname]
          }
          else{
            symbol <- v$output_var$value
          }
          return(HTML(katex_html(symbol, displayMode = FALSE)))
        })
        
        colnames_units <- lapply(colnames(args_table), function(colname) {
          return(v$vars_axis[[colname]])
        })
        
        colnames(args_table) <- colnames_units
        
        args_table <- args_table %>%
          pivot_longer(cols = everything(), names_to = "Argument", values_to = "Value")
        
        
        
        args_table <- args_table %>%
          mutate(Symbol = symbols_latex_col)
        
        args_table_df <- as.data.frame(args_table)
        
        
        kable_styled <- kable(args_table_df, format = "html", escape = FALSE) %>%
          kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive", "bordered"), full_width = FALSE)
        
        return(HTML(kable_styled))
      }
    )
    
    output$plot_relationship <- renderPlotly({
      if (nrow(v$data)) {
        x_axis_title <- v$vars_axis[[independent_var]]
        y_axis_title <- v$vars_axis[[dependent_var]]
        ggplot(v$data, aes(x = .data[[independent_var]], y = .data[[dependent_var]])) +
          geom_line(color = "#18bc9c", size = 0.8) +
          geom_point(color = "#097969", size = 1.6) +
          scale_x_continuous(name = x_axis_title) +
          scale_y_continuous(name = y_axis_title) +
          theme(
            plot.margin = margin(30, 40, 30, 30, unit = "pt"),
            text = element_text(family = "Lato"),
            title = element_text(size = 9),
            axis.title = element_text(size = 10),
            axis.text = element_text(size = 9),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(color = "gray", linetype = "dashed"),
          )+
          ggtitle(paste("Relationship between", x_axis_title, "and", y_axis_title))
      }
    })
    hide("set_arguments")
    show("plot_box")
    
    
  })
  
  observeEvent(input$close,{
    removeModal()
  })
  
  observeEvent(input$reset,{
    hide("plot_box")
    for (rv in v$vars_eqs){
      v[[rv]] <- NULL
      if (rv != v$output_var$id){
        if (rv != input$plot_type){
          updateTextInput(session, rv, value = "")
        }
        else{
          updateTextInput(session, "min", value = "")
          updateTextInput(session, "max", value = "")
        }
      }
      
    }
    show("set_arguments")
  })
}