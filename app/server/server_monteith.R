# server_monteith.R

monteith_server <- function(input, output, session) {
  v <- reactiveValues(
    vars_plot = NULL,
    vars_def = NULL,
    output_var = NULL,
    vars_eqs = NULL,
    vars_axis = NULL,
    data = NULL,
    enable_submit = TRUE
  )
  
  
  observeEvent(input$navbar,{
    tab <- input$navbar
    vars_eqs <- NULL
    if (tab == "Penman-Monteith Method") {
      v$vars_plot <- list("\\Delta" = "slope_saturation_pm","R_n"="net_radiation_pm", "G"="ground_heat_flux_pm", "\\gamma" = "psy_constant_pm", "u_2" = "wind_speed_pm", "e_s - e_a" = "vapor_pressure_deficit_pm")
      v$vars_def <- list("slope_saturation_pm" = "Slope of the saturation vapor pressure curve with respect to temperature","net_radiation_pm" = "Net Radiation", "ground_heat_flux_pm"="Ground Heat Flux", "psy_constant_pm"="Psychometric Constant", "wind_speed_pm"="Wind Speed 2m above ground", "vapor_pressure_deficit_pm" = "Vapor Pressure Deficit")
      v$output_var <- list(id = "ref_evapo_transpiration", value = "ET_o")  
      v$vars_eqs <- c(names(v$vars_def), v$output_var$id)
      v$vars_axis <- list("slope_saturation_pm" = "Slope saturation (kPa/°C)","net_radiation_pm" = "Net Radiation (MJ/m²/d )", "ground_heat_flux_pm"="Ground Heat Flux (MJ/m²/d )", "psy_constant_pm"="Psychometric Constant (kPa/°C)", "wind_speed_pm"="Wind Speed 2m above ground (m/s)", "vapor_pressure_deficit_pm" = "Saturation Vapor Pressure Deficit", "ref_evapo_transpiration" = "Reference Evapotranspiration (mm/day)")
    }
    
  })
  
  output$set_monthly_data_pm <- renderUI({
    box(
      width = 12,
      class = "custom-box",
      title = "Set Mean Monthly Data",
      tags$table(
        tags$thead(
          tags$tr(
            tags$th("Parameter", style="width: 30%;"),
            tags$th("Value", style="width: 40%;"),
            tags$th("Units", style="width: 30%;")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(
              HTML(katex_html("H", displayMode = FALSE)),
              tags$i(class = "fa-regular fa-circle-question info", 
                     style = "margin-left: 5px; color: #2c3e50;", 
                     `data-toggle` = "tooltip", 
                     `data-placement` = "right", 
                     title = "Elevation above sea level")
            ),
            tags$td(
              textInput("H_pm", label = "", value = "", width = "60%"),
              align="center"
            ),
            tags$td(
              HTML(katex_html("m", displayMode = FALSE))
            )
          ),
          tags$tr(
            tags$td(
              HTML(katex_html("e_a", displayMode = FALSE)),
              tags$i(class = "fa-regular fa-circle-question info", 
                     style = "margin-left: 5px; color: #2c3e50;", 
                     `data-toggle` = "tooltip", 
                     `data-placement` = "right", 
                     title = "Monthly average daily vapour pressure")
            ),
            tags$td(
              textInput("daily_vapour_pressure_pm", label = "", value = "", width = "60%"),
              align="center"
            ),
            tags$td(
              HTML(katex_html("kPa", displayMode = FALSE))
            )
          ),
          tags$tr(
            tags$td(
              HTML(katex_html("T_i", displayMode = FALSE)),
              tags$i(class = "fa-regular fa-circle-question info", 
                     style = "margin-left: 5px; color: #2c3e50;", 
                     `data-toggle` = "tooltip", 
                     `data-placement` = "right", 
                     title = "Mean monthly average temperature of current month")
            ),
            tags$td(
              textInput("t_pm", label = "", value = "", width = "60%"),
              align="center"
            ),
            tags$td(
              HTML(katex_html("°C", displayMode = FALSE))
            )
          ),
          tags$tr(
            tags$td(
              HTML(katex_html("T_{i-1}", displayMode = FALSE)),
              tags$i(class = "fa-regular fa-circle-question info", 
                     style = "margin-left: 5px; color: #2c3e50;", 
                     `data-toggle` = "tooltip", 
                     `data-placement` = "right", 
                     title = "Mean monthly average temperature of previous month")
            ),
            tags$td(
              textInput("t_prev_pm", label = "", value = "", width = "60%"),
              align="center"
            ),
            tags$td(
              HTML(katex_html("°C", displayMode = FALSE))
            )
          )
        )
      ),
      fluidRow(
        actionButton(
          inputId = "submit_meanmonthly_pm",
          label = HTML("Submit <i class='fas fa-check' style='margin-left: 2px'></i>"),
          class = "submit-btn"
        )  
      )
      
      
    )
  })
  
  observeEvent(input$submit_meanmonthly_pm,{
    t_mean <- as.numeric(input$t_pm)
    H <- as.numeric(input$H_pm)
    specific_heat <- 1.013 * 10^(-3)
    ratio_molecular_weight <- 0.622
    latent_heat <- 2.45
    daily_vapor_pressure <- as.numeric(input$daily_vapour_pressure_pm)
    
    
    
    v[["slope_saturation_pm"]] <- result <- 4098 * (0.6108 * exp((17.27 * t_mean) / (t_mean + 237.3))) / (t_mean + 237.3)^2
    v[["atmospheric_pressure_pm"]] <- 101.3 * ((293 - 0.0065*H)/(293))^5.26
    v[["psy_constant_pm"]] <- (specific_heat * v[["atmospheric_pressure_pm"]]) / (ratio_molecular_weight * latent_heat)
    v[["sat_vapor_pressure_pm"]] <- 0.6108 * exp((17.27 * t_mean) / (t_mean + 237.3))
    v[["vapor_pressure_deficit_pm"]] <- v[["sat_vapor_pressure_pm"]] - daily_vapor_pressure
    v[["t_pm"]] <- t_mean
    
    output$set_arguments_pm <- renderUI({
      box(
        width = 12,
        class = "custom-box",
        title = "Set Arguments",
        uiOutput("plot_by_pm"),
        uiOutput("range_x_pm"),
        uiOutput("default_values_pm"),
        fluidRow(
          actionButton(
            inputId = "submit_pm",
            label = HTML("Submit <i class='fas fa-check' style='margin-left: 2px'></i>"),
            class = "submit-btn"
          )  
        )
        
        
      )
      
    })
    
    hide("set_monthly_data_pm")
    
    
  })
  
  
  
  output$plot_by_pm <- renderUI({
    
    updateSelectizeInput(
      session, "plot_type_pm",
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
        selectizeInput("plot_type_pm", label = "Plot by:", choices = NULL, width = "20%"),
        style = "display: flex; align-items: center; margin-left: 20px;"
      )
      
    )
    
  })
  
  observeEvent(input$slope_saturation_pm_eqs, {
    mean_temperature <- as.character(input$t_pm)
    slope_saturation <- as.character(v$slope_saturation_pm)
    
    showModal(
      modalDialog(
        title = "Calculation",
        div(
          p(
            HTML(paste(
              "The slope of the saturation vapor pressure–temperature curve ",
              katex_html("\\Delta", displayMode = FALSE),
              " can be computed if the mean temperature is known using: "
            ))
          ),
          HTML(katex_html(
            "\\Delta = \\frac{4098 (0.6108 e^{(\\frac{17.27T}{T + 237.3})})}{(T + 237.3)^2}",
            displayMode = TRUE, 
            preview = FALSE,
            include_css = TRUE,
            output = "html"
          )
          ),
          p(
            HTML(paste(
              "where ",
              katex_html("\\Delta", displayMode = FALSE),
              " is in kilopascals per degree Celsius and ",
              katex_html("T", displayMode = FALSE),
              " is the mean temperature in degree Celsius."
            ))
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
  
  observeEvent(input$vapor_pressure_deficit_pm_eqs, {
    daily_vapor <- as.character(input$daily_vapour_pressure_pm)
    sat_vapor <- as.character(v$sat_vapor_pressure_pm)
    mean_temperature <- as.character(input$t_pm)
    vapor_pressure_def <- as.character(v$vapor_pressure_deficit_pm)
    
    
    showModal(
      modalDialog(
        title = "Calculation",
        div(
          p(
            HTML(paste(
              "To determine the vapor pressure deficit, we must first determine ",
              katex_html("e_s", displayMode = FALSE),
              " , the saturation vapor pressure, which can be computed if the mean air temperature is known using: "
            ))
          ),
          HTML(katex_html(
            "e_s = 0.6108 e^{\\left(\\frac{17.27T}{T + 237.3}\\right)}",
            displayMode = TRUE, 
            preview = FALSE,
            include_css = TRUE,
            output = "html"
          )
          ),
          p(
            HTML(paste(
              "where ",
              katex_html("e_s", displayMode = FALSE),
              " is in kilopascals and ",
              katex_html("T", displayMode = FALSE),
              " is the mean air temperature in degree Celsius."
            ))
          ),
          p(
            "Given that: "
          )
        ),
        div(
          HTML(katex_html(paste("T", " = ", mean_temperature, "°C", sep = "~"), displayMode = TRUE)),
          HTML(katex_html(paste0("e_s", " = ", sat_vapor, "~kPa", sep = "~"), displayMode = TRUE)),
          HTML(katex_html(paste0("e_a", " = ", daily_vapor, "~kPa", sep = "~"), displayMode = TRUE))
        ),
        p(
          HTML(paste(
            "We can determine the vapor pressure deficit, using the following equation: "
          ))
        ),
        HTML(katex_html(
          paste("e_s - e_a = ", vapor_pressure_def , "~kPa"),
          displayMode = TRUE, 
          preview = FALSE,
          include_css = TRUE,
          output = "html"
        )
        ),
        footer = tagList(
          actionButton(inputId = "close", label = "Close", class = "submit-btn")
        )
      )
    )
  })
  
  observeEvent(input$psy_constant_pm_eqs, {
    H <- as.character(input$H_pm)
    P <- as.character(v$atmospheric_pressure_pm)
    gamma <- as.character(v$psy_constant_pm)
    
    
    showModal(
      modalDialog(
        title = "Calculation",
        div(
          p(
            HTML(paste(
              "To calculate the psychrometric constant, we must first calculate ",
              katex_html("P", displayMode = FALSE),
              ", the atmospheric pressure using:"
            ))
          ),
          HTML(katex_html(
            "P = 101.3\\left( \\frac{293 - 0.0065H}{293} \\right)^{5.26}",
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
          p(
            HTML(paste(
              "Using ",
              katex_html(paste("P", "c_{p}", sep = ","), displayMode = FALSE),
              " the specific heat of water at constant pressure (0.001013 kJ/kg/°C),",
              katex_html("\\lambda", displayMode = FALSE),
              " the latent heat of vaporization (2.45 MJ/kg/°C),",
              katex_html("\\varepsilon", displayMode = FALSE),
              " ratio molecular weight of water vapour/dry air (0.622),",
              " the psychrometric constant (in kPa/°C) can be calculated with the equation:"
            ))
          ),
          HTML(katex_html(
            paste("\\gamma = \\frac{c_pP}{\\varepsilon \\lambda} = ", gamma , "~kPa/°C"),
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
  
  observeEvent(input$plot_type_pm,{
    
    output$definition_pm <- renderUI({
      def <- v$vars_def[[input$plot_type_pm]]
      label_html <- sprintf('<i class="fa-regular fa-circle-question info" data-toggle="tooltip" data-placement="right" title="%s"></i>', def)
      div(
        HTML(label_html),
        style = "margin-left: 10px; margin-bottom: 5px;"
      )
      
    })
    
    output$range_x_pm <- renderUI({
      def <- v$vars_def[[input$plot_type_pm]]
      mapping <- setNames(names(v$vars_plot), v$vars_plot)
      x_value <- mapping[input$plot_type_pm]
      if (!is.null(x_value)){
        label_html <- paste0(
          katex_html(x_value, displayMode = FALSE),
          sprintf('<i class="fa-regular fa-circle-question info" style="margin-left: 5px;" data-toggle="tooltip" data-placement="right" title="%s"></i>', def)
        )
        if (input$plot_type_pm == "slope_saturation_pm" || input$plot_type_pm == "psy_constant_pm" || input$plot_type_pm == "vapor_pressure_deficit_pm"){
          min_box <- textInput("min_pm", label = "", value = v[[input$plot_type_pm]], width = "10%")
          
        }
        else{
          min_box <- textInput("min_pm", label = "", value = "", width = "10%")
        }
        div(
          style = "display: flex; align-items: center; justify-content: center;",
          min_box,
          div(HTML(katex_html("\\leq", displayMode = FALSE)),  style = "margin-left: 15px;"),
          div(HTML(label_html), style = "margin-left: 15px;"),
          div(HTML(katex_html("\\leq", displayMode = FALSE)),  style = "margin-left: 15px; margin-right: 15px;"),
          textInput("max_pm", label = "", value = "", width = "10%")
        )
        
      }
      
    })
    
    output$default_values_pm <- renderUI({
      mapping <- setNames(names(v$vars_plot), v$vars_plot)
      inputs <- lapply(names(mapping), function(var_id) {
        if (var_id != input$plot_type_pm) {
          def <- v$vars_def[[var_id]]
          label_html <- paste0(
            katex_html(mapping[var_id], displayMode = FALSE),
            sprintf('<i class="fa-regular fa-circle-question info" style="margin-left: 5px;" data-toggle="tooltip" data-placement="right" title="%s"></i>', def)
          )
          if (var_id == "slope_saturation_pm" || var_id == "psy_constant_pm" || var_id == "vapor_pressure_deficit_pm"){
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
  
  
  observeEvent(input$submit_pm, {
    v$data <- NULL
    x_axis_values <- seq(input$min_pm, input$max_pm, length.out = 10)
    dependent_var <- v$output_var$id
    
    default_vars <- setdiff(names(v$vars_def), input$plot_type_pm)
    for (dv in default_vars){
      v[[dv]] <- as.numeric(input[[dv]])
    }
    independent_var <- input$plot_type_pm
    
    v[[independent_var]] <- x_axis_values
    
    # Initialize an empty data frame to store the results
    result_df <- data.frame(matrix(ncol = length(v$vars_eqs), nrow = length(x_axis_values)))
    colnames(result_df) <- v$vars_eqs
    
    y_axis_values <- (0.408 * v$slope_saturation_pm * (v$net_radiation_pm - v$ground_heat_flux_pm) + v$psy_constant_pm * (900 / (v$t_pm + 273)) * v$wind_speed_pm * v$vapor_pressure_deficit_pm) / (v$slope_saturation_pm + v$psy_constant_pm *(1 + 0.34 * v$wind_speed_pm))
      
    
    for (j in seq_along(v$vars_eqs)) {
      if (v$vars_eqs[j] == input$plot_type_pm) {
        result_df[, v$vars_eqs[j]] <- x_axis_values
      } else if (v$vars_eqs[j] == dependent_var) {
        result_df[, v$vars_eqs[j]] <- y_axis_values
      } else {
        result_df[, v$vars_eqs[j]] <- input[[v$vars_eqs[j]]]
      }
    }
    
    v$data <- result_df
    
    
    output$plot_box_pm <- renderUI({
      t <- as.character(input$t_pm)
      H <- as.character(input$H_pm)
      daily_vapor_pressure <- as.character(input$daily_vapour_pressure_pm)
      
      box(
        width = 12, title = "Results", class = "custom-box",
        actionButton(inputId = "reset_pm", label = "Reset", class = "reset-btn"),
        tabBox(
          width = NULL,
          id = "results_tab_pm",
          title = "",
          tabPanel(
            title = "Graph",
            plotlyOutput("plot_relationship_pm")
          ),
          tabPanel(
            title = "Summary",
            tags$div(
              div(
                style = "margin: auto; width: 70%;",
                p(
                  "Given that: " 
                ),
                p(
                  HTML(paste(
                    HTML(katex_html(paste("~T", " = ", t, "~°C~", sep = "~"), displayMode = FALSE)),
                    " , ",
                    HTML(katex_html(paste("~e_a", " = ", daily_vapor_pressure, "~kPa~", sep = "~"), displayMode = FALSE)),
                    " and ",
                    HTML(katex_html(paste("~H", " = ", H, "~m~", sep = "~"), displayMode = FALSE))
                  ))
                ),
              ),
              uiOutput("args_table_pm"),
              style = "overflow-x: auto; overflow-y: auto; width: 100%; background-color: white !important; padding: 15px;"
            )
          )
        )
      )
      
      
    })
    
    
    output$args_table_pm <- renderUI(
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
    
    output$plot_relationship_pm <- renderPlotly({
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
}