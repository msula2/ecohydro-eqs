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
              HTML(katex_html("u_2", displayMode = FALSE)),
              tags$i(class = "fa-regular fa-circle-question info", 
                     style = "margin-left: 5px; color: #2c3e50;", 
                     `data-toggle` = "tooltip", 
                     `data-placement` = "right", 
                     title = "Monthly average daily wind speed")
            ),
            tags$td(
              textInput("wind_speed_pm", label = "", value = "", width = "60%"),
              align="center"
            ),
            tags$td(
              HTML(katex_html("m/s", displayMode = FALSE))
            )
            
          ),
          tags$tr(
            tags$td(
              HTML(katex_html("n", displayMode = FALSE)),
              tags$i(class = "fa-regular fa-circle-question info", 
                     style = "margin-left: 5px; color: #2c3e50;", 
                     `data-toggle` = "tooltip", 
                     `data-placement` = "right", 
                     title = "Monthly average sunshine duration")
            ),
            tags$td(
              textInput("sunshine_duration", label = "", value = "", width = "60%"),
              align="center"
            ),
            tags$td(
              HTML(katex_html("hours/day", displayMode = FALSE))
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
    v[["sat_vapour_pressure_pm"]] <- 0.6108 * exp((17.27 * t_mean) / (t_mean + 237.3))
    v[["vapor_pressure_deficit_pm"]] <- v[["sat_vapour_pressure_pm"]] - daily_vapor_pressure
      
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
    
    hide("set_meanmonthy_data_pm")
    
    
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
}