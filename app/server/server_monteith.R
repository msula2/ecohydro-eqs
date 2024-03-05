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
      v$vars_plot <- list("\\Delta" = "slope_saturation_pm","R_n"="net_radiation_pm", "G"="ground_heat_flux_pm", "\\gamma" = "psy_constant_pm", "u_2" = "wind_speed_pm", "e_s - e_a" = "sat_vapor_pressure_deficit_pm")
      v$vars_def <- list("slope_saturation_pm" = "Slope of the saturation vapor pressure curve with respect to temperature","net_radiation_pm" = "Net Radiation", "ground_heat_flux_pm"="Ground Heat Flux", "psy_constant_pm"="Psychometric Constant", "wind_speed_pm"="Wind Speed 2m above ground", "vapor_pressure_deficit_pm" = "Vapor Pressure Deficit")
      v$output_var <- list(id = "ref_evapo_transpiration", value = "ET_o")  
      v$vars_eqs <- c(names(v$vars_def), v$output_var$id)
      v$vars_axis <- list("slope_saturation_pm" = "Slope saturation (kPa/°C)","net_radiation_pm" = "Net Radiation (MJ/m²/d )", "ground_heat_flux_pm"="Ground Heat Flux (MJ/m²/d )", "psy_constant_pm"="Psychometric Constant (kPa/°C)", "wind_speed_pm"="Wind Speed 2m above ground (m/s)", "sat_vapor_pressure_deficit_pm" = "Saturation Vapor Pressure Deficit", "ref_evapo_transpiration" = "Reference Evapotranspiration (mm/day)")
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
              HTML(katex_html("T_{max}", displayMode = FALSE)),
              tags$i(class = "fa-regular fa-circle-question info", 
                     style = "margin-left: 5px; color: #2c3e50;", 
                     `data-toggle` = "tooltip", 
                     `data-placement` = "right", 
                     title = "Monthly average daily maximum temperature")
            ),
            tags$td(
              textInput("t_max_pm", label = "", value = "", width = "60%"),
              align="center"
            ),
            tags$td(
              HTML(katex_html("°C", displayMode = FALSE))
            )
          ),
          tags$tr(
            tags$td(
              HTML(katex_html("T_{min}", displayMode = FALSE)),
              tags$i(class = "fa-regular fa-circle-question info", 
                     style = "margin-left: 5px; color: #2c3e50;", 
                     `data-toggle` = "tooltip", 
                     `data-placement` = "right", 
                     title = "Monthly average daily minimum temperature")
            ),
            tags$td(
              textInput("t_min_pm", label = "", value = "", width = "60%"),
              align="center"
            ),
            tags$td(
              HTML(katex_html("°C", displayMode = FALSE))
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
              textInput("e_a_pm", label = "", value = "", width = "60%"),
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
    t_max <- as.numeric(input$t_max_pm)
    t_min <- as.numeric(input$t_min_pm)
    t_mean <- (t_max + t_min) / 2
    H <- as.numeric(input$H_pm)
    specific_heat <- 1.013 * 10^(-3)
    ratio_molecular_weight <- 0.622
    latent_heat <- 2.45
    
    
    
    v[["slope_saturation_pm"]] <- result <- 4098 * (0.6108 * exp((17.27 * t_mean) / (t_mean + 237.3))) / (t_mean + 237.3)^2
    v[["atmospheric_pressure_pm"]] <- 101.3 * ((293 - 0.0065*H)/(293))^5.26
    v[["psy_constant_pm"]] <- (specific_heat * v[["atmospheric_pressure_pm"]]) / (ratio_molecular_weight * latent_heat)
    v[["saturation_vapour_pressure_tmax"]] <- 0.6108 * exp((17.27 * t_max) / (t_max + 237.3))
    v[["saturation_vapour_pressure_tmin"]] <- 0.6108 * exp((17.27 * t_min) / (t_min + 237.3))
    v[["saturation_vapour_pressure_pm"]] <- (v[["saturation_vapour_pressure_tmax"]] + v[["saturation_vapour_pressure_tmin"]]) / 2
      
    hide("set_meanmonthy_pm")
    
    
  })
}