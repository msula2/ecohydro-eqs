server <- function(input, output, session) {
  v <- reactiveValues(
    vars_plot = NULL,
    vars_def = NULL,
    output_var = NULL,
    vars_eqs = NULL,
    vars_axis = NULL,
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
      v$vars_axis <- list("slope_saturation" = "Slope saturation (kPa/°C)","net_radiation" = "Net Radiation (MJ/m²/d )", "ground_heat_flux"="Ground Heat Flux (MJ/m²/d )", "latent_heat"="Latent Heat of Vaporation (MJ/kg)", "psy_constant"="Psychometric Constant (kPa/°C)", "wind_speed"="Wind Speed 2m above ground (m/s)", "vapor_pressure_deficit" = "Vapor Pressure Deficit", "evapo_transpiration" = "Evapotranspiration (mm/day)")
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
            label = HTML("Submit <i class='fas fa-check' style='margin-left: 2px'></i>"),
            class = "submit-btn"
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
    independant_var <- input$plot_type
    
    v[[independant_var]] <- x_axis_values
    
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
    
    
    output$plot_box <- renderUI({
      box(
        width = 12, title = "Results", class = "custom-box",
        tabBox(
          width = NULL,
          title = "",
          tabPanel(
            title = "Graph",
            plotlyOutput("plot_relationship")
          ),
          tabPanel(
            title = "Summary",
            div(
              uiOutput("args_table"),
              style = "overflow-x: auto; overflow-y: auto; width: 100%; background-color: white !important; padding: 12px;"
            )
          ),
          tabPanel(
            title = "Table",
            div(
              DT::dataTableOutput("data_table"),
              style = "overflow-x: auto; overflow-y: auto; width: 100%; background-color: white !important; padding: 12px;"
            )
          )
        )
      )
      

    })
    
    output$args_table <- renderUI(
      {
        default_table <- v$data %>% 
          select(default_vars) %>% 
          summarise_all(max)
        
        select_vals <- c(independant_var, dependant_var)
        results_table <- v$data %>% 
          select(select_vals) %>% 
          summarise_all(~sprintf("%s to %s", min(.), max(.)))
        
        args_table <- bind_cols(default_table, results_table)
        
        colnames_units <- lapply(colnames(args_table), function(colname) {
          return(v$vars_axis[[colname]])
        })
        
        colnames(args_table) <- colnames_units
        
        args_table <- args_table %>%
          pivot_longer(cols = everything(), names_to = "Argument", values_to = "Value")
        
        
        
        kable_styled <- kbl(args_table) %>%
          kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive", "bordered"))
        
        return(HTML(kable_styled))
      }
    )
    
    output$data_table <- DT::renderDataTable({
      v$data %>% select(independant_var, dependant_var)
    }, options = list(
      pageLength = 5,
      lengthMenu = c(5,10, 25, 50, 100),
      searching = FALSE
      
      ),
      caption = "Results"
    )
    
    output$plot_relationship <- renderPlotly({
      if (nrow(v$data)) {
        x_axis_title <- v$vars_axis[[independant_var]]
        y_axis_title <- v$vars_axis[[dependant_var]]
          ggplot(v$data, aes(x = .data[[independant_var]], y = .data[[dependant_var]])) +
          geom_line(color = "#18bc9c", size = 0.8) +
          geom_point(color = "#097969", size = 1.6) +
          scale_x_continuous(name = x_axis_title) +
          scale_y_continuous(name = y_axis_title) +
          theme_tufte() +
          theme(
            plot.margin = margin(30, 40, 30, 30, unit = "pt"),
            text = element_text(family = "Lato"),
            title = element_text(size = 9),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 8),
            panel.grid.major = element_line(color = "gray", linetype = "dashed"),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "white"))+
          labs(title = paste("Relationship between", x_axis_title, "and", y_axis_title))
      }
    })
    
  })
}