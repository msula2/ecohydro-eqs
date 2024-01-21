# server_open.R
open_server <- function(input, output, session) {
  
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
    if (tab == "Meyer's Open Water Evaporation") {
      v$vars_plot <- list("C" = "coefficient", "e_s - e_d" = "vapor_pressure_deficit_open", "u_{25}" = "avg_wind_velocity")
      v$vars_def <- list("coefficient" = "Coefficient that equals 11 for small lakes and reservoirs and 15 for shallow ponds","vapor_pressure_deficit_open" = "Vapor Pressure Deficit", "avg_wind_velocity" = "Average wind velocity")
      v$output_var <- list(id = "evaporation", value = "E")
      v$vars_eqs <- c(names(v$vars_def), v$output_var$id)
      v$vars_axis <- list("coefficient" = "Coefficient", "vapor_pressure_deficit_open" = "Vapor Pressure Deficit (Hg)", "avg_wind_velocity" = "Average Wind Velocity (miles/hour)", "evaporation" = "Evaporation (inches/month)")
    }
    
  })
  
  
  
  output$plot_by_open <- renderUI({
    
    updateSelectizeInput(
      session, "plot_type_open",
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
        selectizeInput("plot_type_open", label = "Plot by:", choices = NULL, width = "20%"),
        style = "display: flex; align-items: center; margin-left: 20px;"
      )
      
    )
    
  })
  
  
  observeEvent(input$plot_type_open,{
    
    output$definition_open <- renderUI({
      def <- v$vars_def[[input$plot_type_open]]
      label_html <- sprintf('<i class="fa-regular fa-circle-question info" data-toggle="tooltip" data-placement="right" title="%s"></i>', def)
      div(
        HTML(label_html),
        style = "margin-left: 10px; margin-bottom: 5px;"
      )
      
    })
    
    output$range_x_open <- renderUI({
      def <- v$vars_def[[input$plot_type_open]]
      mapping <- setNames(names(v$vars_plot), v$vars_plot)
      x_value <- mapping[input$plot_type_open]
      if (!is.null(x_value)){
        label_html <- paste0(
          katex_html(x_value, displayMode = FALSE),
          sprintf('<i class="fa-regular fa-circle-question info" style="margin-left: 5px;" data-toggle="tooltip" data-placement="right" title="%s"></i>', def)
        )
        min_box <- textInput("min_open", label = "", value = "", width = "10%")
        div(
          style = "display: flex; align-items: center; justify-content: center;",
          min_box,
          div(HTML(katex_html("\\leq", displayMode = FALSE)),  style = "margin-left: 15px;"),
          div(HTML(label_html), style = "margin-left: 15px;"),
          div(HTML(katex_html("\\leq", displayMode = FALSE)),  style = "margin-left: 15px; margin-right: 15px;"),
          textInput("max_open", label = "", value = "", width = "10%")
        )
        
      }
      
    })
    
    output$default_values_open <- renderUI({
      mapping <- setNames(names(v$vars_plot), v$vars_plot)
      inputs <- lapply(names(mapping), function(var_id) {
        if (var_id != input$plot_type_open) {
          def <- v$vars_def[[var_id]]
          label_html <- paste0(
            katex_html(mapping[var_id], displayMode = FALSE),
            sprintf('<i class="fa-regular fa-circle-question info" style="margin-left: 5px;" data-toggle="tooltip" data-placement="right" title="%s"></i>', def)
          )
          return(
            column(
              width = 6,
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
  output$set_arguments_open <- renderUI({
    box(
      width = 12,
      class = "custom-box",
      title = "Set Arguments",
      uiOutput("plot_by_open"),
      uiOutput("range_x_open"),
      uiOutput("default_values_open"),
      fluidRow(
        actionButton(
          inputId = "submit_open",
          label = HTML(" <i class='fa-solid fa-floppy-disk' style='margin-left: 1px;margin-right: 1px;'></i> Submit"),
          class = "submit-btn"
        )  
      )
      
      
    )
    
  })
  
  
  observeEvent(input$submit_open, {
    v$data <- NULL
    x_axis_values <- seq(input$min_open, input$max_open, length.out = 10)
    dependent_var <- v$output_var$id
    
    default_vars <- setdiff(names(v$vars_def), input$plot_type_open)
    for (dv in default_vars){
      v[[dv]] <- as.numeric(input[[dv]])
    }
    independent_var <- input$plot_type_open
    
    v[[independent_var]] <- x_axis_values
    
    # Initialize an empty data frame to store the results
    result_df <- data.frame(matrix(ncol = length(v$vars_eqs), nrow = length(x_axis_values)))
    colnames(result_df) <- v$vars_eqs
    
    y_axis_values <- v$coefficient * (v$vapor_pressure_deficit_open) * (1 + (v$avg_wind_velocity/10))

    
    for (j in seq_along(v$vars_eqs)) {
      if (v$vars_eqs[j] == input$plot_type_open) {
        result_df[, v$vars_eqs[j]] <- x_axis_values
      } else if (v$vars_eqs[j] == dependent_var) {
        result_df[, v$vars_eqs[j]] <- y_axis_values
      } else {
        result_df[, v$vars_eqs[j]] <- input[[v$vars_eqs[j]]]
      }
    }
    v$data <- result_df
    
    
    output$plot_box_open <- renderUI({

      box(
        width = 12, title = "Results", class = "custom-box",
        actionButton(inputId = "reset_open", label = HTML("<i class='fa-solid fa-arrow-rotate-right' style='margin-left: 1px;margin-right: 1px;'></i> Reset"), class = "reset-btn"),
        tabBox(
          width = NULL,
          id = "results_tab",
          title = "",
          tabPanel(
            title = "Graph",
            plotlyOutput("plot_relationship_open")
          ),
          tabPanel(
            title = "Summary",
            tags$div(
              uiOutput("args_table_open"),
              style = "overflow-x: auto; overflow-y: auto; width: 100%; background-color: white !important; padding: 15px;"
            )
          )
        )
      )
      
      
    })
    
    
    output$args_table_open <- renderUI(
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
    
    output$plot_relationship_open <- renderPlotly({
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
    hide("set_arguments_open")
    show("plot_box_open")
    
    
  })
  
  observeEvent(input$close,{
    removeModal()
  })
  
  observeEvent(input$reset_open,{
    hide("plot_box_open")
    for (rv in v$vars_eqs){
      v[[rv]] <- NULL
      if (rv != v$output_var$id){
        if (rv != input$plot_type_open){
          updateTextInput(session, rv, value = "")
        }
        else{
          updateTextInput(session, "min_open", value = "")
          updateTextInput(session, "max_open", value = "")
        }
      }
      
    }
    show("set_arguments_open")
  })
  
}


