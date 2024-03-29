# server_pan.R
pan_server <- function(input, output, session) {
  
  delay(3500, hide("loader_background_pn"))
  delay(3500, hide("loader_pn"))
  
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
    if (tab == "Pan Evaporation") {
      v$vars_plot <- list("K_{p}" = "pan_coefficient", "E_{pan}" = "pan_evaporation")
      v$vars_def <- list("pan_coefficient" = "Crop specific pan coefficient","pan_evaporation" = "Pan Evaporation")
      v$output_var <- list(id = "reference_et", value = "ET_{o}")
      v$vars_eqs <- c(names(v$vars_def), v$output_var$id)
      v$vars_axis <- list("pan_coefficient" = "Crop specific pan coefficient", "pan_evaporation" = "Pan Evaporation (inches)", "reference_et" = "Alfalfa reference Crop Evaportranspiration (inches)")
    }
    
  })
  
  
  
  output$plot_by_pan <- renderUI({
    
    updateSelectizeInput(
      session, "plot_type_pan",
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
        selectizeInput("plot_type_pan", label = "Plot by:", choices = NULL, width = "20%"),
        style = "display: flex; align-items: center; margin-left: 20px;"
      )
      
    )
    
  })
  
  
  observeEvent(input$plot_type_pan,{
    
    output$definition_pan <- renderUI({
      def <- v$vars_def[[input$plot_type_pan]]
      label_html <- sprintf('<i class="fa-regular fa-circle-question info" data-toggle="tooltip" data-placement="right" title="%s"></i>', def)
      div(
        HTML(label_html),
        style = "margin-left: 10px; margin-bottom: 5px;"
      )
      
    })
    
    output$range_x_pan <- renderUI({
      def <- v$vars_def[[input$plot_type_pan]]
      mapping <- setNames(names(v$vars_plot), v$vars_plot)
      x_value <- mapping[input$plot_type_pan]
      if (!is.null(x_value)){
        label_html <- paste0(
          katex_html(x_value, displayMode = FALSE),
          sprintf('<i class="fa-regular fa-circle-question info" style="margin-left: 5px;" data-toggle="tooltip" data-placement="right" title="%s"></i>', def)
        )
        min_box <- textInput("min_pan", label = "", value = "", width = "10%")
        div(
          style = "display: flex; align-items: center; justify-content: center;",
          min_box,
          div(HTML(katex_html("\\leq", displayMode = FALSE)),  style = "margin-left: 15px;"),
          div(HTML(label_html), style = "margin-left: 15px;"),
          div(HTML(katex_html("\\leq", displayMode = FALSE)),  style = "margin-left: 15px; margin-right: 15px;"),
          textInput("max_pan", label = "", value = "", width = "10%")
        )
        
      }
      
    })
    
    output$default_values_pan <- renderUI({
      mapping <- setNames(names(v$vars_plot), v$vars_plot)
      inputs <- lapply(names(mapping), function(var_id) {
        if (var_id != input$plot_type_pan) {
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
  output$set_arguments_pan <- renderUI({
    box(
      width = 12,
      class = "custom-box",
      title = "Set Arguments",
      uiOutput("plot_by_pan"),
      uiOutput("range_x_pan"),
      uiOutput("default_values_pan"),
      fluidRow(
        actionButton(
          inputId = "submit_pan",
          label = HTML(" <i class='fa-solid fa-floppy-disk' style='margin-left: 1px;margin-right: 1px;'></i> Submit"),
          class = "submit-btn"
        )  
      )
      
      
    )
    
  })
  
  
  observeEvent(input$submit_pan, {
    
    toggle("loader_background_pn")
    toggle("loader_pn")
    delay(3500, hide("loader_background_pn"))
    delay(3500, hide("loader_pn"))
    
    v$data <- NULL
    x_axis_values <- seq(input$min_pan, input$max_pan, length.out = 10)
    dependent_var <- v$output_var$id
    
    default_vars <- setdiff(names(v$vars_def), input$plot_type_pan)
    for (dv in default_vars){
      v[[dv]] <- as.numeric(input[[dv]])
    }
    independent_var <- input$plot_type_pan
    
    v[[independent_var]] <- x_axis_values
    
    # Initialize an empty data frame to store the results
    result_df <- data.frame(matrix(ncol = length(v$vars_eqs), nrow = length(x_axis_values)))
    colnames(result_df) <- v$vars_eqs
    
    y_axis_values <- v$pan_coefficient * v$pan_evaporation
    
    
    for (j in seq_along(v$vars_eqs)) {
      if (v$vars_eqs[j] == input$plot_type_pan) {
        result_df[, v$vars_eqs[j]] <- x_axis_values
      } else if (v$vars_eqs[j] == dependent_var) {
        result_df[, v$vars_eqs[j]] <- y_axis_values
      } else {
        result_df[, v$vars_eqs[j]] <- input[[v$vars_eqs[j]]]
      }
    }
    v$data <- result_df
    
    
    output$plot_box_pan <- renderUI({
      
      box(
        width = 12, title = "Results", class = "custom-box",
        actionButton(inputId = "reset_pan", label = HTML("<i class='fa-solid fa-arrow-rotate-right' style='margin-left: 1px;margin-right: 1px;'></i> Reset"), class = "reset-btn"),
        tabBox(
          width = NULL,
          id = "results_tab",
          title = "",
          tabPanel(
            title = "Graph",
            plotlyOutput("plot_relationship_pan")
          ),
          tabPanel(
            title = "Summary",
            tags$div(
              uiOutput("args_table_pan"),
              style = "overflow-x: auto; overflow-y: auto; width: 100%; background-color: white !important; padding: 15px;"
            )
          )
        )
      )
      
      
    })
    
    
    output$args_table_pan <- renderUI(
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
    
    output$plot_relationship_pan <- renderPlotly({
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
    hide("set_arguments_pan")
    show("plot_box_pan")
    
    
  })
  
  observeEvent(input$close,{
    removeModal()
  })
  
  observeEvent(input$reset_pan,{
    hide("plot_box_pan")
    for (rv in v$vars_eqs){
      v[[rv]] <- NULL
      if (rv != v$output_var$id){
        if (rv != input$plot_type_pan){
          updateTextInput(session, rv, value = "")
        }
        else{
          updateTextInput(session, "min_pan", value = "")
          updateTextInput(session, "max_pan", value = "")
        }
      }
      
    }
    show("set_arguments_open")
  })
  
}


