# server_criddle.R
criddle_server <- function(input, output, session) {
  
  v <- reactiveValues(
    data_criddle = data.frame(Month = character(0),
                          T_avg = numeric(0),
                          P = numeric(0)
    )
  )
  
  observeEvent(input$criddle_calc_choice, {
    k_c_label <- paste0(
      katex_html("K_{c}", displayMode = FALSE),
      sprintf('<i class="fa-regular fa-circle-question info" style="margin-left: 5px;" data-toggle="tooltip" data-placement="right" title="%s"></i>', "Seasonal crop coefficient for a crop with a normal growing season")
    )
    showModal(
      modalDialog(
        title = "Table",
        div(
          style = "display: flex; justify-content: space-around;",
          textInput("k_c",
                    label = HTML(k_c_label),
                    value = "",
                    width = "20%"
          ),
          sliderTextInput(
            inputId = "monthsRange",
            label = "Choose months for season: ",
            choices = month.name,
            selected = month.name[c(4, 7)]
          )
        ),
        div(
          actionButton(
            inputId = "add_criddle_rows",
            label = HTML("<i class='fa fa-table' style='margin-left: 1px;margin-right: 1px;'></i> Create Table"),
            class = "reset-btn",
            style = "margin-top: 10px;"
          )
        ),
        DTOutput("criddle_table"),
        footer = tagList(
          actionButton(
            inputId = "submit_criddle_data",
            label = HTML(" <i class='fa-solid fa-floppy-disk' style='margin-left: 1px;margin-right: 1px;'></i> Submit"),
            class = "submit-btn"
          )
        )
      )
    )
  })
  
  observeEvent(input$add_criddle_rows, {
    start_month <- input$monthsRange[1]
    end_month <- input$monthsRange[2]
    
    start_num <- match(start_month, month.name)
    end_num <- match(end_month, month.name)
    
    
    for (i in seq(start_num, end_num)) {
      month <- month.name[i]
      
      new_row <- data.frame(
        Month = month,
        T_avg = "",
        P = ""
      )
      
      v$data_criddle <- rbind(v$data_criddle, new_row)
      
    }
    
  })
  
  observeEvent(input$criddle_table_cell_edit, {
    info <- input$criddle_table_cell_edit
    
    edited_row <- as.numeric(info$row)
    edited_col <- as.numeric(info$col)
    new_value <- as.numeric(info$value)
    
    # Update the data frame with the edited value
    v$data_criddle[edited_row, edited_col] <- new_value
  })
  
  output$criddle_table <- renderDT({
    datatable(
      v$data_criddle,
      editable = TRUE,
      options = list(
        pageLength = 5 
      )
    )
  })
  
  
  observeEvent(input$submit_criddle_data, {
    
    toggle("loader_background_cr")
    toggle("loader_cr")
    delay(3500, hide("loader_background_cr"))
    delay(3500, hide("loader_cr"))
    
    output$criddle_results <- renderUI({
      
      K_c <- as.numeric(input$k_c)
      
      ET_c <- K_c * sum((as.numeric(v$data_criddle$T_avg) * as.numeric(v$data_criddle$P)) / 100)
      
      n_rows <- nrow(v$data_criddle)
      
      start_month <- input$monthsRange[1]
      end_month <- input$monthsRange[2]
      
      individual_values <- sapply(1:n_rows, function(i) {
        return(paste0("\\frac{", as.character(v$data_criddle$T_avg[i]), "(", as.character(v$data_criddle$P[i]), ")", "}{100}"))
      }, USE.NAMES = FALSE)
      
      total_expression <- paste("ET_{c} = K_{c}\\sum_{i=1}^{", n_rows, "} ET_{o}", " = ", "K_{c}\\sum_{i=1}^{", n_rows, "} \\frac{t_{i}P_{i}}{100}", " = ", K_c , "[", paste(individual_values, collapse = " + "), "]", " = ", ET_c , "in.")
      
      div(
        style="margin-top: 20px;",
        p(
          HTML(paste(
            "For the crop with seasonal crop coefficient ",
            katex_html("K_{c}", displayMode = FALSE),
            " equal to ",
            K_c,
            " the predicted crop ",
            katex_html("ET_{c}", displayMode = FALSE),
            "equals ",
            ET_c,
            " in. ",
            "for the growing season from ",
            start_month, 
            " to ",
            end_month
          ))
        ),
        p(
          HTML(paste(
            katex_html("ET_{c}", displayMode = FALSE),
            "was calculated as follows: "
          ))
        ),
        HTML(katex_html(total_expression))
      )
      
    })
    
    removeModal()
    
    
  })
  
}

