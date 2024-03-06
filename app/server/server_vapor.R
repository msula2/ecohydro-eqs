# server_vapor.R
vapor_server <- function(input, output, session) {
  
  delay(3500, hide("loader_background_vp"))
  delay(3500, hide("loader_vp"))
  
  v <- reactiveValues(
    data_vpd = data.frame(Hour = numeric(0),
                          T = numeric(0),
                          ed = numeric(0),
                          RH = numeric(0)
    )
  )
  
  
  observeEvent(input$vpd_calc_choice,{
    updateSelectizeInput(
      session, "ed_arg",
      choices = list("RH" = "relative_humidity","T_{d}"="t_dew"),
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
    choice <- input$vpd_calc_choice
    if (choice == "arguments"){
      showModal(
        modalDialog(
          title = "Calculation",
          div(
            style = "display: flex; justify-content: space-around;",
            div(
              selectizeInput("ed_arg", label =  HTML(paste("Calculate ", katex_html("e_{d}", displayMode = FALSE), " using:")), choices = NULL, width = "100%")
            ),
            uiOutput("ed_args")
          ),
          footer = tagList(
            actionButton(
              inputId = "submit_vpd_args",
              label = HTML("<i class='fa-solid fa-floppy-disk' style='margin-left: 1px;margin-right: 1px;'></i> Submit"),
              class = "submit-btn"
            )
          )
        )
      )
    }
    else{
      showModal(
        modalDialog(
          title = "Table",
          div(
            style = "display: flex; justify-content: space-between;",
            sliderInput("timeRange", label = "Choose the time range for your observations",
                        min = as.POSIXct("2011-06-04 01:00:00", tz = ""),
                        max = as.POSIXct("2011-06-04 24:00:00", tz = ""),
                        timeFormat = "%I %p",
                        value = c(as.POSIXct("2011-06-04 01:00:00", tz = ""),
                                  as.POSIXct("2011-06-04 12:00:00", tz = "")),
                        step = 3600),
            div(
              actionButton(
                inputId = "add_vpd_rows",
                label = HTML("<i class='fa fa-table' style='margin-left: 1px;margin-right: 1px;'></i> Create Table"),
                class = "reset-btn",
                style = "margin-top: 10px;"
              )
            )
          ),
          DTOutput("vpd_table"),
          footer = tagList(
            actionButton(
              inputId = "submit_vpd_data",
              label = HTML(" <i class='fa-solid fa-floppy-disk' style='margin-left: 1px;margin-right: 1px;'></i> Submit"),
              class = "submit-btn"
            )
          )
        )
      )
    }
  })
  
  output$vpd_table <- renderDT({
    datatable(
      v$data_vpd,
      editable = TRUE,
      options = list(
        pageLength = 5 
      )
    )
  })
  
  
  observeEvent(input$add_vpd_rows, {
    start_time <- input$timeRange[1]
    end_time <- input$timeRange[2]
    
    start_time_local <- format(start_time, "%I:%M %p", tz = Sys.timezone())
    end_time_local <- format(end_time, "%I:%M %p", tz = Sys.timezone())
    
    num_rows <- as.numeric(difftime(end_time, start_time, units = "hours"))
    
    for (i in seq(0, num_rows)) {
      time <- start_time + i * 3600
      
      new_row <- data.frame(
        Hour = format(time, "%I:%M %p", tz = Sys.timezone()),
        T = "",
        ed = "",
        RH = ""
      )
      
      v$data_vpd <- rbind(v$data_vpd, new_row)
      
    }
    
    
    
    
    
  })
  
  output$ed_args <- renderUI({
    mapping <- list("relative_humidity" = "RH", "t_dew" = "T_{d}")
    definitions <- list("relative_humidity" = "Relative humidity in percentage", "t_dew" = "Dew temperature in degree Celsius")
    var_id <- input$ed_arg
    
    if (var_id %in% names(mapping)) {
      katex_exp <- mapping[[var_id]]
      label_html <- paste0(
        katex_html(katex_exp, displayMode = FALSE),
        sprintf('<i class="fa-regular fa-circle-question info" style="margin-left: 5px;" data-toggle="tooltip" data-placement="right" title="%s"></i>', definitions[var_id])
      )
      t_avg_label <- paste0(
        katex_html("T_{avg}", displayMode = FALSE),
        sprintf('<i class="fa-regular fa-circle-question info" style="margin-left: 5px;" data-toggle="tooltip" data-placement="right" title="%s"></i>', "Average temperature in degrees Celcuis")
      )
      div(
        style="display: flex; justify-content: space-around; align-items: center; margin-top: 12px;",
        textInput("t_avg",
                  label = HTML(t_avg_label),
                  value = "",
                  width = "20%"
        ),
        textInput(var_id,
                  label = HTML(label_html),
                  value = "",
                  width = "20%"
        )
      )
    }
  })
  observeEvent(input$submit_vpd_args,{
    
    toggle("loader_background_vp")
    toggle("loader_vp")
    delay(3500, hide("loader_background_vp"))
    delay(3500, hide("loader_vp"))
    
    ed_arg <- input$ed_arg
    T <- as.numeric(input$t_avg)
    es <- exp((16.78 * T - 116.9) / (T + 237.3))
    if(ed_arg == "relative_humidity"){
      RH <- as.numeric(input$relative_humidity)
      ed <- es * (RH / 100)
      
    }
    else {
      td <- as.numeric(input$t_dew)
      ed <- exp((16.78 * td - 116.9) / (td + 237.3))
    }
    v[["e_d"]] = ed
    v[["e_s"]] = es
    v[["t_avg"]] = T
    
    removeModal()
    
    output$vpd_results <- renderUI({
      
      arg <- input$ed_arg
      
      if(arg == "relative_humidity"){
        katex_arg <- HTML(katex_html(paste("RH = ", input$relative_humidity, "\\%", sep="~"), displayMode = FALSE))
        supporting_text <- HTML(paste("using Equation 3 at  ",
                                      katex_html(paste("RH = ", input$relative_humidity, "\\%", sep="~"), displayMode = FALSE)))
      }
      else{
        katex_arg <- HTML(katex_html(paste("T_{d} = ", input$t_dew, "°C", sep="~"), displayMode = FALSE))
        supporting_text <- paste("using Equation 2 at ", 
                                 katex_html(paste("T_{d} = ", input$t_dew, "°C", sep="~"), displayMode = FALSE))
      }  
      
      div(
        style="margin-top: 20px;",
        p(
          "Given that: "
        ),
        p(
          style = "text-align: center;",
          katex_arg
        ),
        p(
          "We first determine: "
        ),
        p(
          style = "text-align: center;",
          HTML(paste(
            katex_html(paste("e_{s} = ", v$e_s, "kPa~", sep="~"), displayMode = FALSE), 
            "using Equation 1 at ", 
            katex_html(paste("T_{avg} = ", input$t_avg, "°C", sep="~"), displayMode = FALSE))
          )
        ),
        p(
          "Next, we determine: "
        ),
        p(
          style = "text-align: center;",
          HTML(paste( 
            katex_html(paste("e_{d} = ", v$e_d, "kPa~", sep="~"), displayMode = FALSE),
            supporting_text
          ))
        ),
        p(
          "Hence the vapor pressure deficit is calculated as "
        ),
        p(
          HTML(katex_html(paste("e_{s} - e_{d}", " = ", v$e_s - v$e_d, "~kPa", sep = "~")))
        )
      )
    })
    
  })
  
  observeEvent(input$vpd_table_cell_edit, {
    info <- input$vpd_table_cell_edit
    
    edited_row <- as.numeric(info$row)
    edited_col <- as.numeric(info$col)
    new_value <- as.numeric(info$value)
    
    # Update the data frame with the edited value
    v$data_vpd[edited_row, edited_col] <- new_value
  })
  
  
  observeEvent(input$submit_vpd_data, {
    
    toggle("loader_background_vp")
    toggle("loader_vp")
    delay(3500, hide("loader_background_vp"))
    delay(3500, hide("loader_vp"))
    
    max_T <- max(as.numeric(v$data_vpd$T))
    print(max_T)
    min_T <- min(as.numeric(v$data_vpd$T))
    Tavg <- (max_T + min_T) / 2
    avg_ed <- mean(as.numeric(v$data_vpd$ed), na.rm = TRUE)
    
    es <- exp((16.78 * Tavg - 116.9) / (Tavg + 237.3))
    ed <- avg_ed
    
    
    v[["e_d"]] = ed
    v[["e_s"]] = es
    v[["t_max"]] = max_T
    v[["t_min"]] = min_T
    v[["t_avg"]] = Tavg
    
    print(es - ed)
    removeModal()
    
    output$vpd_results <- renderUI({
      katex_T_max <- HTML(katex_html(paste("T_{max} = ", v$t_max, "°C", sep="~"), displayMode = FALSE))
      katex_T_min <- HTML(katex_html(paste("T_{min} = ", v$t_min, "°C", sep="~"), displayMode = FALSE))
      katex_arg <- HTML(paste(katex_T_max, ", " ,katex_T_min))
      
      div(
        style="margin-top: 20px;",
        p(
          "From the table:"
        ),
        p(
          style = "text-align: center;",
          katex_arg
        ),
        p(
          "We first calculate : "
        ),
        p(
          HTML(katex_html(paste("T_{avg} = ", v$t_avg, "°C", sep="~")))
        ),
        p(
          "Using : "
        ),
        p(
          HTML(katex_html("T_{avg} = \\frac{T_{max} + T_{min}}{2}"))
        ),
        p(
          "Next, we determine: "
        ),
        p(
          style = "text-align: center;",
          HTML(paste(
            katex_html(paste("e_{s} = ", v$e_s, "kPa~", sep="~"), displayMode = FALSE), 
            "using Equation 1 at ", 
            katex_html(paste("T_{avg} = ", v$t_avg, "°C", sep="~"), displayMode = FALSE))
          )
        ),
        p(
          "We can calculate: "
        ),
        p(
          style = "text-align: center;",
          HTML(paste( 
            katex_html(paste("e_{d} = ", v$e_d, "kPa~", sep="~"), displayMode = FALSE),
            " by averaging the values entered in the table"            
          ))
        ),
        p(
          "Hence the vapor pressure deficit is calculated as "
        ),
        p(
          HTML(katex_html(paste("e_{s} - e_{d}", " = ", v$e_s - v$e_d, "~kPa", sep = "~")))
        )
      )
    })
    
  })
  
}

