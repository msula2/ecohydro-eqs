server <- function(input, output, session) {
  v <- reactiveValues(
    vars_plot = NULL,
    vars_def = NULL
  )
  
  observeEvent(input$navbar,{
    tab <- input$navbar
    if (tab == "Penman-Monteith") {
      vars_ids <- list("net_radiation", "ground_heat_flux", "latent_heat", "psy_constant", "wind_speed", "vapor_sat", "vapor_act")
      vars_text <- list("R_n", "G", "\\lambda", "\\gamma", "u_2", "e_s", "e_d")
      v$vars_def <- list("net_radiation" = "Net Radiation", "ground_heat_flux"="Ground Heat Flux", "latent_heat"="Latent Heat of Vaporation", "psy_constant"="Psychometric Constant", "wind_speed"="Wind Speed 2m above ground", "vapor_sat"="Saturation Vapor Pressure", "vapor_act"= "Actual Vapor Pressure")
      v$vars_plot <- setNames(vars_ids, vars_text)
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
  
  output$definition <- renderUI({
    def <- v$vars_def[input$plot_type]
    label_html <- sprintf('<i class="fa-regular fa-circle-question info" data-toggle="tooltip" data-placement="right" title="%s"></i>', def)
    div(
      HTML(label_html)
    )
    
  })
}
