# server.R

# Source the server functions for each tab
source("server_penman.R")
source("server_open.R")
source("server_vapor.R")
source("server_criddle.R")
source("server_pan.R")

# Define the main server function
shinyServer(function(input, output, session) {
  
  observe({
    # Get the currently selected tab
    current_tab <- input$navbar
    
    # Call the corresponding server function based on the selected tab
    if (current_tab == "Penman's Method") {
      penman_server(input, output, session)
    } else if (current_tab == "Meyer's Open Water Evaporation") {
      open_server(input, output, session)
    } else if (current_tab == "Vapor Pressure Deficit") {
      vapor_server(input, output, session)
    } else if (current_tab == "Blaney Criddle Method"){
      criddle_server(input, output, session)
    }
    else if (current_tab == "Pan Evaporation"){
      pan_server(input, output, session)
    }
  })
})
