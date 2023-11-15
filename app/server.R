server <- function(input, output) {
  # Example plot outputs
  output$plot1 <- renderPlot({
    plot(cars)
  })
  
  output$plot2 <- renderPlot({
    plot(pressure)
  })
}