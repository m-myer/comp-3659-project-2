server <- function(session, input, output) {
  
  output$header <- renderUI({
    tagList(
      h3(paste("Selected Algorithm: ", input$schedulingChoices))
    )
  })
  
}