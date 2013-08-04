shinyServer(function(input, output) {
  output$main_plot <- renderPlot({
   print(ggplot(csat[csat$citystate==input$citystate,]) + aes(x=Response, y=Freq, fill=Response)+ geom_bar())
  })
})