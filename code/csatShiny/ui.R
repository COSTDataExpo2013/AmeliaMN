shinyUI(bootstrapPage(
  selectInput(inputId = "citystate",
              label = "Community:",
              choices =  levels(csat$citystate),
              selected = levels(csat$citystate)[1]),
  plotOutput(outputId = "main_plot", width = "1000px", height="600px")
))