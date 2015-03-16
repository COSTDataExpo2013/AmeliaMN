{

# Histograms
# This example is just a histogram of the variable diamonds$table. What is x? X is just the variable tag. 
# Not related to the variable x in the diamonds data set. 
gigvis(diamonds, props(x ~ table),
       branch_histogram()
)

# Bar charts
gigvis(csat[csat$citystate=="St. Paul, MN",],
       props = props(x ~ Response, y ~ Freq, fill ~ Response),
       mark_rect(props(y2 = constant(0, scale = TRUE), width = band())),
       scales = scales(scale_ordinal("x", range = "width", padding = 0, points = FALSE))
)
}
# What if I wanted it to cycle through all the communities?
i <- 1
mtc1 <- reactive({
  i <<- (i %% 26) + 1 
  invalidateLater(2000, NULL);
  csat[csat$citystate==levels(csat$citystate)[i],]
})
gigvis(mtc1, props(x ~ Response, y ~ Freq, fill ~ Response),
       mark_rect(props(y2 = constant(0, scale = TRUE), width = band())),
       scales = scales(scale_ordinal("x", range = "width", padding = 0, points = FALSE)),
       dynamic = TRUE
)