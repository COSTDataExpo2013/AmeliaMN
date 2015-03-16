# Create a loop that calculates the percentages of cases overall
dfall <- data.frame()
for (i in 1:length(twolevel)){
  tmp <- data.frame(Question=twolevel[i], prop.table(table(sotc[,twolevel[i]])))
  dfall <- rbind(dfall, tmp)
}
names(dfall) <- c("Question", "Response", "Freq")

# Create a loop that calculates the percentages of cases in each factor level for each city
dfcities <- data.frame()
for (j in 1:length(levels(sotc$citystate))){
  thiscity <- subset(sotc, citystate==levels(sotc$citystate)[j])
  for (i in 1:length(twolevel)){
    tmp <- data.frame(levels(sotc$citystate)[j], Question=twolevel[i], prop.table(table(thiscity[,twolevel[i]])))
    dfcities <- rbind(dfcities, tmp)
  }
}
names(dfcities) <- c("citystate", "Question", "Response", "Freq")


# Lets see the difference between the overall and the cities to see where the variation comes from
dfcities$diff <- 0
for (i in 1:length(levels(dfcities$Question))){
  # Yes
  overallrate <- dfall[dfall$Question==levels(dfall$Question)[i] & dfall$Response=="Yes",]$Freq
  dfcities$diff[(dfcities$Question==levels(dfcities$Question)[i]& dfcities$Response=="Yes")] <- (overallrate - dfcities[(dfcities$Question==levels(dfcities$Question)[i] & dfcities$Response=="Yes"),]$Freq)/overallrate
}

dfcitiesYes <- dfcities[dfcities$Response=="Yes",]
dfcitiesYes$col <- dfcitiesYes$dif>0
dfcitiesYes$Question <- factor(dfcitiesYes$Question, levels=levels(dfcitiesYes$Question)[order(dfall[dfall$Response=="Yes",]$Freq)])


baseplot <- ggplot(dfcitiesYes) + aes(Question, diff, fill = col) +facet_wrap(~citystate) + geom_bar()
baseplot <- baseplot +coord_flip()+ggtitle("Percentage difference from overall survey rates")
baseplot <- baseplot + guides(fill=FALSE)+scale_y_continuous(breaks=seq(from=-.5, to=.5, by=.125),labels=c("50%","", "25%","", "0","", "25%","", "50%"))
baseplot <- baseplot + ylab("") +xlab("")
baseplot <- baseplot + scale_x_discrete(labels=rev(c("Registered to vote", "Voted in the local election", "Donated money to a local organization", "Attended a local event", "Gave money or food to an individual", "Participated in a church event", "Performed local volunteer work", "Worked with other residents to make change", "Attended a local public meeting", "Provided free shelter to an individual")))
baseplot
