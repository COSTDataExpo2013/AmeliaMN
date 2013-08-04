twolevel <- c("Q21", "Q22A", "Q22B", "Q22C", "Q22D")

positive <- "Yes"
negative <- "No"


# Doing the yes/nos overall
dfall <- data.frame()
for (i in 1:length(twolevel)){
  tmp <- data.frame(Question=twolevel[i], prop.table(table(sotc08[,twolevel[i]])), Year="2008")
  dfall <- rbind(dfall, tmp)
}
names(dfall) <- c("Question", "Response", "Freq", "Year")
pos <- dfall[dfall$Response %in% positive,]
neg <- dfall[dfall$Response %in% negative,]
neg$Freq <- -neg$Freq
theorder <- order(pos$Freq[pos$Year==2008], decreasing=T)
pos$Question <- factor(pos$Question, levels=levels(pos$Question)[rev(theorder)])
neg$Question <- factor(neg$Question, levels=levels(neg$Question)[rev(theorder)])

baseplot <- ggplot(dfall) + aes(Question, Freq, fill = Response) +facet_wrap(~Year)
baseplot <- baseplot + geom_bar(data = neg, stat = "identity") + geom_bar(data = pos, stat = "identity")
baseplot <- baseplot +ggtitle("Yes/no questions")+scale_y_continuous(breaks=seq(from=-1, to=1, by=.125), labels=c("", "", "", "", "50%", "", "25%", "", "0%", "", "25%", "", "50%", "", "75%", "", ""))
baseplot <- baseplot + coord_flip()+ xlab("")
baseplot <- baseplot + scale_x_discrete(labels=c("Attended a local public meeting", "Worked with other residents to make change", "Performed local volunteer work", "Voted in the local election","Registered to vote"))
baseplot <- baseplot + ylab("Percentage")
baseplot
