# Pick out the appropriate variables

sat10 <- sotc10$QCE1
sat9 <- sotc09$QCE1
sat8 <- sotc08$QCE1
sat10 <- factor(sat10, levels(sat10)[c(5, 1:3, 4)])
sat9 <- factor(sat9, levels(sat9)[c(5, 1:3, 4)])
sat8 <- sat8[sat8 != "(DK)" & sat8 != "(Not applicable)" & sat8 !="(Refused)"]
sat8 <- factor(sat8)
sat8 <- factor(sat8, levels(sat9)[c(5, 1:3, 4)])

# Which factor levels are positive?
neutral <- c("3")
positive <- c("Extremely satisfied", "4")
negative <- c("2", "Not at all satisfied")




dfall <- data.frame()
# Did this by hand because writing the correct loop would have been annoying
tmp <- data.frame(Year="2010", prop.table(table(sat10)))
names(tmp) <-c("Year", "Response", "Freq")
dfall <- rbind(dfall, tmp)
names(dfall) <- c("Year", "Response", "Freq")
dfall$Response <- factor(dfall$Response, levels=levels(dfall$Response)[c(4:1, 5)])

neu <- dfall[dfall$Response=="3",]
neu$Freq <- neu$Freq/2

pos <- dfall[dfall$Response %in% positive,]
pos <- rbind(pos, neu)
neg <- dfall[dfall$Response %in% negative,]
neg <- rbind(neg, neu)
neg$Freq <- -neg$Freq
neg$Response <- factor(neg$Response)
neg$Response <- factor(neg$Response, levels=rev(levels(neg$Response)))
pos$Response <- factor(pos$Response)
pos$Response <- factor(pos$Response, levels=levels(pos$Response)[c(2,1,3)])


baseplot <- ggplot(dfall) + aes(Year, Freq, fill = Response, order=Response)
baseplot <- baseplot  +geom_bar(data = neg, stat = "identity") + geom_bar(data = pos, stat = "identity")
baseplot <- baseplot +coord_flip() + ggtitle("Community satisfaction over time") + xlab("") +ylab("")
baseplot <- baseplot + scale_y_continuous(breaks=seq(from=-0.3, to=0.9, by=0.15), labels=percent)
baseplot

baseplot <- ggplot(sotc) + aes(citystate, fill=community_sat) +geom_bar()

# What about by community in 2010
neutral <- c("3")
positive <- c("Extremely satisfied", "4")
negative <- c("2", "Not at all satisfied")



dfall <- data.frame()
for (i in 1:length(levels(sotc10$QSB))){
  tmp <- data.frame(citystate=levels(sotc10$QSB)[i], prop.table(table(sotc10[sotc10$QSB==levels(sotc10$QSB)[i],]$QCE1)))
  dfall <- rbind(dfall, tmp)
}
names(dfall) <- c("citystate", "Response", "Freq")

dfall$Response <- factor(dfall$Response, levels=levels(dfall$Response)[c(5, 1:3, 4)])


ordering <- 0
for (i in 1:length(levels(dfall$citystate))){
  ordering[i] <- sum(dfall[dfall$citystate==levels(dfall$citystate)[i] & dfall$Response != "Not at all satisfied" & dfall$Response != "2" & dfall$Response != "3",]$Freq)
  ordering[i] <- ordering[i] + (dfall[dfall$citystate==levels(dfall$citystate)[i] & dfall$Response == "3",]$Freq/2)
}


dfall$citystate <- factor(dfall$citystate, levels=levels(dfall$citystate)[order(ordering)])




neu <- dfall[dfall$Response=="3",]
neu$Freq <- neu$Freq/2
pos <- dfall[dfall$Response %in% positive,]
pos <- rbind(pos, neu)
neg <- dfall[dfall$Response %in% negative,]
neg <- rbind(neg, neu)
neg$Freq <- -neg$Freq
neg$Response <- factor(neg$Response)
neg$Response <- factor(neg$Response, levels=rev(levels(neg$Response)))
pos$Response <- factor(pos$Response)


                          
baseplot <- ggplot(dfall) + aes(citystate, Freq, fill = Response, order=Response)
baseplot <- baseplot  +geom_bar(data = neg, stat = "identity") + geom_bar(data = pos, stat = "identity")
baseplot <- baseplot +coord_flip() + ggtitle("Community satisfaction in 2010") + xlab("") +ylab("")
baseplot <- baseplot + scale_y_continuous(breaks=seq(from=-0.5, to=0.75, by=0.25), labels=percent)
baseplot