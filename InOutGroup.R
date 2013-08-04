# The goal for this plot is to show the difference between the responses to questions by people in a particular group and
# those outside the group. 
# The groups are: seniors (55+), variable age and variable q8f, families with young children, variable qd4 and q8e, minorities, variable qd111, 

# Just in the 2010 data, removing the NAs
sotc10 <- read.csv("sotc10.csv")
noNA <- subset(sotc10, Q3A != "NA")

# Lets try with seniors first
{
noNA$Q8F <- factor(noNA$Q8F, levels(noNA$Q8F)[c(4, 1:3, 5)])
noNA$seniors <- noNA$QD1 >= 55



# Which factor levels are positive?
neutral <- c("3")
positive <- c("Very good", "4")
negative <- c("2", "Very bad")

{
seniors <- data.frame()
# Did this by hand because writing the correct loop would have been annoying
tmp <- data.frame("Not seniors", prop.table(table(noNA[noNA$seniors==FALSE,]$Q8F)))
names(tmp) <-c("InGroup", "Response", "Freq")
seniors <- rbind(seniors, tmp)


neu <- seniors[seniors$Response=="3",]
neu$Freq <- neu$Freq/2

pos <- seniors[seniors$Response %in% positive,]
pos <- rbind(pos, neu)
neg <- seniors[seniors$Response %in% negative,]
neg <- rbind(neg, neu)
neg$Freq <- -neg$Freq
neg$Response <- factor(neg$Response)
neg$Response <- factor(neg$Response, levels=rev(levels(neg$Response)))
pos$Response <- factor(pos$Response)
# pos$Response <- factor(pos$Response, levels=levels(pos$Response)[c(2,1,3)])

require(ggplot2)
require(scales)
baseplot <- ggplot(seniors) + aes(InGroup, Freq, fill = Response, order=Response)
baseplot <- baseplot  +geom_bar(data = neg, stat = "identity") + geom_bar(data = pos, stat = "identity")
baseplot <- baseplot +coord_flip()
baseplot <- baseplot + scale_y_continuous(breaks=seq(from=-0.25, to=0.75, by=0.25), labels=percent)
baseplot <- baseplot + ggtitle("How are seniors treated in your community?") + xlab("") + ylab("")
baseplot



}



# Need a loop to go over all the cities with seniors
sencities <- data.frame()
bothchoices <- c("TRUE", "FALSE")
for (j in 1:length(levels(noNA$QSB))){
  thiscity <- subset(noNA, QSB==levels(noNA$QSB)[j])
  for (i in 1:2){
    tmp <- data.frame(levels(noNA$QSB)[j], Question=bothchoices[(i %% 2)+1], prop.table(table(thiscity[thiscity$seniors==bothchoices[(i %% 2)+1],]$Q8F)))
    sencities <- rbind(sencities, tmp)
  }
}
names(sencities) <- c("citystate",  "InGroup","Response", "Freq")




neu <- sencities[sencities$Response=="3",]
neu$Freq <- neu$Freq/2

pos <- sencities[sencities$Response %in% positive,]
pos <- rbind(pos, neu)
neg <- sencities[sencities$Response %in% negative,]
neg <- rbind(neg, neu)
neg$Freq <- -neg$Freq
neg$Response <- factor(neg$Response)
neg$Response <- factor(neg$Response, levels=rev(levels(neg$Response)))
pos$Response <- factor(pos$Response)
# pos$Response <- factor(pos$Response, levels=levels(pos$Response)[c(2,1,3)])

require(ggplot2)
require(scales)
baseplot <- ggplot(sencities) + aes(InGroup, Freq, fill = Response, order=Response) +facet_wrap(~citystate)
baseplot <- baseplot  +geom_bar(data = neg, stat = "identity") + geom_bar(data = pos, stat = "identity")
baseplot <- baseplot +coord_flip()
baseplot <- baseplot + scale_y_continuous(breaks=seq(from=-0.25, to=1, by=0.25), labels=percent) + scale_x_discrete(labels=c("Non-seniors", "Seniors"))
baseplot <- baseplot + ggtitle("How are seniors treated in your community?") + xlab("") + ylab("")
baseplot


# # How are seniors treated?
# seniorplot <- ggplot(noNA, aes(x=Q8F))+geom_bar()+facet_wrap(~QSB)
# 
# 
# 
# +xlab("How seniors are treated")+ggtitle("Whole survey population beliefs about senior treatment")
# ggplot(sotc[sotc$age>=55,], aes(x=seniors))+geom_bar()
# ggplot(sotc[sotc$age<55,], aes(x=seniors))+geom_bar()
}



# Families with children
noNA$Q8E <- factor(noNA$Q8E)
noNA$Q8E <- factor(noNA$Q8E, levels=levels(noNA$Q8E)[c(4, 1:3, 5)])

kidsData <- noNA[noNA$QD4=="No" | noNA$QD4=="Yes",]
kidsData$QD4 <- factor(kidsData$QD4)


kidcities <- data.frame()
bothchoices <- c("No", "Yes")
for (j in 1:length(levels(noNA$QSB))){
  thiscity <- subset(kidsData, QSB==levels(kidsData$QSB)[j])
  for (i in 1:2){
    tmp <- data.frame(levels(kidsData$QSB)[j], Question=bothchoices[(i %% 2)+1], prop.table(table(thiscity[thiscity$QD4==bothchoices[(i %% 2)+1],]$Q8E)))
    kidcities <- rbind(kidcities, tmp)
  }
}
names(kidcities) <- c("citystate", "InGroup", "Response", "Freq")
kidcities$InGroup <- factor(kidcities$InGroup, levels=rev(levels(kidcities$InGroup)))

neu <- kidcities[kidcities$Response=="3",]
neu$Freq <- neu$Freq/2

pos <- kidcities[kidcities$Response %in% positive,]
pos <- rbind(pos, neu)
neg <- kidcities[kidcities$Response %in% negative,]
neg <- rbind(neg, neu)
neg$Freq <- -neg$Freq
neg$Response <- factor(neg$Response)
neg$Response <- factor(neg$Response, levels=rev(levels(neg$Response)))
pos$Response <- factor(pos$Response)



baseplot <- ggplot(kidcities) + aes(InGroup, Freq, fill = Response, order=Response) +facet_wrap(~citystate)
baseplot <- baseplot  +geom_bar(data = neg, stat = "identity") + geom_bar(data = pos, stat = "identity")
baseplot <- baseplot +coord_flip()
baseplot <- baseplot + scale_y_continuous(breaks=seq(from=-0.5, to=0.75, by=0.25), labels=percent) + scale_x_discrete(labels=c("Kids", "No kids"))
baseplot <- baseplot + ggtitle("How is your community for families with young children?") + xlab("") + ylab("")
baseplot



# Minorities
{
minorities <- noNA[noNA$QD111 != " " & noNA$QD111 != "4" & noNA$QD111 != "Don't Know" ,]
# Tried taking out Refused, but then there were communities with almost no data. Assuming Refused is White, or at least is someone
# unwilling to label themself a minority on a telephone survey. 

minorities$QD111 <- factor(minorities$QD111)
minorities$min <- (minorities$QD111 != "White" & minorities$QD111 != "Refused")
minorities$min <- factor(minorities$min)
minorities$Q8C <- factor(minorities$Q8C, levels=levels(minorities$Q8C)[c(4, 1:3, 5)])

mincities <- 0
bothchoices <- c("FALSE", "TRUE")
for (j in 1:length(levels(minorities$QSB))){
  thiscity <- subset(minorities, QSB==levels(minorities$QSB)[j])
  for (i in 1:2){
    tmp <- data.frame(levels(minorities$QSB)[j], Question=bothchoices[(i %% 2)+1], prop.table(table(thiscity[thiscity$min==bothchoices[(i %% 2)+1],]$Q8C)))
    mincities <- rbind(mincities, tmp)
  }
}
names(mincities) <- c("citystate", "InGroup", "Response", "Freq")
mincities <- mincities[-1,]


neu <- mincities[mincities$Response=="3",]
neu$Freq <- neu$Freq/2

pos <- mincities[mincities$Response %in% positive,]
pos <- rbind(pos, neu)
neg <- mincities[mincities$Response %in% negative,]
neg <- rbind(neg, neu)
neg$Freq <- -neg$Freq
neg$Response <- factor(neg$Response)
neg$Response <- factor(neg$Response, levels=rev(levels(neg$Response)))
pos$Response <- factor(pos$Response)


baseplot <- ggplot(mincities) + aes(InGroup, Freq, fill = Response, order=Response) +facet_wrap(~citystate)
baseplot <- baseplot  +geom_bar(data = neg, stat = "identity") + geom_bar(data = pos, stat = "identity")
baseplot <- baseplot +coord_flip()
baseplot <- baseplot + scale_y_continuous(breaks=seq(from=-0.5, to=0.75, by=0.25), labels=percent) + scale_x_discrete(labels=c("Minority", "Not a minority"))
baseplot <- baseplot + ggtitle("How is your community for racial and ethnic minorities?") + xlab("") + ylab("")
baseplot

# Lets look at the distributions of racial identifications over the cities just to see what's going on
# ggplot(noNA[noNA$QD111 != "Refused",])+ggtitle("Distribution without 'Refused' from 2010 survey")
raceOver <- ggplot(noNA) + aes(QD111, fill=QD111) + geom_bar() #+ facet_wrap(~QSB) 
raceOver <- raceOver + xlab("")
raceOver <- raceOver +guides(fill=guide_legend(title="Race"))+scale_x_discrete(labels="")+ggtitle("Overall distribution of race from 2010 survey")
raceOver

}

# Difference in perception about kids

kidcities$Response <- factor(kidcities$Response, levels=levels(kidcities$Response)[c(2, 3,4,1,5)])

percepdif <- data.frame(levels(kidcities$citystate))
for (i in 1:length(levels(kidcities$citystate))){
  tmp <- subset(kidcities, Response %in% c("3", "4", "Very good") & citystate==levels(citystate)[i])
  percepdif$dif[i] <- sum(tmp[tmp$InGroup=="Yes",]$Freq)-sum(tmp[tmp$InGroup=="No",]$Freq)
}

popdata <- dget("popdata.robj")
percepdif$oh10 <- (popdata$CENSUS2010POP-popdata$POPESTIMATE2009)/popdata$CENSUS2010POP
names(percepdif) <- c("citystate", "dif", "oh10")



kidrelate <- ggplot(percepdif, aes(x=dif, y=oh10, label=citystate)) + geom_point() +geom_smooth(method=lm)
kidrelate <- kidrelate + xlab("Difference between perception of community as child-friendly by people with childrn versus those without")+ylab("Population growth in 2010")
kidrelate <- kidrelate + geom_text(data = percepdif[percepdif$citystate == "Milledgeville, GA" ,], aes(x=dif,y=oh10, label = citystate),hjust=0, vjust=0)
kidrelate <- kidrelate + geom_text(data = percepdif[percepdif$citystate == "Aberdeen, SD" ,], aes(x=dif,y=oh10, label = citystate),hjust=0, vjust=0)
kidrelate <- kidrelate + geom_text(data = percepdif[percepdif$citystate == "Detroit, MI" ,], aes(x=dif,y=oh10, label = citystate),hjust=0, vjust=0)
kidrelate



ggplot(noNA) + geom_bar(aes(INTRO1)) + scale_x_discrete(labels=c("Female", "Male"))+xlab("")+ggtitle("Gender distribution from 2010 survey")
