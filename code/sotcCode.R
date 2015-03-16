sotc10 <- read.csv("sotc10.csv")
origNames <- names(sotc10)

#sotc <- sotc10[sample(dim(sotc10)[1], 1000),]
sotc <- sotc10
sotc <- noNA

#Cleaning and naming the data
{There are a few repeated columns, or columns filled with NAs.
sotc$QS3_02 <- NULL
sotc$QS5_2 <- NULL
sotc$Q7J <- NULL
sotc$Q12 <- NULL
sotc$Q16B <- NULL
sotc$Q16C <- NULL
sotc$Q16D <- NULL
sotc$Q17 <- NULL
sotc$Q27 <- NULL
sotc$Q28 <- NULL
sotc$Q29 <- NULL
sotc$Q30 <- NULL
sotc$Q31A <- NULL
sotc$Q31B <- NULL
sotc$Q31C <- NULL
sotc$Q31D <- NULL
sotc$Q31E <- NULL
sotc$Q31F <- NULL
sotc$Q31G <- NULL
sotc$Q31H <- NULL
sotc$QD3 <- NULL
sotc$Q15VERA <- NULL
sotc$Q15VERB <- NULL
sotc$Q15VERAS <- NULL
sotc$Q15VERBS <- NULL
sotc$QD111R <- NULL
sotc$QD112R <- NULL
sotc$QD113R <- NULL
# Recoding "Yes, [] respondent available" to gender. 
levels(sotc$INTRO1)<- c("Female", "Male")

names(sotc)[2] <- "gender"
names(sotc)[3] <- "citystate"
names(sotc)[4] <- "county"
names(sotc)[5] <-  "zipcode"
names(sotc)[6] <- "age"
names(sotc)[7] <- "area_descrip"
names(sotc)[8] <- "further"
names(sotc)[9] <- "current_life"
names(sotc)[10] <- "future_life"
names(sotc)[11] <- "community_sat"
names(sotc)[12] <- "recommend"
names(sotc)[13] <- "proud"
names(sotc)[14] <- "perfect"
names(sotc)[15] <- "reputation"
names(sotc)[16] <- "problem1"
names(sotc)[17] <- "problem2"
names(sotc)[18] <- "problem3"
names(sotc)[19] <- "ratherlive"
names(sotc)[20] <- "five_years_ago"
names(sotc)[21] <- "five_years_future"
names(sotc)[22] <- "parks"
names(sotc)[23] <- "beauty"
names(sotc)[24] <- "highways"
names(sotc)[25] <- "affordable_housing"
names(sotc)[26] <- "job_opportunites"
names(sotc)[27] <- "public_schools"
names(sotc)[28] <- "colleges"
names(sotc)[29] <- "nightlife"
names(sotc)[30] <- "friends"
names(sotc)[31] <- "heathcare"
names(sotc)[32] <- "leadership"
names(sotc)[33] <- "care"
names(sotc)[34] <- "police"
names(sotc)[35] <- "arts"
names(sotc)[36] <- "community_events"
names(sotc)[37] <- "talented_grads"
names(sotc)[38] <- "immigrants"
names(sotc)[39] <- "minorities"
names(sotc)[40] <- "families_kids"
names(sotc)[41] <- "gay_lesbian"
names(sotc)[42] <- "seniors"
names(sotc)[43] <- "young_adults"
names(sotc)[44] <- "economy"
names(sotc)[45] <- "econ_better"
names(sotc)[46] <- "trust_government"
names(sotc)[47] <- "employment"
names(sotc)[48] <- "job_satisfaction"
names(sotc)[49] <- "company_workforce"
names(sotc)[50] <- "enough_income"
names(sotc)[51] <- "area_hiring"
names(sotc)[52] <- "representative_leaders"
names(sotc)[53] <- "treated_respectfully"
names(sotc)[54] <- "night_safety"
names(sotc)[55] <- "crime"
names(sotc)[56] <- "crime_increasing"
names(sotc)[57] <- "registered_vote"
names(sotc)[58] <- "volunteer"
names(sotc)[59] <- "public_meeting"
names(sotc)[60] <- "voted_local"
names(sotc)[61] <- "worked_change"
names(sotc)[62] <- "church"
names(sotc)[63] <- "festival"
names(sotc)[64] <- "donated_org"
names(sotc)[65] <- "donated_individual"
names(sotc)[66] <- "donated_shelter"
names(sotc)[67] <- "impact"
names(sotc)[68] <- "clubs"
names(sotc)[69] <- "friends_nearby"
names(sotc)[70] <- "friends_friends"
names(sotc)[71] <- "family_nearby"
names(sotc)[72] <- "talk_neighbors"
names(sotc)[73] <- "years_lived"
names(sotc)[74] <- "permanent_resident"
names(sotc)[75] <- "household_adults"
names(sotc)[76] <- "children"
names(sotc)[77] <- "under6"
names(sotc)[78] <- "six_12"
names(sotc)[79] <- "thirteen_17"
names(sotc)[80] <- "married"
names(sotc)[81] <- "education"
names(sotc)[82] <- "own_home"
names(sotc)[83] <- "income"
names(sotc)[84] <- "hispanic"
names(sotc)[85] <- "race"
names(sotc)[86] <- "race2"
names(sotc)[87] <- "race3"
names(sotc)[88] <- "white_hispanic"
names(sotc)[89] <- "phone_numbers"
names(sotc)[90] <- "cell_only"
names(sotc)[91] <- "have_cell"
names(sotc)[92] <- "cell_calls"
# THERE ARE EVEN MORE VARIABLES!!!

# Okay, here's the deal. They called people up on the phone and asked them a bunch of questions (seriously, how long is this phone survey?)
# They entered the data as it was given to them (although within a pre-decided data collection scheme)
# Then, because they had CRAZILY decided to do everything on different Likert scales they recoded a bunch of data to be high-medium-low instead of numbers
# So, almost all the variables are repeated in the data set
# Then, they used the recoded variables to determine a couple of different measures, for example safety and civic involvedness. It's not clear what they did for these numbers 
}
{
sotc$current_life <- factor(sotc$current_life, levels(sotc$current_life)[c(1:2,13,3:12)])
sotc$future_life <- factor(sotc$future_life, levels(sotc$future_life)[c(1:2,13,3:12)])
sotc$community_sat <- factor(sotc$community_sat, levels(sotc$community_sat)[c(6, 5, 1:3, 4)])
sotc$seniors <- factor(sotc$seniors, levels(sotc$seniors)[c(4, 1:3, 5)])}
sotc$recommend <- factor(sotc$recommend, levels(sotc$recommend)[c(5, 1:4)])
sotc$ratherlive <- factor(sotc$ratherlive, levels(sotc$ratherlive)[c(4, 3,1,2)])

# How many questions are there at each number of items on the scale?
{
numLevels <- 0
for (i in 2:92){
  numLevels[i] <- length(levels(as.factor(sotc[,i])))
}
numLevels <- numLevels[-c(3, 4, 6, 16:18, 73, 75)]
resp <- qplot(numLevels,binwidth=1,xlab="Number of levels on the scale", ylab="Number of questions using the scale", main="Distribution of various scale lengths used in 2010 survey")
resp <- resp + scale_x_continuous(breaks=seq(from=0, to=15, by=1), limits=c(0,14))


satisfaction <- summary(sotc$community_sat)/length(sotc$community_sat)

# Tableplot, helps see missing data
{library(tabplot)
tableplot(sotc, select=c(5:10), sortCol=current_life)}

# I've been thinking about this in a couple ways. First, there's individual-level data
# Then, there's city-level data. I've used the ggmap package to find the lats and lons of the cities
# and put in the corresponding names. Probably I should have a data set that has the information that is only at the city level
# For example, the measures the Knight foundation came up with.

# Here's the lat/lon and city name data
{geocoded <- dget("latlonsotc.robj")

# Part of the process for getting it
#geocoded$citystate <- levels(sotc$citystate)
}

# Making some maps
{
geocoded$oh10 <- summary(sotc10$QSB)/popdata$CENSUS2010POP
geocoded$oh9 <- summary(sotc09$QSB)/popdata$POPESTIMATE2009
geocoded$oh8 <- summary(sotc08$QSB)/popdata$POPESTIMATE2008

bigfirst <- order(geocoded$oh8, decreasing=TRUE)
MakeMap(geocoded$lat[bigfirst], geocoded$lon[bigfirst], scaleby=geocoded$oh8[bigfirst])
symbols(x=rep(-13569284, times=3), y=c(3086741,3000000,2980000), circles=radius2, inches=0.35, add=TRUE, bg="blue")
text(rep(-13400000, times=3), c(3206741,3100000,3000000), labels=c("5%", "0.5%", "0.05%"), pos=4, cex=0.75)
}


# Decided to put it in with the sotc data so every individual would have a (crappy and inaccurate) set of coordinates
# Probably should not have taken this approach
{
# sotc$lat <- 0
# sotc$lon <- 0
# for (i in 1:length(levels(sotc$citystate))){
#   cityname <- levels(sotc$citystate)[i]
#   sotc$lat[which(sotc$citystate==cityname)] <- geocoded$lat[geocoded$citystate==cityname]
#   sotc$lon[which(sotc$citystate==cityname)] <- geocoded$lon[geocoded$citystate==cityname]
# }

# dput(sotc, file="cleanedSotc10.robj")
# sotc2 <- dget("cleanedSotc10.robj")
}
# Instead, I should be taking the city-level data and putting it in the geocoded data set
# Take a look at this:

View(sotc[,c("citystate", "THRIVING", "STRUGGLI", "SUFFERIN", "TOTALN")])

# Well, it looks like there should only be one value for each city, but check these out
length(levels(sotc$citystate))
length(levels(as.factor(sotc$SUFFERIN)))
length(levels(as.factor(sotc$THRIVING)))
length(levels(as.factor(sotc$TOTALN)))


for (i in 1:length(levels(sotc$citystate))){
  table(sotc[sotc$citystate==levels(sotc$citystate)[i],]$THRIVING)
  
}

# Interesting plot, need to think more about:
plot(table(sotc$citystate,sotc$THRIVING), las=2)

ggplot(sotc09,  aes(x=THRIVING)) + geom_bar()+ facet_wrap(~QSB) + xlab("Values for the variable 'Thriving'")+ggtitle("2009")


# Lets get the gender info for all the years

baseplot <- ggplot(dfall) + aes(Question, Freq, fill = Response) #+facet_wrap(~citystate)
baseplot <- baseplot + geom_bar(data = neg, stat = "identity") + geom_bar(data = pos, stat = "identity")

genderbreak <- ggplot(sotc)+ aes(gender, (..count..)/sum(..count..)) + geom_bar()
genderbreak <- genderbreak + geom_bar(data=sotc08)
genderbreak <- genderbreak +scale_y_continuous(breaks=seq(from=0, to=0.5, by=0.1), labels=percent)
genderbreak <- genderbreak + ylab("") +xlab("")+ ggtitle("Gender breakdown for 2010 survey")
genderbreak

# Structure of the data
str(sotc)


# Ages of respondents
barplot(tapply(sotc$age, sotc$citystate, mean), las=2)
# Looks pretty random

# How are seniors treated?
ggplot(sotc, aes(x=seniors))+geom_bar()+xlab("How seniors are treated")+ggtitle("Whole survey population beliefs about senior treatment")
ggplot(sotc[sotc$age>55,], aes(x=seniors))+geom_bar()
ggplot(sotc[sotc$age<55,], aes(x=seniors))+geom_bar()

# Could look at the same thing with minorities, etc


# Maybe one measure is how well the community agrees with the people
# Like, if the community says it's good for seniors and seniors say it's good for seniors, that's good
# But is it good or bad if the community says it's bad and the seniors say it's bad?

# Should also try the Likert scale plots. Looks cool!

baby <- sotc[1:100, c("community_sat", "seniors")]

# Yes/no questions
# We can look at all the yes/no questions together using percentages
{# Pick out the appropriate variables
twolevel <- c("registered_vote", "volunteer", "public_meeting", "voted_local", "worked_change", "church", "festival", "donated_org", "donated_individual", "donated_shelter")

# Which factor levels are positive?
positive <- "Yes"
negative <- "No"

# Doing the yes/nos overall
dfall <- data.frame()
for (i in 1:length(twolevel)){
  tmp <- data.frame(Question=twolevel[i], prop.table(table(sotc[,twolevel[i]])))
  dfall <- rbind(dfall, tmp)
}
names(dfall) <- c("Question", "Response", "Freq")
pos <- dfall[dfall$Response %in% positive,]
neg <- dfall[dfall$Response %in% negative,]
neg$Freq <- -neg$Freq
theorder <- order(pos$Freq, decreasing=T)
pos$Question <- factor(pos$Question, levels=levels(pos$Question)[rev(theorder)])
neg$Question <- factor(neg$Question, levels=levels(neg$Question)[rev(theorder)])

baseplot <- ggplot(dfall) + aes(Question, Freq, fill = Response) #+facet_wrap(~citystate)
baseplot <- baseplot + geom_bar(data = neg, stat = "identity") + geom_bar(data = pos, stat = "identity")
baseplot <- baseplot +coord_flip()+ggtitle("Yes/no questions")+scale_y_continuous(breaks=c(-1.0, -.5, 0, .5, 1),labels=c("100%","50%", "0", "50%", "100%"))
baseplot <- baseplot + xlab("")
baseplot <- baseplot + scale_x_discrete(labels=rev(c("Registered to vote", "Voted in the local election", "Donated money to a local organization", "Attended a local event", "Gave money or food to an individual", "Participated in a church event", "Performed local volunteer work", "Worked with other residents to make change", "Attended a local public meeting", "Provided free shelter to an individual")))
baseplot 

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





dfall$citystate <- "Overall"
names(dfall) <- c("Question", "Response", "Freq", "citystate")
dfcities <- rbind(dfall, dfcities)
dfcities$citystate <- factor(dfcities$citystate)
levels(dfcities$citystate) <- levels(dfcities$citystate)[c(20,1:19,21:27)]
# Subset the positive and negative responses
pos <- dfcities[dfcities$Response %in% positive,]
neg <- dfcities[dfcities$Response %in% negative,]
# pos = ddply(pos, .("citystate", "Question"), transform, ypos = cumsum(Freq) - 0.5*Freq)
# neg = ddply(neg, "Question", transform, ypos = rev(cumsum(rev(Freq)) - 0.5*rev(Freq)))
neg$Freq <- -neg$Freq
# neg$ypos <- -neg$ypos
# neg$Response <- ordered(neg$Response, levels = rev(levels(neg$Response)))
# pos$Question <- factor(pos$Question, levels=levels(pos$Question)[rev(theorder)])
# neg$Question <- factor(neg$Question, levels=levels(neg$Question)[rev(theorder)])
# Don't actually need this
#theorder <- order(pos$Freq, decreasing=T)
# pos <- pos[theorder,]
# neg <- neg[theorder,]


baseplot <- ggplot(dfcitiesYes) + aes(Question, diff, fill = col) +facet_wrap(~citystate) + geom_bar()
#baseplot <- baseplot + geom_bar(data = neg, stat = "identity") + geom_bar(data = pos, stat = "identity")
baseplot <- baseplot +coord_flip()+ggtitle("Difference from overall rates")
baseplot <- baseplot + guides(fill=FALSE)
#+scale_y_continuous(breaks=c(-1.0, -.5, 0, .5, 1),labels=c("100%","50%", "0", "50%", "100%"))
baseplot <- baseplot + ylab("") +xlab("")
baseplot <- baseplot + scale_x_discrete(labels=rev(c("Registered to vote", "Voted in the local election", "Donated money to a local organization", "Attended a local event", "Gave money or food to an individual", "Participated in a church event", "Performed local volunteer work", "Worked with other residents to make change", "Attended a local public meeting", "Provided free shelter to an individual")))
baseplot
# Something wrong with this right now, and it might not reflect the data set as a whole
}

# Frequency polygons
ggplot(sotc, aes(community_sat))+ geom_freqpoly(aes(group=gender, color=gender))


# Population data
{popdata <- dget("popdata.robj")
popdata <- popdata[,c(1,4:16)]
names(popdata)[1]<- "citystate"
row.names(popdata) <- NULL
popdata <- t(popdata)
popdata <- data.frame(popdata)
colnames(popdata) <- popdata[1,]
popdata <- popdata[2:14,]

for (i in 1:length(levels(sotc$citystate))){
  sotc$pop2010[sotc$citystate==levels(sotc$citystate)[i]] <- popdata$CENSUS2010POP[popdata$levels.sotc10.QSB==levels(sotc$citystate)[i]]
}
}

# I guess I need to think about what exactly I want to plot!!!! Seems like recasting the data might not have fixed my woes.



# Macon, GA has a bunch of data where they basically only answered one question: community_sat. Same thing with Akron, OH
# Charlotte, NC and probably Detroit, MI; Miami, FL; and on and on! Lets see what the percentage is for each community.

summary(sotc10[is.na(sotc10$QN1A),]$QSB)

4880/20271
# So about 25% of the data is essentially missing. They only answered the one question (+ demographics). Helps me focus on that question, though!
# community_sat is the crucial question. 

# Question, is there a relationship between being missing and something else? For example, did they just add a few more responses in certain cities?

# This is the only question that I want to use the full 2010 data set for. Lets do one of those Likert scale plots




popdata$oh6 <- 0
popdata$oh7oh8<- (popdata$POPESTIMATE2008-popdata$POPESTIMATE2007)/popdata$POPESTIMATE2008
popdata$oh8oh9<- (popdata$POPESTIMATE2009-popdata$POPESTIMATE2008)/popdata$POPESTIMATE2009
popdata$oh09oh10 <- (popdata$CENSUS2010POP-popdata$POPESTIMATE2009)/popdata$CENSUS2010POP
popdata$gro10 <- TRUE
popdata$gro10[popdata$oh09oh10<0] <- FALSE

tmp <- melt(popdata)
tmp <- tmp[tmp$variable=="oh09oh10" | tmp$variable=="oh8oh9" | tmp$variable=="oh7oh8" | tmp$variable=="oh6",]
names(tmp) <- c("citystate", "city", "state", "year", "value")
tmp$year <- factor(tmp$year)
tmp$year <- factor(tmp$year, levels=levels(tmp$year), labels=c("2007", "2008", "2009", "2010"))
baseplot <- ggplot(tmp, aes(x=year, y=value)) + geom_line(aes(group=citystate, color=gro10))+  geom_dl(aes(label=citystate), method="last.points")
baseplot + theme(element_text(size=8))
# ARGH! Words are too big. The right idea, though.



