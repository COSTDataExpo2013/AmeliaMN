## ----packages, echo=FALSE------------------------------------------------
require(knitr)

## ----chunkoptions, echo=FALSE, include=FALSE-----------------------------
opts_chunk$set(echo=FALSE, cache=FALSE, message=FALSE, warning=FALSE, error=FALSE, fig.align='center')
require(dplyr)
require(ggplot2)
require(scales)
require(RColorBrewer)
require(ggmap)
require(xtable)
require(reshape2)
require(forcats)

## ----readdata------------------------------------------------------------
sotc10 <- read.csv("data/sotc10.csv")
sotc09 <- read.csv("data/sotc09.csv")
sotc08 <- read.csv("data/sotc08.csv")

## ----renaming------------------------------------------------------------
renameData <- function(datasetname){
  plyr::rename(datasetname,c("INTRO1" = "gender", "QSB" = "citystate", "QS3"="county", "QS3_02"="county2", "QS3A"="zipcode", "QD1"="age", "QS4"="area_descrip", "QS5"="further", "QS5_2"="further2", "QN1A"="current_life", "QN1B"="future_life", "QCE1"="community_sat","QCE2"="recommend","Q3A"="proud", "Q3B"="perfect", "Q3C"="reputation", "Q4_1"="problem_1", "Q4_2"="problem_2", "Q4_3"="problem_3", "Q4_1_1"="problem_1", "Q4_1_2"="problem_2", "Q4_1_3"="problem_3","Q4_2_1"="problem_4", "Q4_2_2"="problem_5", "Q4_2_3"="problem_6","Q4_3_1"="problem_7", "Q4_3_2"="problem_8", "Q4_3_3"="problem_9","Q5"="rather_live", "Q6"= "five_years_ago", "Q6A" = "five_years_future", "Q7A"= "parks", "Q7B"= "beauty", "Q7C"= "highways", "Q7D"= "affordable_housing", "Q7E" =  "job_opportunites", "Q7F"=  "public_schools", "Q7G" = "colleges", "Q7H"= "nightlife", "Q7I"= "friends", "Q7J" = "raise_kids", "Q7K" = "heathcare", "Q7L" ="leadership", "Q7M" = "care", "Q7N"= "police", "Q7O" = "arts", "Q7P" =  "community_events", "Q8A" = "talented_grads", "Q8B" = "immigrants", "Q8C"= "minorities", "Q8D"= "families_kids", "Q8E" = "gay_lesbian", "Q8F" = "seniors", "Q8G" = "young_adults", "Q9" = "economy", "Q10" = "econ_better", "Q10A" = "trust_government", "Q11" = "employment", "Q12" = "commute", "Q13" = "job_satisfaction", "Q14" = "company_workforce", "Q15" = "enough_income", "Q15AA" = "area_hiring", "Q15AB" = "representative_leaders", "Q16A" = "treated_respectfully", "Q16B" = "well_rested", "Q16C" = "high_stress", "Q16D" = "learned_yest", "Q17" = "stress_source", "Q18" = "night_safety", "Q19" = "crime", "Q20" = "crime_increasing", "Q21"=  "registered_vote", "Q22A" = "volunteer", "Q22B" = "public_meeting", "Q22C"= "voted_local", "Q22D" = "worked_change", "Q22E" = "church", "Q22F" = "festival", "Q22G" = "donated_org", "Q22H" = "donated_individual", "Q22I" = "donated_shelter", "Q22_A" = "impact", "Q23" = "clubs", "Q24" = "friends_nearby", "Q24A" =  "friends_friends", "Q25"= "family_nearby", "Q26"= "talk_neighbors", "QD2" = "years_lived", "QD2A" ="permanent_resident", "QD3" = "work_cat", "QD3A" = "household_adults", "QD4" = "children", "QD5A" = "under6", "QD5B" = "six_12", "QD5C" = "thirteen_17", "QD6" = "married", "QD7"= "education", "QD8" = "own_home", "QD9"= "income", "QD10" = "hispanic", "QD111" = "race", "QD112" = "race2", "QD113" = "race3", "QD12" = "white_hispanic", "QD13" = "phone_numbers", "QD13A" = "cell_only", "QD13B" = "have_cell", "QD13C" = "cell_calls"))
}


sotc10 <- renameData(sotc10)
sotc09 <- renameData(sotc09)
sotc08 <- renameData(sotc08)

## ----usedalot------------------------------------------------------------
n1 <- c("citystate", "InGroup", "Response", "n", "Freq", "year")
colors2 <- c("#f1a340", "#998ec3")
colors1 <- brewer.pal(5, "Spectral")
colorsA <- colors1[c(2,3,4,1,5)]
colorsB <- colors1[c(2,3,4,5,1)]

## ----LikertDataFunction--------------------------------------------------
LikertData <- function(positive, negative, neutral=NULL, dataset){
  pos <- dataset[dataset$Response %in% positive,]
  neg <- dataset[dataset$Response %in% negative,]
  if (is.null(neutral)==FALSE){
    neu <- dataset[dataset$Response %in% neutral, ]
    neu$Freq <- neu$Freq/2
    pos <- rbind(pos, neu)
    neg <- rbind(neg, neu)
  } else {
  neu <- NULL
  }
  neg$Freq <- -neg$Freq
  neg$Response <- factor(neg$Response)
  neg$Response <- factor(neg$Response, levels=rev(levels(neg$Response)))
  pos$Response <- factor(pos$Response)
  return(list(pos=pos, neu=neu, neg=neg))
}

## ----calculatingInGroupFeelings------------------------------------------
FeelingsC <- function(dataset){
  Feelings <- dataset %>% 
    group_by(citystate, InGroup) %>% 
    summarise(percents= sum(Freq))
  Feelings <- data.frame(Feelings)
  newDF <- data.frame(InGroup=NA, OutGroup=NA, citystate=levels(Feelings$citystate))
  # just changed, because subtraction was in the wrong direction...
  newDF$ratio <- (Feelings[Feelings$InGroup=="TRUE" | Feelings$InGroup=="No",]$percents) - (Feelings[Feelings$InGroup=="FALSE" | Feelings$InGroup=="Yes",]$percents)
  newDF$InGroup <- Feelings[Feelings$InGroup=="TRUE" | Feelings$InGroup=="Yes",]$percents
  newDF$OutGroup <- Feelings[Feelings$InGroup=="FALSE" | Feelings$InGroup=="No",]$percents
  return(newDF)
}

## ----gettinggeodata------------------------------------------------------
placesPop <- dget("data/popdata.robj")

percent1 <- summary(sotc10$citystate)/placesPop$CENSUS2010POP
percent1 <-c(percent1, summary(sotc09$citystate)/placesPop$POPESTIMATE2009)
percent1 <- c(percent1, summary(sotc08$citystate)/placesPop$POPESTIMATE2008)

popData <- data.frame(year=c(rep(2010, 26), rep(2009, 26), rep(2008, 26)), percent=percent1)
popData$percent <- popData$percent*100
popData$citystate <- c(rep(levels(sotc10$citystate),3))

## ----surveyrates, fig.align="left", fig.cap="Yearly survey percentages, displayed on a log scale. Communityies are ordered by median survey rate. Notice that some communities are always over-surveyed (for example, Palm Beach, FL) and some always appear under-surveyed (for example, Long Beach, CA). Population data compiled by the Census Bureau for Intercensal population estimates", out.width="0.99\\linewidth",fig.width = 9.5,fig.height=7.5----

popData <- popData %>%
  group_by(citystate) %>%
  mutate(medianpercent = median(percent)) %>%
  ungroup() %>%
  mutate(citystate = fct_reorder(citystate, desc(medianpercent)))

p <- ggplot(aes(x=year, y=percent), data=popData) + 
  facet_wrap(~citystate, ncol=5) + 
  geom_line() + 
  scale_x_continuous(
    breaks=c(2008, 2008.5, 2009, 2009.5, 2010), 
    labels=c("2008","", "2009", "", "2010")
    ) + 
  scale_y_log10(breaks=c(0.04, 0.4, 4)) + 
  xlab("") + 
  ylab("Percent of community surveyed")
p

## ----numlevels-----------------------------------------------------------
pullLevels <- function(dataset, range){
  numLevels <- NA
  for (i in range){
    numLevels[i] <- length(levels(as.factor(dataset[,i])))
  }
  return(numLevels)
}

#Removing the ones that don't make sense-- listing problems, in particular

# For sotc10, this works:
numLevels10 <- pullLevels(sotc10, c(11:80))
numLevels10[which(numLevels10>25)] <- NA

#For sotc09
numLevels09 <- pullLevels(sotc09, c(11:70))
numLevels09[16:24] <- NA

#For sotc08
numLevels08 <- pullLevels(sotc08, c(9:64))
# This takes out the problem questions as well as the commute question
numLevels08[which(numLevels08>20)] <- NA

numLevels <- data.frame(levels=c(numLevels08, numLevels09, numLevels10), year=c(rep(2008, times=length(numLevels08)), rep(2009, times=length(numLevels09)), rep(2010, times=length(numLevels10))))

## ----LikertSizes, fig.cap="Scale length distributions for each year of the survey. Notice that the distribution from 2008 is centered around 7, and the 2009 and 2010 distributions are centered around 5. All three distributions have large variation, suggesting that the Knight surveys were quite complex.",out.width="0.99\\linewidth",fig.width = 9.5,fig.height=4.5----
resp<- ggplot(numLevels, aes(x=levels)) + 
  geom_histogram(breaks=seq(from=0, to=15, by=1)) + 
  facet_grid(.~year) + 
  labs(x="Number of levels on the scale", y="Number of questions using the scale") + 
  theme(strip.text=element_text(size=14))
resp

## ----missingmatch--------------------------------------------------------
matchup1 <- sum(is.na(sotc10$recommend) & is.na(sotc10$college))
matchup2 <- sum(is.na(sotc10$college) & is.na(sotc10$registered_vote))

## ----missingdatatable, results="asis"------------------------------------
count_na <- function(x) sum(is.na(x))
missing10 <- count_na(sotc10$recommend)
missing09 <- count_na(sotc09$recommend)
missing08 <- sotc08 %>%
  group_by(recommend == "(Refused)") %>% 
  summarise(n = n()) 
missing <- c(missing08$n[2], missing09, missing10)
missingtable <- as.matrix(data.frame(Year=c(2008, 2009, 2010), Missing=missing, Total=c(dim(sotc08)[1], dim(sotc09)[1], dim(sotc10)[1]), Percent=missing/c(dim(sotc08)[1], dim(sotc09)[1], dim(sotc10)[1])*100))
rownames(missingtable) <- NULL
print(xtable(x=missingtable,caption="Percent of missing data from surveys. The 2010 survey has almost 25 percent missing data. The total number of entries in 2010 is also much larger, suggesting that the 2008 and 2009 datasets used a different criteria for inclusion of entries.", display=c("d", "d", "d","d", "f"), label="missingtable"),include.rownames=FALSE)

## ----communitysat--------------------------------------------------------
tmp1 <- tbl_df(sotc08) %>% 
  group_by(citystate, community_sat) %>% 
  summarise(n = n()) %>% 
  mutate(frac=prop.table(n), year=2008)
tmp2 <- tbl_df(sotc09) %>% 
  group_by(citystate, community_sat) %>% 
  summarise(n = n()) %>% 
  mutate(frac=prop.table(n), year=2009)
tmp3 <- tbl_df(sotc10) %>% 
  group_by(citystate, community_sat) %>% 
  summarise(n = n()) %>% 
  mutate(frac=prop.table(n), year=2010)

tmp <- rbind(tmp1, tmp2, tmp3)
names(tmp) <- c("citystate", "Response", "n", "Freq", "year")

#Now all I have to do is make the plot
neutral <- c("3")
positive <- c("Extremely satisfied", "4")
negative <- c("2", "Not at all satisfied")

trial2 <- LikertData(positive, negative, neutral, tmp)

extremelysat08 <- tbl_df(trial2$pos) %>% 
  filter(year==2008) %>% 
  group_by(citystate) %>% 
  summarise(tot=sum(Freq)) 
extremelysat <- tbl_df(trial2$pos) %>% 
  group_by(citystate, year) %>% 
  summarise(tot=sum(Freq)) 
mostsat <- order(extremelysat08$tot)

masterData <- data.frame(citystate=extremelysat$citystate, year=extremelysat$year, communitysat=extremelysat$tot)

trial2$pos$citystate <- factor(trial2$pos$citystate, levels=levels(trial2$pos$citystate)[mostsat])
trial2$neg$citystate <- factor(trial2$neg$citystate, levels=levels(trial2$neg$citystate)[mostsat])
trial2$neg$Response <- factor(trial2$neg$Response)
trial2$pos$Response <- factor(trial2$pos$Response)
trial2$neg$Response <- factor(trial2$neg$Response, levels=levels(trial2$neg$Response)[c(2,3,1)])

posform <- trial2$pos

maconsat <- tbl_df(posform) %>% 
  filter(citystate=="Macon, GA") %>% 
  group_by(year) %>% 
  summarise(totalper=sum(Freq))
brandetonsat <- tbl_df(posform) %>% 
  filter(citystate=="Bradenton, FL") %>% 
  group_by(year) %>% 
  summarise(totalper=sum(Freq))

## ----communitysatplot, fig.cap="Responses to the question, \`\`Taking everything into account, how satisified are you with this community as a place to live?\" Communities are ordered by percentage of positive responses in 2008, making it clear the differences in distribution in 2009 and 2010.", out.width="0.99\\linewidth",fig.width = 9.5,fig.height=15,----
baseplot <- ggplot(mapping = aes(x=citystate, y=Freq, fill = Response, order=Response)) + 
  facet_wrap(~year, nrow=3) + 
  geom_bar(data = trial2$neg, stat = "identity") + 
  scale_fill_manual(
    breaks=c("Not at all satisfied", "2", "3", "4", "Extremely satisfied"), 
    values=colorsB, 
    name="Response"
    ) + 
  geom_bar(data = trial2$pos, stat = "identity") + 
  coord_flip() + 
  ggtitle("Community satisfaction") + 
  xlab("") + 
  ylab("") + 
  scale_y_continuous(
    limits=c(-0.5, 1), 
    breaks=seq(from=-0.5, to=0.75, by=0.25), 
    labels=c("50%", "25%", "0", "25%", "50%", "75%")
    ) + 
  theme(
    legend.text=element_text(size=14), 
    legend.title=element_text(size=16), 
    axis.text=element_text(size=14), 
    strip.text=element_text(size=14))
baseplot 

## ----twolevel------------------------------------------------------------
twolevel <- c("registered_vote", "volunteer", "public_meeting", "voted_local", "worked_change", "church", "festival", "donated_org", "donated_individual", "donated_shelter")

df10 <- sotc10 %>%
  select(one_of(twolevel)) %>%
  mutate(id = 1:n()) %>%
  tidyr::gather("Question", "Response", -id) %>%
  group_by(Question, Response) %>%
  filter(!is.na(Response)) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))
df10$Year <- 2010


df09 <- sotc09 %>%
  select(one_of(twolevel[c(1:5)])) %>%
  mutate(id = 1:n()) %>%
  tidyr::gather("Question", "Response", -id) %>%
  group_by(Question, Response) %>%
  filter(!is.na(Response)) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))
df09$Year <- 2009

df08 <- sotc08 %>%
  select(one_of(twolevel[c(1:5)])) %>%
  mutate(id = 1:n()) %>%
  tidyr::gather("Question", "Response", -id) %>%
  group_by(Question, Response) %>%
  filter(!is.na(Response) & Response != "(DK)" & Response != "(Refused)") %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n))
df08$Year <- 2008

dfall <- rbind(df10, df09, df08)
dfall$Question <- factor(dfall$Question, levels=levels(dfall$Question)[order(df10$Freq[df10$Response=="Yes"])])
positive <- "Yes"
negative <- "No"
trial1 <- LikertData(positive, negative, neutral=NULL, dataset=dfall) 

## ----YNall, out.width="0.99\\linewidth",fig.width = 15,fig.height=9.5,fig.cap="Responses to yes/no questions about participants' behaviors, comparing all three survey years."----
baseplot <- ggplot(mapping = aes(Question, Freq, fill = Response)) + 
  facet_wrap(~Year) + 
  geom_bar(data = trial1$neg, stat = "identity") + 
  geom_bar(data = trial1$pos, stat = "identity") + 
  scale_y_continuous(
    breaks=seq(from=-0.75, to=0.75, length.out = 7), 
    labels=c("", "-50%", "", "0","",  "50%", "")
    ) + 
  coord_flip()+ 
  xlab("") + 
  scale_x_discrete(
    labels=rev(c("Registered to vote \n", "Voted in local election \n", "Donated money to a \n local organization", "Attended a local \n festival or event", "Gave money or food \n to an individual", "Participated in a \n church event", "Performed local \n volunteer work", "Worked with other residents \n to make change", "Attended a local \n public meeting", "Provided free shelter \n to an individual"))
    ) + 
  ylab("") + 
  theme(
    legend.text=element_text(size=14), 
    legend.title=element_text(size=16), 
    axis.text.y=element_text(size=20), 
    title=element_text(size=24), 
    axis.text.x=element_text(size=14), 
    strip.text=element_text(size=18))
baseplot

## ----YN2010--------------------------------------------------------------
dfcities10 <- sotc10 %>%
  select(citystate, one_of(twolevel)) %>%
  mutate(id = 1:n()) %>%
  group_by(citystate) %>%
  tidyr::gather("Question", "Response", -id, -citystate) %>%
  group_by(citystate, Question, Response) %>%
  filter(!is.na(Response)) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n)) %>%
  filter(Response=="Yes") %>%
  merge(select(filter(df10, Response=="Yes"), Freq), by="Question") %>%
  mutate(diff=Freq.x-Freq.y) %>%
  select(-Freq.y, -n, Freq=Freq.x) %>%
  arrange(citystate) %>%
  mutate(year="2010")

dfcitiesYes <- dfcities10 %>%
  filter(Response=="Yes") %>%
  mutate(col = diff>0)

dfcitiesYes$Question <- factor(dfcitiesYes$Question, levels=levels(dfcitiesYes$Question)[order(dfall[dfall$Response=="Yes",]$Freq)])

## ----startingoverall-----------------------------------------------------
dfcities09 <- sotc09 %>%
  select(citystate, one_of(twolevel[1:5])) %>%
  mutate(id = 1:n()) %>%
  group_by(citystate) %>%
  tidyr::gather("Question", "Response", -id, -citystate) %>%
  group_by(citystate, Question, Response) %>%
  filter(!is.na(Response)) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n)) %>%
  filter(Response=="Yes") %>%
  merge(select(filter(df10, Response=="Yes"), Freq), by="Question") %>%
  mutate(diff=Freq.y-Freq.x) %>%
  select(-Freq.y, -n, Freq=Freq.x) %>%
  arrange(citystate) %>%
  mutate(year="2009")

dfcities08 <- sotc08 %>%
  select(citystate, one_of(twolevel[1:5])) %>%
  mutate(id = 1:n()) %>%
  group_by(citystate) %>%
  tidyr::gather("Question", "Response", -id, -citystate) %>%
  group_by(citystate, Question, Response) %>%
  filter(!is.na(Response)) %>%
  summarise(n = n()) %>%
  mutate(Freq = n/sum(n)) %>%
  filter(Response=="Yes") %>%
  merge(select(filter(df10, Response=="Yes"), Freq), by="Question") %>%
  mutate(diff=Freq.y-Freq.x) %>%
  select(-Freq.y, -n, Freq=Freq.x) %>%
  arrange(citystate) %>%
  mutate(year="2008")


md1 <-rbind(dfcities10, dfcities09, dfcities08) %>% 
  select(-Response, -diff) %>% 
  filter(Question %in% c("registered_vote", "voted_local", "volunteer", "worked_change", "public_meeting"))
md1 <- melt(md1)
md1 <- dcast(md1, citystate + year ~ Question, mean)
md1$year <- as.numeric(md1$year)
masterData <- left_join(masterData, md1)

## ----YN2010plot, out.width="0.99\\linewidth",fig.width = 10, fig.height=18, fig.cap="Percentage-point difference from overall survey rates (2010 data). The letters A-J represent the activities listed in Figure \\ref{fig:YNall}. A: Registered to vote, B: Voted in a local election, C: Donated money to a local organization, D: Attended a local event, E: Gave money or food to an individual, F: Participated in a church event, G: Performed local volunteer work, H: Worked with other residents to make change, I: Attended a local public meeting, J: Provided free shelter to an individual."----
baseplot <- ggplot(dfcitiesYes) + 
  aes(Question, diff, fill = col) + 
  facet_wrap(~citystate, ncol=3) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  guides(fill=FALSE) + 
  scale_y_continuous(
    breaks=seq(from=-.15, to=.15, length.out = 5),
    labels=c("", "-7.5", "0","+7.5",  "")
    ) +
  scale_x_discrete(labels=LETTERS[10:1]) + 
  ylab("") + 
  xlab("") + 
  theme(
    axis.text.y=element_text(size=12), 
    title=element_text(size=18), 
    axis.text.x=element_text(size=11), 
    strip.text=element_text(size=16))
baseplot

## ----minorities----------------------------------------------------------
races10 <- sotc10 %>% 
  filter(race != " " & race != "4" & race != "Don't Know") %>% 
  group_by(race) %>% 
  summarise(n = n()) %>% 
  mutate(frac=prop.table(n), year=2010)
races10$race <- factor(races10$race)
levels(races10$race) <- c(levels(races10$race)[1:5], "Some other race", levels(races10$race)[7])

races09 <- sotc09 %>% 
  filter(race != " " & race != "4" & race != "Don't know") %>% 
  group_by(race) %>% 
  summarise(n = n()) %>% 
  mutate(frac=prop.table(n), year=2009)
races09$race <- factor(races09$race)

races08 <- sotc08 %>% 
  filter(race != " " & race != "4" & race != "(DK)" & race != "None") %>% 
  group_by(race) %>% 
  summarise(n = n()) %>% 
  mutate(frac=prop.table(n), year=2008)
races08$race <- factor(races08$race)
levels(races08$race) <- c("Hispanic", "More than one", "Refused", "American Indian or Alaskan", "Asian", "Black or African-American", "Native Hawaiian or other Pacific Islander", "Some other race", "White")

races <- rbind(races08, races09, races10)

## ----overallRaceplot, out.width="0.99\\linewidth",fig.width = 15,fig.height=10, fig.cap="Responses to the question, \`\`Which of these groups best describes your racial background?\" Notice that 2010 shows an overrepresentation of Refused responses, compared to the other years."----
raceOver <- ggplot(races, mapping = aes(race, n)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~year,nrow=3) + 
  ylab("Number of respondents") + 
  xlab("") + 
  theme(
    axis.text.y=element_text(size=20), 
    title=element_text(size=24), 
    axis.text.x=element_text(size=18), 
    strip.text=element_text(size=25)) + 
  coord_flip()

raceOver 

## ----minoritiesOverYearsE------------------------------------------------
minoritiesYN08 <- sotc08 %>% 
  filter(race != " " & race != "4" & race != "(DK)" & race != "None" & race != "(Refused)") %>% 
  filter(minorities != "(DK)" & minorities != "(Refused)") %>% 
  group_by(citystate, race != "White", minorities) %>% 
  summarise(n = n()) %>% 
  mutate(frac=prop.table(n), year=2008)
names(minoritiesYN08) <- n1

minoritiesYN09 <- sotc09 %>% 
  filter(race != " " & race != "4" & race != "Don't know" & race != "Refused") %>% 
  filter(!is.na(minorities)) %>% group_by(citystate, race != "White", minorities) %>% 
  summarise(n = n()) %>% 
  mutate(frac=prop.table(n), year=2009)
names(minoritiesYN09) <- n1

minoritiesYN10 <- sotc10 %>% 
  filter(race != " " & race != "4" & race != "Don't Know" & race != "Refused") %>% 
  filter(!is.na(minorities)) %>% 
  group_by(citystate, race != "White", minorities) %>% summarise(n = n()) %>% 
  mutate(frac=prop.table(n), year=2010)
names(minoritiesYN10) <- n1

whiteYN <- rbind(minoritiesYN08, minoritiesYN09, minoritiesYN10) %>% 
  group_by(year, InGroup, Response) %>% 
  summarise(tot = sum(n)) %>% 
  mutate(newFreq=prop.table(tot))
whiteYN <- rename(whiteYN, Freq=newFreq, n=tot)

neutral <- c("3")
positive <- c("Very good", "4")
negative <- c("2", "Very bad")

whiteYears <- LikertData(positive, negative, neutral, whiteYN)
whiteYears$neg$Response <- factor(whiteYears$neg$Response, levels=levels(whiteYears$neg$Response)[c(2,3,1)])

## ----samplesize, out.width="0.99\\linewidth",fig.width = 8.5,fig.height=4.5, fig.cap="Sample sizes for plots about meta-knowledge regarding the community as a place for minorities."----
whiteYN1 <- tbl_df(whiteYN) %>% 
  group_by(year, InGroup) %>% 
  summarise(howmany=sum(n)) %>% 
  mutate(frac=prop.table(howmany))

whiteplot <- ggplot(aes(x=year, y=howmany), data=whiteYN1) +
  geom_bar(aes(fill=InGroup), position="dodge", stat="identity") + 
  xlab("Year") + 
  ylab("Sample size") + 
  guides(fill=guide_legend(title=NULL)) + 
  guides(fill=guide_legend(title=NULL)) + 
  scale_fill_manual(values=colors2, labels=c("White", "Non-white"))
whiteplot

## ----overallRaceResponsePlot,  out.width="0.99\\linewidth",fig.width = 8.5, fig.height=4.5, fig.cap="Responses to the question, \`\`How is your community as a place for racial and ethnic minorities?\" White denotes survey respondents who listed their race as White, and Non-white is all other race responses (not including survey participants who refused to report a race). Notice the difference between the 2008/2009 responses and the 2010 responses, but also refer to Figure \\ref{fig:samplesize} for the absolute sample sizes for each year-- 2010 has a much smaller sample of responses to the question overall."----

baseplot <- ggplot(mapping = aes(InGroup, Freq, fill = Response, order=Response)) + 
  facet_wrap(~year, nrow=3) + 
  geom_bar(data = whiteYears$neg, stat = "identity")  + 
  geom_bar(data = whiteYears$pos, stat = "identity") + 
  scale_fill_manual(
    breaks=c("Very bad", "2", "3", "4", "Very good"), 
    values=colorsA, 
    name="Response"
    ) + 
  coord_flip() +
# TRUE means minority, and in 2008 minorities were under-rating
  scale_y_continuous(
    breaks=seq(from=-0.25, to=0.75, by=0.25), 
    labels=percent
    ) + 
  scale_x_discrete(labels=c("White", "Non-white"))  + 
  theme(
    legend.text=element_text(size=14), 
    legend.title=element_text(size=16), 
    axis.text.y=element_text(size=16), 
    title=element_text(size=16), 
    axis.text.x=element_text(size=14), 
    strip.text=element_text(size=14)
    ) + 
  xlab("") + 
  ylab("")

baseplot

## ----facetcalculations---------------------------------------------------
minGroups10 <- LikertData(positive, negative, neutral, minoritiesYN10)
minGroups10$neg$Response <- factor(minGroups10$neg$Response, levels=levels(minGroups10$neg$Response)[c(2,3,1)])

minGroups08 <- LikertData(positive, negative, neutral, minoritiesYN08)
minGroups08$neg$Response <- factor(minGroups08$neg$Response, levels=levels(minGroups08$neg$Response)[c(2,3,1)])

minGroups09 <- LikertData(positive, negative, neutral, minoritiesYN09)
minGroups09$neg$Response <- factor(minGroups09$neg$Response, levels=levels(minGroups09$neg$Response)[c(2,3,1)])

totalMin09 <- minoritiesYN09 %>%
  group_by(citystate, InGroup) %>%
  summarize(totalN = sum(n))

## ----allMinorities,out.width="0.99\\linewidth",fig.width = 10.5, fig.height=12.5, fig.cap="Responses to the question, \`\`How is your community as a place for racial and ethnic minorities?\" faceted by community (2009 data). Numbers in parentheses indicate sample sizes."----
baseplot <- ggplot() + 
  facet_wrap(~citystate, ncol=3) + 
  geom_bar(mapping = aes(InGroup, Freq, fill = Response, order=Response), data = minGroups09$neg, stat = "identity") + 
  geom_bar(mapping = aes(InGroup, Freq, fill = Response, order=Response), data = minGroups09$pos, stat = "identity") + 
  scale_fill_manual(
    values=colorsA, 
    breaks=c("Very bad", "2", "3", "4", "Very good"),
    name="Response"
    ) + 
  coord_flip() +
#TRUE means minorities, in Aberdeen they are under-rating
  scale_y_continuous(
    breaks=seq(from=-0.5, to=0.75, by=0.25), 
    labels=percent
    ) + 
  scale_x_discrete(labels=c("White", "Non-white")) + 
  xlab("") + 
  ylab("") + 
  theme(
    legend.text=element_text(size=14), 
    legend.title=element_text(size=16), 
    axis.text.y=element_text(size=16), 
    title=element_text(size=16), 
    axis.text.x=element_text(size=11), 
    strip.text=element_text(size=14)) +
  geom_text(data=totalMin09, aes(label=paste0("(",totalN, ")"), x=InGroup, y=1.1), size=3, hjust=1) +
  expand_limits(y=1) 

baseplot 

## ----anotherlook---------------------------------------------------------
newDF09 <- FeelingsC(minGroups09$pos)
newDF09$year <- "2009"
names(newDF09) <- c("nonwhite", "white", "citystate", "racediffs", "year")
newDF08 <- FeelingsC(minGroups08$pos)
newDF08$year <- "2008"
names(newDF08) <- c("nonwhite", "white", "citystate", "racediffs", "year")
newDF <- rbind(newDF09, newDF08)
# Too much missing data to use 2010
newDF$year <- as.numeric(newDF$year)

masterData <- left_join(masterData, newDF)

horiz <- mean(newDF09$white)
vert <- mean(newDF09$nonwhite)

## ----anotherlookplot, out.width="0.76\\linewidth", fig.cap=paste0("Relationship between positive responses to the question, \`\`How is your community as a place for minorities?\" comparing ratings of Whites and Non-whites. Each community is represented, and the plot uses 2009 data. The darker grey region corresponds to positive meta-knowledge scores. Some communities, like State College, PA, are under-rated by Whites, some are over-rated, like Grand Forks, ND, and some are rated the same by both groups, like Gary, IN. The black line shows y=x, for comparison. Grey lines at x=", round(vert, digits=2), " and y=", round(horiz, digits=2), " show the mean ratings by each group.")----
df_poly <- data.frame(
    x=c(-Inf, Inf, Inf),
    y=c(-Inf, Inf, -Inf)
)

al <- ggplot(newDF09) + 
  geom_polygon(data=df_poly, aes(x, y), fill="grey", alpha=0.6) +
  geom_vline(aes(xintercept=vert), color="grey") + 
  geom_hline(aes(yintercept=horiz), color="grey") + 
  geom_abline(intercept=0, slope=1) + 
  geom_point(aes(y=white, x=nonwhite)) + 
  geom_text(
    data = newDF09[newDF09$citystate %in% c("Duluth, MN", "Gary, IN", "Grand Forks, ND", "State College, PA"),],
    aes(y=white, x=nonwhite, label = paste(citystate, "\nMK =", round(1/racediffs))), 
    vjust=c(-0.4, 1.1, 1.3, -0.4), 
    hjust=0, 
    size=4
    ) + 
  xlab("Non-whites rating") + 
  ylab("Whites rating") + 
  xlim(0.4, 1) + 
  ylim(0.4, 1) 
al

## ----diffscor------------------------------------------------------------
diffscor <- cor(masterData[masterData$year != 2010,]$communitysat, abs(1/masterData[masterData$year != 2010,]$racediffs))

## ----OverallSeniors------------------------------------------------------
seniorGroup10 <- sotc10 %>% 
  filter(!is.na(seniors)) %>% 
  group_by(citystate, age >= 62, seniors) %>% 
  summarise(n = n()) %>% 
  mutate(frac=prop.table(n), year=2010)

names(seniorGroup10) <- n1

seniorGroup09 <- sotc09 %>% 
  filter(!is.na(age)) %>% 
  group_by(citystate, age >= 62, seniors) %>% 
  summarise(n = n()) %>% 
  mutate(frac=prop.table(n), year=2009)
names(seniorGroup09) <- n1

seniorGroup08 <- sotc08 %>%
  group_by(citystate, age >=62, seniors) %>% 
  summarise(n = n()) %>% 
  mutate(frac=prop.table(n), year=2008)
names(seniorGroup08) <- n1

seniorYears <- rbind(seniorGroup08, seniorGroup09, seniorGroup10)
seniorYears$Response <- factor(seniorYears$Response, levels=levels(seniorYears$Response)[c(6,3:5, 7,1:2)])

## ----seniorSS, out.width= "0.99\\linewidth", fig.width = 8.5, fig.height= 4.5, fig.cap = "Sample sizes for plots about meta-knowledge regarding the community as a place for seniors."----
seniorSS <- tbl_df(seniorYears) %>% 
  group_by(year, InGroup) %>% 
  summarise(number=sum(n)) %>% 
  mutate(percentage=(100*number)/sum(number))
seniorSS$InGroup[seniorSS$InGroup=="FALSE"] <- "Non-seniors"
seniorSS$InGroup[seniorSS$InGroup=="TRUE"] <- "Seniors"

seniorplot <- ggplot(aes(x=year, y=number), data=seniorSS) + 
  geom_bar(aes(fill=InGroup), position="dodge", stat="identity") + 
  xlab("Year") + 
  ylab("Sample size") + 
  guides(fill=guide_legend(title=NULL)) + 
  scale_fill_manual(values=colors2)
seniorplot

## ----OverallSeniorRatings------------------------------------------------

seniorOverall <- LikertData(positive, negative, neutral, seniorYears)
seniorGroup09 <- LikertData(positive, negative, neutral, seniorGroup09)
seniorGroup09$neg$Response <- factor(seniorGroup09$neg$Response, levels=levels(seniorGroup09$neg$Response)[c(2,3,1)])

totPos <- FeelingsC(seniorGroup09$pos)
names(totPos) <- c("Seniors", "Nonseniors","citystate", "seniordiffs")

horiz <- mean(totPos$Nonseniors)
vert <- mean(totPos$Seniors)

## ----seniorOverallData---------------------------------------------------
forM <- filter(seniorOverall$pos, year==2008)
forM <- tbl_df(forM)
comp08 <- FeelingsC(forM) %>% 
  mutate(year="2008")
forM <- filter(seniorOverall$pos, year==2009)
forM <- tbl_df(forM)
comp09 <- FeelingsC(forM) %>% 
  mutate(year="2009")
forM <- filter(seniorOverall$pos, year==2010)
forM <- tbl_df(forM)
comp10 <- FeelingsC(forM) %>% 
  mutate(year="2010")

comp <- rbind(comp08, comp09, comp10)
names(comp) <- c("seniors", "nonseniors", "citystate", "seniordiffs", "year")
comp$year <- as.numeric(comp$year)
masterData <- left_join(masterData, comp)

## ----seniorOverallPlot,out.width="0.99\\linewidth",fig.width = 8.5, fig.height=5.5, fig.cap="Responses to the question, \`\`How is your community as a place for seniors?\" Seniors are defined as survey participants aged 62 and older, non-seniors are those under 62. Notice that seniors consistently rated their community more highly than did non-seniors over all three survey years."----

seniorYears2 <- seniorYears %>% 
  group_by(year, InGroup, Response) %>% 
  summarise(n3 = sum(n)) %>% 
  mutate(newFreq = prop.table(n3))
names(seniorYears2) <- c("year", "InGroup", "Response", "n", "Freq")
seniorYears2 <- LikertData(positive, negative, neutral, seniorYears2)

baseplot <- ggplot(mapping = aes(InGroup, Freq, fill = Response, order=Response)) + 
  facet_wrap(~year, nrow=3) + 
  geom_bar(data = seniorYears2$neg, stat = "identity") + 
  geom_bar(data = seniorYears2$pos, stat = "identity") + 
  scale_fill_manual(breaks=c("Very bad", "2", "3", "4", "Very good"), values=colorsA, name="Response") + 
  coord_flip() + 
  scale_y_continuous(breaks=seq(from=-0.25, to=0.75, by=0.25), labels=percent) + 
  scale_x_discrete(labels=c("Non-seniors", "Seniors")) + 
  xlab(" ") + 
  ylab("") + 
  theme(
    legend.text=element_text(size=14), 
    legend.title=element_text(size=16), 
    axis.text.y=element_text(size=12), 
    title=element_text(size=16), 
    axis.text.x=element_text(size=12), 
    strip.text=element_text(size=14))

baseplot

## ----seniorPlot, out.width="0.99\\linewidth",fig.width = 10.5, fig.height=12.5, fig.cap="Responses to the question, \`\`How is your community as a place for seniors?\" faceted by community (2009 data). Numbers in parentheses are sample sizes. Every community follows the pattern of over-rating by seniors, but some communities have a smaller discrepancy between ratings of seniors and non-seniors."----

totalSeniors09 <- seniorYears %>%
  ungroup() %>%
  filter(year==2009) %>%
  group_by(citystate, InGroup) %>%
  summarize(totalN = sum(n))

baseplot <- ggplot() + 
  facet_wrap(~citystate, ncol=3) + 
  geom_bar(mapping = aes(InGroup, Freq, fill = Response, order = Response), data = seniorGroup09$neg, stat = "identity") + 
  geom_bar(mapping = aes(InGroup, Freq, fill = Response, order = Response), data = seniorGroup09$pos, stat = "identity") + 
  scale_fill_manual(
    breaks=c("Very bad", "2", "3", "4", "Very good"), 
    values=colorsA, 
    name="Response"
    ) + 
  coord_flip() + 
  scale_y_continuous(
    breaks=seq(from=-0.25, to=0.75, by=0.25), 
    labels=percent
    ) + 
  scale_x_discrete(labels=c("Non-seniors", "Seniors")) + 
  xlab("") + 
  ylab("") + 
  theme(
    legend.text=element_text(size=14), 
    legend.title=element_text(size=16), 
    axis.text.y=element_text(size=16), 
    title=element_text(size=16), 
    axis.text.x=element_text(size=11),
    strip.text=element_text(size=14)) +
  geom_text(data=totalSeniors09, aes(label=paste0("(",totalN, ")"), x=InGroup, y=1.2), size=3, hjust=1) +
  expand_limits(y=1) 

baseplot

## ----anotherlookseniors, out.width="0.76\\linewidth", fig.cap=paste0("Relationship between positive responses to the question, \`\`How is your community as a place for seniors?\" comparing ratings of non-seniors and seniors. Each community is represented, and the plot uses 2009 data. The darker grey region corresponds to positive meta-knowledge scores. The black line shows y=x, for comparison. Grey lines at x=", round(mean(totPos$Seniors), digits=2), " and y=", round(mean(totPos$Nonseniors), digits=2), " show the mean ratings by each group.")----
sr <- ggplot(totPos) + 
  geom_polygon(data=df_poly, aes(x, y), fill="grey", alpha=0.6) +
  geom_vline(aes(xintercept=vert), color="grey") + 
  geom_hline(aes(yintercept=horiz), color="grey") + 
  geom_abline(intercept=0, slope=1) + 
  geom_point(aes(y=Nonseniors, x=Seniors)) + 
  geom_text(
    data = totPos[totPos$citystate %in% c("Wichita, KS", "Gary, IN", "Bradenton, FL"),], 
    aes(y=Nonseniors, x=Seniors, label = paste(citystate, "\nMK =", round(1/seniordiffs))
        ), 
    vjust=c(-0.4,-0.4,1), 
    hjust=c(1,0,0), 
    size=4
    ) + 
  xlab("Senior rating") + 
  ylab("Non-senior rating") +
  ylim(0.5, 1) +
  xlim(0.5, 1)

sr

## ----kidsbyYear----------------------------------------------------------
kidGroup10 <- sotc10 %>% 
  filter(children == "Yes" | children == "No", !is.na(families_kids)) %>% 
  group_by(citystate, children, families_kids) %>% 
  summarise(n = n()) %>% 
  mutate(frac=prop.table(n), year=2010)
names(kidGroup10) <- n1

kidGroup09 <- sotc09 %>% 
  filter(children == "Yes" | children == "No", !is.na(families_kids)) %>% 
  group_by(citystate, children, families_kids) %>% 
  summarise(n = n()) %>% 
  mutate(frac=prop.table(n), year=2009)
names(kidGroup09) <- n1

kidGroups <- LikertData(positive, negative, neutral, kidGroup09)
kidGroups$neg$Response <- factor(kidGroups$neg$Response, levels=levels(kidGroups$neg$Response)[c(2,3,1)])

kidGroup08 <- sotc08 %>% 
  filter(children == "Yes" | children == "No", !is.na(families_kids) & families_kids != "(DK)" & families_kids != "(Refused)") %>% 
  group_by(citystate, children, families_kids) %>% 
  summarise(n = n()) %>% 
  mutate(frac=prop.table(n), year=2008)
names(kidGroup08) <- n1

kidYears <- rbind(kidGroup08, kidGroup09, kidGroup10)

forMaster <- LikertData(positive, negative, neutral, kidYears)
forMaster$neg$Response <- factor(forMaster$neg$Response, levels=levels(forMaster$neg$Response)[c(2,3,1)])
forM <- filter(forMaster$pos, year==2008)
forM <- tbl_df(forM)
comp08 <- FeelingsC(forM) %>% 
  mutate(year="2008")
forM <- filter(forMaster$pos, year==2009)
forM <- tbl_df(forM)
comp09 <- FeelingsC(forM) %>% 
  mutate(year="2009")
forM <- filter(forMaster$pos, year==2010)
forM <- tbl_df(forM)
comp10 <- FeelingsC(forM) %>% 
  mutate(year="2010")

comp <- rbind(comp08, comp09, comp10)
names(comp) <- c("withKids", "noKids", "citystate", "kiddiffs", "year")
comp$year <- as.numeric(comp$year)
masterData <- left_join(masterData, comp)

kidYears <- kidYears %>% 
  group_by(year, InGroup, Response) %>% summarise(totalnum = sum(n)) %>% 
  mutate(totalFreq=prop.table(totalnum))
kidYears <- rename(kidYears, Freq=totalFreq, num = totalnum)

## ----kidsSS,out.width="0.99\\linewidth",fig.width = 8.5,fig.height=4.5, fig.cap="Sample sizes for plots about meta-knowledge regarding the community as a place for families with young children."----
kidSS <- group_by(kidYears, year, InGroup) %>% 
  summarise(number=sum(num)) %>% 
  mutate(percentage=100*number/sum(number))

levels(kidSS$InGroup) <- c("Don't know", "Refused", "Childless households", "Families with children")
kidsplot <- ggplot(aes(x=year, y=number), data=kidSS)+geom_bar(aes(fill=InGroup), position="dodge", stat="identity")+xlab("Year")+ylab("Sample size")+guides(fill=guide_legend(title=NULL))+scale_fill_manual(values=colors2)
kidsplot 

## ----kidsbyYearresponses-------------------------------------------------
kidYears <- LikertData(positive, negative, neutral, kidYears)
kidYears$neg$Response <- factor(kidYears$neg$Response, levels=levels(kidYears$neg$Response)[c(2,3,1)]) 

## ----kidsplotOverall, out.width="0.99\\linewidth",fig.width = 8.5, fig.height=5.5, fig.cap="Responses to the question, \`\`How is your community as a place for families with young children?\" Families with children are defined as any household with children under the age of 18, Childless households are any households without children, whether or not the residents have adult children living elsewhere. Ratings were consistently similar between groups and across survey years."----
baseplot <- ggplot(mapping = aes(InGroup, Freq, fill = Response, order=Response)) + 
  facet_wrap(~year, nrow=3) + 
  geom_bar(data = kidYears$neg, stat = "identity") + 
  geom_bar(data = kidYears$pos, stat = "identity") + 
  scale_fill_manual(
    breaks=c("Very bad", "2", "3", "4", "Very good"), 
    values=colorsA, 
    name="Response"
    ) + 
  coord_flip() + 
  scale_y_continuous(
    breaks=seq(from=-0.25, to=0.75, by=0.25), 
    labels=percent
    ) + 
  scale_x_discrete(labels=c("Childless \n households", "Families \n with children")) + 
  xlab("")+ 
  ylab("") + 
  theme(
    legend.text=element_text(size=14), 
    legend.title=element_text(size=16), 
    axis.text.y=element_text(size=16), 
    title=element_text(size=16), 
    axis.text.x=element_text(size=14), 
    strip.text=element_text(size=14))

baseplot 

## ----kidPlot, out.width="0.99\\linewidth",fig.width = 10.5, fig.height=12.5, fig.cap="Responses to the question, \`\`How is your community as a place for families with young children?\" faceted by community (2009 data). Interestingly, most communities follow the pattern of similar ratings by both groups, but there is a lot of variation between communities in terms of the overall rating."----
totalkids09 <- kidGroup09 %>%
  ungroup() %>%
  group_by(citystate, InGroup) %>%
  summarize(totalN = sum(n))

baseplot <- ggplot() + 
  facet_wrap(~citystate, ncol=3) + 
  geom_bar(mapping = aes(InGroup, Freq, fill = Response, order=Response), data = kidGroups$neg, stat = "identity") + 
  geom_bar(mapping = aes(InGroup, Freq, fill = Response, order=Response), data = kidGroups$pos, stat = "identity") + 
  scale_fill_manual(
    breaks=c("Very bad", "2", "3", "4", "Very good"), 
    values=colorsA, 
    name="Response"
    ) + 
  coord_flip() + 
  scale_y_continuous(
    breaks=seq(from=-0.25, to=0.75, by=0.25), 
    labels=percent
    ) + 
  scale_x_discrete(labels=c("Childless \n households", "Families \n with children")) + 
  xlab(" ") + 
  ylab("") + 
  theme(
    legend.text=element_text(size=14), 
    legend.title=element_text(size=16), 
    axis.text.y=element_text(size=16), 
    title=element_text(size=16), 
    axis.text.x=element_text(size=10)) +
  geom_text(data=totalkids09, aes(label=paste0("(",totalN, ")"), x=InGroup, y=1.2), size=3, hjust=1) +
  expand_limits(y=1) 

baseplot 

## ----anotherlookkids, out.width="0.76\\linewidth", fig.cap=paste0("Relationship between positive responses to the question, \`\`How is your community as a place for families with young children?\" comparing ratings of families with kids and those without. Each community is represented, and the plot uses 2009 data. The darker grey region corresponds to positive meta-knowledge scores. The black line shows y=x, for comparison. Grey lines at x=", round(mean(comp09$InGroup), digits=2), " and y=", round(mean(comp09$OutGroup), digits=2), " show the mean ratings by each group.")----
kd <- ggplot(comp09) + 
  geom_polygon(data=df_poly, aes(x, y), fill="grey", alpha=0.6) +
  geom_abline(intercept=0, slope=1) + 
  geom_vline(aes(xintercept=mean(comp09$InGroup)), color="grey") + 
  geom_hline(aes(yintercept=mean(comp09$OutGroup)), color="grey") + 
  geom_point(aes(y=OutGroup, x=InGroup)) + 
  geom_text(
    data = comp09[comp09$citystate %in% c("Grand Forks, ND", "Gary, IN", "St. Paul, MN"),], 
    aes(y=OutGroup, x=InGroup, label = paste(citystate, "\nMK =", round(-1/ratio))), 
    vjust=c(-0.4,-0.4,1), 
    hjust=c(0,1,0), 
    size=4
    ) + 
  xlab("Families with children") + 
  ylab("Childless households") +
  xlim(0.6, 1) +
  ylim(0.6,1)
kd

## ----correlationmatrix, out.width="\\linewidth",  fig.cap="Correlation between variables of interest. In particular, look at the correlations of variables with Community Satisfaction. When reading the plot, recall that `[subgroup] rating` refers to their rating of their community as a place either for the associated minority group or the complement of that group in society, not to satisfaction overall. (2008, 2009, 2010 data, except for race-related questions where 2010 was excluded)."----

masterData$racediffs <- abs(1/masterData$racediffs)
masterData$seniordiffs <- abs(1/masterData$seniordiffs)
masterData$kiddiffs <- abs(1/masterData$kiddiffs)

melted <- select(masterData, -citystate, -year) %>% 
  cor(use="complete.obs") %>% 
  melt()
varnames <- c("Community satisfaction", "Registered to vote", "Volunteered", "Attended a public meeting", "Voted in a local election", "Worked to make change", "Non-white rating", "White rating", "|MK about racial minorities|", "Senior rating", "Non-Senior rating", "|MK about seniors|", "Families with children rating", "Childless household rating", "|MK about children|")
  
cp <- ggplot(data=melted, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() + 
  scale_fill_gradient2(low="#762A83", high="#1B7837", name="Correlation") + 
  scale_x_discrete(labels=varnames) + 
  scale_y_discrete(labels=varnames) + 
  xlab("") + 
  ylab("") + 
  theme(axis.text.x=element_text(angle=-90, hjust=0)) + 
  coord_flip() +
  coord_fixed()
cp

