
# popdata <- read.csv("data/SUB-EST00INT.csv")
# places <- levels(sotc$citystate)
# 
# placesI <- c(68201, 54133, 37974, 3940, 5057,49950, 67591, 6093, 28830, 32606, 15446, 15461, 51356, 25561, 3079, 6277, 5261, 6295, 67701, 5311, 62395,3221,33088,62550, 5387, 21820)
# placesPop <- data.frame(places, popdata[placesI, c(6:20)])
# dput(placesPop, file="popdata.robj")

# require(ggmap)
# latlonsotc <- geocode(places)
# latlonsotc$citystate <- places
# dput(latlonsotc, file="data/latlonsotc.robj")

geocoded <- dget("data/latlonsotc.robj")

# renameData <- function(datasetname){
#   rename(datasetname,gender = INTRO1,  citystate = QSB,  county = QS3,   county2 = QS3_02,   zipcode = QS3A,  age = QD1,  area_descrip = QS4,  further = QS5, further2 = QS5_2,  current_life = QN1A, future_life = QN1B, community_sat =  QCE1,  recommend = QCE2, proud = Q3A,  perfect = Q3B,  reputation = Q3C,   problem_1 = Q4_1, problem_2 = Q4_2,  problem_3 = Q4_3,  problem_1 = Q4_1_1,   problem_2 = Q4_1_2,   problem_3 = Q4_1_3,  problem_4 = Q4_2_1,   problem_5 = Q4_2_2,   problem_6 = Q4_2_3, problem_7 = Q4_3_1,   problem_8 = Q4_3_2,   problem_9 = Q4_3_3,  rather_live = Q5,  five_years_ago = Q6,  five_years_future = Q6A,  parks = Q7A,  beauty = Q7B,  highways =  Q7C,  affordable_housing = Q7D, job_opportunites = Q7E,  public_schools = Q7F,  colleges = Q7G,  nightlife = Q7H,  friends = Q7I,  raise_kids = Q7J,  heathcare = Q7K,  leadership = Q7L,  care = Q7M,  police = Q7N, arts =  Q7O,  community_events = Q7P,  talented_grads = Q8A, immigrants =  Q8B,  minorities = Q8C,  families_kids = Q8D,  gay_lesbian = Q8E,  seniors = Q8F,  young_adults = Q8G,  economy = Q9,  econ_better = Q10,  trust_government = Q10A,  employment = Q11, commute = Q12,  job_satisfaction = Q13,  company_workforce = Q14,  enough_income = Q15,  area_hiring = Q15AA,  representative_leaders = Q15AB,  treated_respectfully = Q16A,  well_rested = Q16B,  high_stress = Q16C,  learned_yest = Q16D,  stress_source = Q17, night_safety =  Q18,  crime = Q19,  crime_increasing = Q20,  registered_vote = Q21, volunteer =  Q22A,  public_meeting = Q22B,  voted_local = Q22C,  worked_change = Q22D, church =  Q22E,  festival = Q22F,  donated_org = Q22G,  donated_individual = Q22H, donated_shelter =  Q22I, impact =  Q22_A,  clubs = Q23,  friends_nearby = Q24,  friends_friends = Q24A,  family_nearby = Q25,  talk_neighbors = Q26, years_lived = QD2 ,  permanent_resident = QD2A,  work_cat = QD3,  household_adults = QD3A,  children = QD4,  under6 = QD5A,  six_12 = QD5B,  thirteen_17 = QD5C,  married = QD6, education = QD7, own_home =  QD8,  income = QD9,  hispanic = QD10,  race = QD111,  race2 = QD112,  race3 = QD113,  white_hispanic = QD12,  phone_numbers = QD13, cell_only =  QD13A,  have_cell = QD13B,  cell_calls = QD13C)
# }

#baseplot <- baseplot + scale_x_discrete(labels=rev(c("Registered to vote", "Voted in the local election", "Donated money to a local organization", "Attended a local event", "Gave money or food to an individual", "Participated in a church event", "Performed local volunteer work", "Worked with other residents to make change", "Attended a local public meeting", "Provided free shelter to an individual")))

#names(kidSS) <- c("Year", "", "Number", "Percentage")
#print(xtable(kidSS, display=c("d", "d", "s", "d", "f"), caption="Sample sizes for plots about meta-knowledge about families with young children.", label="kidSS"),include.rownames = FALSE)

#races <- dplyr::filter(sotc08, race != " " & race != "4" & race != "(DK)" & race != "None")
#races <- dplyr::filter(sotc09, race != " " & race != "4" & race != "Don't know")


#whiteYN1 <- whiteYN1[whiteYN1$InGroup == TRUE,-c(2)]
#names(whiteYN1) <- c("Year", "Number", "Percent")
#whiteYN1$Percent <- whiteYN1$Percent*100
#whiteYN1$Percent2 <- 100 - whiteYN1$Percent

#whiteYN1 <- whiteYN1[, -c(2)]
#names(whiteYN1) <- c("Year", "Percent White", "Percent Non-white")
#print(xtable(whiteYN1,caption = "Percent of dataset that is White versus Non-white. 2010 saw an increase in the percentage of survey respondents who were identifying as Non-white, but it also saw a marked decrease in any respondents reporting race, as can be seen in Table \\ref{racetable}.", label="minorities", display=c("d", "d", "f","f")), include.rownames=FALSE)



# tmp2 <- dplyr::summarise(group_by(sotc09, citystate, community_sat), n = n()) 
# tmp2 <- dplyr::mutate(tmp2, frac=prop.table(n))
# names(tmp2) <- c("citystate", "Response", "n", "Freq")
# neutral <- c("3")
# positive <- c("Extremely satisfied", "4")
# negative <- c("2", "Not at all satisfied")
# 
# tmp3 <- LikertData(positive, negative, neutral,  tmp2)
# cityfeelings <- dplyr::summarise(group_by(tmp3$pos, citystate), percents=sum(Freq))
# cityfeelings <- data.frame(cityfeelings)
# cityfeelings$diffs <- diffs

# popdata <- dget("popdata.robj")
# id <- (popdata$POPESTIMATE2009-popdata$POPESTIMATE2008)/popdata$POPESTIMATE2009
# cityfeelings$id <- id

# relationplot <- ggplot(CA09, aes(y=aveCA, x=diffs, label=citystate)) + geom_point()+xlab("Difference between minority and white ratings")+ylab("Mean community attachment for community")+ggtitle("Relationship between meta-knowledge and community attachment")+xlim(-0.25, 0.225)+ylim(2.7, 4.25)
# relationplot <- relationplot + geom_text(data = CA09[CA09$citystate %in% c( "Gary, IN", "Duluth, MN", "State College, PA", "Bradenton, FL", "Detroit, MI" , "Macon, GA", "Miami, FL", "Wichita, KS", "Grand Forks, ND"),], aes(x=diffs,y=aveCA, label = citystate),hjust=0, vjust=-0.1)
# relationplot <- relationplot + geom_vline(x=0, colour="dark grey")
# relationplot


#print(xtable(seniorSS, display=c("d", "d", "s", "d", "f"), caption="Sample sizes for plots about meta-knowledge about seniors.", label="seniorSS"),include.rownames = 

# 
# seniorGroups <- LikertData(positive, negative, neutral, seniorGroup)
# seniorGroups$neg$Response <- factor(seniorGroups$neg$Response, levels=levels(seniorGroups$neg$Response)[c(2,3,1)])


<<seniorData>>=
  # The goal for this plot is to show the difference between the responses to questions by people in a particular group and those outside the group. 
  # The groups are: seniors (55+), variable age and variable seniors, 
  # seniorGroup <- filter(sotc10, seniors != "NA") %>% group_by(citystate, age >= 55, seniors) %>% summarise( n = n()) %>% mutate(frac=prop.table(n))
  # n2 <- c("citystate", "InGroup", "Response", "n", "Freq")
  # names(seniorGroup) <- n2
  # seniorGroup$Response <- factor(seniorGroup$Response, levels=levels(seniorGroup$Response)[c(4, 1:3, 5)])
  # 
  # seniorGroup2 <- LikertData(positive, negative, neutral, seniorGroup)
  # seniorGroup2$neg$Response <- factor(seniorGroup2$neg$Response)
  # seniorGroup2$pos$Response <- factor(seniorGroup2$pos$Response)
  @
  
  %' 
%' <<withoutRefused, out.width="0.99\\linewidth",fig.width = 15,fig.height=10, fig.cap="The distribution of responses to the question, \`\`Which of these groups best describes your racial background?\" with Refused responses removed.">>=
  %' withoutRefused <- filter(races, race != "Refused")
%' 
%' raceOver2 <- ggplot(withoutRefused) + aes(race, n) + geom_bar(stat="identity") + facet_wrap(~year,nrow=3) 
%' raceOver2 <- raceOver2 +ylab("Number of respondents")+xlab("")
%' raceOver2 <- raceOver2 +theme(axis.text.y=element_text(size=20), title=element_text(size=24), axis.text.x=element_text(size=18), strip.text=element_text(size=25))
%' #+ggtitle("Race response distribution without Refused responses")
%' raceOver2 + coord_flip()
%' @
  %' 
%' <<withoutRefusedTable, results='asis'>>=
  %' withoutRefused$frac <- withoutRefused$frac*100
%' colnames(withoutRefused) <- c("Race", "Number of Responses", "Percent", "Year")
%' 
%' print(xtable(withoutRefused[order(withoutRefused$Year),], caption="Absolute response numbers and percentages for race responses, with Refused responses removed.", label="racetable", display = c("d", "s", "d","f", "d" )), include.rownames = FALSE)
%' @

#levels(races$race)<- c("Hispanic", "More than \n one", "Refused", "American Indian \n or Alaskan", "Asian", "Black or \n African-American", "Native Hawaiian \n or other Pacific Islander", "Some other \n race",  "White")



scatterplot, out.width="0.99\\linewidth",fig.width = 8.5,fig.height=8.5, fig.cap="Relationship between mean community attachment and meta-knowledge, defined as the difference between Non-white and White ratings of their communities as a place for minorities (2009 data). Communitites where whites highly over-rate their community as a place for minorities are to the left of zero, while communitites where whites under-rate their community are shown to the right. Notice that there is little perceptable relationship between this definition of meta-knowledge and the mean community attachment score."




<<sizedbyratings, fig.cap="Relationship between community satisfaction and $|MK|$, with points sized by Non-white ratings of their community as a place for minorities.">>=
relationplot3 <- ggplot(CA3, aes(y=communitysat, x=diffs, label=citystate, size=nonwhite)) + geom_point()+xlab("Absolute difference between minority and white ratings")+ylab("Community satisfaction") +xlim(0, 0.3) 
# +ggtitle("Relationship between meta-knowledge and community satisfaction") 
relationplot3 + scale_size_continuous(name="Percent positive \n Non-white ratings", labels=c("40%", "50%", "60%", "70%", "80%"))
@

<<CA09>>=
CA09 <- group_by(sotc09, citystate) %>% summarise(aveCA = mean(CA, na.rm=TRUE)) %>% mutate(diffs=abs(newDF09$racediffs))
CA09 <- data.frame(CA09)
@

positivesat <- tbl_df(posform) %>% filter(year==2009) %>% group_by(citystate) %>% summarise(newfrac=sum(Freq))
CA3 <- merge(CA09, positivesat)
CA3$nonwhite <- newDF09$nonwhite
names(CA3) <- c("citystate", "aveCA", "diffs", "communitysat", "nonwhite")


#+ggtitle("Relationship between meta-knowledge and community satisfaction") 
relationplot2 <- relationplot2 + geom_text(data = masterData[masterData$citystate %in% c( "Gary, IN", "Duluth, MN", "State College, PA", "Bradenton, FL", "Detroit, MI" , "Macon, GA", "Miami, FL", "Wichita, KS", "Grand Forks, ND"),], aes(x=abs(racediffs), y=communitysat, label = citystate),hjust=-0.1, vjust=-0.4)





Interestingly, this relationship can also be seen with Gallup Poll's favorite composite variable, Community Attachment (discussed in Section \ref{data}). For those model estimates, see Table \ref{model2}.

<<CAmodel, results='asis'>>=
  # m13 <- lm(`Average Community Attachment`~`|MK|`+`Non-white ratings`, data=CA3)
  # print(xtable(summary(m13), caption="Linear model predicting average 2009 Community Attachment scores using $|MK|$ and ratings by Non-white respondents.", label="model2"))
  @

  
  %' 
%' First, to study whether the relationships between $|MK_R$, $|MK_S$, and $MK_C$ and community satisfaction persist on the larger scale, we examined the model shown in Table \ref{model1}. 
%' 
%' <<MKmodel, results='asis'>>=
  %' masterData$absrace <- abs(masterData$racediffs)
%' masterData$abssenior <- abs(masterData$seniordiffs)
%' 
%' masterData <- rename(masterData, `|MK_R|`=absrace, `|MK_S|`=abssenior, `MK_C`=kiddiffs, 'Community satisfaction'=communitysat)
%' m1 <- lm(`Community satisfaction`~`|MK_R|`+`|MK_S|`+`MK_C`, data=masterData)
%' print(xtable(summary(m1), caption="Linear model predicting community satisfaction scores using $|MK_R|$, $|MK_S|$ and $MK_C$.", label="model1"))
%' @
%' 
%' While the $|MK_R|$ term is significant at the 10\% level, the other meta-knowledge components do not appear to be significant. The model tells us that without any prior information we would predict a community satisfaction rating of 75\%. However, with a one unit increase in $|MK_R|$ (meaning, a much larger discrepency between White and non-White ratings), we would see a 4 point increase in community satisfaction rating.
%' 
%' This model explains only a small portion of the variability in the data, but intuitively it makes sense that there would be other factors influencing overall community satisfaction or attachment. However, it is interesting to think about the relationships shown here. Essentially, communities where Whites were over- or under-rating their community as a place for minorities have higher community satisfaction. However, this could be due to the large majority of White respondents; because the community satisfaction score is generally composed of answers by White respondents, it doesn't represent the minority experience as well. 
%' 
%' As the first model does not appear to support our hypothesis about meta-knowledge, we broaden our exploration to consider the community engagement measures, like the percent of respondents registered to vote, those who volunteer their time, etc. For this model, see Table \ref{model2}
%' 
%' <<moremodels, results='asis'>>=
%' m2 <- lm(`Community satisfaction`~.-citystate-year-seniors-nonseniors-withKids-noKids-nonwhite-white-racediffs-seniordiffs, data=masterData)
%' print(xtable(summary(m2), caption="blah.", label="model12"))
%' @
  %' 
%' 
%' 
%' Of course, even these minor conclusions are not very conclusive. We're working with a non-random sample of communities, and a small one at that. However, the relationships studied here can point to areas for further study. 
  

%' <<absdiff,  out.width="0.76\\linewidth", fig.cap="Relationship between community satisfaction and $|MK_R|$. Communities in the lower left corner had good agreement between Whites and Non-whites about whether their community was a good place for minorities, but a low community satisfaction. Communities in the upper right had a lot of discrepency between Whites and Non-whites, but high community community satisfaction. (2008 and 2009 data).">>=
  %' 
%' relationplot2 <- ggplot(masterData, aes(y=communitysat, x=abs(racediffs), label=citystate)) + geom_point()+xlab("Absolute difference between minority and white ratings")+ylab("Community satisfaction") + xlim(0, 0.25)
%' relationplot2+stat_smooth(method="lm", se=FALSE)
%' @
  
  
  % A followup hypothesis is that meta-knowledge may not actually be good for a community. Perhaps the situation has to get quite bad before a community becomes meta-aware of how it actually treats minorities. In order to explore this hypothesis, we created the plot seen in Figure \ref{fig:sizedbyratings}. Figure \ref{fig:sizedbyratings} shows the same data as Figure \ref{fig:absdiff}, with the addition of the points being sized by the percent of positive responses to the question, ``how is your community as a place for minorities?'' by Non-white respondents. From this plot, it seems plausible that the ratings by Non-white respondents are related to the relationship between community satisfaction and $|MK|$ score.

%' <<absdiffsenior, out.width="0.76\\linewidth", fig.cap="Relationship between community satisfaction and $|MK_S|$. Communities in the lower left corner had good agreement between seniors and non-seniors about whether their community was a good place for seniors, but a low community satisfaction. Communities in the upper right had a lot of discrepency between the two groups, but high community community satisfaction. (2008, 2009, and 2010 data).">>=
%' 
%' relationplot2 <- ggplot(masterData, aes(y=communitysat, x=abs(seniordiffs), label=citystate)) + geom_point()+xlab("Absolute difference between senior and non-senior ratings")+ylab("Community satisfaction") + xlim(0, 0.2)
%' relationplot2+stat_smooth(method="lm", se=FALSE)
%' @

%' <<absdiffkids,out.width="0.76\\linewidth",  fig.cap="Relationship between community satisfaction and $MK_C$. Communities in the lower left corner had good agreement between families with children and childless households about whether their community was a good place for families with kids, but a low community satisfaction. Communities in the upper right had a lot of discrepency between the two groups, but high community community satisfaction. (2008, 2009, and 2010 data).">>=
  %' 
%' relationplot2 <- ggplot(masterData, aes(y=communitysat, x=kiddiffs, label=citystate)) + geom_point()+xlab("Absolute difference between families with children and childless household ratings")+ylab("Community satisfaction") 
%' relationplot2+stat_smooth(method="lm", se=FALSE)
%' @