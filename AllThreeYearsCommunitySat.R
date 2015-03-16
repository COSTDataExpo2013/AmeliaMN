# <<communitysat2010>>=
#   sotc_df <- tbl_df(sotc10)
# tmp3 <- dplyr::summarise(group_by(sotc_df, citystate, community_sat), n = n()) 
# tmp3 <- dplyr::mutate(tmp3, frac=prop.table(n))
# 
# # extremelysat <- subset(tmp3, community_sat=="Extremely satisfied")
# # mostsat <- order(extremelysat$frac, decreasing=TRUE)
# # tmp3$citystate <- factor(tmp3$citystate, levels=levels(tmp3$citystate)[mostsat])
# 
# #Now all I have to do is make the plot
# neutral <- c("3")
# positive <- c("Extremely satisfied", "4")
# negative <- c("2", "Not at all satisfied")
# 
# neu <- filter(tmp3, community_sat %in% neutral)
# neu$frac <- neu$frac/2
# 
# pos <- filter(tmp3, community_sat %in% positive)
# pos <- rbind(pos, neu)
# 
# extremelysat <- pos %.% group_by(citystate) 
# extremelysat <- dplyr::summarise(extremelysat, tot=sum(frac)) 
# mostsat <- order(extremelysat$tot)
# pos$citystate <- factor(pos$citystate, levels=levels(pos$citystate)[mostsat])
# 
# neg <- filter(tmp3, community_sat %in% negative)
# neg <- rbind(neg, neu)
# neg$citystate <- factor(neg$citystate, levels=levels(neg$citystate)[mostsat])
# 
# neg$frac <- -neg$frac
# neg$community_sat <- factor(neg$community_sat)
# neg$community_sat <- factor(neg$community_sat, levels=levels(neg$community_sat)[c(2,1,3)])
# pos$community_sat <- factor(pos$community_sat)
# pos$community_sat <- factor(pos$community_sat, levels=levels(pos$community_sat))
# 
# require(RColorBrewer)
# colors1 <- brewer.pal(5, "Spectral")
# colors1 <- colors1[c(2,3,4,5,1)]
# @
# 
# <<communitysat2010plot, fig.cap="Community satisfaction in 2010">>=
#   baseplot <- ggplot(tmp3) + aes(x=citystate, y=frac, fill = community_sat, order=community_sat)
# baseplot <- baseplot  +geom_bar(data = neg, stat = "identity") + scale_fill_manual(breaks=c("Not at all satisfied", "2", "3", "4", "Extremely satisfied"), values=colors1, name="Response") + geom_bar(data = pos, stat = "identity")
# baseplot <- baseplot +coord_flip() + ggtitle("Community satisfaction in 2010") + xlab("") +ylab("")+ggtitle("Community satisfaction in 2010")
# baseplot <- baseplot + scale_y_continuous(breaks=seq(from=-0.5, to=0.75, by=0.25), labels=c("50%", "25%", "0", "25%", "50%", "75%"))
# baseplot 
# @













sotc_df <- tbl_df(sotc10)
sotc_df09 <- tbl_df(sotc09)
sotc_df08 <- tbl_df(sotc08)
tmp1 <- dplyr::summarise(group_by(sotc_df08, citystate, community_sat), n = n()) 
tmp1 <- dplyr::mutate(tmp1, frac=prop.table(n))
tmp1$year <- 2008
tmp2 <- dplyr::summarise(group_by(sotc_df09, citystate, community_sat), n = n()) 
tmp2 <- dplyr::mutate(tmp2, frac=prop.table(n))
tmp2$year <- 2009
tmp3 <- dplyr::summarise(group_by(sotc_df, citystate, community_sat), n = n()) 
tmp3 <- dplyr::mutate(tmp3, frac=prop.table(n))
tmp3$year <- 2010

tmp <- rbind(tmp1, tmp2, tmp3)

#Now all I have to do is make the plot
neutral <- c("3")
positive <- c("Extremely satisfied", "4")
negative <- c("2", "Not at all satisfied")

neu <- filter(tmp, community_sat %in% neutral)
neu$frac <- neu$frac/2

pos <- filter(tmp, community_sat %in% positive)
pos <- rbind(pos, neu)

extremelysat <- subset(pos, year==2010) %.% group_by(citystate) 
extremelysat <- dplyr::summarise(extremelysat, tot=sum(frac)) 
mostsat <- order(extremelysat$tot)
pos$citystate <- factor(pos$citystate, levels=levels(pos$citystate)[mostsat])

neg <- filter(tmp, community_sat %in% negative)
neg <- rbind(neg, neu)
neg$citystate <- factor(neg$citystate, levels=levels(neg$citystate)[mostsat])

neg$frac <- -neg$frac
neg$community_sat <- factor(neg$community_sat)
neg$community_sat <- factor(neg$community_sat, levels=levels(neg$community_sat)[c(2,1,3)])
pos$community_sat <- factor(pos$community_sat)
pos$community_sat <- factor(pos$community_sat, levels=levels(pos$community_sat))

require(RColorBrewer)
colors1 <- brewer.pal(5, "Spectral")
colors1 <- colors1[c(2,3,4,5,1)]

baseplot <- ggplot(tmp) + aes(x=citystate, y=frac, fill = community_sat, order=community_sat)+facet_wrap(~year)
baseplot <- baseplot  +geom_bar(data = neg, stat = "identity") + scale_fill_manual(breaks=c("Not at all satisfied", "2", "3", "4", "Extremely satisfied"), values=colors1, name="Response") + geom_bar(data = pos, stat = "identity")
baseplot <- baseplot +coord_flip() + ggtitle("Community satisfaction") + xlab("") +ylab("")+ggtitle("Community satisfaction in 2010")
baseplot <- baseplot + scale_y_continuous(breaks=seq(from=-0.5, to=0.75, by=0.25), labels=c("50%", "25%", "0", "25%", "50%", "75%"))
baseplot 
