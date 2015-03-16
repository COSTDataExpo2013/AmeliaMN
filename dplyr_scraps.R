filter(sotc_df, citystate == "St. Paul, MN", community_sat == "Extremely satisfied")
summarise(sotc_df, newvar = mean(age))
summarise(group_by(sotc_df, citystate, community_sat), n())

sotc_df <- tbl_df(sotc10)
tmp <- summarise(group_by(sotc_df, citystate, community_sat), number = n())
tmp2 <- summarize(group_by(tmp, citystate), Freq= sum(number))

#levels(tmp3$community_sat) <- levels(tmp3$community_sat)[c(5,1:3,4)]