# Got data from http://www.census.gov/popest/data/intercensal/index.html

popdata <- read.csv("data/SUB-EST00INT.csv")
sotc10 <- read.csv("~/Dropbox/RStudioStuff/RStudioStuff/WorkingDocuments/SoulOfCommunity/sotc10.csv")
places <- levels(sotc$citystate)
places <- strsplit(places, ", ")

placesI <- c(68201, 54133, 37974, 3940, 5057,49950, 67591, 6093, 28830, 32606, 15446, 15461, 51356, 25561, 3079, 6277, 5261, 6295, 67701, 5311, 62395,3221,33088,62550, 5387, 21820)

placesPop <- data.frame(places, popdata[placesI, c(6:20)])
dput(placesPop, file="popdata.robj")