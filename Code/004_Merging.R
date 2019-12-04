library(dplyr)

rm(list = ls())

#####cleaning dataset 2 and merging the two datasets together#####

load("Data/WeatherCleaned.RData")
names(cleanedWeather)[1] <- "date"
load("Data/fires_close_25km.RData")
d <- distance_smaller_25km #easy to use name
rm(distance_smaller_25km)


d <- d[which(d$FIRE_SIZE_CLASS != "A"), ]

d <- select(d, DISCOVERY_DATE) #we only need the dates as all dates in the set mark a forest fire
dup <- duplicated(d) #one fire a day is enouh for us as this is a classification problem
d <- d[which(!dup),] #deselect duplicates
d[,2] <- rep(1, nrow(d)) #adding the y=1 value for later
d[,1] <- as.Date(d[,1])
rm(dup)


colnames(d) <- c("date", "fire")
D <- left_join(cleanedWeather, d, by = "date")
D$fire <- ifelse(is.na(D$fire), 0, 1)
D <- D[-nrow(D),]


save(D, file = "Data/regressionData.RData")


