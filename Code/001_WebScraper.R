#########################
#
# Web Scraper to get the climate data
#
# install.packages("tidyverse")
# install.packages("jsonlite")
# install.packages("plyr")

rm(list = ls())
library(tidyverse)
library(jsonlite)
library(plyr)

location <- c("Ontario, CA")
locationCode <- c("KONT")
locations <- data.frame(location, locationCode)

datesSince <- seq(as.Date("2000-01-01"), as.Date("2015-12-01"), by = "months")
datesUntil <- seq(as.Date("2000-02-01"), as.Date("2016-01-01"), by = "months") - 1

apiKey <- "6532d6454b8aa370768e63d6ba5a832e"
units <- "m"
url <- c("https://api.weather.com/v1/location/",
         ":9:US/observations/historical.json?apiKey=", 
         "&units=",
         "&startDate=",
         "&endDate=")

observations <- list()
numObservations <- 0

for(i in 1:nrow(locations)) {
  for (d in 1:length(datesSince)) {
    reqURL <- paste(url[1], 
                    locations$locationCode[i], 
                    url[2],
                    apiKey,
                    url[3],
                    units,
                    url[4],
                    format(datesSince[d], "%Y%m%d"), 
                    url[5], 
                    format(datesUntil[d], "%Y%m%d"),
                    sep = "")
    
    res <- jsonlite::fromJSON(reqURL)
    
    numObservations <- numObservations + 1
    observations[[numObservations]] <- res %>% as.data.frame
    
    prozent <- d/length(datesSince)*100
    prozent <- round(prozent, digits = 3)
    message(toString(prozent),"% processed")
  }
}

output <- rbind.fill(observations)
save(output,file="Data/climateWeatherMetric.RData")

