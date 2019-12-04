# install.packages("dplyr")
# install.packages("anytime")
# install.packages("data.table")
# install.packages("mltools")

library(dplyr)
library(anytime)
library(data.table)
library(mltools)

rm(list=ls())

#####Cleaning weather dataset#####

load("Data/climateWeatherMetric.RData")

time <- rep(NA, nrow(output))
time <- anytime(output$observations.valid_time_gmt)
time <- time - 28800

#add timestamp
D <- cbind(time, output)
rm(time)

###Step 1: remvoing NA columns and obviously useless data###
deselect <- c("metadata.language", "metadata.transaction_id","metadata.version", 
              "metadata.location_id", "metadata.expire_time_gmt", "metadata.status_code",
              "observations.key","observations.class","observations.obs_id",
              "observations.obs_name", "observations.wdir_cardinal","observations.uv_desc",
              "observations.feels_like", "observations.uv_index","observations.qualifier",
              "observations.qualifier_svrty", "observations.blunt_phrase", 
              "observations.terse_phrase", "observations.clds","observations.water_temp",
              "observations.primary_wave_period","observations.primary_wave_height","observations.primary_swell_period",
              "observations.primary_swell_height","observations.primary_swell_direction",
              "observations.secondary_swell_period", "observations.secondary_swell_height",
              "observations.secondary_swell_direction", "observations.wc","observations.vis",
              "observations.heat_index")
D <- select(D, -deselect)


###Step 2: summarize data to daily values###
#indexing days
count <- 0
index <- rep(NA, nrow(D)-1)
for(i in 1:(nrow(D)-1)){ 
  if(as.Date(D$time[i])==as.Date(D$time[i+1])){
    index[i] <- count
  }else{
    count <- count + 1
    index[i] <- count
  }
  if(i %% 1000 == 0){
    perc <- i/nrow(D)*100
    perc <- round(perc, digits = 2)
    out <- paste(perc, "%", sep = "")
    message(out)
  }
}
index[nrow(D)] = count + 1
D <- cbind(index, D)
rm(index, count, i)



#rows for new data frame
n <- length(unique(D$index))
tavg <- rep(NA, n) #average
tmax <- rep(NA, n) #absolute
tmin <- rep(NA, n) #absolute
weather <- rep(NA, n) #majority
rh <- rep(NA, n) #average
pressure <- rep(NA, n) #average
wdir <- rep(NA, n) #majority
wspd <- rep(NA, n) #average
precip_total <- rep(NA, n) #absolute


source("Code/000_Functions.R")

tempCrawler <- NA
weatherCrawler <- NA
rhCrawler <- NA
pressureCrawler <- NA
wdirCrawler <- NA
wspdCrawler <- NA

count <- 1 #day counting variable

for(i in 1:nrow(D)){
  if(D$index[i] != count){
    #average values    
    tavg[count] <- mean(tempCrawler, na.rm = TRUE)
    rh[count] <- mean(rhCrawler, na.rm = TRUE)
    pressure[count] <- mean(pressureCrawler, na.rm = TRUE)
    wspd[count] <- mean(wspdCrawler, na.rm = TRUE)
    
    #majority values
    weather[count] <- majority(weatherCrawler)
    wdir[count] <- majority(wdirCrawler)
    
    #reset 
    tempCrawler <- NA
    weatherCrawler <- NA
    rhCrawler <- NA
    pressureCrawler <- NA
    wdirCrawler <- NA
    wspdCrawler <- NA
    
    count <- count + 1
  }
  
  #average values
  tempCrawler <- c(tempCrawler, D$observations.temp[i])
  rhCrawler <- c(rhCrawler, D$observations.rh[i])
  pressureCrawler <- c(pressureCrawler, D$observations.pressure[i])
  wspdCrawler <- c(wspdCrawler, D$observations.wspd[i])
  
  #absolute values
  if(!is.na(D$observations.max_temp[i])){
    tmax[count] <- D$observations.max_temp[i]
  }
  if(!is.na(D$observations.min_temp[i])){
    tmin[count] <- D$observations.min_temp[i]
  }
  if(!is.na(D$observations.precip_total[i])){
    precip_total[count] <- D$observations.precip_total[i]
  }
  
  #majority values
  weatherCrawler <- c(weatherCrawler, D$observations.wx_icon[i])
  wdirCrawler <- c(wdirCrawler, D$observations.wdir[i])
  
  if(i %% 1000 == 0){
    perc <- i/nrow(D)*100
    perc <- round(perc, digits = 2)
    out <- paste(perc, "%", sep = "")
    message(out)
  }
}
message("100%")
rm(out, perc, i, count)

#snow removed because all observations are NA

#copy date
d <- rep(NA, n)
d[1] <- D$time[1]

count <- 1
for(i in 1:nrow(D)){
  if(D$index[i] != count){
    d[count] <- D$time[i-1]
    count <- count +1
  }
}

D2 <- data.frame(d, tavg, tmax, tmin, weather, rh, pressure, wdir, wspd, precip_total)
D2$d <- as.Date(anytime(D2$d))

#precip_total and precip_hrly don't match, add separate precip_hrly column and compare
precip <- rep(NA, n)
hprecipCrawler <- 0
count <- 1
for(i in 1:nrow(D)){
  if(D$index[i] != count){
    precip[count] <- hprecipCrawler
    
    hprecipCrawler <- 0
    count <- count +1
  }
  if(!is.na(D$observations.precip_hrly[i])){
    hprecipCrawler <- hprecipCrawler + D$observations.precip_hrly[i]
  }
}
D3 <- cbind(D2, precip)

###Step 3: add days since last rain and heat period variables###
#last recorded rain was just in the last night of 1999
D4 <- dslr(D3, 0)
D5 <- last_week_temp(D4)

#remove precip_total
D5 <- select(D5, -precip_total)
D6 <- D5
#one-hot-encoding of weather
D6$weather <- as.character(D6$weather)
ecd <- data.frame(
  id = D6$d,
  weather = D6$weather
)
ohweather <- one_hot(as.data.table(ecd))
D6 <- cbind(D6, ohweather[,-1])

#final removal of obsolete columns
D6 <- select(D6, -c("wdir","weather"))

rm(list=setdiff(ls(), c("D6","dslr","last_week_temp","majority","n")))

###

cleanedWeather <- D6


save(cleanedWeather, file = "Data/WeatherCleaned.RData")
