##############################
#
# Cleaning the wildfire database
#
# install.packages("RSQLite")
# install.packages("geosphere")

rm(list = ls(all.names = TRUE))
library(RSQLite)
library(geosphere)

con <- dbConnect(RSQLite::SQLite(), "Data/wildfires.sqlite")

# Get wildfires from california
res <- dbSendQuery(con, "SELECT * FROM fires WHERE state = 'CA';")
firesCalifornia <- dbFetch(res, n = Inf)

# Convert discovery date from julian format to normal date
res <- dbSendQuery(con, "SELECT date(DISCOVERY_DATE) AS DISCOVERY_DATE FROM fires WHERE state = 'CA';")
discoveryDates <- dbFetch(res, n = Inf)

firesCalifornia$DISCOVERY_DATE <- discoveryDates

# Close DB connection
dbDisconnect(con)

lat_station <- 33.95110
long_station <- -117.3880

firesCalifornia$distance_m <- NA

lat_long <- data.frame(firesCalifornia$LATITUDE, firesCalifornia$LONGITUDE)
names(lat_long) <- c("latitude", "longitude")
lat_long_matrix <- data.matrix(lat_long, rownames.force = NA)

firesCalifornia$distance_m <- apply(lat_long_matrix, 1, function(x) distm(c(long_station, lat_station), c(x[2], x[1]), fun = distHaversine))

distance_smaller_25km <- firesCalifornia %>% filter(distance_m <= 25000)
save(distance_smaller_25km, file = "Data/fires_close_25km.RData")
