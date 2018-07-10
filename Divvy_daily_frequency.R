# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Course: DS 501 Introduction to Data Science
# Date: July 2018
# -------------------------------------------------------------------------
# Title: Divvy_station_frequency.R
# [Project, Asa Puckette, Kathleen Cachel, Dan Compton]
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Load one set of Divvy data and plot the most common to/from staion
# pairings

# install.packages("plyr")
# install.packages("ggmap")
# install.packages("timeDate")
# install.packages("chron")
# install.packages("corrplot")

library(plyr)
library(ggmap)
library(timeDate)
library(chron)
library(corrplot)

# load trip data and station data from disk
Divvy_Trips_2017_Q3 <- read.csv("C:/Divvy/Divvy_Trips_2017_Q3.csv")
station_id          <- read.csv("C:/Divvy/Divvy_Stations_2017_Q3Q4.csv") 

# convert the start_time stamp into a date 
Divvy_Trips_2017_Q3$date <- as.Date(Divvy_Trips_2017_Q3$start_time, "%m/%d/%Y")


# create a data frame to populate with agregate data
df <- as.data.frame(seq(tail(Divvy_Trips_2017_Q3$date, n=1), 
                        Divvy_Trips_2017_Q3$date[1], by="days"))
colnames(df) <- "date"


# define holidays
hlist <- c("USChristmasDay","USGoodFriday","USIndependenceDay","USLaborDay",
           "USNewYearsDay","USThanksgivingDay")        
myholidays  <- dates(as.character(holiday(2012:2019,hlist)),format="Y-m-d")


# load weather data for Chicago O'Hare
# https://www.ncdc.noaa.gov/cdo-web/search
weather <- read.csv("C:/Divvy/chicago_ohare_weather.csv")
weather$DATE <- as.Date(weather$DATE, "%Y-%m-%d")  


# temporary dates for testing
# date <- as.Date("1990-1-1", "%Y-%m-%d")
# date <- as.Date("2017-09-30", "%Y-%m-%d")
# date <- as.Date("2017-12-25", "%Y-%m-%d")



# pre-allocate variables for the for-loop
date <- Divvy_Trips_2017_Q3$date[1]
nrides <- 0

# pre-allocate data frame columns for the for-loop
df$isholiday <- NA
df$isweekend <- NA
df$TMAX      <- NA
df$TMIN      <- NA
df$TAVG      <- NA
df$TRNG      <- NA
df$PRCP      <- NA
df$SNOW      <- NA



# create a new agregate data frame
for (i in 1:nrow(Divvy_Trips_2017_Q3)) {

  if (i == nrow(Divvy_Trips_2017_Q3)) {
    # fill the data frame for the last date in the list
    # without this, the last date is skipped 
    df$nrides[df$date == date] <- nrides

  } else if (Divvy_Trips_2017_Q3$date[i] == date) {
    nrides <- nrides + 1
    
  } else {
    df$nrides[df$date == date] <- nrides
    nrides       <- 1
    date         <- Divvy_Trips_2017_Q3$date[i]
  }
}


# Count the number of rides per day (using start date)
# Tabulate Dates of Divvy Data
date_tab <- table(cut(Divvy_Trips_2017_Q3$date, 'day'))
# Format
date_frequency <- data.frame(Date=format(as.Date(names(date_tab)), "%Y-%m-%d"),
           Frequency=as.vector(date_tab))
date_frequency$Date <- as.Date(date_frequency$Date, "%Y-%m-%d")

for (i in 1:nrow(df)) {
  date  <- as.Date(df$date[i], "%Y-%m-%d")
  df$nrides[i] <- date_frequency$Frequency[date_frequency$Date==date]
}


for (i in 1:nrow(df)) {
  date                          <- df$date[i]
  df$isholiday[df$date == date] <- is.holiday(date, myholidays)
  df$isweekend[df$date == date] <- is.weekend(date)
  df$TMAX[df$date == date]      <- weather$TMAX[weather$DATE==date]
  df$TMIN[df$date == date]      <- weather$TMIN[weather$DATE==date]
  df$TAVG[df$date == date]      <- weather$TAVG[weather$DATE==date]
  df$TRNG[df$date == date]      <- weather$TMAX[weather$DATE==date] - weather$TMIN[weather$DATE==date]
  df$PRCP[df$date == date]      <- weather$PRCP[weather$DATE==date]
  df$SNOW[df$date == date]      <- weather$SNOW[weather$DATE==date]
}

dfcor <- df
dfcor$date <- NULL

correlations <- cor(dfcor)
corrplot(correlations, method="number")






tail(Divvy_Trips_2017_Q3$trip_id, n=1)

# ----------------------------------------------------------------------- #
#   Find most frequent Divvy station pairings                             #
# ----------------------------------------------------------------------- #


# count the number of unique station start-end pairings
station_freq      <- ddply(Divvy_Trips_2017_Q3,
                                .(from_station_id,to_station_id),nrow)
station_freq_sort <- station_freq[order(-station_freq[["V1"]]),]


# add and label columns for start and end station position
station_freq_sort[,c(4:7)] <- NA
header <- c("from_station_id", "to_station_id", "frequency", "from_long",
            "from_lat", "to_long", "to_lat")
colnames( station_freq_sort ) <- unlist(header)

# look at top 20 station pairs, lookup long and lat of start and end
# stations
max  = 20


xmin = -87.635
xmax = -87.555
ymin = 41.855
ymax = 41.935
# 
# xmin = -87.81
# xmax = -87.54
# ymin = 41.73
# ymax = 42.07


for (i in 1:max){
  
  if (i == 1) {
    size = 5;
    
    
    # dev.new(width=4, height=4)
    plot( xmin,ymin, type="n", xlim=c(xmin, xmax), ylim=c(ymin, ymax))
  }
    
  from_id   <- station_freq_sort[i,1]
  to_id     <- station_freq_sort[i,2]
  if (from_id == to_id) {
    # skip if to and from station is the same
  } else {
    from_lat  <- station_id[station_id[,1]==from_id,4]
    from_long <- station_id[station_id[,1]==from_id,5]
    to_lat    <- station_id[station_id[,1]==to_id,4]
    to_long   <- station_id[station_id[,1]==to_id,5]
    
    station_freq_sort[i,4] <- from_long
    station_freq_sort[i,5] <- from_lat
    station_freq_sort[i,6] <- to_lat
    station_freq_sort[i,7] <- to_long

    arrows(from_long, from_lat, to_long, to_lat, col='blue', 
           lwd=size, length = 0.15)
    size = size - size/20
  }

}


# ----------------------------------------------------------------------- #
#   Plot Results over a map of Chicago (in work)                          #
# ----------------------------------------------------------------------- #


#  https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/ggmap/ggmapCheatsheet.pdf
#  https://stackoverflow.com/questions/17843580/how-to-get-a-map-within-the-latitude-and-longitude-range

map <- get_map(location = c(lon = -87.6408871,
                            lat = 41.8763843), zoom = 12)

# p1 <- ggmap( map )
png("divvy_map.png", width = 640, height = 640)
  ggmap( map )
  scale_x_continuous( limits = c( xmin , xmax ) , expand = c( 0 , 0 ) ) 
  scale_y_continuous( limits = c( ymin , ymax ) , expand = c( 0 , 0 ) )
  dev.off()
# require( gridExtra )
# grid.arrange(p2)
# 
# plot(p2)
# ggmap()