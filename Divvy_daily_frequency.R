# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Course: DS 501 Introduction to Data Science
# Date: July 2018
# -------------------------------------------------------------------------
# Title: Divvy_station_frequency.R
# [Project, Asa Puckette, Kathleen Cachel, Dan Compton]
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Load Divvy data and reshape for regression analysis

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


# ----------------------------------------------------------------------- #
#   Load Data (Update This for your own file structure)                   #
# ----------------------------------------------------------------------- #


# Update based on your working directory
# Include a trailing forward slash at the end! "/"
# Put all the Divvy ride data here with no other files
# Do NOT put the Divvy station data here
mypath = "C:/Divvy/data/"

# load trip data and station data from disk
# Update this with your own filepath 
station_id          <- read.csv("C:/Divvy/Divvy_Stations_2017_Q3Q4.csv") 

# load weather data for Chicago O'Hare
# https://www.ncdc.noaa.gov/cdo-web/search
weather <- read.csv("C:/Divvy/chicago_ohare_weather.csv")
weather$DATE <- as.Date(weather$DATE, "%m/%d/%Y")  

file_names <- dir(mypath) 
file_names <- paste(mypath,file_names, sep = "", collapse = NULL)

for (i in 1:length(file_names)) {
  df_temp <- read.csv(file_names[i]) 
  colnames(df_temp) <- c("trip_id", "start_time", "end_time", "bikeid", 
                         "tripduration", "from_station_id", 
                         "from_station_name", "to_station_id",
                         "to_station_name", "usertype" , "gender" ,
                         "birthyear")
  if (i == 1) {
    Divvy_Trips <- df_temp
  } else {
    Divvy_Trips <- rbind(Divvy_Trips, df_temp) 
  }
  
}



# ----------------------------------------------------------------------- #
#   Process Data, Create a new dataframe with one row per day             #
# ----------------------------------------------------------------------- #

# New data frame df will have daily rider numbers, weather data, and 
# other parameters used for the regression


# convert the start_time stamp into a date 
Divvy_Trips$date <- as.Date(Divvy_Trips$start_time, "%m/%d/%Y")
Divvy_Trips <- Divvy_Trips[order(Divvy_Trips$date),]


# create a data frame to populate with agregate data
df <- as.data.frame(seq(Divvy_Trips$date[1], tail(Divvy_Trips$date, n=1), 
                         by="days"))
colnames(df) <- "date"


# define holidays
hlist <- c("USChristmasDay","USGoodFriday","USIndependenceDay",
           "USLaborDay", "USNewYearsDay","USThanksgivingDay")
myholidays  <- dates(as.character(holiday(2012:2019,hlist)),format="Y-m-d")

# Day off according to the chicago public school calendar?

cps_dates <-c("2016-01-01",
              "2016-02-05",
              "2016-02-15",
              "2016-04-08",
              "2016-04-18",
              "2016-04-19",
              "2016-04-20",
              "2016-04-21",
              "2016-04-22",
              "2016-05-30",
              "2016-06-22",
              "2016-06-23",
              "2016-06-24",
              "2016-06-27",
              "2016-06-28",
              "2016-06-29",
              "2016-06-30",
              "2016-07-01",
              "2016-07-04",
              "2016-07-05",
              "2016-07-06",
              "2016-07-07",
              "2016-07-08",
              "2016-07-11",
              "2016-07-12",
              "2016-07-13",
              "2016-07-14",
              "2016-07-15",
              "2016-07-18",
              "2016-07-19",
              "2016-07-20",
              "2016-07-21",
              "2016-07-22",
              "2016-07-25",
              "2016-07-26",
              "2016-07-27",
              "2016-07-28",
              "2016-07-29",
              "2016-08-01",
              "2016-08-02",
              "2016-08-03",
              "2016-08-04",
              "2016-08-05",
              "2016-08-08",
              "2016-08-09",
              "2016-08-10",
              "2016-08-11",
              "2016-08-12",
              "2016-08-15",
              "2016-08-16",
              "2016-08-17",
              "2016-08-18",
              "2016-08-19",
              "2016-08-22",
              "2016-08-23",
              "2016-08-24",
              "2016-08-25",
              "2016-08-26",
              "2016-08-29",
              "2016-08-30",
              "2016-08-31",
              "2016-09-01",
              "2016-09-02",
              "2016-09-05",
              "2016-10-10",
              "2016-11-04",
              "2016-11-11",
              "2016-11-23",
              "2016-11-24",
              "2016-11-25",
              "2016-12-26",
              "2016-12-27",
              "2016-12-28",
              "2016-12-29",
              "2016-12-30",
              "2017-01-02",
              "2017-01-03",
              "2017-01-04",
              "2017-01-05",
              "2017-01-06",
              "2017-01-16",
              "2017-02-03",
              "2017-02-20",
              "2017-04-07",
              "2017-04-10",
              "2017-04-11",
              "2017-04-12",
              "2017-04-13",
              "2017-04-14",
              "2017-05-29",
              "2017-06-21",
              "2017-06-22",
              "2017-06-23",
              "2017-06-26",
              "2017-06-27",
              "2017-06-28",
              "2017-06-29",
              "2017-06-30",
              "2017-07-03",
              "2017-07-04",
              "2017-07-05",
              "2017-07-06",
              "2017-07-07",
              "2017-07-10",
              "2017-07-11",
              "2017-07-12",
              "2017-07-13",
              "2017-07-14",
              "2017-07-17",
              "2017-07-18",
              "2017-07-19",
              "2017-07-20",
              "2017-07-21",
              "2017-07-24",
              "2017-07-25",
              "2017-07-26",
              "2017-07-27",
              "2017-07-28",
              "2017-07-31",
              "2017-08-01",
              "2017-08-02",
              "2017-08-03",
              "2017-08-04",
              "2017-08-07",
              "2017-08-08",
              "2017-08-09",
              "2017-08-10",
              "2017-08-11",
              "2017-08-14",
              "2017-08-15",
              "2017-08-16",
              "2017-08-17",
              "2017-08-18",
              "2017-08-21",
              "2017-08-22",
              "2017-08-23",
              "2017-08-24",
              "2017-08-25",
              "2017-08-28",
              "2017-08-29",
              "2017-08-30",
              "2017-08-31",
              "2017-09-01",
              "2017-09-04",
              "2017-10-09",
              "2017-11-03",
              "2017-11-22",
              "2017-11-23",
              "2017-11-24",
              "2017-12-25",
              "2017-12-26",
              "2017-12-27",
              "2017-12-28",
              "2017-12-29")
cps_dates <- dates(cps_dates,format="Y-m-d")




# temporary dates for testing
# date <- as.Date("1990-1-1", "%Y-%m-%d")
# date <- as.Date("2017-09-30", "%Y-%m-%d")
# date <- as.Date("2017-12-25", "%Y-%m-%d")



# pre-allocate variables for the for-loop
date       <- Divvy_Trips$date[1]
nrides     <- 0
nrides_76  <- 0
nrides_35  <- 0
nrides_177 <- 0
nrides_268 <- 0
nrides_85  <- 0



# pre-allocate data frame columns for the for-loop
df$cps            <- NA
df$isholiday      <- NA
df$isweekend      <- NA
df$AWND           <- NA
df$TMAX           <- NA
df$TMIN           <- NA
df$TAVG           <- NA
df$TRNG           <- NA
df$PRCP           <- NA
df$SNOW           <- NA
df$nrides         <- 0
df$nrides_76      <- 0
df$nrides_35      <- 0
df$nrides_177     <- 0
df$nrides_268     <- 0
df$nrides_85      <- 0
df$nrides_76r     <- 0
df$nrides_35r     <- 0
df$nrides_177r    <- 0
df$nrides_268r    <- 0
df$nrides_85r     <- 0


# Count the number of rides for the five most popular stations
#   Total rides per day of the five most popular stations
#   and total rides per day that start and stop at those
#   five most popular stations
#   For example, nrides_76 is the total number of daily rides
#     departing from station 76 while ntides_76r is the total
#     number of daily rides that start and stop at station
#     76.
for (i in 1:nrow(Divvy_Trips)) {

  if (Divvy_Trips$from_station_id[i] == 76) {
    df$nrides_76[df$date == Divvy_Trips$date[i]] = 
      df$nrides_76[df$date == Divvy_Trips$date[i]] + 1
    
    if (Divvy_Trips$from_station_id[i] == 76 &
        Divvy_Trips$to_station_id[i]== 76) {
      df$nrides_76r[df$date == Divvy_Trips$date[i]] = 
        df$nrides_76r[df$date == Divvy_Trips$date[i]] + 1
    }
  } 
  else if (Divvy_Trips$from_station_id[i] == 35) {
    df$nrides_35[df$date == Divvy_Trips$date[i]] = 
      df$nrides_35[df$date == Divvy_Trips$date[i]] + 1
    
    if (Divvy_Trips$from_station_id[i] == 35 & 
        Divvy_Trips$to_station_id[i]== 35) {
      df$nrides_35r[df$date == Divvy_Trips$date[i]] = 
        df$nrides_35r[df$date == Divvy_Trips$date[i]] + 1
    }
  }
  else if (Divvy_Trips$from_station_id[i] == 177) {
    df$nrides_177[df$date == Divvy_Trips$date[i]] = 
      df$nrides_177[df$date == Divvy_Trips$date[i]] + 1
    
    if (Divvy_Trips$from_station_id[i] == 177 & 
        Divvy_Trips$to_station_id[i]== 177) {
      df$nrides_177r[df$date == Divvy_Trips$date[i]] = 
        df$nrides_177r[df$date == Divvy_Trips$date[i]] + 1
    }
  }
  else if (Divvy_Trips$from_station_id[i] == 268) {
    df$nrides_268[df$date == Divvy_Trips$date[i]] = 
      df$nrides_268[df$date == Divvy_Trips$date[i]] + 1
    if (Divvy_Trips$from_station_id[i] == 268 & 
        Divvy_Trips$to_station_id[i]== 268) {
      df$nrides_268r[df$date == Divvy_Trips$date[i]] = 
        df$nrides_268r[df$date == Divvy_Trips$date[i]] + 1
    }
  }
  else if (Divvy_Trips$from_station_id[i] == 85) {
    df$nrides_85[df$date == Divvy_Trips$date[i]] = 
      df$nrides_85[df$date == Divvy_Trips$date[i]] + 1
    if (Divvy_Trips$from_station_id[i] == 85 & 
        Divvy_Trips$to_station_id[i]== 85) {
      df$nrides_85r[df$date == Divvy_Trips$date[i]] = 
        df$nrides_85r[df$date == Divvy_Trips$date[i]] + 1
    }
  }
}


# Count the total number of rides per day (using start date)
# Tabulate Dates of Divvy Data
date_tab <- table(cut(Divvy_Trips$date, 'day'))
# Format
date_frequency <- data.frame(Date=format(as.Date(names(date_tab)), "%Y-%m-%d"),
           Frequency=as.vector(date_tab))
date_frequency$Date <- as.Date(date_frequency$Date, "%Y-%m-%d")

for (i in 1:nrow(df)) {
  date  <- as.Date(df$date[i], "%Y-%m-%d")
  df$nrides[i] <- date_frequency$Frequency[date_frequency$Date==date]
}


# Add weather data from Chicago O'Hare airport
for (i in 1:nrow(df)) {
  date                          <- df$date[i]
  df$cps[df$date == date]       <- is.holiday(date, cps_dates)
  df$isholiday[df$date == date] <- is.holiday(date, myholidays)
  df$isweekend[df$date == date] <- is.weekend(date)
  df$AWND[df$date == date]      <- weather$AWND[weather$DATE==date]
  df$TMAX[df$date == date]      <- weather$TMAX[weather$DATE==date]
  df$TMIN[df$date == date]      <- weather$TMIN[weather$DATE==date]
  df$TAVG[df$date == date]      <- weather$TAVG[weather$DATE==date]
  df$TRNG[df$date == date]      <- weather$TMAX[weather$DATE==date] - 
    weather$TMIN[weather$DATE==date]
  df$PRCP[df$date == date]      <- weather$PRCP[weather$DATE==date]
  df$SNOW[df$date == date]      <- weather$SNOW[weather$DATE==date]
}

# Correlation Plot
dfcor <- df
dfcor$date <- NULL

correlations <- cor(dfcor)
corrplot(correlations, method="number")


# make data frame columns for days of the week
# it doesn't matter which day is which for now, just that
#     they're consistent
df$weekday_color <- 0
df$day1 <- 0
df$day2 <- 0
df$day3 <- 0
df$day4 <- 0
df$day5 <- 0
df$day6 <- 0

# Also color the days of the week for visualization 
cl = 1;
for (i in 1:nrow(df)) {
  if (cl == 1) {
    df$weekday_color[i] <- "red"
    df$day1[i] = 1
  } else if (cl == 2) {
    df$weekday_color[i] <- "orange"
    df$day2[i] = 1
  } else if (cl == 3) {
    df$weekday_color[i] <- "yellow"
    df$day3[i] = 1
  } else if (cl == 4) {
    df$weekday_color[i] <- "green"
    df$day4[i] = 1
  } else if (cl == 5) {
    df$weekday_color[i] <- "blue"
    df$day5[i] = 1
  } else if (cl == 6) {
    df$weekday_color[i] <- "purple"
    df$day6[i] = 1
  } else if (cl == 7) {
    df$weekday_color[i] <- "brown"
    df$day1[i] = 1
  }
  if (cl == 7) {
    cl = 1
  } else {
    cl = cl + 1
  }
}

# The ridership clearly 




# ----------------------------------------------------------------------- #
#  Regression Model(s)                                                    #
# ----------------------------------------------------------------------- #

# perhaps inches of precipitation has a stronger correlation
# squared 
df$PRCP2 <- df$PRCP

# first, model the number of rides per day
model_nrides <- lm(nrides ~ isholiday + isweekend + AWND + TMAX + 
                     TMIN + TAVG + PRCP + PRCP2 + SNOW + 
                     day1 + day2+ day3 + day4 + day5 + day6 + cps, data = df)
summary(model_nrides)

df$nrides_model <- predict(model_nrides,df)


plot(df$nrides_model-df$nrides, col=df$weekday_color)
plot(df$nrides)
df$nrides_diff   <- df$nrides_model-df$nrides



# second, model the number of rides per day from station 76
model_nrides_76 <- lm(nrides_76 ~ isholiday + isweekend + AWND + TMAX + 
                        TMIN + TAVG + PRCP + PRCP2 + SNOW + 
                        day1 + day2+ day3 + day4 + day5 + day6 + cps, data = df)
summary(model_nrides_76)
df$nrides_76_model <- predict(model_nrides_76,df)

plot(df$nrides_76_model-df$nrides_76, col=df$weekday_color)
# plot(df$nrides_76)


# third, model the number of rides per day from sstation 76 that also
# end at station 76 

model_nrides_76r <- lm(nrides_76r ~ isholiday + isweekend + AWND + TMAX + 
                         TMIN + TAVG + PRCP + PRCP2 + SNOW + 
                         day1 + day2+ day3 + day4 + day5 + day6 + cps, data = df)
summary(model_nrides_76r)


save(correlations, date_frequency, df, dfcor, Divvy_Trips,
     model_nrides, model_nrides_76, model_nrides_76r,
     station_id, hlist, myholidays, file = "Divvy.RData")

save(correlations, date_frequency, df, dfcor,
     model_nrides, model_nrides_76, model_nrides_76r,
     station_id, hlist, myholidays, file = "Divvy_DailyOnly.RData")


# tail(Divvy_Trips$trip_id, n=1)






# ----------------------------------------------------------------------- #
#   Plot most frequent Divvy station pairings                             #
# ----------------------------------------------------------------------- #


# count the number of unique station start-end pairings
station_freq      <- ddply(Divvy_Trips,
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