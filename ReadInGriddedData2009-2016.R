### READ IN GRIDDED CLIMATE DATA 2009 - 2015

# Explanation for variables
# TAM er temperatur (døgnmiddel)
# UUM - rel.luftfuktighet
# FFM - middelvind
# NNM - midlere skydekke (i 8-deler)
# RR - døgnnedbør


# LIBRARIES
library("lubridate")

# FUNCTIONS
# Function to read in the data
ReadInFiles <- function(textfile){
  dd <- read.table(textfile)
  colnames(dd) <- c("Site", "Year", "Month", "Day", "Temperature", "RelAirMoisture", "Wind", "CloudCover", "Precipitation") # rename variables
  dd <- dd[-1,] # remove first columne
  dd$Date <- ymd(paste(dd$Year, dd$Month, dd$Day)) # create date object
  return(dd)
}


setwd("~/Dropbox/Bergen/SeedClim Climate/SeedClim-Climate-Data/GriddedClimateData2009-2015")


"P:/Ecological and Environmental Change/SeedClim/met-data/..."
myfiles <- list.files(path="~/Dropbox/Bergen/SeedClim Climate/SeedClim-Climate-Data/GriddedClimateData2009-2015", pattern='\\.dat$', full.names = TRUE)

# make a list of textfiles
ddd <- plyr::ldply(myfiles, ReadInFiles)
head(ddd)
dim(ddd)


ddd <- ReadInFiles("888001.dat")
