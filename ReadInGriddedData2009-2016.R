### READ IN GRIDDED CLIMATE DATA 2009 - 2015

# Explanation for variables
# TAM er temperatur (døgnmiddel)
# UUM - rel.luftfuktighet
# FFM - middelvind
# NNM - midlere skydekke (i 8-deler)
# RR - døgnnedbør


# LIBRARIES
library("lubridate")
library("tidyr")
library("dplyr")
library("ggplot2")

# FUNCTIONS
# Function to read in the data
ReadInFiles <- function(textfile){
  dd <- read.table(textfile, colClasses = "character")
  colnames(dd) <- c("Site", "Year", "Month", "Day", "Temperature", "RelAirMoisture", "Wind", "CloudCover", "Precipitation") # rename variables
  dd <- dd[-1,] # remove first columne
  dd$Date <- ymd(paste(dd$Year, dd$Month, dd$Day)) # create date object
  return(dd)
}


setwd("~/Dropbox/Bergen/SeedClim Climate/SeedClim-Climate-Data/GriddedClimateData2009-2015")


"P:/Ecological and Environmental Change/SeedClim/met-data/..."
myfiles <- list.files(path="~/Dropbox/Bergen/SeedClim Climate/SeedClim-Climate-Data/GriddedClimateData2009-2015", pattern='\\.dat$', full.names = TRUE)

# make a list of textfiles
gridclimate <- plyr::ldply(myfiles, ReadInFiles)
head(gridclimate)

climate0915 <- gridclimate %>% 
  # replace site names by real names
  mutate(Site = plyr::mapvalues(Site, c("888001", "888002", "888003", "888004", "888005", "888006", "888007", "888008", "888009", "888010", "888011", "888012"), c("Alr", "Arh", "Fau", "Gud", "Hog", "Lav", "Ovs", "Ram", "Skj", "Ulv", "Ves", "Vik"))) %>% 
  mutate(Site = factor(Site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs"))) %>% 
  mutate(Temperature = as.numeric(Temperature), RelAirMoisture = as.numeric(RelAirMoisture), Wind = as.numeric(Wind), CloudCover = as.numeric(CloudCover), Precipitation = as.numeric(Precipitation))


# Temperature
ggplot(climate0915, aes(x = Date, y = Temperature, color = Site)) +
  geom_line() +
  scale_colour_brewer(palette="Paired") +
  facet_wrap(~ Site) + 
  theme(legend.position="none") +
  ggtitle("Temperature")


# RelAirMoisture
ggplot(climate0915, aes(x = Date, y = RelAirMoisture, color = Site)) +
  geom_line() +
  scale_colour_brewer(palette="Paired") +
  facet_wrap(~ Site) + 
  theme(legend.position="none") +
  ggtitle("Relative Air Moisture")

# Wind
ggplot(climate0915, aes(x = Date, y = Wind, color = Site)) +
  geom_line() +
  scale_colour_brewer(palette="Paired") +
  facet_wrap(~ Site) + 
  theme(legend.position="none") +
  ggtitle("Wind")

# CloudCover
ggplot(climate0915, aes(x = Date, y = CloudCover, color = Site)) +
  geom_line() +
  scale_colour_brewer(palette="Paired") +
  facet_wrap(~ Site) + 
  theme(legend.position="none") +
  ggtitle("Cloud Cover")

# Precipitation
ggplot(climate0915, aes(x = Date, y = Precipitation, color = Site)) +
  geom_line() +
  scale_colour_brewer(palette="Paired") +
  facet_wrap(~ Site) + 
  theme(legend.position="none") +
  ggtitle("Precipitation")


