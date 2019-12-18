### READ IN GRIDDED CLIMATE DATA 2009 - 2017

# Explanation for variables
# TAM er temperatur (døgnmiddel)
# UUM - rel.luftfuktighet
# FFM - middelvind
# NNM - midlere skydekke (i 8-deler)
# RR - døgnnedbør


# LIBRARIES
library("lubridate")
library("tidyverse")
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

# Connect to data on P drive
myfiles <- list.files(path="/Volumes/FELLES/MATNAT/BIO/Ecological and Environmental Change/SeedClimClimateData/met-data/GriddedClimateData2009-2017", pattern='\\.dat$', recursive = TRUE, full.names = TRUE)

# make a list of textfiles
gridclimate <- plyr::ldply(myfiles, ReadInFiles)
head(gridclimate)

climate_all <- gridclimate %>% 
  # replace site names by real names
  mutate(Site = plyr::mapvalues(Site, c("888001", "888002", "888003", "888004", "888005", "888006", "888007", "888008", "888009", "888010", "888011", "888012", "888191", "888192"), c("Alr", "Arh", "Fau", "Gud", "Hog", "Lav", "Ovs", "Ram", "Skj", "Ulv", "Ves", "Vik", "Joa", "Lia"))) %>% 
  mutate(Site = factor(Site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs", "Joa", "Lia"))) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(Temperature = as.numeric(Temperature), RelAirMoisture = as.numeric(RelAirMoisture), Wind = as.numeric(Wind), CloudCover = as.numeric(CloudCover), Precipitation = as.numeric(Precipitation))

climate <- climate_all %>% 
  filter(!Site %in% c("Joa", "Lia"))

climate_threeD <- climate_all %>% 
  filter(Site %in% c("Vik", "Joa", "Lia"))

# Change directory
write_csv(climate, path = "GriddedDailyClimateData2009-2019.csv", col_names = TRUE)
write_csv(climate_threeD, path = "THREE_D_GriddedDailyClimateData2009-2019.csv", col_names = TRUE)

# Calculate Monthly Mean
monthlyClimate <- climate %>%
  select(-Year, -Month, -Day) %>% 
  gather(key = Logger, value = value, -Site, -Date) %>% 
  mutate(dateMonth = dmy(paste0("15-",format(Date, "%b.%Y")))) %>%
  group_by(dateMonth, Logger, Site) %>%
  summarise(n = n(), value = mean(value), sum = sum(value)) %>% 
  mutate(value = ifelse(Logger == "Precipitation", sum, value)) %>% 
  select(-n, -sum)


climate %>%
  as.tibble() %>% 
  # mutate(dateMonth = dmy(paste0("15-",format(Date, "%b.%Y")))) %>%
  # group_by(Site, dateMonth) %>% 
  # summarise(value = sum(Precipitation)) %>% 
  mutate(Year = year(Date)) %>% 
  group_by(Year, Site) %>% 
  summarise(value = sum(Precipitation)) %>% 
  # filter(Logger == "Precipitation") %>% 
  # mutate(year = year(dateMonth)) %>%
  # group_by(Site, year) %>%
  # summarise(value = sum(value)) %>%
  ggplot(aes(x = Year, y = value)) +
  geom_point() +
  facet_wrap(~ Site)


# Calculate Annual Means
annualClimate <- monthlyClimate %>% 
  mutate(dateYear = year(dateMonth)) %>%
  group_by(dateYear, Logger, Site) %>%
  summarise(annualMean = mean(value)) %>% 
  spread(key = Logger, value = annualMean)
  
save(monthlyClimate, annualClimate, file = paste0("GriddedMonth_AnnualClimate2009-2017", ".Rdata"))
#load(file = "GriddedMonth_AnnualClimate2009-2016.RData", verbose = TRUE)


# Making Figures
# Temperature
ggplot(climate, aes(x = Date, y = Temperature, color = Site)) +
  geom_line() +
  scale_colour_brewer(palette="Paired") +
  facet_wrap(~ Site) + 
  theme(legend.position="none") +
  ggtitle("Temperature")


# RelAirMoisture
ggplot(climate, aes(x = Date, y = RelAirMoisture, color = Site)) +
  geom_line() +
  scale_colour_brewer(palette="Paired") +
  facet_wrap(~ Site) + 
  theme(legend.position="none") +
  ggtitle("Relative Air Moisture")

# Wind
ggplot(climate, aes(x = Date, y = Wind, color = Site)) +
  geom_line() +
  scale_colour_brewer(palette="Paired") +
  facet_wrap(~ Site) + 
  theme(legend.position="none") +
  ggtitle("Wind")

# CloudCover
ggplot(climate, aes(x = Date, y = CloudCover, color = Site)) +
  geom_line() +
  scale_colour_brewer(palette="Paired") +
  facet_wrap(~ Site) + 
  theme(legend.position="none") +
  ggtitle("Cloud Cover")

# Precipitation
ggplot(climate, aes(x = Date, y = Precipitation, color = Site)) +
  geom_line() +
  scale_colour_brewer(palette="Paired") +
  facet_wrap(~ Site) + 
  theme(legend.position="none") +
  ggtitle("Precipitation")


