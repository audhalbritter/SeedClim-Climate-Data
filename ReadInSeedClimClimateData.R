####################################
# READ IN SEEDCLIM CLIMATE DATA
####################################
#  Read in ITAS
setwd("~/Dropbox/seedclim klimadaten/rawdata by Site/Alr/ITAS")
# Read in UTL
setwd("~/Dropbox/seedclim klimadaten/rawdata by Site/Alr/UTL")

### LIBRARIES
library(plyr)

setwd("~/Dropbox/seedclim klimadaten/rawdata by Site")
# Import the climate logger data and merge ITAS and UTL data separately for each site

#### IMPORT CLIMATE DATA FOR ALL SITES
climate <- ldply(as.list(c("Fau", "Alr", "Ulv", "Vik", "Hog", "Lav", "Arh", "Ram", "Gud", "Ovs", "Ves", "Skj")), ImportData)
head(climate)
unique(climate$logger)
table(climate$logger)

# -5cm, P, PØN and "" are UTL loggers with wrong name. Need to be assigned to the right logger

# subset soilmoisture, precipitation and temperatur loggers into seperate object
temperature <- subset(climate, logger %in% c("temp1", "temp2", "temp200cm", "temp30cm", "", "PØN", "P", "-5cm"))
precipitation <- subset(climate, logger %in% c("Nedbor", "counter"))
soilmoisture <- subset(climate, logger %in% c("Jordf1", "Jordf2"))


