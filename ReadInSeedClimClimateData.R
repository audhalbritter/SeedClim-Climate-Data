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
table(climate$logger, climate$site)

# subset soilmoisture, precipitation and temperatur loggers into seperate object
temperature <- subset(climate, logger %in% c("temp1", "temp2", "temp200cm", "temp30cm", "", "PØN", "-5cm"))
precipitation <- subset(climate, logger %in% c("nedbor", "counter"))
soilmoisture <- subset(climate, logger %in% c("jordf1", "jordf2"))

table(temperature$logger, temperature$site)
table(temperature$logger, year(temperature$date))
table(temperature$site, year(temperature$date))

temperature %>% filter(year(date) < 2010, !logger %in% c("temp1", "temp2"), site == "Alr") %>% ggplot(aes(x = date, y = value, colour = logger)) + geom_line()
temperature %>% filter(year(date) < 2010, !logger %in% c("temp1", "temp2"), site == "Arh") %>% ggplot(aes(x = date, y = value, colour = logger)) + geom_line()
temperature %>% filter(year(date) < 2010, !logger %in% c("temp1", "temp2"), site == "Fau") %>% ggplot(aes(x = date, y = value, colour = logger)) + geom_line()
temperature %>% filter(year(date) < 2010, !logger %in% c("temp1", "temp2"), site == "Hog") %>% ggplot(aes(x = date, y = value, colour = logger)) + geom_line()
temperature %>% filter(year(date) < 2010, !logger %in% c("temp1", "temp2"), site == "Ulv") %>% ggplot(aes(x = date, y = value, colour = logger)) + geom_line()
temperature %>% filter(year(date) < 2010, !logger %in% c("temp1", "temp2"), site == "Vik") %>% ggplot(aes(x = date, y = value, colour = logger)) + geom_line()
temperature %>% filter(year(date) < 2010, !logger %in% c("temp1", "temp2"), site == "Lav") %>% ggplot(aes(x = date, y = value, colour = logger)) + geom_line()
temperature %>% filter(year(date) < 2010, !logger %in% c("temp1", "temp2"), site == "Gud") %>% ggplot(aes(x = date, y = value, colour = logger)) + geom_line()
temperature %>% filter(year(date) < 2010, !logger %in% c("temp1", "temp2"), site == "Ovs") %>% ggplot(aes(x = date, y = value, colour = logger)) + geom_line()
temperature %>% filter(year(date) < 2010, !logger %in% c("temp1", "temp2"), site == "Ves") %>% ggplot(aes(x = date, y = value, colour = logger)) + geom_line()

# -5cm, PØN and "" are UTL loggers with wrong name. Need to be assigned to the right logger
pon <- subset(temperature, logger=="PØN")
t5cm <- subset(temperature, logger=="-5cm")
nothing <- subset(temperature, logger=="")
t30 <- subset(temperature, logger=="temp30cm")
t200 <- subset(temperature, logger=="temp200cm")


