####################################
# READ IN SEEDCLIM CLIMATE DATA
####################################

#### IMPORT CLIMATE DATA FOR ALL SITES
climate <- ldply(as.list(c("Fau", "Alr", "Ulv", "Vik", "Hog", "Lav", "Arh", "Ram", "Gud", "Ovs", "Ves", "Skj")), ImportData)
head(climate)
unique(climate$logger)
table(climate$logger, climate$site)

# Subset soilmoisture, precipitation and temperatur loggers into seperate object
temperature <- subset(climate, logger %in% c("temp1", "temp2", "temp200cm", "temp30cm", "", "PØN", "-5cm"))
precipitation <- subset(climate, logger %in% c("nedbor", "counter"))
soilmoisture <- subset(climate, logger %in% c("jordf1", "jordf2"))

# Explore temparure data and plot
table(temperature$logger, temperature$site)
table(temperature$logger, year(temperature$date))
table(temperature$site, year(temperature$date))

plot_climate(start_date = "2008.1.1", end_date = "2017.1.1", log = c("temp1", "temp2"), inc = TRUE, SITE = "Skj")

temperature %>%
  filter(site == "Hog", logger == "") %>%
  group_by(file) %>%
  summarise(n = n(), MIN = min(date), max = max(date))
  



#### DATA CLEANING ####

# delete crap data
temperature <- temperature[!(temperature$site == "Lav" & temperature$logger == "-5cm"),]
temperature <- temperature[!(temperature$site == "Gud" & temperature$logger == "-5cm"),]
temperature <- temperature[!(temperature$site == "Ovs" & temperature$logger == "-5cm"),]


# rename wrong logger names
stopifnot ((temperature %>% filter(logger %in% c("", "-5cm", "PØN")) %>% summarise(max(date)))[1,1] < as.POSIXct(ymd("2016.1.1")))

temperature$logger[temperature$logger == "PØN"] <- "temp200cm"
temperature$logger[temperature$logger == "-5cm"] <- "temp30cm"
temperature$logger[temperature$file == "1229_30092009.txt"] <- "temp30cm"
temperature$logger[temperature$file == "2488_21092010.txt"] <- "temp200cm"
temperature$logger[temperature$logger == ""] <- "temp30cm"





