####################################
# READ IN SEEDCLIM CLIMATE DATA
####################################

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
temperature %>% filter(year(date) < 2010, !logger %in% c("temp1", "temp2", "temp30cm"), site == "Ves") %>% ggplot(aes(x = date, y = value, colour = logger)) + geom_line()


temperature %>% filter(year(date) > 2009, year(date) < 2015, logger %in% c("", "temp30cm"), site == "Hog") %>% ggplot(aes(x = date, y = value, colour = logger)) + geom_line()
temperature %>% filter(year(date) > 2009, year(date) < 2015, logger %in% c("", "temp30cm"), site == "Lav") %>% ggplot(aes(x = date, y = value, colour = logger)) + geom_line()
temperature %>% filter(year(date) > 2009, year(date) < 2015, logger %in% c("", "temp30cm"), site == "Ram") %>% ggplot(aes(x = date, y = value, colour = logger)) + geom_line()
temperature %>% filter(year(date) > 2009, year(date) < 2015, logger %in% c("", "temp200cm"), site == "Vik") %>% ggplot(aes(x = date, y = value, colour = logger)) + geom_line()
