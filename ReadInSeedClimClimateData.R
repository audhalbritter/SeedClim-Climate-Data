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

plot_climate(start_date = "2008.1.1", end_date = "2017.1.1", log = c("temp1", "temp2"), inc = TRUE, SITE = "Vik")

# Find file names
temperature %>%
  filter(site == "Hog", logger == "") %>%
  group_by(file) %>%
  summarise(n = n(), MIN = min(date), max = max(date))
  

#### DATA CLEANING ####
temperature$flag <- NA # add column for remarks, when data is doubtfull

# delete crap UTL data
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

# ITAS data
# FAUSKE: delete temp1 logger between June 2013 and October 2014 wrong values
temperature$value[temperature$file == "Fauske_ITAS_13.6.6-13.9.22.txt" & temperature$logger == "temp1"] <- NA
temperature$value[temperature$file == "Fauske_ITAS_13.9.22-14.5.14.txt" & temperature$logger == "temp1"] <- NA
temperature$value[temperature$file == "Fauske_ITAS_14.5.14-14.10.14.txt" & temperature$logger == "temp1"] <- NA
# delete temp1 logger between 2015 and May 2016 soil logger not in the soil
temperature$value[temperature$file == c("fauske_climate 20150129 - 20150429.txt") & temperature$logger == "temp1"] <- NA
temperature$value[temperature$file == c("fauske_climate 20150429 - 20151009.txt") & temperature$logger == "temp1"] <- NA
temperature$value[temperature$file == c("fauske_climate_spring_2016.txt") & temperature$logger == "temp1"] <- NA
# switch logger temp1 and temp2 always


# VIKESLAND:
# flag winter 2014/15 and 2015/16

# ARHELLEREN
# switch temp1 and temp2 for July 2009 - Oct 2009
temperature$logger[temperature$file == "Arhelleren_08072009.txt", & temperature$logger == "temp1"] replace with temp2
temperature$logger[temperature$file == "Arhelleren_08072009.txt", & temperature$logger == "temp2"] replace with temp1

# OVSTEDAL
# flag after winter 2011/2012

# VESKRE
# flag after Jan 2012
# switch temp1 and temp2 after 2014
Veskre_climate 11102015 - 08122015.txt
Veskre_climate 20150909 - 20151011.txt
Veskre_ITAS_140720_140802.txt
Veskre_ITAS_140802_141003.txt

# RAMBAERA
# switch temp1 and temp2 before 2014
# temp2 in summer 2016 too large variance, probably delete

# HOGSETE
# temp1 in autumn 2015 little variation -> flag
# temp2 in spring 2016 very large variation -> useful?

# ALRUST
# switch logger always
# variation of soil logger after ca. May 2015 is too large, delete?

# ULVHAUGEN
# switch logger always

# LAVISDALEN
# switch logger 2009 - ???
# need to delte data? 2011, something strange happens

# GUDMEDALEN
# switch logger until October 2014
# delete veg logger in summer 2014, was not in veg, but soil

# SKJELLINGAHAUGEN
# 2014 data, temp1 logger very little variation

plot_climate(start_date = "2008.1.1", end_date = "2017.1.1", log = c("temp1", "temp2"), inc = TRUE, SITE = "Skj")
temperature %>%
  filter(site == "Lav", logger == "temp1", year(date) >= "2011") %>%
  group_by(file) %>%
  summarise(n = n(), MIN = min(date), max = max(date))
