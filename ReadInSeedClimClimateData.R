####################################
 # READ IN SEEDCLIM CLIMATE DATA #
####################################

### LIBRARIES
library("lubridate")
library("tidyverse")
library("data.table")

#### IMPORT CLIMATE DATA FOR ALL SITES ####
source('~/Dropbox/Bergen/SeedClim Climate/SeedClim-Climate-Data/Functions_ReadInSeedClimClimateData.R')

# DATA FROM OLD PLACE
oldClimateRepo <- list.files(path = "/Volumes/felles/MATNAT/BIO/Felles/007_Funcab_Seedclim/SeedClimClimateData/rawdata by Site/", 
                             pattern = "txt", recursive = TRUE, full.names = TRUE) %>% 
  grep(pattern = "Notes|Notater|UVB", x = ., invert = TRUE, value = TRUE, ignore.case = TRUE) %>%
  grep(pattern = "ITAS\\d{0,4}\\.txt|ITAS-FALL-2014\\.txt", x = ., invert = TRUE, value = TRUE, ignore.case = TRUE) %>%
  ### files that need fixing!!!
  grep(pattern = "1239_23062009.txt", x = ., invert = TRUE, value = TRUE, ignore.case = TRUE) %>%
  #(function(.).[(1:50)]) %>% # only run subset
  map_df(ReadData) %>% 
  mutate(Repo = "old_Bio_Felles")

# DATA FROM NEW PLACE
newClimateRepo <- list.files(path = "/Volumes/felles/MATNAT/BIO/Ecological and Environmental Change/SeedClimClimateData/Climate_data_loggers", 
#climate2 <- list.files(path = "~/Dropbox/seedclim klimadaten/rawdata by Site", 
           pattern = "txt", recursive = TRUE, full.names = TRUE) %>% 
  grep(pattern = "Notes|Notater|UVB", x = ., invert = TRUE, value = TRUE, ignore.case = TRUE) %>%
  grep(pattern = "ITAS\\d{0,4}\\.txt|ITAS-FALL-2014\\.txt", x = ., invert = TRUE, value = TRUE, ignore.case = TRUE) %>%
  #(function(.).[(1:50)]) %>% # only run subset
  map_df(ReadData) %>% 
  mutate(Repo = "new_Bio_EcologicalandEnvironmentalChange")

# Warnings that are ok
# format not recognised: Skjellingahaugen-met1-20120910.txt format not recognised (corrupted file)

# remove first two and last two rows for each file.
newClimateRepo <- newClimateRepo %>% 
  group_by(file) %>% 
  slice(-c(1:2, n()-1, n()))

# bind rows of old and new repo
climate <- oldClimateRepo %>% 
  # remove wrong files
  filter(file != "Skjellingahaugen_ITAS_110624_111015.txt") %>% 
  group_by(file) %>% 
  # remove first two and last two rows for each file.
  slice(-c(1:2, n()-1, n())) %>% 
  bind_rows(newClimateRepo) %>% 
  setDT()

# remove duplicate files
climate <- climate %>% 
  group_by(date, logger, site, type, value) %>% 
  slice(1) %>% 
  group_by(date, logger, site, type) %>% 
  slice(1)

#save(climate, file = "climate.Rdata")
#load(file = "climate.Rdata")  


#### CLEAN DATA ####
# get rid of 1900 date, all empty lines
climate <- climate %>% 
  filter(!date < "2000-01-01")


# Check logger names
unique(climate$logger)
table(climate$logger, climate$site)

# check if there is still an x3 and NA
climate %>% filter(logger == "x3") %>% ungroup() %>% distinct(file, Repo)
climate %>% filter(is.na(logger))

# Subset soilmoisture, precipitation and temperatur loggers into seperate object
temperature <- subset(climate, logger %in% c("temp1", "temp2", "temp200cm", "temp30cm", "", "PØN", "-5cm", "thermistor 1", "thermistor 2", "jord-5cm", "2m", "temperature", "temperature2", "soil temp", "veg. temp", "veg temp"))
precipitation <- subset(climate, logger %in% c("nedbor", "rain", "arg100", "counter", "counter1", "counter2"))
soilmoisture <- subset(climate, logger %in% c("jordf1", "jordf2", "soil.moisture", "soil moisture 2", "soil moisture 1", "soil moisture", "sm300 2", "sm300 1", "jordfukt2"))

save(precipitation, file = "Precipitation.RData")
save(soilmoisture, file = "Soilmoisture.RData")


# Explore temparure data and plot
table(temperature$logger, temperature$site)
table(temperature$logger, year(temperature$date))
table(temperature$site, year(temperature$date))

# plot logger by site
plot_climate(start_date = "2008.1.1", end_date = "2017.11.1", log = c("temp1"), inc = TRUE, SITE = "Lav")

# plot single site
temperature %>% 
  filter(site == "Skj") %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ logger)


# Find file names
temperature %>%
  filter(site == "Skj", logger == "") %>%
  group_by(file) %>%
  summarise(n = n(), MIN = min(date), max = max(date))
  
temperature %>% 
  filter(type == "UTL") %>% 
  ungroup() %>% 
  distinct(logger, site)

#### DATA CLEANING ####
temperature %>% 
  # add column for remarks, when data is doubtfull
  mutate(flag = NA) %>%  
  
  # delete crap UTL data
  filter(! site %in% c("lav", "gud", "ovs") & logger == "-5cm")


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



### THIS NEEDS TO BE FIXED HERE!!!
# # fixes Fauske Fall 2016 files
# if(basename(textfile) %in% c("fauske_climate_soil_moist_prec_Fall2016.txt")){
#   message("removing temp data from fauske fall 2016 file")
#   dat <- dat %>%
#     filter(!logger %in% c("temperature", "temperature2"))
# }
# if(basename(textfile) %in% c("Fauske_temp_Fall2016.txt")){
#   message("removing soil moisture and precipitation data from fauske fall 2016 file")
#   dat <- dat %>%
#     filter(!logger %in% c("jordf1",	"jordf2", "nedbor"))
# }


# ITAS data
# FAUSKE: delete temp1 and temp2 logger between June 2013 and October 2014 wrong values
temperature$value[temperature$file == "Fauske_ITAS_13.6.6-13.9.22.txt" & temperature$logger == "temp1"] <- NA
temperature$value[temperature$file == "Fauske_ITAS_13.9.22-14.5.14.txt" & temperature$logger == "temp1"] <- NA
temperature$value[temperature$file == "Fauske_ITAS_14.5.14-14.10.14.txt" & temperature$logger == "temp1"] <- NA
temperature$value[temperature$file == "Fauske_ITAS_13.6.6-13.9.22.txt" & temperature$logger == "temp2"] <- NA
temperature$value[temperature$file == "Fauske_ITAS_13.9.22-14.5.14.txt" & temperature$logger == "temp2"] <- NA
temperature$value[temperature$file == "Fauske_ITAS_14.5.14-14.10.14.txt" & temperature$logger == "temp2"] <- NA
# Flag temp1 logger between 2015 and May 2016 soil logger not in the soil
temperature$flag[temperature$file == "fauske_climate 20150129 - 20150429.txt" & temperature$logger == "temp1"] <- "VarianceProblem"
temperature$flag[temperature$file == "fauske_climate 20150429 - 20151009.txt" & temperature$logger == "temp1"] <- "VarianceProblem"
temperature$flag[temperature$file == "fauske_climate_spring_2016.txt" & temperature$logger == "temp1"] <- "VarianceProblem"
# Summer 2016 data: both ITAS loggers show very similar variance
temperature$flag[temperature$file == "Fauske_temp_Fall2016.txt" & temperature$logger == "temp1"] <- "VarianceProblem"
temperature$flag[temperature$file == "Fauske_temp_Fall2016.txt" & temperature$logger == "temp2"] <- "VarianceProblem"
# switch logger temp1 and temp2 always !!!
temperature$logger[temperature$site == "Fau" & temperature$logger %in% c("temp1", "thermistor.2")] <- "tempsoil"
temperature$logger[temperature$site == "Fau" & temperature$logger %in% c("temp2", "thermistor.1")] <- "tempabove"


# VIKESLAND:
# flag temp2 from April 2015 - May 2016: bias
temperature$flag[temperature$file == "Vikesland_climate 20150428 - 20151007.txt" & temperature$logger == "temp2"] <- "bias"
temperature$flag[temperature$file == "Vikesland_climate_spring2016.txt" & temperature$logger == "temp2"] <- "bias"


# ARHELLEREN
# switch temp1 and temp2 for July 2009 - Oct 2009
temperature$logger[temperature$file == "Arhelleren_08072009.txt" & temperature$logger == "temp1"] <- "tempsoil"
temperature$logger[temperature$file == "Arhelleren_08072009.txt" & temperature$logger == "temp2"] <- "tempabove"

# OVSTEDAL
# flag temp2 between 2011 and 2015; largeBias from Dec 2012 - 2015
temperature$flag[temperature$site == "Ovs" & temperature$logger == "temp2" & year(temperature$date) > 2011 & year(temperature$date) < 2015] <- "bias"
temperature$flag[temperature$site == "Ovs" & temperature$logger == "temp2" & temperature$file %in% c("Øvstedal_met1_20160511_20160523.txt",  "Øvstedal_met1_Fall2016.txt","Øvstedal_met1_spring2016.txt", "Øvstedal_met1_Spring2017.txt")] <- "biasLarge"

# VESKRE
# flag temp2 after November 2011 until Sept 2013
temperature$flag[temperature$file == "Veskre_ITAS_111111_120725.txt" & temperature$logger == "temp2"] <- "bias"
temperature$flag[temperature$file == "Veskre_ITAS_120725_120913.txt" & temperature$logger == "temp2"] <- "bias"
temperature$flag[temperature$file == "Veskre_ITAS_120913_130603.txt" & temperature$logger == "temp2"] <- "bias"
temperature$flag[temperature$file == "Veskre_ITAS_130603_130909.txt" & temperature$logger == "temp2"] <- "bias"
# switch temp1 and temp2 in 2014
temperature$logger[temperature$file == "Veskre_ITAS_140720_140802.txt" & temperature$logger == "temp1"] <- "tempsoil"
temperature$logger[temperature$file == "Veskre_ITAS_140720_140802.txt" & temperature$logger == "temp2"] <- "tempabove"
temperature$logger[temperature$file == "Veskre_ITAS_140802_141003.txt" & temperature$logger == "temp1"] <- "tempsoil"
temperature$logger[temperature$file == "Veskre_ITAS_140802_141003.txt" & temperature$logger == "temp2"] <- "tempabove"
# Flag temp1 in 2015, because measuring both soil temp
temperature$flag[temperature$file == "Veskre_climate 11102015 - 08122015.txt" & temperature$logger == "temp1"] <- "VarianceProblem"
temperature$flag[temperature$file == "Veskre_climate 20150909 - 20151011.txt" & temperature$logger == "temp1"] <- "VarianceProblem"
# switch temp1 and temp2 in summer 2016, winter 2016
temperature$logger[temperature$file == "Veskre_klima_Fall2016.txt" & temperature$logger == "temp1"] <- "tempsoil"
temperature$logger[temperature$file == "Veskre_klima_Fall2016.txt" & temperature$logger == "temp2"] <- "tempabove"
temperature$logger[temperature$file == "Veskre_klima_Spring2017.txt" & temperature$logger == "temp1"] <- "tempsoil"
temperature$logger[temperature$file == "Veskre_klima_Spring2017.txt" & temperature$logger == "temp2"] <- "tempabove"


# RAMBAERA
# switch temp1 and temp2 before 2014
temperature$logger[temperature$site == "Ram" & year(temperature$date) <= 2013 & temperature$logger == "temp1"] <- "tempsoil"
temperature$logger[temperature$site == "Ram" & year(temperature$date) <= 2013 & temperature$logger == "temp2"] <- "tempabove"
# flag temp2 from Oct 2015 - May 2016 too large variance
temperature$flag[temperature$file == "Rambera_met1 10102015 - 24052016.txt" & temperature$logger == "temp2"] <- "VarianceProblem"

# HOGSETE
# flag temp1 from from Sep/Oct 2015 - May 2016 too little variance
temperature$flag[temperature$file == "Høgsete_met1_spring_2016.txt" & temperature$logger == "temp1"] <- "VarianceProblem"
temperature$flag[temperature$file == "Høgsete_met1_Fall2016.txt" & temperature$logger == "temp1"] <- "VarianceProblem"

# ALRUST
# switch logger until 31.12.2015
temperature$logger[temperature$site == "Alr" & temperature$date < "2016-01-01 00:00:00" & temperature$logger == "temp1"] <- "tempsoil"
temperature$logger[temperature$site == "Alr" & temperature$date < "2016-01-01 00:00:00" & temperature$logger == "temp2"] <- "tempabove"
# flag temp1 from April 2012 - Dec 2012 too little variance
temperature$flag[temperature$site == "Alr" & temperature$logger == "tempabove" & temperature$date > "2012-04-01 00:00:00" & temperature$date < "2014-12-01 00:00:00"] <- "VarianceProblem"
# flag temp2 in 2010 and April 2015 - May 2016 too large variance
temperature$flag[temperature$site == "Alr" & temperature$logger == "tempsoil" & temperature$date > "2010-04-01 00:00:00" & temperature$date < "2010-09-01 00:00:00"] <- "VarianceProblem"
temperature$flag[temperature$site == "Alr" & temperature$logger == "tempsoil" & temperature$date > "2015-04-01 00:00:00" & temperature$date < "2016-06-01 00:00:00"] <- "VarianceProblem"

# ULVHAUGEN
# switch temp1 to soil and temp2 to abovegroun
temperature <- temperature %>% 
  mutate(logger = ifelse(logger == "temp1" & site == "Ulv", "tempsoil", logger)) %>% 
  mutate(logger = ifelse(logger == "temp2"& site == "Ulv", "tempabove", logger))

# LAVISDALEN
# switch logger until 2011 (because 2012 and 2013 data is crap, delete)
temperature$logger[temperature$site == "Lav" & temperature$date < "2012-06-27 21:00:00" & temperature$logger == "temp1"] <- "tempsoil"
temperature$logger[temperature$site == "Lav" & temperature$date < "2012-06-27 21:00:00" & temperature$logger == "temp2"] <- "tempabove"
# Variance problems with tempsoil in autumn 2011
temperature$flag[temperature$site == "Lav" & temperature$logger == "tempsoil" & temperature$date > "2011-08-01 00:00:00" & temperature$date < "2011-12-01 00:00:00"] <- "VarianceProblem"
# Flag few and strange data points for both loggers in 2013
temperature$flag[temperature$site == "Lav" & year(temperature$date) == "2013" & temperature$logger == "temp1"] <- "FewData"
temperature$flag[temperature$site == "Lav" & year(temperature$date) == "2013" & temperature$logger == "temp2"] <- "FewData"
# Remove wrong data points in 2012/2013
temperature <- temperature %>% 
  mutate(value = ifelse(logger == "temp1" & file %in% c("Lavisdalen_met1.txt", "Lavisdalen_met1 (2).txt", "Lavisdalen-met1-20120913.txt"), NA, value)) %>% 
  mutate(value = ifelse(logger == "temp2" & file %in% c("Lavisdalen-met1-20120913.txt", "Lavisdalen_met1 (2).txt", "Lavisdalen_met1.txt"), NA, value)) %>% 
  mutate(value = ifelse(logger == "tempsoil" & file %in% c("Lavisdalen_08062011_25102011.txt"), NA, value))


# GUDMEDALEN
# switch logger until end of 2014
temperature$logger[temperature$site == "Gud" & temperature$date < "2015-01-01 00:00:00" & temperature$logger == "temp1"] <- "tempsoil"
temperature$logger[temperature$site == "Gud" & temperature$date < "2015-01-01 00:00:00" & temperature$logger == "temp2"] <- "tempabove"
# Variance problems with above logger between end of June 2014 - Oct 2014
temperature$flag[temperature$file == "Gudmedalen_ITAS_140619_141013.txt" & temperature$logger == "tempabove"] <- "VarianceProblem"

# SKJELLINGAHAUGEN
# Flag variance problems with temp1 logger in 2014
temperature$flag[temperature$site == "Skj" & year(temperature$date) == "2014" & temperature$logger == "temp1"] <- "VarianceProblem"
# Flag few data points for both loggers in 2012
temperature$flag[temperature$site == "Skj" & year(temperature$date) == "2012" & temperature$logger == "temp1"] <- "FewData"
temperature$flag[temperature$site == "Skj" & year(temperature$date) == "2012" & temperature$logger == "temp2"] <- "FewData"
# switch logger from 2015 - 2016
temperature$logger[temperature$site == "Skj" & temperature$date > "2015-01-01 00:00" & temperature$date < "2016-10-04 12:00" & temperature$logger == "temp1"] <- "tempsoil"
temperature$logger[temperature$site == "Skj" & temperature$date > "2015-01-01 00:00" & temperature$date < "2016-10-04 12:00" & temperature$logger == "temp2"] <- "tempabove"

# remove everything before 1.10.2008, values too high
temperature <- temperature %>% 
  filter(date > "2008-10-01 00:00:00")

# Change remaining logger names
temperature$logger[temperature$logger == "temp1"] <- "tempabove"
temperature$logger[temperature$logger == "temp2"] <- "tempsoil"

# order sites
temperature <- temperature %>% 
  mutate(site = factor(site, levels = c("Skj", "Gud", "Lav", "Ulv", "Ves", "Ram", "Hog", "Alr", "Ovs", "Arh", "Vik", "Fau")))

# fill missing dates with NA and merging with complete dataset
full_grid <- expand.grid(logger = unique(temperature$logger), site = unique(temperature$site), date = seq(min(temperature$date), max(temperature$date), by = "hour"))

temperature <- left_join(full_grid, temperature) %>% tbl_df()

save(temperature, file = "Temperature.RData")

# Fix location problem, duplicates
files_db <- dir(path = paste0("~/Dropbox/seedclim klimadaten/rawdata by Site/"), pattern = "txt", recursive = TRUE, full.names = TRUE)
files_p <- dir(path = paste0("/Volumes/felles/MATNAT/BIO/Felles/007_Funcab_Seedclim/SeedClimClimateData/rawdata by Site/"), pattern = "txt", recursive = TRUE, full.names = TRUE)

setdiff(basename(files_p), basename(files_db))
  
  