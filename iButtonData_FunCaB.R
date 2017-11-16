#### FUNCAB iButton Data ######
library("tidyverse")
library("lubridate")
library("readr")
library("readxl")


# Extract file names from all iButton files to create dictionary
myfiles <- dir(path = paste0("/Volumes/FELLES/MATNAT/BIO/Felles/007_Funcab_Seedclim/SeedClimClimateData/iButtondata/2016"), pattern = "csv", recursive = TRUE, full.names = TRUE)
myfiles <- dir(path = paste0("/Volumes/FELLES/MATNAT/BIO/Felles/007_Funcab_Seedclim/SeedClimClimateData/iButtondata/2016"), pattern = "txt", recursive = TRUE, full.names = TRUE)
myfiles <- tibble(unlist(myfiles))
# 2017 files
filenames <- dir(path = paste0("~/Desktop/iButton plots/Arhelleren"), pattern = "txt", recursive = TRUE)
filenames <- gsub(pattern = ".txt", "", filenames)

ddd <- myfiles %>% 
  separate(col = `unlist(myfiles)`, into = c("a", "b", "c", "d","e","f","g","h","i", "year","siteID", "iButtonID"), sep = "/") %>% 
  select(year, siteID, iButtonID) %>% print(n = 100)

write.csv(ddd, "iButtonID.csv")


#### Read in iButtons Function
ReadIniButtons <- function(textfile){
  print(textfile)
  ending <- substr(textfile, nchar(textfile)-4+1, nchar(textfile))
  # Extract Date, Unit and Value
 if(ending == ".txt"){
    dat <- read_delim(textfile, delim = ",", col_names = FALSE)
    dat$Date <- dmy_hms(dat$X1) # convert to date
    dat$Unit <- dat$X2
    dat$Value <- as.numeric(paste(dat$X3, dat$X4, sep = ".")) # merge col 3 and 4 to one number
    dat <- dat %>% select(-X1, -X2, -X3, -X4)
  } else if(ending == ".csv"){
    # import body of data
    dat <- read_csv(textfile, skip = 19)
    dat$Date <- mdy_hms(dat$`Date/Time`) # convert to date
    dat <- dat %>% select(Date, Unit, Value, -`Date/Time`)
  } else {
    warning(paste(textfile, "format not recognised")) # warning if logger not recognized
    dat <- NULL
  }
  
  # Extract siteID, iButtonID and Year from file name
  dat$iButtonID <- basename(textfile)
  textfile2 <- tibble(dirname(textfile))
  colnames(textfile2) <- "ID"
  textfile2 <- textfile2 %>% 
    separate(col = ID, into = c("a", "b", "c", "d","e","f","g","h","i", "Year", "siteID"), sep = "/")
  dat$siteID <- textfile2$siteID
  dat$Year <- textfile2$Year
  return(dat)
}




# read in iButtonID dictionary 2016
dictionary16 <- read_excel(path = "iButtonDictionary.xlsx", sheet = 1, col_names = TRUE)

dictionary16 <- dictionary16 %>% 
  mutate(Block = plyr::mapvalues(Block, c("FCIII", "FCII", "FCI", "FCV", "FCIV", "FCXV", "FCXII", "FCXIII", "FCI ", "FCVI", "FCVIII", "IX", "II", "V", "E2"), c("3", "2", "1", "5", "4", "15", "12", "13", "1","6", "8", "9", "2", "5", "E2")))

# read in iButtonID dictionary 2017
dictionary17 <- read_excel(path = "iButtonDictionary.xlsx", sheet = 2, col_names = TRUE)
dictionary17 <- dictionary17 %>% 
  mutate(siteID = plyr::mapvalues(siteID, c("LAV", "GUD", "SKJ", "ULV", "ALR", "FAU", "HOG", "VIK", "RAM", "VES", "OVS", "ARH"), c("Lavisdalen", "Gudmedalen", "Skjellingahaugen", "Ulvhaugen", "Alrust", "Fauske", "Hogsete", "Vikesland", "Rambera", "Veskre", "Ovstedal", "Arhelleren")))

dictionary <- dictionary16 %>% 
  bind_rows(dictionary17)


# MAKE LIST OF ALL TXT FILES AND MERGE THEM TO ONE DATA FRAME
myfiles <- dir(path = paste0("/Volumes/FELLES/MATNAT/BIO/Felles/007_Funcab_Seedclim/SeedClimClimateData/iButtondata"), pattern = "csv|txt", recursive = TRUE, full.names = TRUE)
myfiles <- myfiles[!grepl("log", myfiles, ignore.case = TRUE)] # remove log files
myfiles <- myfiles[!grepl("Empty", myfiles, ignore.case = TRUE)] # remove log files
myfiles <- myfiles[!grepl("unknown", myfiles, ignore.case = TRUE)] # remove log files


mdat <- map_df(myfiles, ReadIniButtons)
head(mdat)

#remove .txt/csv suffix from 2017 data so it can join with dictionary
mdat <- mdat %>% 
  mutate(iButtonID = if_else(Year == 2017,
    substr(iButtonID, 1, nchar(iButtonID)-4),
    iButtonID)) %>% 
  mutate(iButtonID = if_else(nchar(iButtonID) == 16 ,
                             gsub(pattern = substr(iButtonID, 3, 8), ""),
                             iButtonID))
  

iButtonData <- mdat %>% 
  mutate(Year = as.numeric(Year)) %>% 
  #filter(!(format(Date, "%Y-%m-%d") < "2016-08-01" & Year == 2017)) %>% 
  full_join(dictionary, by = c("Year", "siteID", "iButtonID"))

save(iButtonData, file = "iButton2016-2017.RData")
load(file = "/Volumes/fja062/PhD/Projects/2017_temperature_regulation_of_functional_groups/SeedClim-Climate-Data/iButton2016-2017.RData")

# Check start and end date
iButtonData %>% 
  group_by(Year, siteID) %>% 
  summarise(min(Date, na.rm = TRUE), max(Date, na.rm = TRUE)) %>% 
  arrange(siteID) %>% print(n = Inf)


# Remove ibuttons that are apparently logging until December 2017
wonkyiButtons <- iButtonData %>% 
  filter(Date > "2017-09-01 00:07:01") %>% 
  distinct(iButtonID) %>% 
  pull(iButtonID)

iButtonData %>%
  filter(iButtonID %in% wonkyiButtons) %>% 
  ggplot(aes(x = Date, y = Value)) +
  geom_line() +
  facet_wrap(~ siteID)

iButtonData <- iButtonData %>% 
  filter(!iButtonID %in% wonkyiButtons) %>% 
  filter(Value > -40, Value < 50) %>% # crop impossible values, but look into a smoother option
  filter(!iButtonID %in% c("3E369341.csv", "3E3DF841.csv")) %>% # remove 2 iButtons from Gudmeldalen; these loggers need to be checked!!!
  filter(!(Date < ReplacementDate & Year == 2016)) %>%  # remove values in 2015 before ibuttons have been placed
  #filter(!(Date < ReplacementDate & Year == 2017)) %>% 
  filter(!(Date > RemovalDate & Year == 2016)) %>% 
  filter(!(Year == 2017))%>% 
  #filter(!(Date > RemovalDate & Year == 2017))
  #filter(!is.na(Treatment))
  mutate(Temperature_level = if_else(siteID %in% c("Lavisdalen", "Gudmedalen", "Skjellingahaugen", "Ulvhaugen"), "6.5",
                                     if_else(siteID %in% c("Alrust", "Hogsete", "Rambera", "Veskre"), "8.5", "10.5"))) %>% 
  mutate(Precipitation_level = if_else(siteID %in% c("Alrust", "Fauske", "Ulvhaugen"), "0.6",
                                     if_else(siteID %in% c("Lavisdalen","Hogsete", "Vikesland"), "1.2",
                                             ifelse(siteID %in% c("Rambera", "Gudmedalen", "Arhelleren"), "2.0", "2.7")))) %>% 
  mutate(Precipitation_level = as.numeric(Precipitation_level), Temperature_level = as.numeric(Temperature_level))


iButtonData %>% filter(format(Date, "%Y-%m") == "2016-06") %>%
  filter(Treatment %in% c("C", "2m_site")) %>%
  mutate(hour = hour(Date)) %>% 
  group_by(Treatment, iButtonID, hour, Temperature_level, Precipitation_level) %>%
  summarise(mean = mean(Value)) %>%
  ggplot(aes(x = hour, y = mean, colour = Treatment)) +
  geom_smooth(se = TRUE) +
  facet_grid(Precipitation_level ~ Temperature_level) +
  scale_color_manual(values = cbPalette) +
  theme_classic()


# read in soil moisture data

soilMoisture <- read_excel("/Volumes/fja062/PhD/Data/Soilmoisture_1516.xlsx")

soilMoisture

soilMoisture[, 8 : 11] <- plyr::colwise(as.numeric)(soilMoisture[, 8 : 11])

x <- soilMoisture %>% 
  filter(comments == "fewer readings"|is.na(comments)) %>% 
  filter(!is.na(treatment)) %>% 
  mutate(TurfID = paste0(site, block, treatment)) %>% 
  group_by(date, TurfID) %>% 
  mutate(n = n()) %>% 
  filter(n > 1)
  mutate(SM = mean(c(M1, M2, M3, M4), na.rm = TRUE))



# need to check ibuttons that don't seem to be logging regularly. happens in several sites.
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#1C9099", "#A6BDDB", "#ECE2F0", "orange3")


iButtonData %>% 
  filter(!is.na(Treatment)) %>% 
  filter(format(Date, "%Y-%m-%d") == "2016-05-23") %>%
  ggplot(aes(x = Date, y = Value, color = Temperature_level)) +
  geom_point() +
  facet_wrap(~ Treatment) +
  theme_bw() +
  scale_color_manual(values = cbPalette[c(2, 3, 4, 7)]) 
  ggsave(filename = "ibutton_dailyAmplitude_christmas_2015.jpg", path = "/Users/fja062/Documents")

iButtonData %>% 
  filter(!is.na(Treatment)) %>% 
  filter(format(Date, "%Y-%m-%d") == "2016-06-23") %>%
  ggplot(aes(x = Date, y = Value, color = Treatment)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~ Temperature_level) +
  theme_bw() +
  scale_color_manual(values = cbPalette) +
  ggsave(filename = "ibutton_dailyAmplitude_jun_2016.jpg")


iButtonData %>% 
  ggplot(aes(x = Date, y = Value, colour = Treatment)) +
  geom_line() +
  facet_wrap(~ siteID) +
  ggsave(filename = "ibutton_allSites_2016.jpg", path = "/Users/fja062/Documents")


minmax <- iButtonData %>% 
  #mutate(bryophytes = if_else(Treatment %in% c("B", "FB", "GB", "FGB"), "removed", "remained")) %>% 
  group_by(siteID, Treatment, week = week(Date)) %>% 
  mutate(min = min(Value, na.rm = TRUE),
            max = max(Value, na.rm = TRUE)) %>% 
  arrange(siteID) %>% ungroup() %>% 
  gather(range, temperature, min, max)

minmax %>% 
  filter(!is.na(Treatment)) %>% 
  filter(Temperature_level == "6.5") %>% 
  ggplot(aes(x = Date, y = temperature, colour = range)) +
  geom_point() +
  facet_wrap(~ Treatment) +
  scale_color_manual(values = cbPalette) +
  theme_bw() +
  ggsave(filename = "ibutton_alpine_TempRange_2016.jpg", path = "/Users/fja062/Documents")


ibutton2017 %>% 
  filter(siteID == "Lavisdalen") %>%
  filter(Date < "2016-07-15" & Year == 2017) %>%
  filter(Date > "2016-06-20") %>% 
  ggplot(aes(x = Date, y = Value)) +
  geom_line()


load("Temperature.RData")

dict_Site <- read.table(header = TRUE, stringsAsFactors = FALSE, text = 
                          "old new
                        Arh Arhelleren
                        Ovs Ovstedal
                        Ves Veskre
                        Skj Skjellingahaugen
                        Lav Lavisdalen
                        Gud Gudmedalen
                        Ulv Ulvhaugen
                        Vik Vikesland
                        Hog Hogsete
                        Alr Alrust
                        Fau Fauske
                        Ram Rambera")

temperature <- temperature %>% 
  filter(!is.na(value)) %>% 
  filter(date > "2015-07-01") %>% 
  filter(date < "2016-10-01") %>% 
  mutate(site = plyr::mapvalues(site, from = dict_Site$old, to = dict_Site$new)) %>%
  filter(is.na(flag)) %>% 
  select(siteID = site, Date = date, Value = value) %>% 
  mutate(Treatment = "2m_site")


iButtonData <- iButtonData %>% 
  full_join(temperature, by = c("siteID", "Date", "Treatment", "Value"))



###### bayesian analysis #####
sink("/Users/fja062/Documents/SeedClim-Climate-Data/ibuttonModel.txt")

cat(
  "model{
#likelihood
  for(datIter in 1:nData){
  obs[datIter] ~ dnorm(mu[datIter], tau)
  mu[datIter] <- intercept + treatment[dataIter] + time[dataIter] + temp[datIter] + precip[dataIter]
  }



#random effects
  for(site in 1:siteID){
    for(block in 1:blockID){
    eps[site[block]] ~ dnorm(0,tau.block)
    eps[site] ~ dnorm(0,tau.site)
    }
  }


#priors
  tau.block
  tau.site
  tau.obs
  sigma.block
  sigma.site
  sigma.obs
  treatment
  time
  temp



}
  "
  
)

























# and we need to add figures to the .gitignore


#####################################
######## PHENOLOGY PLOTTING #########

RemoveDates <- data_frame(siteID = c("Gudmedalen", "Skjellingahaugen", "Rambera", "Veskre"),
                          MinDate = ymd_hms(c("2015-07-13 00:00:00", "2015-09-24 00:00:00", "2015-08-12 00:00:00", "2015-07-30 00:00:00")),
                          MaxDate = ymd_hms(c("2016-07-04 00:00:00", "2016-07-02 00:00:00",  "2016-06-30 00:00:00", "2016-06-27 00:00:00")))

# Only retain certain blocks: VES: 3,4; SKJ: 1:4; GUD: 5; RAM: 4,5
Loggers <- data_frame(
  siteID = c("Gudmedalen", rep("Skjellingahaugen", 4), rep("Rambera", 2), rep("Veskre", 2)),
  Block = as.character(c(5, 1, 2, 3, 4, 4, 5, 3, 4)))

iButtonData %>% 
  filter(siteID %in% c("Gudmedalen", "Skjellingahaugen", "Rambera", "Veskre")) %>% 
  filter(Treatment == "C") %>% 
  # select loggers important for pollination plots
  inner_join(Loggers) %>% 
  # Remove dates before logger in the soil
  left_join(RemoveDates, by = "siteID") %>% 
  filter(Date >= "2016-04-01 00:00:00", Date <= MaxDate) %>% 
  mutate(Date_Day = round_date(Date, unit = "day")) %>% 
  group_by(Date_Day, siteID) %>% 
  summarise(DailyTemp = mean(Value)) %>% 
  #ungroup() %>% group_by(siteID) %>% 
  #mutate(CumTemp = cumsum(DailyTemp)) %>% 
  ggplot(aes(x = Date_Day, y = DailyTemp, color = siteID)) +
  geom_line()


load(file = "iButton2016.Rdata")

# -- phenology end -- #

