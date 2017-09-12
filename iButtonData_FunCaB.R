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

iButtonData <- mdat %>% 
  mutate(Year = as.numeric(Year)) %>% 
  full_join(dictionary, by = c("Year", "siteID", "iButtonID"))

save(iButtonData, file = "iButton2016-2017.RData")
load(file = "iButton2016-2017.RData")

# Check start and end date
iButtonData %>% 
  group_by(Year, siteID) %>% 
  summarise(min(Date, na.rm = TRUE), max(Date, na.rm = TRUE)) %>% 
  arrange(siteID) %>% print(n = Inf)


iButtonData %>% 
  filter(siteID == "Lavisdalen") %>% 
  filter(format(Date, "%Y-%m-%d") == "2015-08-20") %>%
  filter(Value > -40, Value < 50) %>%
  ggplot(aes(x = Date, y = Value, color = Block)) +
  geom_line() +
  facet_wrap(~ Treatment)

iButtonData %>% 
  filter(!iButtonID %in% c("3E369341.csv", "3E3DF841.csv")) %>% # remove 2 iButtons from Gudmeldalen; these loggers need to be checked!!!
  filter(Value > -40, Value < 50) %>% # crop impossible values
  ggplot(aes(x = Date, y = Value)) +
  geom_line() +
  facet_wrap(~ siteID) +
  ggsave(filename = "ibutton_allSites_allYears.jpg")


iButtonData %>% 
  #filter(siteID == "Hogsete") %>%
  filter(!iButtonID %in% c("3E369341.csv", "3E3DF841.csv")) %>% # remove 2 iButtons from Gudmedalen; these loggers need to be checked!!!
  filter(format(Date, "%Y-%m-%d") < "2016-10-20") %>% 
  filter(Treatment != "NA") %>% 
  ggplot(aes(x = Date, y = Value, colour = siteID)) +
  geom_line() +
  facet_wrap(~ Treatment)


# having a look at, and removing, ibuttons that are apparently logging until December 2017
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
  filter(!iButtonID %in% wonkyiButtons)

# we need to find some way in the code to allow ibutton id to change from removal date to replacement date...

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

