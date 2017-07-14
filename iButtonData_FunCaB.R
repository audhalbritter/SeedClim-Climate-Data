#### FUNCAB iButton Data ######
library("tidyverse")
library("lubridate")
library("readr")
library("readxl")


# Extract file names from all iButton files to create dictionary
myfiles <- dir(path = paste0("/Volumes/FELLES/MATNAT/BIO/Felles/007_Funcab_Seedclim/SeedClimClimateData/iButtondata/2016"), pattern = "csv", recursive = TRUE, full.names = TRUE)
myfiles <- dir(path = paste0("/Volumes/FELLES/MATNAT/BIO/Felles/007_Funcab_Seedclim/SeedClimClimateData/iButtondata/2016"), pattern = "txt", recursive = TRUE, full.names = TRUE)
myfiles <- tibble(unlist(myfiles))

ddd <- myfiles %>% 
  separate(col = `unlist(myfiles)`, into = c("a", "b", "c", "d","e","f","g","h","i", "year","siteID", "iButtonID"), sep = "/") %>% 
  select(year, siteID, iButtonID) %>% print(n = 100)

write.csv(ddd, "iButtonID.csv")


#### Read in iButtons Function
ReadIniButtons <- function(textfile){
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
    separate(col = ID, into = c("a", "b", "c", "d","e","f","g","h","i", "Year","siteID"), sep = "/")
  dat$siteID <- textfile2$siteID
  dat$Year <- textfile2$Year
  return(dat)
}




# read in iButtonID dictionary
dictionary <- read_excel(path = "iButtonID_2016.xlsx", sheet = 1, col_names = TRUE)

dictionary <- dictionary %>% 
  mutate(Block = plyr::mapvalues(Block, c("FCIII", "FCII", "FCI", "FCV", "FCIV", "FCXV", "FCXII", "FCXIII", "FCI ", "FCVI", "FCVIII", "IX", "II", "V", "E2"), c("3", "2", "1", "5", "4", "15", "12", "13", "1","6", "8", "9", "2", "5", "E2")))

# MAKE LIST OF ALL TXT FILES AND MERGE THEM TO ONE DATA FRAME
myfiles <- dir(path = paste0("/Volumes/FELLES/MATNAT/BIO/Felles/007_Funcab_Seedclim/SeedClimClimateData/iButtondata"), pattern = "csv|txt", recursive = TRUE, full.names = TRUE)
myfiles <- myfiles[!grepl("log", myfiles, ignore.case = TRUE)] # remove log files


mdat <- map_df(myfiles, ReadIniButtons)
head(mdat)

iButtonData <- mdat %>% 
  mutate(Year = as.numeric(Year)) %>% 
  left_join(dictionary, by = c("Year", "siteID", "iButtonID"))

save(iButtonData, file = "iButton2016.RData")
load(file = "iButton2016.RData")

iButtonData %>% 
  filter(siteID == "Alrust") %>% 
  filter(format(Date, "%Y-%m-%d") == "2015-09-20") %>%
  filter(Value > -40, Value < 50) %>%
  ggplot(aes(x = Date, y = Value, color = Block)) +
  geom_line() +
  facet_wrap(~ Treatment)

mdat %>% 
  filter(!iButtonID %in% c("3E369341.csv", "3E3DF841.csv")) %>% # remove 2 iButtons from Gudmeldalen; these loggers need to be checked!!!
  filter(Value > -40, Value < 50) %>% # crop impossible values
  ggplot(aes(x = Date, y = Value)) +
  geom_line() +
  facet_wrap(~ siteID)
