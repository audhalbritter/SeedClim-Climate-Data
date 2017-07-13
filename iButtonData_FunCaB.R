#### FUNCAB iButton Data ######
library("tidyverse")
library("lubridate")
library("readr")


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



# MAKE LIST OF ALL TXT FILES AND MERGE THEM TO ONE DATA FRAME
myfiles <- dir(path = paste0("/Volumes/FELLES/MATNAT/BIO/Felles/007_Funcab_Seedclim/SeedClimClimateData/iButtondata"), pattern = "csv|txt", recursive = TRUE, full.names = TRUE)
myfiles <- myfiles[!grepl("log", myfiles, ignore.case = TRUE)] # remove log files

mdat <- map_df(myfiles, ReadIniButtons)
head(mdat)

save(mdat, file = "iButton2016.RData")

mdat %>% 
  filter(siteID == "Gudmedalen") %>% 
  filter(format(Date, "%Y-%m-%d") == "2015-08-12") %>% print(n = 100)
  filter(Value > -40, Value < 50) %>%
  ggplot(aes(x = Date, y = Value)) +
  geom_line() +
  facet_wrap(~ iButtonID)

mdat %>% 
  filter(!iButtonID %in% c("3E369341.csv", "3E3DF841.csv")) %>% # remove 2 iButtons from Gudmeldalen; these loggers need to be checked!!!
  filter(Value > -40, Value < 50) %>% # crop impossible values
  group_by(siteID, Date, Year) %>% 
  summarise(mean = mean(Value)) %>% 
  ggplot(aes(x = Date, y = mean)) +
  geom_line() +
  facet_wrap(~ siteID)
