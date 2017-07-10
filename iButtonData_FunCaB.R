#### FUNCAB iButton Data ######
library("tidyverse")
library("lubridate")
library("readr")


# Extract file names from all iButton files to create dictionary
myfiles <- dir(path = paste0("/Volumes/FELLES/MATNAT/BIO/Felles/000_Funcab_Seedclim/SeedClimClimateData/iButtondata"), pattern = "csv", recursive = TRUE, full.names = TRUE)
myfiles <- tibble(unlist(myfiles))

ddd <- myfiles %>% 
  separate(col = `unlist(myfiles)`, into = c("a", "b", "c", "d","e","f","g","h","i","siteID","iButtonID"), sep = "/") %>% 
  select(siteID, iButtonID)

write.csv(ddd, "iButtonID.csv")

dd <- tibble(dirname("/Volumes/FELLES/MATNAT/BIO/Felles/000_Funcab_Seedclim/SeedClimClimateData/iButtondata/Alrust/3E39E941.csv"))
colnames(dd) <- "ID"
dd %>% 
  separate(col = ID, into = c("a", "b", "c", "d","e","f","g","h","siteID","iButtonID"), sep = "/") %>% 
  select(siteID, iButtonID)

#### Read in iButtons Function
ReadIniButtons <- function(textfile){
  # import body of data
  dat <- read_csv(textfile, skip = 19)
  dat$Date <- mdy_hms(dat$`Date/Time`) # convert to date
  
  # extract site and file name from file name
  dat$iButtonID <- basename(textfile)
  textfile2 <- tibble(dirname(textfile))
  colnames(textfile2) <- "ID"
  textfile2 <- textfile2 %>% 
    separate(col = ID, into = c("a", "b", "c", "d","e","f","g","h","i","siteID"), sep = "/")
  dat$siteID <- textfile2$siteID
  dat
}


# read in iButtonID dictionary


# MAKE LIST OF ALL TXT FILES AND MERGE THEM TO ONE DATA FRAME
myfiles <- dir(path = paste0("/Volumes/FELLES/MATNAT/BIO/Felles/000_Funcab_Seedclim/SeedClimClimateData/iButtondata"), pattern = "csv", recursive = TRUE, full.names = TRUE)

mdat <- plyr::ldply(as.list(myfiles), ReadIniButtons)
head(mdat)

mdat %>% 
  left_join() %>% 
  mutate(Year = 2016)

mdat %>% 
  filter(siteID == "Gudmedalen") %>% 
  ggplot(aes(x = Date, y = Value)) +
  geom_line() +
  facet_wrap(~ iButtonID)
