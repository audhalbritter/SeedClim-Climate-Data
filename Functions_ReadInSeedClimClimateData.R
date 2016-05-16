####################################
# READ IN SEEDCLIM CLIMATE DATA
#          FUNCTIONS
####################################
library(plyr)
library(lubridate)
library(reshape2)

#### IMPORT DATA ####

#### NAME CHECK FUNCTION ####
NameCheck <- function(site, textfile){
  site <- substr(site, 1,3)
  site <- gsub('(^)([[:alpha:]])','\\1\\U\\2\\', site, perl = TRUE)
  if(site %in% c("Aal", "Ålr", "Ål")) site <- "Alr"
  if(site %in% c("Arn")) site <- "Arh"
  if(site %in% c("Høg")) site <- "Hog"
  if(site %in% c("Låv")) site <- "Lav"
  if(site %in% c("Øvs")) site <- "Ovs"
  if(site %in% c("Ule")) site <- "Ulv"
  
  #check
  if(!site %in% c("Fau", "Alr", "Ulv", "Vik", "Hog", "Lav", "Arh", "Ram", "Gud", "Ovs", "Ves", "Skj"))stop(paste("site is ", site, "from", textfile))
  site
}


#  READ IN ITAS LOGGERS
ReadInBodyITAS <- function(textfile){
  # import body of data
  dat <- read.table(textfile, header=TRUE, sep="\t", dec=",", comment.char = "", stringsAsFactors = FALSE)
  dat[dat == "#-INF"] <- NA # delete #-Inf
  dat[dat == "#+INF"] <- NA # delete #+Inf
  dat <- colwise(gsub, pattern = ",", replacement = ".")(dat)#fixing number formats
  dat <- dat[-1,] # delete first row with units
  
  # some files have an extra time column (less than 15 characters)
  if(nchar(dat$Label[1]) <= 15){
    date <- paste(strsplit(dat$Label[1], " ")[[1]][1], dat$X[1], sep=" ")
    dat$X <- NULL
  } else { # for normal files
    date <- dat$Label[1]
  }
  dat[,-1] <- colwise(as.numeric)(dat[,-1]) # remove first column
  
  # convert to date
  dat$date <- dmy_hms(date, tz = "Africa/Algiers")
  
  dat$Label <- NULL # delet first column
  attr(dat, "type") <- "ITAS" # give each file an attribute
  dat <- melt(dat, id=c("date"))
  colnames(dat)[2] <- "logger"
  dat$logger <- tolower(dat$logger)
  
  # extract site name from file name
  textfile2 <- basename(textfile)
  textfile2 <- enc2utf8(gsub("ITAS ", "", textfile2))
  dat$site <- NameCheck(textfile2, textfile2) # check the site name
  dat
}
ddd <- ReadInBodyITAS("Skjeldingahaugen_ITAS_111015_120705.txt")
head(ddd)



##### READ IN UTL LOGGERS ####
ReadInBodyUTL <- function(textfile, SITE){browser()
  #find sample
  f <- readLines(textfile, n = 30)
  skip <- which(f == "Sample")
  if(length(skip) != 1)stop(paste("no Sample", textfile))
  # import body of data, without header
  dat <- read.table(textfile, header=FALSE, sep="\t", skip=skip, dec=",")
  if(ncol(dat) == 1){
    dat <- read.table(textfile, header=FALSE, sep=" ", skip=skip, dec=",")
    dat <- data.frame(paste(dat[, 1], dat[, 2]), dat[, 3])
  }
  if(ncol(dat) == 3){
    dat <- data.frame(paste(dat[, 1], dat[, 2], sep=" "), dat[, 3])
  }
  colnames(dat) <- c("date", "temp")
  dat$date <- ymd_hms(dat$date, tz = "Africa/Algiers") # define dates
  dat$temp <- as.numeric(dat$temp)
  
  # import head of data to extract logger name
  dat.h <- read.csv(textfile, sep="\t", header=FALSE, nrow=15)
  temp.logger <- gsub(" ", "",dat.h$V2[4], fixed=TRUE) #extract logger: 30cm or 200cm, delete space between nr and unit
  
  # give a warning if logger name is wrong
  if(!temp.logger %in% c("200cm", "30cm")){
    warning(paste(textfile, temp.logger, "logger name wrong"))
    # label these files
    t.name <- temp.logger  
  }
  else {t.name <- paste("temp", temp.logger, sep="")}
  
  # rename temp column with temp logger name
  dat$logger <- t.name
  colnames(dat)[colnames(dat)=="temp"] <- "value"
  dat$site <- dat.h$V2[3] # extract site
  if(site == "" | site == " " ){
    site <- SITE
    warning(paste("site imputed for", SITE, basename(textfile)))
  }
  dat$site <- NameCheck(site, textfile)
  attr(dat, "type") <- "UTL" # give each file an attribute
  dat
}
ddd <- ReadInBodyUTL("#001068_20090419_1600.txt")
head(ddd)


#### Read in ITAS or UTL ####
ReadData <- function(textfile){
  # print(textfile)
  first <- substring(readLines(textfile, n = 1), 1, 5) # read first line to check which logger it is
  if(first == "Label"){ #check format based on first line
    dat <- ReadInBodyITAS(textfile)
  } else if(first=="Versi"){
    dat <- ReadInBodyUTL(textfile)
  } else {
    warning(paste(textfile, "format not recognised")) # warning if logger not recognized
    dat <- NULL
  }
  dat
}
ddd <- ReadData("#001035_20080924_1000.txt")
head(ddd)


#### IMPORT DATA PER SITE ####
ImportData <- function(site){
  # Define directory (recursive = true reads subdirectories)
  myfiles <- dir(path = paste0("/Users/audhalbritter/Dropbox/seedclim klimadaten/rawdata by Site/",site), pattern = "txt", recursive = TRUE, full.names = TRUE)
  
  # make a list of textfiles
  mdat <- ldply(as.list(myfiles), ReadData)
  mdat
}
ddd <- ImportData("Arh")
head(ddd)
unique(ddd$logger)


#############################################################################################################
#### METADATA ####

####  READ IN META DATA ITAS LOGGERS ####
ReadInMetaDataITAS <- function(textfile, SITE){
  # extract site name from file name
  textfile2 <- basename(textfile)
  textfile2 <- enc2utf8(gsub("ITAS ", "", textfile2))
  site <- NameCheck(textfile2, textfile2) # check the site name
  
  # extract start and stop date from data
  dat <- read.table(textfile, header=TRUE, sep="\t", dec=",", comment.char = "", stringsAsFactors = FALSE)
  dat <- dat[-1,] # delete first row with units
  dat$date <- dat$Label # extract date
  # some files have an extra time column (less than 15 characters)
  if(nchar(dat$date[1]) <= 15){
    start.date <- paste(strsplit(dat$date[1], " ")[[1]][1], dat$X[1], sep=" ")
    stop.date <- paste(strsplit(dat$date[length(dat$date)], " ")[[1]][1], dat$X[length(dat$date)], sep=" ")
  } else { # for normal files
    start.date <- dat$date[1]
    stop.date <- dat$date[length(dat$date)]
  }
  # convert to date
  start.date <- dmy_hms(start.date, tz = "Africa/Algiers")
  stop.date <- dmy_hms(stop.date, tz = "Africa/Algiers")
  temp.logger <- NA # column needed because UTL will have a logger name
  meta.dat <- data.frame(site, temp.logger, start.date, stop.date, logger = "ITAS")
  attr(meta.dat, "type") <- "ITAS" # give each file an attribute
  meta.dat
}
ddd <- ReadInMetaDataITAS("Aalrust_klima 20150527-20151008.txt")
ddd


####  READ IN META DATA UTL LOGGERS ####
ReadInMetaDataUTL <- function(textfile, SITE){
  # import head of data
  dat <- read.table(textfile, header=FALSE, nrows = 15, dec=",", comment.char = "", fill = TRUE)
  site <- dat$V2[3]
  # warning if there is no site name. Site name will be taken from folder name
  if(site == "" | site == " " ){
    site <- SITE
    warning(paste("site imputed for", SITE, basename(textfile)))
  }
  site <- NameCheck(site, textfile) # check site names
  temp.logger <- dat$V2[4]
  start.date <- ymd(dat$V2[10], tz = "Africa/Algiers")
  stop.date <- ymd(dat$V2[grep("SamplingEndsAt", dat$V1)], tz = "Africa/Algiers") # in some files the end date are split to two lines 
  meta.dat <- data.frame(site, temp.logger, start.date, stop.date, logger = "UTL")
  attr(meta.dat, "type") <- "UTL" # give each file an attribute
  meta.dat
}
ddd <- ReadInMetaDataUTL("#001035_20080924_1000.txt")
head(ddd)



#### Read in Meta data from ITAS or UTL loggers ####
ReadMetadata <- function(textfile, SITE){
  print(textfile)
  first <- substring(readLines(textfile, n = 1), 1, 5)
  if(first == "Label"){ #check format based on first line
    dat <- ReadInMetaDataITAS(textfile)
  } else if(first=="Versi"){
    dat <- ReadInMetaDataUTL(textfile, SITE = SITE)
  } else {
    warning(paste(textfile, "format not recognised"))
    dat <- NULL
  }
  dat
}
ddd <- ReadMetadata("Aalrust_klima 20150527-20151008.txt")
ddd


#### IMPORT METADATA ####
ImportMetadata <- function(site){
  # Define directory (recursive = true reads subdirectories)
  myfiles <- dir(path = paste0("/Users/audhalbritter/Dropbox/seedclim klimadaten/rawdata by Site/",site), pattern = "txt", recursive = TRUE, full.names = TRUE)
  
  # make a list of textfiles
  mdat <- lapply(myfiles, ReadMetadata, SITE = site)
  ftype <- sapply(mdat, attr, "type") =="ITAS" # vector with all ITAS files
  f2type <- sapply(mdat, attr, "type") =="UTL" # vector with all ITAS files
  
  # divide ITAS and UTL into 2 data sets
  dat.ITAS <- mdat[ftype]
  dat.UTL <- mdat[f2type]
  
  # rbind textfiles
  all.ITAS <- do.call(rbind, c(dat.ITAS, dat.UTL))
  all.ITAS
}
ddd <- ImportMetadata("Skj")
ddd
