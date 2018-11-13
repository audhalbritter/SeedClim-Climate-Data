####################################
# READ IN SEEDCLIM CLIMATE DATA
#          FUNCTIONS
####################################

#### IMPORT DATA ####

#### NAME CHECK FUNCTION ####
NameCheck <- function(textfile){
  textfile <- iconv(textfile, "latin1", "ASCII", sub = "q")

  if(grepl("ulv|ule", textfile)) {site <- "ulv"}
  else if(grepl("gud", textfile)) {site <- "gud"}
  else if(grepl("skj", textfile)) {site <- "skj"}
  else if(grepl("ram", textfile)) {site <- "ram"}
  else if(grepl("ves", textfile)) {site <- "ves"}
  else if(grepl("fau", textfile)) {site <- "fau"}
  else if(grepl("vik", textfile)) {site <- "vik"}
  else if(grepl("arn|arh", textfile)) {site <- "arh"}
  else if(grepl(".vs|qqvs", textfile)) {site <- "ovs"}
  else if(grepl("h.g|hqqg", textfile)) {site <- "hog"}
  else if(grepl("qqlr|aqqlr|.lr|aal|ålr|ål", textfile)) {site <- "alr"}
  else if(grepl("l.v|laqqv", textfile)) {site <- "lav"}
  else site <- "panic"

  #check
   if(!site %in% c("fau", "alr", "ulv", "vik", "hog", "lav", "arh", "ram", "gud", "ovs", "ves", "skj"))stop(paste("wrong site from", textfile))
  return(site)
}


#### Read in ITAS or UTL ####
ReadData <- function(textfile){
  print(textfile)
  first <- readLines(textfile, n = 1) # read first line to check which logger it is
  if(grepl("Label", first)){ #check format based on first line
    dat <- ReadInBodyITAS(textfile)
  } else if(grepl("Versi", first)){
    dat <- ReadInBodyUTL(textfile)
  } else {
    warning(paste(textfile, "format not recognised")) # warning if logger not recognized
    dat <- NULL
  }
  dat$file <- basename(textfile) # puts file in extra column
  return(dat)
}



#  READ IN ITAS LOGGERS
ReadInBodyITAS <- function(textfile){
  # import body of data
  dat <- readLines(textfile) %>% 
    gsub(pattern = "\xf8", replacement = "o", x = .) %>% # replace stupid multibyte character with o
    paste(collapse = "\n") %>% 
    read_delim(delim = "\t")
  
  names(dat) <- gsub("-", "", names(dat)) # remove "-" on some column names 
  
  dat <- dat %>% 
    mutate_all(na_if, "#-INF") %>% # remove #-Inf
    mutate_all(na_if, "#+INF") %>% 
    mutate_all(gsub, pattern = ",", replacement = ".") %>% # replace comma with dot
    slice(-1)  # delete first row with units

  
  # some files have an extra time column (should be in X2 column)
  if(any(names(dat) == "X2")){ 
    dat <- dat %>% 
      mutate(date = paste(gsub("(.*) .*", "\\1", Label), X2)) %>% 
      select(-X2, -Label)
  } 
    
    dat <- dat %>% rename(date = Label)
  
   dat <- dat %>% 
     mutate_at(vars(-date), as.numeric) %>% 
     mutate(date = dmy_hms(date, truncated = 1, tz = "Africa/Algiers")) %>% 
     gather(key = logger, value = value, -date) %>% 
     mutate(logger = tolower(logger))
   
   
  # extract site name from file name
  dat$site <- NameCheck(basename(tolower(textfile))) # check the site name
  dat$type <- "ITAS"

  return(dat)
}



##### READ IN UTL LOGGERS ####
ReadInBodyUTL <- function(textfile){
  # There are not always the same number of header lines, find last line with Sample
  f <- readLines(textfile, n = 30)
  skip <- which(f == "Sample")
  if(length(skip) != 1)stop(paste("no Sample", textfile)) # warming is there is no Sample
  
  # import data, without header
  dat <- read_delim(textfile, delim = "\t", skip = skip, col_names = FALSE, locale = local(decimal_mark = ","))
  
  if(ncol(dat) == 1){
    #dat <- read.table(textfile, header=FALSE, sep=" ", skip=skip, dec=",", stringsAsFactors = FALSE)
    dat <- read_delim(textfile, delim = " ", skip = skip, col_names = FALSE, locale = local(decimal_mark = ","))
    dat <- data_frame(paste(dat[, 1], dat[, 2]), dat[, 3])
  }
  if(ncol(dat) == 3){
    dat <- data_frame(paste(dat[, 1], dat[, 2], sep=" "), dat[, 3])
  }
  
  dat <- dat %>% 
    rename(date = X1, value = X2) %>% 
    mutate(date = ymd_hms(date, tz = "Africa/Algiers"),
           # replace all commas with dot and make numeric
           value = as.numeric(gsub(pattern = ",", replacement = ".", x = value)))
  
  # import head of data to extract logger and site name
  dat.h <- read.csv(textfile, sep="\t", header=FALSE, nrow=15, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  temp.logger <- gsub(" ", "",dat.h$V2[4], fixed=TRUE) #extract logger: 30cm or 200cm, delete space between nr and unit
  
  # extract site
  site.logger <- dat.h$V2[3]
  
  # give a warning if logger name is wrong
  if(!temp.logger %in% c("200cm", "30cm")){
    warning(paste(textfile, temp.logger, "logger name wrong"))
    # label these files
    t.name <- temp.logger  
  }
  else {t.name <- paste("temp", temp.logger, sep="")}
  
  # rename temp column with temp logger name
  dat <- dat %>% 
    mutate(logger = t.name,
           site = site.logger)
  
  # Missing site information
  if(dat$site[1] == "" | dat$site[1] == " " ){
    
    # extract site name from path
    SITE <- last(unlist(strsplit(x = dirname(textfile), split = "/")))
    dat <- dat %>% 
      mutate(site = SITE)
    
    warning(paste("site imputed from path for", SITE, basename(textfile)))
  }
  
  
  # check site name
  dat$site <- NameCheck(tolower(dat$site[1]))
  dat$type <- "UTL"
  
  return(dat)
}






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


#### PLOTS ####

# Function to the climate data and zoom in on different time steps. Data is temperature by default. log will include a list of loggers if inc = TRUE. If inc = FALSE it will take the opposite of the logger list.
plot_climate <- function(data = temperature, SITE, start_date = "2000.1.1", end_date = "2100.1.1", log, inc = TRUE, breaks = "month"){
  data %>% 
    filter(date > as.POSIXct(ymd(start_date)), date < as.POSIXct(ymd(end_date))) %>%
    filter(site == SITE) %>% 
    filter ((logger %in% log) == inc) %>% 
    ggplot(aes(x = date, y = value, colour = logger)) + 
    geom_line() +
    scale_x_datetime(date_breaks = breaks) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0)) +
    ggtitle(label = SITE)
}



# Function to plot climate and gridded data. log will include a list of loggers if inc = TRUE. If inc = FALSE it will take the opposite of the logger list.
plot_gridded_temp <- function(data, start_date = "2000.1.1", end_date = "2100.1.1", SITE, log, inc = TRUE, breaks = "month"){
  data %>% 
    filter(date > as.POSIXct(ymd(start_date)), date < as.POSIXct(ymd(end_date))) %>%
    filter ((logger %in% log) == inc) %>%
    filter (site %in% SITE) %>% 
    filter(value < 40) %>%
    ggplot(aes(x = date, y = value, colour = logger, size = logger)) + 
    geom_line() +
    scale_color_manual(values = c("darkgray", "lightblue")) +
    scale_size_manual(values = c(3,1)) +
    scale_x_datetime(date_breaks = breaks) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0)) +
    facet_wrap(~site) +
    xlab("") + ylab("Temperature in °C")
}
