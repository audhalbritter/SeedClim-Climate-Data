###############################################
### Extract gridded data from met.no
###############################################

#### Libraries ####
library("raster")
library("ncdf4")
library("lubridate")
library("plyr")
library("reshape2")
library("ggplot2")


#### Load SeedClim coordinates ####
sites <- read.csv("W:/Dropbox/Bergen/SeedClim Climate/sites.csv",header=T, sep=";")
head(sites)
site.names <- sites$siteID
coords <- cbind(sites$x_UTM33_North, sites$y_UTM33_north)

#### Climate data on P ####
setwd("P:/Ecological and Environmental Change/SeedClim/met-data/AirTempData") # all data
#setwd("/Volumes/FELLES/MATNAT/BIO/Ecological and Environmental Change/SeedClim/met-data/AirTempData")

# make a list with all file names
#files <- list.files(path="/Volumes/FELLES/MATNAT/BIO/Ecological and Environmental Change/SeedClim/met-data/AirTempData", pattern='\\.nc$', full.names = TRUE)
files <- list.files(path="P:/Ecological and Environmental Change/SeedClim/met-data/AirTempData", pattern='\\.nc$', full.names = TRUE)


#### Function to read in data ####
df2 <- ldply(as.list(files), function(fl){
        # create a multilayer object
        print(fl)      
        b.obj <- brick(fl) # gives a warming message, next line defines projection
        proj4string(b.obj) <- CRS("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
        
        # extent of the seedclim sites (round ranges and add 1km)
        e <- extent(round(range(sites$x_UTM33_North))+c(-1000,1000), round(range(sites$y_UTM33_north))+c(-1000,1000))
        b.obj2 <- crop(b.obj,e) # crop the raster object to the seedclim area
        
        # extract data from raster object for the seedclim coordinates
        h.airtemp <- extract(b.obj2, coords)
        h.airtemp2 <- data.frame(t(h.airtemp))
        
        # add site names to columns
        colnames(h.airtemp2) <- site.names
        
        # convert rownames into date and add to data set
        date.time <- ymd_hms(substring(names(b.obj), 2, length(names(b.obj))))
        h.airtemp2$date <- date.time
        
        h.airtemp2
})
head(df2)
df1 <- df2
# if code gives strange minutes and seconds, replace with 0
minute(df1$date) <- 0
second(df1$date) <- 0

# exclude 3 time points with extreme high values (10^36)
df1[,1:12][df1[,1:12] > 1000] <- NA

#### reshape (slim and long dataframe) and plot data ####
D <- melt(df1, id='date')
# sort the sites along temperature and precipitation gradient
D$variable <- factor(D$variable, levels=c("Skjellingahaugen", "Gudmedalen", "Lavisdalen", "Ulvhaugen", "Veskre", "Rambera", "Hogsete", "Alrust", "Ovstedal", "Arhelleren", "Vikesland", "Fauske"))
ggplot(D,aes(date,value, color=variable))+geom_line() + scale_colour_brewer(type = "qual", palette="Paired") + facet_wrap(~variable) + theme(legend.position = "none") + xlab("") + ylab("Temperature in °C")


#### Calculate monthly means per site and save ####
D$my <- format(D$date, "%m%y") # reformat month and year
monthly <- aggregate(value ~ my+variable, D, mean)
monthly$my <- paste(15, monthly$my, sep="") # add a day
monthly$date <- dmy(monthly$my) # back to date format
monthly.temp <- monthly[order(monthly$variable, monthly$date),]
monthly.temp$logger <- "gridded"
monthly.temp <- monthly.temp[,c(4,5,3,2)]
colnames(monthly.temp) <- c("date", "logger", "value", "site")
monthly.temp$site <- substring(monthly.temp$site, 1,3)
save(monthly.temp, file = "Monthly.Temp_GriddedData_2010-2015.RData")


#### Calculate daily means per site and save ####
D$dmy <- format(D$date, "%d%m%y") # reformat day, month and year
daily <- aggregate(value ~ dmy+variable, D, mean)
daily$date <- dmy(daily$dmy)
daily.temp <- daily[order(daily$variable, daily$date),]
daily.temp$logger <- "gridded"
daily.temp <- daily.temp[,c(4,5,3,2)]
colnames(daily.temp) <- c("date", "logger", "value", "site")
daily.temp$site <- substring(daily.temp$site, 1,3)
save(daily.temp, file = "Daily.Temp_GriddedData_2010-2015.RData")
