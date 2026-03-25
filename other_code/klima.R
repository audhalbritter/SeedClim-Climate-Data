library("ggplot2")
library("plyr")
library("reshape2")
library("lubridate")


#load data
datadir <- "/home/gbsrt/Dropbox/seedclim klimadaten/csv/"

sites <- c("Alrust", "Arhelleren", "Fauske", "Gudmedalen" , "Hogsete", "Lavisdalen", "Ovstedal", "Rambera", "Skjellingahaugen", "Ulvehaugen", "Veskre", "Vikesland")

clim <- lapply(sites, function(S){
  k<-read.table(paste0(datadir, S, ".csv"), header = TRUE, sep=";", dec = ",")
  k$date <- as.POSIXct(k$date, format = "%d.%m.%Y %H:%M", tz = "NMT")
  k$SiteShort <- NULL
  k$day <-NULL
  k$month <-NULL
  k$year <-NULL
  k$hour <- NULL
  k$ID <-NULL
  k
  })
names(clim)<-sites

plot <- FALSE
#plot all temperature data for each site
if(plot){
  sapply(clim,function(x){
    clim1 <- melt(x, id.vars = c("Site", "date"), measure.vars = c("temp2m", "temp30cm", "tempGroundGrass", "tempSoil"))
    g <- ggplot(clim1, aes(x = date, y = value, colour = variable)) + geom_line() + ggtitle(x$Site[1])
    print(g)
  })
}

#calculate monthly means
inclusionThreshold <- 0.5

climMeans<-ldply(clim, function(cli){
  ddply(cli, .(format(date, "%b.%Y")), function(x){ 
    means<-colMeans(x[, c("temp2m", "temp30cm", "tempGroundGrass", "tempSoil")], na.rm = TRUE)
    ndata<-colMeans(!is.na(x[, c("temp2m", "temp30cm", "tempGroundGrass", "tempSoil")]))
    means[ndata < inclusionThreshold] <- NA #Checks that mean is based on a reasonable amount of data
    means
    })
})
names(climMeans)[1:2]<-c("site", "date")
climMeans$site<-factor(climMeans$site)
climMeans$date <- as.POSIXct(paste0("15.",climMeans$date), format = "%d.%b.%Y", tz = "NMT")

head(climMeans)

#colour scale
#library(RColorBrewer)
#display.brewer.all() 
sc <- scale_colour_brewer(type = "qual", palette = "Paired")
if(plot){
  ggplot(climMeans, aes(x = date, y = temp2m, colour = site)) + geom_line() + sc
  ggplot(climMeans, aes(x = date, y = temp30cm, colour = site)) + geom_line() + sc
  ggplot(climMeans, aes(x = date, y = tempGroundGrass, colour = site)) + geom_line() + sc 
  ggplot(climMeans, aes(x = date, y = tempSoil, colour = site)) + geom_line() + sc
  ggplot(climMeans, aes(x = date, y = tempSoil, colour = site)) + geom_line() + ylim(-3,16) + sc
}

subset(climMeans, tempSoil<(-5))
subset(climMeans, tempGroundGrass>20)
subset(clim$Lavisdalen, format(date, "%b.%Y") == "Jul.2013", tempGroundGrass)[,1]



#sapply(clim,function(x){
#  x$delta <- x$temp2m - x$temp30cm
#  print(summary(x$delta))
#  g <- ggplot(x, aes(x = date, y = delta, colour  = temp2m)) + geom_line() + scale_colour_distiller(type = "div" , palette =  "RdYlBu") + ggtitle(x$Site[1]) 
#  print(g)
#})

#deltas for one site
clim[[1]] <- within(clim[[1]], {
  delta30 <- temp30cm - temp2m
  deltaGroundGrass <- tempGroundGrass - temp2m
  deltaSoil <- tempSoil - temp2m
    })

clim1 <- melt(clim[[1]][,c(1:4,7:8)], id = 1:2)
clim1d <- melt(clim[[1]][,c(1:3,16:18)], id = 1:2)
