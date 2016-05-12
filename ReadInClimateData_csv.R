load("W:/Dropbox/seedclim klimadaten/plotting")
save.image("/Users/audhalbritter/Dropbox/Bergen/SeedClim Climate/plots")
getwd()

clim1 <- read.csv("Fauske.csv",header=T, dec=",", sep=";",fill=TRUE)
clim2 <- read.csv("Alrust.csv",header=T, dec=",", sep=";",fill=TRUE)
clim3 <- read.csv("Ulvehaugen.csv",header=T, dec=",", sep=";",fill=TRUE)
clim4 <- read.csv("Vikesland.csv",header=T, dec=",", sep=";",fill=TRUE)
clim5 <- read.csv("Hogsete.csv",header=T, dec=",", sep=";",fill=TRUE)
clim6 <- read.csv("Arhelleren.csv",header=T, dec=",", sep=";",fill=TRUE)
clim7 <- read.csv("Rambera.csv",header=T, dec=",", sep=";",fill=TRUE)
clim8 <- read.csv("Gudmedalen.csv",header=T, dec=",", sep=";",fill=TRUE)
clim9 <- read.csv("Lavisdalen.csv",header=T, dec=",", sep=";",fill=TRUE)
clim10 <- read.csv("Ovstedal.csv",header=T, dec=",", sep=";",fill=TRUE)
clim11 <- read.csv("Veskre.csv",header=T, dec=",", sep=";",fill=TRUE)
clim12 <- read.csv("Skjellingahaugen.csv",header=T, dec=",", sep=";",fill=TRUE)

clim <- rbind(clim1, clim2, clim3, clim4,clim5, clim6, clim7, clim8,clim9, clim10, clim11, clim12)

rm(clim1, clim2, clim3, clim4,clim5, clim6, clim7, clim8,clim9, clim10, clim11, clim12)

summary(clim)
head(clim)
str(clim)
#clim$date<-with(clim,as.POSIXct(strptime(paste(year,"-",month,"-",day," ",hour,":00:00", sep=""),"%Y-%m-%d %H:%M:%S")))
clim$date<-with(clim, as.POSIXct(strptime(paste(year,"-",month,"-",day," ",hour,":00:00", sep=""),tz="Africa/Algiers", "%Y-%m-%d %H:%M:%S"))) # to correct for change in winter and summer time.
clim<-clim[order(clim$SiteShort,clim$date),]


#### Get Climate data for 2014 and 2015
library(plyr)
dailyMeans <- ddply(clim, c("Site","SiteShort","year","month", "day"),summarise, temp30cm=mean(c(temp30cm)))
clim14.15 <- subset(dailyMeans, dailyMeans$year>=2014&dailyMeans$SiteShort!="Fau"&dailyMeans$SiteShort!="Vik"&dailyMeans$SiteShort!="Arh"&dailyMeans$SiteShort!="Ovs")
write.csv(clim14.15, "clim14.15.csv", row.names=FALSE)

### Gridded data
met.monthly <- read.csv("E:\\work\\projects\\SeedClim\\Climate measurments\\all collected\\precipitation gridded met.no\\nearestRealAlt\\griddedPreciipAndTempMonthly.csv",header=T, dec=",", sep=";",fill=TRUE)
head(met.monthly)
met.monthly$date<-with(met.monthly,as.POSIXct(strptime(paste(Year,"-",Month,"-","01"," ","00:00:00", sep=""),"%Y-%m-%d %H:%M:%S")))


### Calculating mean annual temperature by site (2009 - 2014)
par(mfrow=c(1,2))
monthMeans <- ddply(clim, c("Site","SiteShort","year","month"),summarise, temp2m=mean(c(temp2m)))
meanTbyMonth <- by(monthMeans, monthMeans$Site, function(x){
  air2m <- mean(x$temp2m, na.rm=TRUE)
})
plot(meanTbyMonth, col=c(2,3,4,2,3,2,3,4,4,2,3,4), pch=16)


meanT <- by(clim, clim$Site, function(x){
  air2m <- mean(x$temp2m, na.rm=TRUE)
})
plot(meanT, col=c(2,3,4,2,3,2,3,4,4,2,3,4), pch=16)
# skj and ulv probably not good data


### calculating monthly means
library(plyr)
head(clim)
monthMeans <- ddply(clim, c("Site","SiteShort","year","month"),summarise, temp2m=mean(c(temp2m)), temp30cm=mean(c(temp30cm)), tempGroundGrass=mean(c(tempGroundGrass)), tempSoil=mean(c(tempSoil)), precip=sum(c(precip)), soilmoist1=mean(c(soilmoist1)), soilmoist2=mean(c(soilmoist2)))           #
head(monthMeans)
monthMeans$date<-with(monthMeans,as.POSIXct(strptime(paste(year,"-",month,"-","01"," ","00:00:00", sep=""),"%Y-%m-%d %H:%M:%S")))

### calculating annual means/sums
library(plyr)
head(met.monthly)
met.annual <- ddply(met.monthly, c("Site","Year"),summarise, temp=mean(c(temp)), prec=sum(c(prec)))           #
head(met.annual)



### adding the site coding
library(car)
monthMeans$SiteCode <- monthMeans$Site
monthMeans$SiteCode <- recode(monthMeans$SiteCode, '
                              "Ulv" = "ALP1"; "Lav" = "ALP2"; "Gud" = "ALP3"; "Skj" = "ALP4";
                              "Alr" = "INT1"; "Hog" = "INT2"; "Ram" = "INT3"; "Ves" = "INT4";
                              "Fau" = "LOW1"; "Vik" = "LOW2"; "Arh" = "LOW3"; "Ovs" = "LOW4"
                              ')

clim$SiteCode <- clim$Site
clim$SiteCode <- recode(clim$SiteCode, '
                        "Ulv" = "ALP1"; "Lav" = "ALP2"; "Gud" = "ALP3"; "Skj" = "ALP4";
                        "Alr" = "INT1"; "Hog" = "INT2"; "Ram" = "INT3"; "Ves" = "INT4";
                        "Fau" = "LOW1"; "Vik" = "LOW2"; "Arh" = "LOW3"; "Ovs" = "LOW4"
                        ')                                      

library(car)
met.monthly$SiteCode <- met.monthly$Site
met.monthly$SiteCode <- recode(met.monthly$SiteCode, '
                               "Ulv" = "ALP1"; "Lav" = "ALP2"; "Gud" = "ALP3"; "Skj" = "ALP4";
                               "Alr" = "INT1"; "Hog" = "INT2"; "Ram" = "INT3"; "Ves" = "INT4";
                               "Fau" = "LOW1"; "Vik" = "LOW2"; "Arh" = "LOW3"; "Ovs" = "LOW4"
                               ')


# plotting  monthMeans  
dirOutput <- "C:\\Users\\admin\\Dropbox\\seedclim klimadaten\\plot by site\\monthlyMean"
setwd(dirOutput)


by(monthMeans,monthMeans$Site, function(x){
  #x11()
  jpeg(file = paste("monthlyTempMeans_", x$SiteShort, ".jpg", sep = ""), width = 40, height = 20, units = "cm", pointsize = 12, quality = 100, res=300)  
  plot(x$date, x$"temp2m",type="n", main=x$Site[1], ylab="temperature (?C)", ylim=range(cbind(clim$"temp2m", clim$"temp30cm", clim$tempGroundGrass, clim$tempSoil), na.rm=TRUE))
  matlines(x$date, cbind(x$"temp2m", x$"temp30cm", x$tempGroundGrass, x$tempSoil), col=1:4, lty=1)
  legend("topleft", legend=c("2m", "30cm", "Ground", "Soil"), lty=1, col=1:4)
  dev.off()
})                                      

by(monthMeans,monthMeans$Site, function(x){  
  #x11()
  jpeg(file = paste("monthlySoilmoistMean_", x$SiteShort, ".jpg", sep = ""), width = 40, height = 20, units = "cm", pointsize = 12, quality = 100, res=300)  
  plot(x$date, x$soilmoist1, col=1, lty=1, type="l",main=x$Site[1], ylab="moisture (%/100)", ylim=c(0,max(max(clim$soilmoist1, na.rm=TRUE), max(clim$soilmoist2, na.rm=TRUE))))
  lines(x$date, x$soilmoist2, col=2)
  legend("topleft", legend=c("soilmoist1", "soilmoist2"), lty=1, col=1:4)
  dev.off()
})             

by(monthMeans,monthMeans$Site, function(x){  
  #x11()
  jpeg(file = paste("monthlyPrecipSum_", x$SiteShort, ".jpg", sep = ""), width = 40, height = 20, units = "cm", pointsize = 12, quality = 100, res=300)  
  plot(x$date, x$precip, col=1, lty=1, type="l",main=x$Site[1], ylab="precipitation (mm/month)", ylim=c(0,max(monthMeans$precip, na.rm=TRUE)))
  dev.off()
})                          


# plotting hourly values
dirOutput <- "C:\\Users\\admin\\Dropbox\\seedclim klimadaten\\plot by site"
setwd(dirOutput)


by(clim,clim$Site, function(x){
  #x11()
  jpeg(file = paste("hourlyTemp_", x$SiteShort, ".jpg", sep = ""), width = 40, height = 20, units = "cm", pointsize = 12, quality = 100, res=300)  
  plot(x$date, x$"temp2m",type="n", main=x$Site[1], ylab="temperature (°C)", ylim=range(cbind(clim$"temp2m", clim$"temp30cm", clim$tempGroundGrass, clim$tempSoil), na.rm=TRUE))
  matlines(x$date, cbind(x$"temp2m", x$"temp30cm", x$tempGroundGrass, x$tempSoil), col=1:4, lty=1)
  legend("topleft", legend=c("2m", "30cm", "Ground", "Soil"), lty=1, col=1:4)
  dev.off()
})  

plot(clim$date, clim$temp2m,type="n")
cl <- rainbow(12)
for(i in 1:length(clim)) {
  lines(((clim[[i]]$Site)), col = cl[i])
  plotcol[i] <- cl[i]
}

library(lattice)
xyplot(temp2m~date,type=c('l'),groups= Site,data=clim,auto.key=T)




by(clim,clim$Site, function(x){  
  #x11()
  jpeg(file = paste("hourlySoilmoist_", x$SiteShort, ".jpg", sep = ""), width = 40, height = 20, units = "cm", pointsize = 12, quality = 100, res=300)  
  plot(x$date, x$soilmoist1, col=1, lty=1, type="l",main=x$Site[1], ylab="moisture (%/100)", ylim=c(0,max(max(clim$soilmoist1, na.rm=TRUE), max(clim$soilmoist2, na.rm=TRUE))))
  lines(x$date, x$soilmoist2, col=2)
  legend("topleft", legend=c("soilmoist1", "soilmoist2"), lty=1, col=1:4)
  dev.off()
})

by(clim,clim$Site, function(x){
  #x11()
  jpeg(file = paste("hourlyPrecip_", x$SiteShort, ".jpg", sep = ""), width = 40, height = 20, units = "cm", pointsize = 12, quality = 100, res=300)  
  plot(x$date, x$"precip",type="n", main=x$Site[1], ylab="precipitation (mm/h)", ylim=range(clim$"precip", na.rm=TRUE))
  matlines(x$date, x$"precip", col=1, lty=1)
  legend("topleft", legend="precip", lty=1, col=1)
  dev.off()
})




###plotting met.no gridded data
# monthly
dirOutput <- "C:\\Users\\admin\\Dropbox\\seedclim klimadaten\\plot by site\\met.no gridded\\monthly"
setwd(dirOutput)

by(met.monthly,met.monthly$Site, function(x){
  #x11()
  jpeg(file = paste("metMonthlyPrecip_", x$Site, ".jpg", sep = ""), width = 40, height = 20, units = "cm", pointsize = 12, quality = 100, res=300)
  plot(x$date, x$prec, col=1, lty=1, type="l",main=x$Site[1], ylab="precipitation (mm/month)", ylim=c(0,max(met.monthly$prec, na.rm=TRUE)))
  dev.off()
})


# annual
dirOutput <- "C:\\Users\\admin\\Dropbox\\seedclim klimadaten\\plot by site\\met.no gridded\\annual"
setwd(dirOutput)

by(met.annual,met.annual$Site, function(x){
  #x11()
  jpeg(file = paste("metAnnualPrecip_", x$Site, ".jpg", sep = ""), width = 20, height = 20, units = "cm", pointsize = 12, quality = 100, res=300)
  plot(x$Year, x$prec, col=1, lty=1, type="l",main=x$Site[1], ylab="precipitation (mm/month)", ylim=c(0,max(met.annual$prec, na.rm=TRUE)))
  dev.off()
})



# check if Soil - Grass is switched
alr <- subset(clim, clim$Site=="Hogsete"&year=="2015")
head(alr)
plot(alr$date, alr$temp2m,type="n", ylab="temperature", ylim=c(-30,45))
matlines(alr$date, alr$temp2m, col=1, lty=1)
matlines(alr$date, alr$temp30cm, col=2, lty=1)
matlines(alr$date, alr$tempGroundGrass, col=3, lty=1)
matlines(alr$date, alr$tempSoil, col=4, lty=1)
matlines(alr$date, cbind(alr$temp2m, alr$temp30cm, alr$tempGroundGrass, alr$tempSoil), col=1:3, lty=1)
legend("bottomleft", legend=c("2m", "30cm", "Ground", "Soil"), lty=1, col=1:4)

alr$cumsum<-cumsum(alr$tempGroundGrass>1)
alr[4484,c(3,22)]
