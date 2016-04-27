library(raster)
library(rgdal)
library(ncdf4)


#setwd("P:/Ecological and Environmental Change/SeedClim/met-data/AirTempData") # all data
setwd("/Volumes/FELLES/MATNAT/BIO/Ecological and Environmental Change/SeedClim/met-data/AirTempData")
#files <- list.files(c("seNorge2_TEMP1h_grid_201001.nc", "seNorge2_TEMP1h_grid_201002.nc"))
files <- list.files(pattern='\\.nc$')
# combine all list elements
airtemp <- stack(files)
# quick plot to make sure nothing went drastically wrong
plot(airtemp)

setwd()
sites <- read.csv("sites.csv",header=T, sep=";")
head(sites)
coordinates(sites)<-c("longitude", "latitude")

AirT <- extract(airtemp, sites, method="simple", buffer=NULL, fun=mean, df=TRUE) 
head(AirT)


coordinates(sites) <- ~ Longitude + Latitude
proj4string(sites) <- CRS("+proj=longlat")




at <- raster("P:/Ecological and Environmental Change/SeedClim/met-data/AirTempData/seNorge2_TEMP1h_grid_201001.nc")
at
plot(at)
crs(at)
coordinates(pos2) <- ~ Longitude + Latitude
nlayers(at)
extract(at, sites, method="simple", buffer=NULL, fun=mean, df=TRUE)
