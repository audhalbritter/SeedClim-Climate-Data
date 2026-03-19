### Data prep for Høye and Rixen paper

library("tidyverse")

### load in cleaned temperature data 
load("Temperature.RData", verbose=TRUE)
head(temperature2)

# get just soil temp
tempsoil <- filter(temperature2, logger=="tempsoil")
tail(tempsoil)

# geographic coordinates of the sites
Sitename <- c("Skjellingahaugen", "Gudmedalen","Lavisdalen","Ulvhaugen","Veskre", "Rambera", "Hogsete",
              "Alrust","Ovstedal","Arhelleren", "Vikesland","Fauske")
Latitude <- c(60.9335,60.8328,60.8231,61.0243,60.5445,61.0866,60.8760,60.8203,60.6901,60.6652,60.8803,
              61.0355)
Longitude <- c(6.41504,7.17561,7.27596,8.12343,6.51468,6.63028,7.17666,8.70466,5.96487,6.33738,7.16982,
               9.07876)
Elevation <- c(1088,1213,1097,1208,797,769,700,815,346,431,474,589)
sitcor <- data.frame(Sitename, Latitude,Longitude,Elevation)
sitcor$Sitename <- as.factor(Sitename)

## rename variables according to specifications, take columns needed, and add geo data
tempsoil <- tempsoil %>%
  rename(Sitename=site) %>%
  rename(Temperature=value) %>%
  rename(Comments=flag) %>% 
  select(Sitename, Temperature, date, Comments) %>%
  left_join(sitcor, by="Sitename")

# add desired values 
tempsoil$Country <- rep("Norway", times=length(tempsoil$Temperature))
tempsoil$Timezone <- rep("UTC +1", times=length(tempsoil$Temperature))
tempsoil$Treatment <- rep("Control", times=length(tempsoil$Temperature))
tempsoil$Plotcode <- rep("NA", times=length(tempsoil$Temperature))
tempsoil$Sensordepth <- rep(5, times=length(tempsoil$Temperature))
tempsoil$Soilmoisture <- rep("NA", times=length(tempsoil$Temperature))
tempsoil$Sensormodel <- rep("Delta T GP1 loggers", times=length(tempsoil$Temperature))

# parse the date files and remove the old one
tempsoil$Date <- as.Date(tempsoil$date)
tempsoil$Time <- format(as.POSIXct(tempsoil$date), format = "%H:%M:%S")
tempsoil <- select(tempsoil, -date)

write.csv(tempsoil, "VandvikLynnHalbritter_soiltempdata.csv")
save(tempsoil, file = "VandvikLynnHalbritter_soiltempdata.RData")
hist(tempsoil$Temperature)
