
# is alrust missing temp30cm recent data

head(temperature2)
all2019 <- temperature2 %>%
  filter(site=="Alrust")%>%
  filter(logger=="temp200cm") %>%
  filter(date >= "2018-01-01")

all2019 <- all2019 %>% filter(!is.na(value))
max(all2019$date)
# "2018-09-25 05:00:00 CET" last value temp30cm in the data
# "2019-05-08 10:00:00 CET" last value for temp200cm
# no data to in UTL for fall missing data for temp30cm

##### arh is good
# flagged variance in tempabove since 2018 being high
ahrtem <- temperature2 %>% filter(logger=="tempabove") %>%
  filter(site=="arh") %>% filter(year(date) >=2018)
tail(ahrtem)

##### fau needs work
fau2019 <- temperature2 %>%
  filter(site=="fau")%>%
  filter(logger=="tempabove") %>%
  filter(date >= "2018-01-01")
## 2018 approropriately flagged for variance
head(fau2019)

temperature2 %>% 
  filter(site == "fau") %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ logger)

fau2019 <- fau2019 %>% filter(!is.na(value))
max(fau2019$date)
### "2019-05-08 07:00:00 CET" last date value
### missing the data files for the 2019 growing season

###### Hogsete looks good
temperature2 %>% 
  filter(site == "hog") %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ logger)

###### Lavisdalen looks good
temperature2 %>% 
  filter(site == "lav") %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ logger)
###### Rambera looks good
temperature2 %>% 
  filter(site == "ram") %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ logger)

###### gudmedalen -> missing the latest soil data 
temperature2 %>% 
  filter(site == "gud") %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ logger)
## was due to removal of all temperature and temperature2 data <-  which included 2019

###### Ovstedal most recent temp200cm missing
temperature2 %>% 
  filter(site == "ovs") %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ logger)
## both fall2019 data are labled at 30cm
## 200cm for may didnt sample #002473_20181003_1400.txt has data, _1100 does not
## ovstedal is also missing spring 2019 smapled at 30cm
head(temperature2)
ovesprob <- temperature2 %>%
  filter(file==c("#002486_20190506_1100.txt", "#002473_20190506_1200.txt"))

ovesprob %>% 
  filter(site == "ovs") %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ file)
levels(factor(ovesprob$logger))
## "#002473" is the above 200cm logger

###### Skjellingahaugen missing recent soil temperature
temperature2 %>% 
  filter(site == "skj") %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ logger)

skjprob <- temperature2 %>% filter(site == "skj") %>% 
  filter(!is.na(value)) %>% filter(type=="ITAS")
tail(skjprob)
skjprob <- newClimateRepo %>% filter(file == "Skjellingahaugen_met1_autumn2017.txt") # this
######file is not being loaded
## 2019 growing season data was measured every two seconds for soil temp
## 2018 is missing too

###### Ulvhaugen missing recent 200 and 30cm data
temperature2 %>% 
  filter(site == "ulv") %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ logger)
ulvcheck <- temperature %>% filter(site=="ulv")%>% filter(type=="UTL") 
max(ulvcheck$date)
# max of the UTL data was from "2019-05-29 12:00:00 CET"-- need fall data

###### Veskre looks good after "temperature" was moved to "tempsoil"
temperature2 %>% 
  filter(site == "ves") %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ logger)
  
vescheck <- temperature2 %>%
  filter(site=="Veskre")%>%
  filter(logger=="temperature") %>%
  filter(!is.na(value))
head(vescheck)
levels(factor(vescheck$file))

###### Vikesland soil temperature is weird
temperature2 %>% 
  filter(site == "vik") %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ logger)

vikts <- temperature2 %>%
  filter(site=="Vikesland")%>%
  filter(logger=="tempsoil") %>%
  filter(date >= "2015-06-01")
head(vikts)
levels(factor(vikts$file))

# fall 2019 data(Vikesland_climate.txt) -Inf Produced for one of the temperatures 

####### Alrust
# first, check whats up witht he newClimate

head(newClimateRepo)
levels(factor(temperature$file))
allklimnew <- temperature %>% filter(file=="Aalrust_klima.txt")
allklimnew
max(allklimnew)
levels(factor(allklimnew$logger))
# most recent data not being loaded for temp200cm and temp30cm
levels(factor(temperature2$site))
