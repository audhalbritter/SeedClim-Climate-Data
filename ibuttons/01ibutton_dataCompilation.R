source("~/OneDrive - University of Bergen/Research/FunCaB/SeedclimComm/inst/graminoidRemovals/funcab_data_processing.R")

load("~/OneDrive - University of Bergen/Research/FunCaB/Data/soilTemp.RData")
load("~/OneDrive - University of Bergen/Research/FunCaB/Data/vegComp.RData")


mossHeight <- composition %>% 
  filter(Year == 2017) %>% 
  select(turfID, Treatment, mossHeight, bryophyteCov)

composition2015 <- composition %>% 
  filter(Year == 2015, !is.na(Treatment), !Treatment == "XC")

vegComp <- soilTemp %>%
  ungroup() %>% 
  filter(between(date, ymd("2015-07-01"), ymd("2016-08-30")), !Treatment == "temp200cm", !TOD == "spinup") %>% 
  group_by(date, siteID, Treatment, TOD, Temperature_level, Precipitation_level) %>%
  mutate(meanTemp = mean(Value)) %>% 
  ungroup() %>%
  distinct() %>%
  left_join(composition2015, by = c("siteID", "turfID", "Treatment")) %>% 
  mutate(vegcov = case_when(
    Treatment == "G" ~ bryophyteCov + forbCov,
    Treatment == "F" ~ bryophyteCov + graminoidCov,
    Treatment == "B" ~ graminoidCov + forbCov,
    Treatment == "GF" ~ bryophyteCov,
    Treatment == "GB" ~ forbCov,
    Treatment == "FB" ~ graminoidCov,
    Treatment == "FGB" ~ 0,
    Treatment == "c" ~ bryophyteCov + forbCov + graminoidCov
  ))

vegComp <- vegComp %>%
  left_join(mossHeight, by = "turfID", suffix = c("", ".new")) %>% 
  mutate(mossHeight = if_else(is.na(mossHeight), mossHeight.new, mossHeight),
         bryophyteCov = if_else(is.na(bryophyteCov), bryophyteCov.new, bryophyteCov)) %>%
  select(-bryophyteCov.new, -mossHeight.new)

#save(vegComp, file = "/Volumes/fja062/PhD/Projects/2017_temperature_regulation_of_functional_groups/SeedClim-Climate-Data/vegComp.RData")

maxmin <- vegComp %>%
  mutate(vegCov = case_when(
    Treatment == "GF" ~ "alone",
    Treatment == "GB" ~ "alone",
    Treatment == "FB" ~ "alone",
    Treatment == "G" ~ "together",
    Treatment == "B" ~ "together",
    Treatment == "F" ~ "together",
    Treatment == "C" ~ "intact"
  ),
  month = month(date)) %>% 
  filter(month %in% c(6,7,8), !Treatment == "temp200cm", !TOD == "spinup", !is.na(Block)) %>% 
  group_by(date, siteID, turfID, Treatment, Block, sunniness, Temperature_level, Precipitation_level, litter, graminoidCov, forbCov, bryophyteCov, graminoid, forb, mossHeight, vegetationHeight, vegCov, month) %>%
  summarise(maxTemp = max(Value),
            minTemp = min(Value))


# temperature anomalies and proportion of vegetation for each functional group PLUS total vegetation cover.
maxmin <- maxmin %>% 
  #left_join(maxmin %>% filter(Treatment == "FGB") %>% ungroup() %>% select(FGBmaxTemp = maxTemp, date, siteID, Block)) %>%
  #mutate(maxAnom = maxTemp - FGBmaxTemp) %>% 
  mutate(weather = case_when(
    sunniness > 0.6 ~ "sunny",
    sunniness > 0.3 ~ "variable",
    sunniness < 0.3 ~ "cloudy")) %>% 
  ungroup()

maxmin <- maxmin %>% 
  mutate(Temp = recode(siteID, Ulvhaugen=6.17, Lavisdalen =6.45, Gudmedalen =5.87, Skjellingahaugen =6.58, Alrust =9.14, Hogsete =9.17, Rambera =8.77, Veskre =8.67, Fauske =10.3, Vikesland =10.55, Arhelleren =10.60, Ovstedal=10.78),
         Precip= recode(siteID, Ulvhaugen =596, Lavisdalen =1321, Gudmedalen =1925, Skjellingahaugen =2725, Alrust =789, Hogsete =1356, Rambera =1848, Veskre =3029, Fauske =600, Vikesland =1161, Arhelleren =2044, Ovstedal=2923))

maxmin %>% 
  filter(between(date, left = dmy("01-06-2016"), right = dmy("31-08-2016"))) %>% 
  group_by(turfID, date) %>% 
  filter(maxTemp > 4) %>% 
  ggplot(aes(x = date, y = maxTemp)) + geom_point()

maxminTEST <- maxmin %>% 
  group_by(turfID, date) %>% 
  filter(maxTemp > 4) %>% 
  ggplot(aes(x = date, y = maxTemp, colour = Block)) + 
  geom_point() +
  facet_wrap(~siteID)

save(maxmin, file = "~/OneDrive - University of Bergen/Research/FunCaB/SeedClim-Climate-Data/ibuttons/maxmin.RData")
