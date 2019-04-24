# load libraries
library(lubridate)

#source biomass and composition
source("~/OneDrive - University of Bergen/Research/FunCaB/SeedClim-Climate-Data/funcab/vegetation/00funcab_data_processing.R")

source("~/OneDrive - University of Bergen/Research/FunCaB/SeedClim-Climate-Data/funcab/vegetation/biomass_cleaning.R")

# load soil temperature data
load("~/OneDrive - University of Bergen/Research/FunCaB/Data/soilTemp.RData")

#extract air temperature
airTemp <- soilTemp %>% 
  filter(!is.na(airTemp)) %>% 
  filter(between(date, ymd("2015-07-01"), ymd("2015-10-30"))) %>% 
  group_by(siteID, date) %>% 
  summarise(maxTemp = max(airTemp),
            meanTemp = mean(airTemp))

# use air temperature to create cut-off for summer temperature analyses, ie. below daily average of 5deg.
ggplot(airTemp, aes(x = date, y = meanTemp, colour = siteID)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "line") +
  geom_hline(yintercept = 5)

airTemp %>% filter(between(date, ymd("2015-08-01"), ymd("2015-11-30"))) %>%
  group_by(siteID) %>% 
  filter(maxTemp <= 5) %>% 
  filter(date == min(date)) %>% 
  ggplot(aes(date, maxTemp, colour = siteID)) +
  geom_point()
# cut-off date: 20 Oct 2015.

#summarise to mean daily temp
soilTemp <- soilTemp %>%
  ungroup() %>% 
  filter(between(date, ymd("2015-07-01"), ymd("2016-08-30")), !Treatment == "temp200cm", !TOD == "spinup") %>% 
  group_by(siteID, turfID, blockID = Block, Treatment, date, sunniness, TOD, ID) %>%
  summarise(meanTemp = mean(Value),
            maxTemp = max(Value),
            minTemp = min(Value),
            magTemp = (maxTemp - minTemp)) %>% 
  ungroup() %>% 
  mutate(turfID = recode(turfID, "GudNAGB" = "Gud13GB"),
         blockID = if_else(turfID == "Gud13GB", "13", blockID))

biomassReg %>% filter(term == "covValue") %>% 
  spread(key = functionalGroup, value = coefVal)


vegComp <- soilTemp  %>%
  #left_join(composition2015, by = c("siteID", "blockID", "turfID", "Treatment")) %>% 
  left_join(biomassReg) %>%
  group_by(turfID) %>% 
  distinct(siteID, turfID, blockID, Treatment, date, sunniness, TOD, meanTemp, .keep_all = TRUE) %>% 
  mutate(vegcov = case_when(
    Treatment == "G" ~ mossCov + forbCov,
    Treatment == "F" ~ mossCov + graminoidCov,
    Treatment == "B" ~ graminoidCov + forbCov,
    Treatment == "GF" ~ mossCov,
    Treatment == "GB" ~ forbCov,
    Treatment == "FB" ~ graminoidCov,
    Treatment == "FGB" ~ 0,
    Treatment == "C" ~ mossCov + forbCov + graminoidCov
  )) %>% 
  filter(TOD == "day")

# turn covers to zero where FG has been removed
vegComp <- vegComp %>% 
  mutate(forbCov = if_else(Treatment %in% c("F", "FB", "GF", "FGB"), 0, forbCov), 
         graminoidCov = if_else(Treatment %in% c("G", "GF", "GB", "FGB"), 0, graminoidCov),
         vegetationHeight = if_else(Treatment %in% c("GF", "FGB"), 0, vegetationHeight),
         mossHeight = if_else(Treatment %in% c("B" ,"GB", "FGB"), 0, mossHeight))

# categorise weather and filter for summer months
vegComp <- vegComp %>% 
  mutate(weather = case_when(
    sunniness > 0.6 ~ "sunny",
    sunniness > 0.3 ~ "variable",
    sunniness < 0.3 ~ "cloudy")) %>% 
  ungroup()


#--- soil freezing ---#
FD <- vegComp %>% 
  filter(!turfID == "Ves3G",
         between(date, left = dmy("01-08-2015"), right = dmy("30-06-2016")),
         TOD == "day") %>% 
  mutate(x = minTemp < 0) %>%
  arrange(date) %>% 
  group_by(turfID) %>% 
  mutate(sum = cumsum(x)) %>%
  ungroup() %>% 
  filter(date == ymd("2016-05-30"))

# --- temperature anomalies ---#
maxmin <- vegComp %>% 
  mutate(month = month(date)) %>%
  filter(month %in% c(6, 7, 8, 9)) %>% 
  left_join(weather)

maxminAnom <- maxmin %>%
  filter(TOD == "day") %>% 
  left_join(maxmin %>% filter(Treatment == "FGB") %>% ungroup() %>% select(FGBmaxTemp = maxTemp, date, siteID, blockID)) %>%
  mutate(maxAnom = maxTemp - FGBmaxTemp) %>% 
  ungroup()

magAmpAnom <- maxmin %>%
  group_by(turfID, date) %>% 
  mutate(magTemp = maxTemp - minTemp) %>% 
  left_join(maxmin %>% filter(Treatment == "FGB") %>% ungroup() %>% select(FGBmagTemp = magTemp, date, siteID, blockID)) %>%
  mutate(magAnom = magTemp - FGBmagTemp) %>% 
  ungroup()


maxmin %>% 
  filter(between(date, left = dmy("01-08-2015"), right = dmy("30-09-2015")), tempLevel == 6.5) %>% 
  group_by(turfID, date) %>% 
  ggplot(aes(x = date, y = maxTemp, colour = Treatment)) + stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6)) + stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line")