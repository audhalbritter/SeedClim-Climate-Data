# load libraries

source("~/OneDrive - University of Bergen/Research/FunCaB/SeedClim-Climate-Data/funcab/vegetation/00funcab_data_processing.R")

load("~/OneDrive - University of Bergen/Research/FunCaB/Data/soilTemp.RData")

#extract air temperature
airTemp <- temperature %>% 
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

# fill in missing moss values with those from 2017
mossHeight <- comp2 %>% 
  filter(Year == 2017) %>% 
  select(turfID, mossHeight, mossCov) %>%
  filter(!(is.na(mossHeight) & is.na(mossCov))) %>% 
  distinct(turfID, .keep_all = TRUE) %>% 
  ungroup()

composition2015 <- comp2 %>% 
  filter(Year == 2015, !is.na(Treatment), !Treatment == "XC") %>% 
  ungroup() %>%
  left_join(mossHeight, by = "turfID", suffix = c("", ".new")) %>% 
  mutate(mossHeight = if_else(is.na(mossHeight), mossHeight.new, mossHeight),
         mossCov = if_else(is.na(mossCov), mossCov.new, mossCov)) %>%
  distinct(siteID, Treatment, turfID, vegetationHeight, mossHeight, litter, mossCov, forbCov, graminoidCov, precip0916, precip7010, precipLevel, temp0916, temp7010, tempLevel)


vegComp <- soilTemp  %>%
  left_join(composition2015) %>% 
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

# categorise weather
vegComp <- vegComp %>% 
  mutate(weather = case_when(
    sunniness > 0.6 ~ "sunny",
    sunniness > 0.3 ~ "variable",
    sunniness < 0.3 ~ "cloudy")) %>% 
  ungroup()

# filter for summer months
maxmin <- vegComp %>% 
  mutate(vegInt = case_when(
    Treatment == "GF" ~ "alone",
    Treatment == "GB" ~ "alone",
    Treatment == "FB" ~ "alone",
    Treatment == "G" ~ "together",
    Treatment == "B" ~ "together",
    Treatment == "F" ~ "together",
    Treatment == "C" ~ "intact"
  ),
  month = month(date)) %>%
  filter(month %in% c(6, 7, 8, 9))


maxmin %>% 
  filter(between(date, left = dmy("01-08-2015"), right = dmy("30-09-2015")), tempLevel == 6.5) %>% 
  group_by(turfID, date) %>% 
  ggplot(aes(x = date, y = maxTemp, colour = Treatment)) + stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6)) + stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line")

#--- soil freezing ---#
FD <- vegComp %>% 
  filter(between(date, left = dmy("01-08-2015"), right = dmy("30-06-2016"))) %>% 
  mutate(x = minTemp < 0) %>%
  arrange(date) %>% 
  group_by(turfID) %>% 
  mutate(sum = cumsum(x)) %>%
  ungroup() %>% 
  filter(date == ymd("2016-05-30"))

# --- temperature anomalies ---#
maxminAnom <- maxmin %>% 
  left_join(maxmin %>% filter(Treatment == "FGB") %>% ungroup() %>% select(FGBmaxTemp = maxTemp, date, siteID, blockID)) %>%
  mutate(maxAnom = maxTemp - FGBmaxTemp) %>% 
  ungroup()
