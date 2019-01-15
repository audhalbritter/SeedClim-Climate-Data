source("~/OneDrive - University of Bergen/Research/FunCaB/SeedclimComm/inst/graminoidRemovals/plotting_dim.R")

library(tidyverse)

#################### 
##### plotting ##### 

soilTemp %>%
  filter(Treatment %in% c("F", "G", "B", "FGB", "C"), date > "2015-07-01", date < "2015-09-01") %>% 
  ggplot(aes(x = hour, y = Value, colour = Treatment)) +
  geom_smooth(se = FALSE) +
  #stat_summary(fun.y = mean, geom = "line", size = 1) +
  facet_grid(Precipitation_level ~ Temperature_level, labeller = labeller(Temperature_level = tempLabs, Precipitation_level = precipLabs)) +
  scale_color_manual(values = cbPalette[c(1, 3, 8, 4, 7)], labels = c("Control", "Bryophyte", "Forb", "Graminoid", "All"), name = "Removal") +
  labs(x = "Time (hr)", y = "Soil temperature (ºC)") +
  theme_classic() +
  axis.dimLarge 
ggsave(filename = paste0("summer_temp_No_weather.jpg"), width = 9, height = 7, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")


soilTemp %>%
  mutate(weather = case_when(
    sunniness > 0.66 ~ "sunny",
    sunniness > 0.33 ~ "variable",
    sunniness < 0.33 ~ "cloudy")) %>% 
  filter(Treatment %in% c("F", "G", "B", "FGB", "C"), weather %in% c("sunny", "cloudy"), between(date, ymd("2015-07-01"), ymd("2015-09-01"))) %>% 
  ggplot(aes(x = hour, y = Value, colour = Treatment, linetype = weather)) +
  geom_smooth(se = FALSE) +
  #stat_summary(fun.y = mean, geom = "line", size = 1) +
  facet_grid(Precipitation_level ~ Temperature_level, labeller = labeller(Temperature_level = tempLabs, Precipitation_level = precipLabs)) +
  scale_color_manual(values = cbPalette[c(1, 3, 8, 4, 7)], labels = c( "Control", "Bryophyte", "Forb", "Graminoid", "All"), name = "Removal") +
  labs(x = "Time (hr)", y = "Soil temperature (ºC)") +
  theme_classic() +
  axis.dimLarge 
  ggsave(filename = paste0("summer_temp_With_weather.jpg"), width = 9, height = 7, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")


soilTemp %>%
  filter(Treatment %in% c("C", "FGB", "B", "G", "F")) %>% 
  filter(date > "2015-08-01", date < "2016-08-01") %>% 
  ggplot(aes(x = date, y = Value, colour = Treatment)) +
  stat_summary(fun.y = mean, geom = "line", size = 1) +
  facet_grid(Precipitation_level ~ Temperature_level, labeller = labeller(Temperature_level = tempLabs, Precipitation_level = precipLabs)) +
  scale_color_manual(values = cbPalette[c(1, 3, 4, 8, 7)], labels = c("Control", "Bryophyte", "Graminoid", "Forb", "All"), name = "Removal") +
  labs(y = "Soil temperature (ºC)") +
  theme_classic() +
  axis.dimLarge + 
  theme(axis.text.x  = element_text(angle = 45),
        axis.title.x = element_blank()) +
  ggsave(filename = paste0("summer_temp_annual-cycle.jpg"), width = 9, height = 7, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")

soilTemp <- arrange(mutate(soilTemp, Treatment = factor(Treatment, levels=treatOrder)), Treatment)


plotibuttontempsites <- plot_grid(vik +theme(legend.position = "none"), lav, nrow = 1, rel_widths = c(0.75,1))
ggsave(plotibuttontempsites, file = "~/Documents/seedclimComm/figures/plotibuttontempsites.jpg", dpi = 300, width = 10, height = 4)

soilTemp %>% 
  ungroup() %>% 
  #filter(Treatment %in% c("C", "FGB", "B", "G", "F")) %>% 
  filter(Value < 28, Value > -18, !Treatment == "temp200cm", siteID == "Ulvhaugen") %>% #work in progress
  mutate(Value = if_else(Value < 0, Value, NA_real_)) %>% 
  filter(date > "2015-08-15", date < "2016-04-15") %>% 
  group_by(Treatment, siteID) %>% 
  arrange(date) %>% 
  filter(!is.na(Value)) %>% 
  mutate(FS = cumsum(Value)) %>% 
  ungroup() %>% 
  ggplot(aes(x = date, y = FS, colour = Treatment)) +
  geom_line()


FS <- arrange(mutate(FS, Treatment = factor(Treatment, levels=treatOrder)), Treatment)

soilTemp %>%
  ungroup() %>% 
  filter(between(date, ymd("2015-07-15"), ymd("2016-06-29")), !Treatment == "temp200cm", siteID == "Lavisdalen") %>%
  #mutate(Treatment = factor(Treatment, levels = c("C", "FB", "GB", "GF", "FGB"))) %>% 
  group_by(Block, date, Treatment) %>% 
  summarise(minTemp = min(Value)) %>% 
  mutate(x = minTemp < -1.5) %>%
  arrange(date) %>% 
  group_by(Treatment, Block) %>% 
  mutate(sum = cumsum(x), n = n()) %>% 
  ggplot(aes(x = date, y = sum, colour = Treatment)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "line") +
  #geom_line(size = 0.8) +
  scale_color_manual(values = cbPalette[c(1, 4, 8, 3, 7)], labels = c("Control", "Graminoid", "Forb", "Bryophyte", "All"), name = "Removal") +
  axis.dimLarge +
  ylab("Sum of frost days") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.ticks=element_blank(),
        axis.line.x = element_blank(),
        axis.title=element_text(size=15))


ggplot(FS, aes(x = as.factor(Precipitation_level), y = FS, colour = Treatment)) +
  geom_boxplot(size = 0.8, position = position_dodge(0.95)) +
  facet_grid(.~ Temperature_level, labeller = labeller(Temperature_level = tempLabs)) +
  scale_colour_manual(values = cbPalette[c(1, 3, 4, 8, 7, 9, 10, 2)], labels = c("Control", "Bryophyte", "Graminoid", "Forb", "All"), name = "Removal") +
  labs(y = "Frost sum", x = "Precipitation (mm/1000)") +
  theme_classic() +
  axis.dimLarge 
  ggsave(filename = "frostSum_15-16.jpg", path = "/Users/fja062/Documents/seedclimComm/figures/", dpi = 300, width = 11, height = 5)

#################### 
#### frost days ####

FD <- soilTemp %>% 
  ungroup() %>% 
  filter(Value < 28, Value > -18, !Treatment == "temp200cm") %>% #work in progress
  filter(Value < 0) %>% 
  filter(date > "2015-08-01", date < "2016-08-01") %>% 
  distinct(date, .keep_all = TRUE) %>% 
  group_by(Temperature_level, Precipitation_level, siteID, Treatment, Block) %>% 
  summarise(sum = n()) %>% 
  ungroup()

FD <- arrange(mutate(FD, Treatment = factor(Treatment, levels=treatOrder)), Treatment)


ggplot(FD, aes(x = as.factor(Precipitation_level), y = sum, colour = Treatment)) +
  geom_boxplot(size = 0.8, position = position_dodge(0.95)) +
  facet_wrap(~Temperature_level, labeller = labeller(Temperature_level = tempLabs)) +
  scale_colour_manual(values = cbPalette[c(1, 3, 4, 8, 7)], labels = c("Control", "Bryophyte", "Graminoid", "Forb", "All"), name = "Removal") +
  labs(y = "Frost days", x = "Precipitation (mm/1000)") +
  theme_classic() +
  axis.dimLarge +
  ggsave(filename = "frostDays_15-16.jpg", path = "/Users/fja062/Documents/seedclimComm/figures/", dpi = 300, width = 11, height = 5)


lav <- soilTemp %>%
  mutate(weather = case_when(sunniness > 0.66 ~ "sunny",
                             sunniness > 0.33 ~ "variable",
                             sunniness < 0.33 ~ "cloudy")) %>% 
  filter( weather %in% c("sunny", "cloudy"), between(date, ymd("2015-08-01"), ymd("2015-09-01")), siteID == "Lavisdalen") %>% 
  ggplot(aes(x = hour, y = Value, colour = Treatment, linetype = weather)) +
  geom_smooth(se = FALSE, method = "loess") +
  scale_colour_manual("Functional groups", values = c(cbPalette[3],"black", cbPalette[c(7,5,6,2,4,10,1)]), limits = c("temp200cm","FGB","GF", "GB", "FB", "G", "F", "B", "C"), labels = c("air 200cm","Bare ground","Bryophytes", "Forbs","Graminoids", "Bryophytes and forbs", "Graminoids and bryophytes", "Forbs and graminoids", "Bryophytes, forbs and\ngraminoids")) +
  labs(x = "Time (hr)", y = "Soil temperature (ºC)") +
  #scale_linetype_manual(values = c("73",1)) +
  theme(plot.title = element_text(hjust = 0.5, size = 19)) +
  #ylim(7,19) +
  axis.dimLarge

soilTemp %>% 
  filter(between(date, ymd("2015-07-15"), ymd("2016-05-29")), !Treatment == "temp200cm") %>% 
  group_by(turfID, date) %>% 
  count() %>% 
  filter(n < 8) %>% ungroup() %>% 
  count(turfID)


soilTemp %>% 
  filter(between(date, ymd("2015-12-15"), ymd("2016-05-29")), !Treatment == "temp200cm") %>%
  distinct(turfID, date) %>% 
  count(turfID) %>% 
  arrange(n)
  
  
  # approx(x = dateTime, y = temp, xout = seq(midnight), ) to interpolate which gives list of x and y %>% as.dataframe() %>% rename() and add missing cols mutate()
FD <- soilTemp %>%
  ungroup() %>% 
  filter(between(date, ymd("2015-07-15"), ymd("2016-06-29")), !Treatment == "temp200cm", siteID == "Lavisdalen", !is.na(Value)) %>%
  #filter(!turfID %in% c("Lav1GF", "Lav4GF", "Lav2GF")) %>% 
  group_by(Block, date, Treatment) %>% 
  summarise(minTemp = min(Value)) %>% 
  mutate(x = minTemp < 0) %>%
  arrange(date) %>% 
  group_by(Treatment, Block) %>% 
  mutate(sum = cumsum(x), n = n()) %>%
  ungroup()

FD %>% 
  ggplot(aes(x = date, y = sum, colour = Treatment)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "line", size = 0.8) +
  scale_x_discrete(limits = c("FGB", "GF", "GB", "FB", "G", "F", "B", "C"), labels = c("bare","B", "F", "G", "FB", "GB", "GF", "FGB")) +
  scale_colour_manual("Vegetation", values = c("black", cbPalette[c(7,5,6,2,4,10,1)]), limits = c("FGB", "GF", "GB", "FB", "G", "F", "B", "C")) +
  scale_fill_manual("Vegetation", values = c("black", cbPalette[c(7,5,6,2,4,10,1)]), limits = c("FGB", "GF", "GB", "FB", "G", "F", "B", "C")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  axis.dimLarge +
  ylab("Cumulative frost\ndays") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.ticks=element_blank(),
        axis.line.x = element_blank(),
        axis.title=element_text(size=15))

y <- soilTemp %>%
  filter(between(date, ymd("2015-07-15"), ymd("2016-06-29")),siteID == "Lavisdalen", !Treatment == "temp200cm") %>% 
  #mutate(Treatment = factor(Treatment, levels = c("C", "FB", "GB", "GF", "FGB"))) %>% 
  group_by(Treatment, date) %>% 
  mutate(meanTemp = mean(Value)) %>% 
  ggplot(aes(x = date, y = meanTemp, colour = Treatment)) +
  geom_line(size = 0.8, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_colour_manual("Functional groups", values = c("black", cbPalette[c(7,5,6,2,4,10,1)]), limits = c("FGB","GF", "GB", "FB", "G", "F", "B", "C"), labels = c("Bare ground","Bryophytes", "Forbs","Graminoids", "Bryophytes & forbs", "Graminoids & bryophytes", "Forbs & graminoids", "Bryophytes, forbs &\ngraminoids")) +
  ylab("Soil temperature (°C)") +
  axis.dimLarge +
  theme(axis.title=element_text(size=15),
        axis.title.x = element_blank())

legend <- get_legend(lav)
z <- plot_grid(y + theme(legend.position = "none"), x, ncol = 1, rel_heights = c(1.65, 1), align = "hv", labels = c("A", "B"), label_x = 0.1, label_y = 0.95)
#eps <- plot_grid(legend, lav, ncol = 1, rel_heights = c(1,1.65))

frostSumAnn <- plot_grid(z, lav + theme(legend.position = "none"), legend, nrow = 1, rel_widths = c(1, 0.75, 0.45), labels = "C", label_x = 1.02, label_y =0.97)
ggsave(frostSumAnn, filename = "~/Documents/seedclimComm/figures/frostSumAnn2.jpg", dpi = 300, width = 11.5, height = 4.55)


iButtonData %>% filter(format(Date, "%Y-%m") == "2015-09") %>%
  filter(Treatment %in% c("C","FGB", "2m_site")) %>%
  mutate(hour = hour(Date)) %>% 
  group_by(Treatment, iButtonID, hour, Temperature_level, Precipitation_level) %>%
  summarise(mean = mean(Value)) %>%
  ggplot(aes(x = hour, y = mean, colour = Treatment)) +
  geom_smooth(se = TRUE) +
  facet_grid(Precipitation_level ~ Temperature_level) +
  scale_color_manual(values = cbPalette) +
  theme_classic()

iButtonData %>% 
  filter(!Treatment == "2m_site") %>% 
  filter(!is.na(Treatment)) %>% 
  ggplot(aes(x = Date, y = Value, colour = Treatment)) +
  geom_line() +
  facet_wrap(~ siteID) +
  ggsave(filename = "ibutton_allSites_2016.jpg", path = "/Users/fja062/Documents")


minmax <- iButtonData %>% 
  #mutate(bryophytes = if_else(Treatment %in% c("B", "FB", "GB", "FGB"), "removed", "remained")) %>% 
  group_by(siteID, Treatment, week = week(Date)) %>% 
  mutate(min = min(Value, na.rm = TRUE),
         max = max(Value, na.rm = TRUE)) %>% 
  arrange(siteID) %>% ungroup() %>% 
  gather(range, temperature, min, max)

minmax %>% 
  filter(!is.na(Treatment)) %>% 
  filter(Temperature_level == "6.5") %>% 
  ggplot(aes(x = Date, y = temperature, colour = range)) +
  geom_point() +
  facet_wrap(~ Treatment) +
  scale_color_manual(values = cbPalette) +
  theme_bw() +
  ggsave(filename = "ibutton_alpine_TempRange_2016.jpg", path = "/Users/fja062/Documents")


ibutton2017 %>% 
  filter(siteID == "Lavisdalen") %>%
  filter(Date < "2016-07-15" & Year == 2017) %>%
  filter(Date > "2016-06-20") %>% 
  ggplot(aes(x = Date, y = Value)) +
  geom_line()

# need to check ibuttons that don't seem to be logging regularly. happens in several sites.
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#1C9099", "#A6BDDB", "#ECE2F0", "orange3")


iButtonData %>% 
  filter(!is.na(Treatment)) %>% 
  filter(format(Date, "%Y-%m-%d") == "2016-05-23") %>%
  ggplot(aes(x = Date, y = Value, color = Temperature_level)) +
  geom_point() +
  facet_wrap(~ Treatment) +
  theme_bw() +
  scale_color_manual(values = cbPalette[c(2, 3, 4, 7)]) 
ggsave(filename = "ibutton_dailyAmplitude_christmas_2015.jpg", path = "/Users/fja062/Documents")

iButtonData %>% 
  filter(!is.na(Treatment)) %>% 
  filter(format(Date, "%Y-%m-%d") == "2016-06-23") %>%
  ggplot(aes(x = Date, y = Value, color = Treatment)) +
  geom_smooth(se = FALSE) +
  facet_wrap(~ Temperature_level) +
  theme_bw() +
  scale_color_manual(values = cbPalette) +
  ggsave(filename = "ibutton_dailyAmplitude_jun_2016.jpg")

# and we need to add figures to the .gitignore
