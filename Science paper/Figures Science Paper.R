load(file = "Temperature.RData")

library("tidyverse")
library("lubridate")
library("gridExtra")

#### Figures Sciece Paper ####

meta.data <- data.frame(site = c("Fau", "Alr", "Ulv", "Vik", "Hog", "Lav", "Arh", "Ram", "Gud", "Ovs", "Ves",  "Skj"),
                        T_level = rep(c("Boreal","Sub-alpine","Alpine"), 4),
                        P_level = c(1,1,1,2,2,2,3,3,3,4,4,4))

# select dates where all sites have climate data up until end of 2015
temperaturedata <- temperature %>% 
  filter(date > "2009-07-02 00:00:00" & date < "2013-12-31 23:00:00") %>% 
  mutate(flag = ifelse(is.na(flag), "ok", flag)) %>% 
  filter(flag != "FewData") %>% 
  left_join(meta.data, by = "site")



# Calculate monthly data
monthlyTemperaturedata <- temperaturedata %>% 
  filter(!is.na(value)) %>%
  mutate(date = dmy(paste0("15-",format(date, "%b.%Y")))) %>%
  group_by(date, logger, site) %>%
  summarise(n = n(), value = mean(value), sum = sum(value)) %>%
  filter(n < 3024) %>%
  left_join(meta.data, by = "site") %>%
  mutate(T_level = factor(T_level), P_level = factor(P_level)) %>%
  select(-n, -sum)


# add missing months
full_grid <- expand.grid(logger = unique(monthlyTemperaturedata$logger), site = unique(monthlyTemperaturedata$site), date = seq(min(monthlyTemperaturedata$date), max(monthlyTemperaturedata$date), by = "month"))

monthlyTemperaturedata <- left_join(full_grid, monthlyTemperaturedata) %>% tbl_df()

save(temperaturedata, file = "temperaturedata.RData")
save(monthlyTemperaturedata, file = "monthlyTemperaturedata.RData")


FigS1A_MonthlyAirTemp <- monthlyTemperaturedata %>% 
  filter(logger %in% c("temp200cm", "tempabove", "tempsoil")) %>%
  filter(!is.na(T_level)) %>% 
  mutate(T_level = factor(T_level, levels = c("Boreal","Sub-alpine","Alpine"))) %>% 
  mutate(logger = plyr::mapvalues(logger, c("temp200cm", "tempabove", "tempsoil"), c("Air temperature 2m", "Ground temperature 0cm", "Soil temperature -5cm"))) %>% 
  group_by(T_level, date, logger) %>% 
  summarise(mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = date, y = mean, color = T_level)) +
  geom_line(size = 0.9) +
  labs(x = "", y = "Average monthly temperature in °C") +
  scale_color_manual(name = "Temperature level", values = c("darkred", "red", "orange"), guide = FALSE) +
  facet_wrap(~ logger, nrow = 3) +
  theme_minimal(base_size = 20)

# Boxplots
FigS1B_MonthlyAirTemp <- monthlyTemperaturedata %>% 
  filter(logger %in% c("temp200cm", "tempabove", "tempsoil")) %>%
  filter(month(date) %in% 6:9) %>% 
  filter(!is.na(T_level)) %>% 
  mutate(T_level = factor(T_level, levels = c("Boreal","Sub-alpine","Alpine"))) %>% 
  mutate(logger = plyr::mapvalues(logger, c("temp200cm", "tempabove", "tempsoil"), c("Air temperature 2m", "Ground temperature 0cm", "Soil temperature -5cm"))) %>% 
  ggplot(aes(x = T_level, y = value, fill = T_level)) +
  geom_boxplot() +
  labs(x = "", y = "Average summer temperature in °C") +
  scale_fill_manual(name = "Temperature level", values = c("darkred", "red", "orange")) +
  facet_wrap(~ logger, nrow = 3) +
  theme_minimal(base_size = 20) +
  theme(legend.position="bottom")


#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend <- g_legend(FigS1B_MonthlyAirTemp)

p3 <- grid.arrange(arrangeGrob(FigS1A_MonthlyAirTemp,
                               FigS1B_MonthlyAirTemp + theme(legend.position="none"),
                               nrow=1), mylegend, nrow=2,heights=c(10, 1))
ggsave("FigS1_MonthlyAirTemp.jpg", p3, path = "~/Dropbox/Bergen/SeedClim Climate/SeedClim-Climate-Data/Science Figures", width = 10, height = 7, dpi = 300)
ggsave("FigS1_MonthlyAirTemp.pdf", p3, path = "~/Dropbox/Bergen/SeedClim Climate/SeedClim-Climate-Data/Science Figures", width = 10, height = 7, dpi = 300)



FigS4_MonthlyAirGroundTemp <- monthlyTemperaturedata %>% 
  filter(logger %in% c("temp200cm", "tempabove")) %>% 
  filter(!is.na(value)) %>% 
  group_by(date, logger) %>% 
  summarise(mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = date, y = mean, color = logger)) +
  geom_line(size = 0.9) +
  labs(x = "", y = "Monthly air temperature in °C") +
  scale_color_manual(name = "Temperature", values = c("royalblue2", "seagreen3"), labels = c("Air (2m)", "Ground (5cm)")) +
  #facet_wrap(~T_level, nrow = 3) +
  theme_minimal(base_size = 15)
ggsave("FigS4_MonthlyAirGroundTemp.pdf", path = "~/Desktop", width = 10, height = 7, dpi = 300)



### GRIDDED CLIMATE DATA
load("GriddedDailyClimateData2009-2017.RData", verbose = TRUE)

climate2 <- climate %>% 
  filter(Date > "2009-06-30 00:00:00", Date < "2013-12-31 23:00:00")

# calculate monthly values
monthlyClimate2 <- climate2 %>%
  select(Site, Date, Precipitation, Temperature) %>% 
  mutate(Date = dmy(paste0("15-",format(Date, "%b.%Y")))) %>%
  gather(key = logger, value = value, -Site, -Date) %>% 
  group_by(Date, Site, logger) %>%
  summarise(n = n(), mean = mean(value), sum = sum(value)) %>% 
  mutate(mean = ifelse(logger == "Precipitation", sum, mean)) %>% 
  select(-n, -sum) %>% 
  rename(date = Date, site = Site, value = mean) %>% 
  left_join(meta.data, by = "site") %>%
  mutate(T_level = factor(T_level, levels = c("Boreal","Sub-alpine","Alpine")), P_level = factor(P_level))

save(monthlyClimate2, file = "monthlyClimate2.RData")
load("monthlyClimate2.RData", verbose = TRUE)

FigS2A_MonthlyGriddedPrecip <- monthlyClimate2 %>% 
  filter(logger == "Precipitation") %>% 
  group_by(P_level, date) %>% 
  summarise(mean = mean(value, na.rm = TRUE)) %>% 
  ggplot(aes(x = date, y = mean, color = P_level)) +
  geom_line(size = 0.9) +
  labs(x = "", y = "Average monthly precipitation in mm") +
  scale_color_manual(name = "Precipitation level", values = c("#DFEBF7", "#9DC3E6", "#2E75B6", "#213964"), guide = FALSE) +
  #scale_linetype_manual(name = "Precipitation level", values = c(3, 2, 5, 1), guide = FALSE) + #"dotted", "dashed", "long-dashed", "solid"
  theme_minimal(base_size = 20)

# Boxplots
FigS2B_MonthlyGriddedPrecip <- monthlyClimate2 %>% 
  filter(logger == "Temperature" & month(date) %in% 6:9 | logger == "Precipitation") %>% 
  filter(year(date) != 2016) %>% 
  group_by(logger, T_level, P_level, year(date)) %>% 
  summarise(n = n(), mean = mean(value, na.rm = TRUE), sum = sum(value, na.rm = TRUE)) %>% 
  mutate(mean = ifelse(logger == "Precipitation", sum, mean)) %>% 
  #group_by(logger, P_level) %>% 
  #summarise(N = n(), Mean = mean(mean), SE = sd(mean, na.rm = TRUE) / sqrt(N)) %>% 
  filter(logger == "Precipitation") %>% 
  ggplot(aes(x = P_level, y = sum, fill = P_level)) +
  geom_boxplot() +
  labs(x = "", y = "Average annual precipitation in mm") +
  scale_fill_manual(name = "Precipitation level", values = c("#DFEBF7", "#9DC3E6", "#2E75B6", "#213964")) +
  theme_minimal(base_size = 20) +
  theme(legend.position="bottom")

mylegend <- g_legend(FigS2B_MonthlyGriddedPrecip)

p4 <- grid.arrange(arrangeGrob(FigS2A_MonthlyGriddedPrecip,
                               FigS2B_MonthlyGriddedPrecip + theme(legend.position="none"),
                               nrow=1), mylegend, nrow=2,heights=c(10, 1))
ggsave("FigS2_MonthlyGriddedPrecip.jpg", p4, path = "~/Dropbox/Bergen/SeedClim Climate/SeedClim-Climate-Data/Science Figures", width = 10, height = 7, dpi = 300)
ggsave("FigS2_MonthlyGriddedPrecip.pdf", p4, path = "~/Dropbox/Bergen/SeedClim Climate/SeedClim-Climate-Data/Science Figures", width = 10, height = 7, dpi = 300)


#### SOIL MOISTURE
load(file = "Soilmoisture.RData")

soilmoisture2 <- soilmoisture %>% 
  #filter(between(date, "2009-07-02 00:00:00", "2013-12-31 23:00:00")) %>% 
  filter(month(date) %in% 6:9) %>% 
  mutate(flag = NA) %>% 
  mutate(flag = ifelse(file == "Alrust_met1 (4).txt" & logger == "jordf2", "wrongValues", flag)) %>% 
  mutate(flag = ifelse(file %in% c("Vikesland_met1_Fall2016.txt", "Vikesland-GP1-9-114.txt", "Vikesland-met1-20120911.txt") & logger == "jordf2", "wrongValues", flag)) %>% 
# Check Gud, 2012, 2013 jordf1
  filter(is.na(flag)) # does not work with flag != "wrongValues"

monthlySoilmoisture <- soilmoisture2 %>% 
  filter(!is.na(value)) %>%
  mutate(date = dmy(paste0("15-",format(date, "%b.%Y")))) %>%
  group_by(date, site) %>%
  summarise(n = n(), value = mean(value), sum = sum(value)) %>%
  left_join(meta.data, by = "site") %>%
  mutate(P_level = factor(P_level)) %>%
  mutate(T_level = factor(T_level, levels = c("Boreal", "Sub-alpine", "Alpine"))) %>% 
  select(-sum)
  
# add missing months
full_grid2 <- crossing(site = unique(monthlySoilmoisture$site), date = seq(min(monthlySoilmoisture$date), max(monthlySoilmoisture$date), by = "month"))

monthlySoilmoisture2 <- left_join(full_grid2, monthlySoilmoisture, by = c("site", "date")) %>% 
  fill(T_level, P_level, .direction = "down")


FigS5_Soilmoisture <- monthlySoilmoisture %>%
  filter(year(date) < 2013) %>% 
  ggplot(aes(x = date, y = value, shape = T_level, fill = P_level, color = P_level)) +
  geom_point(size = 3) +
  labs(x = "", y = "Average monthly soilmoisture \n in the four warmest month") +
  scale_fill_manual(name = "Precipitation level", values = c("#DFEBF7", "#9DC3E6", "#2E75B6", "#213964")) +
  scale_color_manual(name = "Precipitation level", values = c("#DFEBF7", "#9DC3E6", "#2E75B6", "#213964")) +
  scale_shape_manual(name = "Temperature level", values = c(25, 21, 24)) +
  facet_wrap(~ T_level) +
  theme_minimal(base_size = 20)

ggsave("FigS5_Soilmoisture.jpg", FigS5_Soilmoisture, path = "~/Dropbox/Bergen/SeedClim Climate/SeedClim-Climate-Data/Science Figures", width = 12, height = 5, dpi = 300)


Longterm <- data_frame(T_level = as.factor(c(rep("Boreal", 4), rep("Sub-alpine", 4), rep("Alpine", 4))),
                       P_level = as.factor(c(rep(c(1,2,3,4),3))),
                       P_mean = c(600, 1161, 2044, 2923, 789, 1356, 1848, 3029, 596, 1321, 1925, 2725),
                       T_mean = c(10.3, 10.55, 10.6, 10.78, 9.14, 9.17, 8.77, 8.67, 6.17, 6.45, 5.87, 6.58),
                       logger = "30 year normal")



# Filter precipitation and tetraterm temperature
FigS3_NormalsvsGridded <- monthlyClimate2 %>% 
  filter(logger == "Temperature" & month(date) %in% 6:9 | logger == "Precipitation") %>% 
  #filter(year(date) != 2016) %>% 
  group_by(logger, T_level, P_level, year(date)) %>% 
  summarise(n = n(), mean = mean(value, na.rm = TRUE), sum = sum(value, na.rm = TRUE)) %>% 
  mutate(mean = ifelse(logger == "Precipitation", sum, mean)) %>% 
  group_by(logger, T_level, P_level) %>% 
  summarise(N = n(), Mean = mean(mean), SE = sd(mean, na.rm = TRUE) / sqrt(N)) %>% 
  unite(Mean_SE, Mean, SE, sep = "_") %>% 
  spread(key = logger, value = Mean_SE) %>% 
  separate(col = Precipitation, into = c("P_mean", "P_se"), sep = "_", convert = TRUE) %>% 
  separate(col = Temperature, into = c("T_mean", "T_se"), sep = "_", convert = TRUE) %>%
  select(-N) %>% 
  mutate(logger = "5 year gridded") %>% 
  bind_rows(Longterm) %>%
  ungroup() %>% 
  mutate(P_level = factor(P_level, levels = c(1,2,3,4))) %>% 
  mutate(T_level = factor(T_level, levels = c("Boreal", "Sub-alpine", "Alpine"))) %>% 
  ggplot(aes(x = P_mean, xmin = P_mean - P_se, xmax = P_mean + P_se, y = T_mean, ymin = T_mean -T_se, ymax = T_mean + T_se, color = P_level, shape = T_level, fill = factor(ifelse(logger == "5 year gridded", P_level, logger)))) +
  geom_errorbar() +
  geom_errorbarh() +
  geom_point(aes(size = logger)) +
  labs(x = "Annual precipitation in mm", y = "Tetraterm temperature in °C") +
  scale_color_manual(name = "Precipitation level", values = c("#DFEBF7", "#9DC3E6", "#2E75B6", "#213964")) +
  scale_shape_manual(name = "Temperature level", values = c(25, 21, 24)) +
  scale_fill_manual(name = "Data", values = c("#DFEBF7", "#9DC3E6", "#2E75B6", "white", "#213964")) +
  scale_size_manual(name = "Source", values = c(3, 3.01))+
  guides(fill = "none", size = guide_legend(override.aes = list(shape = c(1, 16)))) +
  theme_minimal(base_size = 20)

ggsave("FigS3_NormalsvsGridded.jpg", path = "~/Dropbox/Bergen/SeedClim Climate/SeedClim-Climate-Data/Science Figures", width = 10, height = 7, dpi = 300) 
ggsave("FigS3_NormalsvsGridded.pdf", path = "~/Dropbox/Bergen/SeedClim Climate/SeedClim-Climate-Data/Science Figures", width = 10, height = 7, dpi = 300) 





### Numbers
monthlyTemperaturedata %>% 
  filter(logger %in% c("temp200cm", "tempabove", "tempsoil")) %>%
  filter(month(date) %in% 6:9) %>% 
  filter(!is.na(T_level)) %>% 
  mutate(T_level = factor(T_level, levels = c("Boreal","Subalpine","Alpine"))) %>% 
  mutate(logger = plyr::mapvalues(logger, c("temp200cm", "tempabove", "tempsoil"), c("Air temperature 2m", "Ground temperature 0cm", "Soil temperature -5cm"))) %>% 
  group_by(T_level) %>% 
  summarise(mean = mean(value))



monthlyClimate2 %>% 
  filter(logger == "Temperature" & month(date) %in% 6:9 | logger == "Precipitation") %>% 
  filter(year(date) != 2016) %>% 
  group_by(logger, T_level, P_level, year(date)) %>% 
  summarise(n = n(), mean = mean(value, na.rm = TRUE), sum = sum(value, na.rm = TRUE)) %>% 
  mutate(mean = ifelse(logger == "Precipitation", sum, mean)) %>% 
  group_by(logger, T_level) %>% 
  summarise(N = n(), Mean = mean(mean), SE = sd(mean, na.rm = TRUE) / sqrt(N))
