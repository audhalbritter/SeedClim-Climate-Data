# MONTHLY AND DAILY TEMPERATURE

library("lubridate")
library("tidyverse")


load("Temperature.RData", verbose = TRUE)

#### Calculate monthly means ####
threshold <-  3 * 7 * 24 # three weeks. Minimum accepted

monthlyTemperature <- temperature2 %>%
  filter(!is.na(value)) %>%
  mutate(date = dmy(paste0("15-",format(date, "%b.%Y")))) %>%
  group_by(date, logger, site) %>%
  summarise(n = n(), value = mean(value), sum = sum(value)) %>%
  filter(n > threshold) %>%
  select(-n, -sum)


#fill missing dates with NA y merging with complete dataset
full_grid <- expand.grid(logger = unique(monthlyTemperature$logger), site = unique(monthlyTemperature$site), date = seq(min(monthlyTemperature$date), max(monthlyTemperature$date), by = "month"))

monthlyTemperature <- left_join(full_grid, monthlyTemperature) %>% tbl_df()

monthlyTemperature$site <- factor(monthlyTemperature$site, levels=c("Skj", "Gud", "Lav", "Ulv", "Ves", "Ram", "Hog", "Alr", "Ovs", "Arh", "Vik", "Fau"))

monthlyTemperature <- monthlyTemperature %>% 
  mutate(site = factor(site, levels = c("Skjellingahaugen", "Gudmedalen", "Lavisdalen", "Ulvehaugen", "Veskre", "Rambera", "Hogsete", "Alrust", "Ovstedalen", "Arhelleren", "Vikesland", "Fauske")))

save(monthlyTemperature, file = "Monthly.Temperature_2008-2017.RData")

monthlyTemperature %>% 
  filter(logger == "tempabove") %>% 
  ggplot(aes(x = date, y = value, color = site)) +
  geom_line() +
  scale_colour_brewer(palette="Paired") +
  facet_wrap(~ site) + 
  ggtitle("Monthly temperature")


# Rbind gridded and Seedclim data
monthly <- rbind(monthly.temp, monthlyTemperature)
monthly$site <- factor(monthly$site, levels=c("Skj", "Gud", "Lav", "Ulv", "Ves", "Ram", "Hog", "Alr", "Ovs", "Arh", "Vik", "Fau"))


monthly %>%
  filter(site == "Skj", logger %in% c("temp30cm", "gridded")) %>%
  ggplot() + geom_line(aes(x = date, y = value, group = logger, color = logger))


monthly %>%
  filter(logger %in% c("temp30cm", "gridded")) %>%
  #filter(value < 40) %>% # should be able to take this out with the threshold thing above!!!
  ggplot(aes(x = date, y = value, color = logger, size = logger)) +
    geom_line() +
    scale_color_manual(values = c("darkgray", "lightblue")) +
    scale_size_manual(values = c(3,1)) +
    #scale_colour_brewer(type = "qual", palette="Paired") +
    facet_wrap(~site) +
    xlab("") + ylab("Monthly temperature in °C")

monthly %>%
  filter(logger %in% c("temp200cm", "temp30cm", "tempsoil", "tempabove")) %>%
  filter(value < 40) %>%
  ggplot(aes(x = date, y = value, color = logger)) +
  geom_line() +
  #scale_color_manual(values = c("blue", "lightblue")) +
  facet_wrap(~site) +
  xlab("") + ylab("Monthly temperature in °C")




#### Calculate daily means ####
dailyTemperature <- temperature2 %>%
  filter(!is.na(value)) %>%
  mutate(date = dmy(format(date, "%d.%b.%Y"))) %>%
  group_by(date, logger, site) %>%
  summarise(n = n(), value = mean(value), sum = sum(value)) %>%
  #filter(n > threshold) %>%
  select(-n, -sum)


#fill missing months with NA y merging with complete dataset
filler <- expand.grid(
  site = unique(dailyTemperature$site),
  logger = unique(dailyTemperature$logger),
  date = seq(
    min(dailyTemperature$date),
    max(dailyTemperature$date),
    by = "day"
  )
)
dailyTemperature <- merge(dailyTemperature, filler, all = TRUE)

dailyTemperature <- dailyTemperature %>% 
  mutate(site = factor(site, levels = c("Skjellingahaugen", "Gudmedalen", "Lavisdalen", "Ulvehaugen", "Veskre", "Rambera", "Hogsete", "Alrust", "Ovstedalen", "Arhelleren", "Vikesland", "Fauske")))

save(dailyTemperature, file = "Daily.Temperature_2008-2017.RData")


# Rbind gridded and Seedclim data
daily <- rbind(daily.temp, dailyTemperature)
daily$site <- factor(daily$site, levels=c("Skj", "Gud", "Lav", "Ulv", "Ves", "Ram", "Hog", "Alr", "Ovs", "Arh", "Vik", "Fau"))



daily %>%
  filter(logger %in% c("temp200cm", "gridded")) %>%
  #filter(value < 40) %>% # should be able to take this out with the threshold thing above!!!
  ggplot(aes(x = date, y = value, color = logger, size = logger)) +
  geom_line() +
  scale_color_manual(values = c("darkgray", "lightblue")) +
  scale_size_manual(values = c(3,1)) +
  #scale_colour_brewer(type = "qual", palette="Paired") +
  facet_wrap(~site) +
  xlab("") + ylab("Daily temperature in °C")

plot_gridded_temp(data = daily, start_date = "2014.1.1", end_date = "2015.12.31", SITE = c("Skj", "Gud", "Lav", "Ves", "Ram", "Hog"), log = c("tempabove", "gridded"), inc = TRUE, breaks = "month")

temperature %>% 
  filter(date > as.POSIXct(ymd("2013.09.23")), date < as.POSIXct(ymd("2016.10.05"))) %>%
  filter(site %in% c("Skj")) %>% 
  filter (logger == "temp200cm") %>% 
  ggplot(aes(x = date, y = value)) + 
  geom_line() +
  facet_wrap(~site)

# Calculate mean annual temp
dailyTemperature %>% 
  filter(logger == "temp200cm") %>% 
  group_by(site, year(date)) %>% 
  summarise(mean = mean(value))

