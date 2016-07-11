# MONTHLY AND DAILY TEMPERATURE

library("tidyr")
library("dplyr")
library("ggplot2")

#### Calculate monthly means ####
threshold <- 30 * 24 * 6 / 4 #~ one week. Minimum accepted

mtemp <- temperature %>%
  filter(!is.na(value)) %>%
  mutate(month = dmy(paste0("15-",format(date, "%b.%Y")))) %>%
  group_by(site, month, logger) %>%
  summarise(mean = mean(value), sum = sum(value)) %>%
  #filter(n > threshold) %>%
  #select(-mean, -sum)


monthlyClimate <- mklima %>%
  select(-n, -month) %>%  
  spread(key = variable, value = value)  


save(klima, file = "climate.Rdata")
save(monthlyClimate, mklima, file = "monthlyclimate.Rdata")


#### Calculate monthly means per site and save ####
temperature$my <- format(temperature$date, "%m%y") # reformat month and year
monthly <- aggregate(value ~ my+logger+site, temperature, mean)
monthly$my <- paste(15, monthly$my, sep="") # add a day
monthly$date <- dmy(monthly$my) # back to date format
monthly.temperature <- monthly[order(monthly$logger, monthly$site, monthly$date),]
monthly.temperature <- monthly.temperature[,c(5,2,3,4)]
save(monthly.temperature, file = "Monthly.Temperature_2008-2016.RData")


#### Calculate daily means per site and save ####
temperature$dmy <- format(temperature$date, "%d%m%y") # reformat day, month and year
daily <- aggregate(value ~ dmy+logger, temperature, mean)
daily$date <- dmy(daily$dmy)
daily.temperature <- daily[order(daily$logger, daily$date),]
daily.temperature <- daily.temperature[,c(4,2,3)]
colnames(daily.temperature) <- c("date", "site", "value")
save(daily.temperature, file = "Daily.Temperature_2008-2016.RData")

# gridded data
monthly.temp$value <- monthly.temp$temperature
monthly.temp$logger <- "gridded"
monthly.temp$site <- substring(monthly.temp$site, 1,3)
monthly.temp <- monthly.temp[,c(1,5,2,4)]
monthly <- rbind(monthly.temp, monthly.temperature)

monthly %>% 
    filter(site == "Alr", logger == "temp200cm", logger == "gridded") %>%
    ggplot() + geom_line(aes(x = date, y = value, group = logger))

ggplot() + 
  geom_line(data = mt, aes(x = date, y = temperature), color = "gray") +
  geom_line(data = mt, aes(x = date, y = value)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0)) +
  ggtitle(label = "Alr")
