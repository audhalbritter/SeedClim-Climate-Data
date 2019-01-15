library(lme4)
library(tidyverse)
library(broom)
library(lubridate)

source("/Volumes/fja062/PhD/Projects/2017_temperature_regulation_of_functional_groups/SeedClim-Climate-Data/plotting_dim.R")

load("~/OneDrive - University of Bergen/Research/FunCaB/SeedClim-Climate-Data/ibuttons/maxmin.RData")
#Q1 – does removal of functional groups affect soil temperature?
  #And if so, is one more important than another?
#a. treatment as predictor


modTreat1 <- maxmin %>% 
  mutate(Year = year(date),
         Treatment = recode(Treatment, "FGB" = "aFGB")) %>% 
  #filter(!Treatment == "FGB", !weather == "variable") %>% 
  #filter(Year==2015) %>% 
  #filter(between(date, ymd("2015-08-01"), ymd("2015-09-30"))) %>%
  #filter(weather %in% c("sunny", "cloudy")) %>% 
  distinct(siteID, turfID, Treatment, Temp, Precip, maxTemp, sunniness, weather, Block) %>%
  na.omit() %>% 
  #group_by(weather) %>% 
  do({
    mod <- lmer(maxTemp ~ Treatment*scale(Temp) + Treatment*sunniness + (1|siteID/Block), REML = FALSE, data = .)
    tidy(mod)}) %>%  
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>%
  as.data.frame()

vegCompMaxmod1 <- modTreat1 %>% 
  filter(!term %in% c("(Intercept)", "scale(Temp)", "sunniness")) %>% 
  filter(!grepl("^sd_", term)) %>% 
  #filter(!term == "scale(Temperature_level") %>% 
  #filter(!grepl("_|sunn", term)) %>% 
  mutate(term = gsub("scale\\(|\\)", "", term),
         term = gsub("Treatment", "", term),
         term = gsub("^C", "Control", term)) %>% 
  mutate(term2 = if_else(grepl("sunniness:", term), "PFG x UV",
                         if_else(grepl(":sunniness", term), "PFG x UV",
                                 if_else(grepl(":Temp", term), "PFG x t", "PFG"))))


vegCompmod1plot  <- vegCompMaxmod1 %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper, fill = term2, shape = term2)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_shape_manual(legend.title.climate, values = c(21,24, 23)) + #, labels = c("0.6","2.7","6.5","10.5")
  scale_fill_manual(legend.title.climate, values = c("grey60", "white", "black")) + #guide = guide_legend(reverse=TRUE)
  scale_x_discrete(limits = c("Control:sunniness", "Control:Temp", "Control", "B:sunniness", "B:Temp", "B", "F:sunniness", "F:Temp", "F", 'G:sunniness', "G:Temp", "G", "FB:sunniness", "FB:Temp", "FB", "GB:sunniness", "GB:Temp", "GB",  "GF:sunniness", "GF:Temp", "GF"), labels = c("","C", "", '', "GF", "", "", "GB", "", "", "FB", '', "", "G", "", "", "F", "", "", "B", "")) +
  geom_vline(xintercept =  c(3.5,6.5,9.5,12.5,15.5, 18.5), colour = "grey90") +
  coord_flip() +
  axis.dimLarge +
  labs(y = "standardised coefficients") +
  theme(axis.title.y = element_blank())

ggsave(vegCompmod1plot, file = "~/Documents/seedclimComm/figures/vegCompmodT3plotFINAL.jpg", dpi = 300, width = 6, height = 4)


vegCompMaxmod1 <- modTreat1 %>% 
  filter(!term == "(Intercept)",
         !term == "sunniness",
         !term == "scale(Temp)") %>% 
  filter(!grepl("^sd_", term)) %>% 
  # %>% 
  #filter(!grepl("_|sunn", term)) %>% 
  mutate(term = gsub("scale\\(|\\)", "", term),
         term = gsub("Treatment", "", term),
         term = gsub("^C", "Control", term))


vegCompmod1plotZOOM  <- vegCompMaxmod1 %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper, fill = term2, colour = term2)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_shape_manual(legend.TOD, values = c(21,24)) + #, labels = c("0.6","2.7","6.5","10.5")
  scale_fill_manual(legend.TOD, values = c("grey90", "white", "black")) + #guide = guide_legend(reverse=TRUE)
  scale_x_discrete(limits = c("Control:Temp","B:Temp", "F:Temp", "G:Temp", "FB:Temp", "GB:Temp", "GF:Temp", "Control:sunniness", "B:sunniness", "F:sunniness", 'G:sunniness', "FB:sunniness", "GB:sunniness", "GF:sunniness"), labels = c("Control","GF", "GB", 'FB', "G", "F", "B", "control x UV", "GF x UV", "GB x UV", 'FB x UV', "G x UV", "F x UV", "B x UV")) +
  geom_vline(xintercept =  c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5,12.5,13.5,14.5), colour = "grey90") +
  coord_flip() +
  axis.dimLarge +
  geom_vline(xintercept =  7.5, colour = "grey60", linetype = "dashed") +
  labs(y = "standardised coefficients") +
  theme(axis.title.y = element_blank())

ggsave(vegCompmod1plotZOOM, file = "~/Documents/seedclimComm/figures/vegCompmodT3plotZOOM.jpg", dpi = 300, width = 6, height = 4)

#use raw data, with bare ground as intercept
#use actual climate temperature and precipitations instead of level
# treatment*sunniness + treatment*temp

# how many sunny vs cloudy days are there?


#Q2 – if it seems that none is particularly more important than the others, is it purely a percent cover effect? Ie, removing x% of any functional group has the same effect, regardless of the functional group?

#b. with cover as predictor
modCover <- maxmin %>% 
  mutate(Year = year(date),
         Treatment = recode(Treatment, "FGB" = "aFGB")) %>% 
  distinct(graminoidCov, forbCov, bryophyteCov, siteID, turfID, Treatment, Temp, maxTemp, sunniness, Precip, Block) %>%
  na.omit() %>% 
  do({
    mod <- lmer(maxTemp ~ graminoidCov*sunniness + forbCov*sunniness + bryophyteCov*sunniness + graminoidCov*scale(Temp) + forbCov*scale(Temp) + bryophyteCov*scale(Temp) + (1|siteID/Block), REML = FALSE, data = .)
    tidy(mod)}) %>%  
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>%
  as.data.frame()

vegCompMaxmod1C <- modCover %>% 
  filter(!term == "(Intercept)") %>% 
  filter(!grepl("^sd_", term),
         climate == "Temp",
         !term == "scale(climVal)",
         !term == "sunniness") %>% 
  #filter(!grepl("_|sunn", term)) %>% 
  mutate(term = gsub("scale\\(|\\)", "", term),
         #term = gsub("graminoid", "graminoid height", term),
         #term = gsub("forb", "forb height", term),
         term = gsub("mossHeight", "moss height", term),
         term = gsub("bryophyteCov", "moss cover", term)) %>% 
  mutate(test = if_else(
    str_count(term, ":") == 2, "x",
    if_else(str_count(term, ":") ==1, "y", "z")
  )) %>%
  mutate(term2 = if_else(grepl("sunniness:", term), "PFG x UV",
                         if_else(grepl(":sunniness", term), "PFG x UV",
    if_else(grepl(":climVal", term), "PFG x t", "PFG")))) %>% 
  arrange(desc(test, .by_group = TRUE))

vegCompmod1plotCALL <- vegCompMaxmod1C %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper, fill = term2, shape = term2)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_fill_manual(legend.title.climate, values = c("grey60", "white", "black")) +
  scale_shape_manual(legend.title.climate, values = c(21,24,23)) +
  scale_x_discrete(limits = c("sunniness:moss cover", "moss cover:climVal", "moss cover", "sunniness:forbCov", "forbCov:climVal", "forbCov", "graminoidCov:sunniness", "graminoidCov:climVal", "graminoidCov"), labels = c("sunniness:moss cover" = "", "sunniness:forbCov" = "", "graminoidCov:sunniness" = "", "moss cover:climVal" = "bryophyte", "forbCov:climVal" = "forb", "graminoidCov:climVal" = "graminoid", "moss cover" = "", "forbCov" = "", "graminoidCov" = "")) +
  #geom_vline(xintercept =  c(1.5, 2.5, 4.5, 5.5), colour = "grey90") +
  geom_vline(xintercept =  c(3.5, 6.5), colour = "grey90") +
  coord_flip() +
  axis.dimLarge +
  labs(y = "standardised coefficients") +
  theme(axis.title.y = element_blank())

ggsave(vegCompmod1plotCALL, file = "~/Documents/seedclimComm/figures/vegCompmodC2plotFINAL.jpg", dpi = 300, width = 6, height = 4)


vegCompMaxmod1 <- modCover %>% 
  filter(!term == "(Intercept)",
         !term == "sunniness",
         !term == "scale(Temp)") %>% 
  filter(!grepl("^sd_", term)) %>% 
  #filter(!term == "scale(Temperature_level") %>% 
  #filter(!grepl("_|sunn", term)) %>% 
  mutate(term = gsub("scale\\(|\\)", "", term),
         #term = gsub("graminoid", "graminoid height", term),
         #term = gsub("forb", "forb height", term),
         term = gsub("mossHeight", "moss height", term),
         term = gsub("bryophyteCov", "moss cover", term)) %>% 
  mutate(test = if_else(
    str_count(term, ":") == 2, "x",
    if_else(str_count(term, ":") ==1, "y", "z")
  )) %>%  
  arrange(desc(test, .by_group = TRUE))

vegCompmod1plotCZOOM <- vegCompMaxmod1 %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_shape_manual(legend.TOD, values = c(21,24)) + #, labels = c("0.6","2.7","6.5","10.5")
  scale_fill_manual(legend.TOD, values = c("grey90", "black")) + #guide = guide_legend(reverse=TRUE)
  scale_x_discrete(limits = c("sunniness:moss cover", "sunniness:forbCov", "graminoidCov:sunniness","moss cover:Temp", "forbCov:Temp", "graminoidCov:Temp"), labels = c("sunniness:moss cover" = "bryophyte x UV", "sunniness:forbCov" = "forb x UV", "graminoidCov:sunniness" = "graminoid x UV", "moss cover:Temp" = "bryophyte", "forbCov:Temp" = "forb", "graminoidCov:Temp" = "graminoid")) +
  geom_vline(xintercept =  c(1.5, 2.5, 4.5, 5.5), colour = "grey90") +
  geom_vline(xintercept =  3.5, colour = "grey60", linetype = "dashed") +
  coord_flip() +
  axis.dimLarge +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())

ggsave(vegCompmod1plotCZOOM, file = "~/Documents/seedclimComm/figures/vegCompmodC2plotZOOM.jpg", dpi = 300, width = 6, height = 4)

#c. height as predictor
modHeight <- maxmin %>% 
  mutate(Year = year(date),
         Treatment = recode(Treatment, "FGB" = "aFGB")) %>% 
  distinct(vegetationHeight, mossHeight, siteID, turfID, Treatment, Temp, maxTemp, sunniness, Precip, Block) %>%
  na.omit() %>% 
  do({
    mod <- lmer(maxTemp ~ vegetationHeight*sunniness + mossHeight*sunniness + vegetationHeight*scale(Temp) + mossHeight*scale(Temp) + (1|siteID/Block), REML = FALSE, data = .)
    tidy(mod)}) %>%  
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>%
  as.data.frame()

vegCompMaxmod1H <- modHeight %>% 
  filter(!term == "(Intercept)", 
        !term == "scale(climVal)",
        !term == "sunniness") %>% 
  filter(!grepl("^sd_", term),
         climate == "Temp") %>% 
  mutate(term = gsub("scale\\(|\\)", "", term),
         term = gsub("mossHeight", "moss height", term)) %>% 
  mutate(test = if_else(
    str_count(term, ":") == 2, "x",
    if_else(str_count(term, ":") ==1, "y", "z")
  )) %>%  
  mutate(term2 = if_else(grepl("sunniness:", term), "PFG x UV",
                         if_else(grepl(":sunniness", term), "PFG x UV",
                                 if_else(grepl(":climVal", term), "PFG x t", "PFG")))) %>% 
  arrange(desc(test, .by_group = TRUE))

vegCompmod1plotHALL <- vegCompMaxmod1H %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper, fill = term2, shape = term2)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_fill_manual(legend.title.climate, values = c("grey60", "white", "black")) +
  scale_shape_manual(legend.title.climate, values = c(21,24,23)) +
  scale_x_discrete(limits = c("sunniness:moss height", "moss height:climVal", "moss height", "vegetationHeight:sunniness", "vegetationHeight:climVal", "vegetationHeight"), labels = c("sunniness:moss height" = "", "vegetationHeight:sunniness" = "", "moss height:climVal" = "non-vascular", "vegetationHeight:climVal" = "vascular", "moss height" = "", "vegetationHeight" = "")) +
  #geom_vline(xintercept =  c(1.5,3.5, 5.5, 6.5), colour = "grey90") +
  geom_vline(xintercept =  3.5, colour = "grey90") +
  coord_flip() +
  axis.dimLarge +
  theme(axis.title.y = element_blank()) +
  labs(y = "standardised coefficients")

ggsave(vegCompmod1plotHALL, file = "~/Documents/seedclimComm/figures/vegCompmodH2plotFINAL.jpg", dpi = 300, width = 6, height = 4)

vegCompMaxmod1H <- modHeight %>% 
  filter(!term == "(Intercept)",
         !term == "scale(Temp)",
         !term == "sunniness") %>% 
  filter(!grepl("^sd_", term)) %>% 
  mutate(term = gsub("scale\\(|\\)", "", term),
         term = gsub("mossHeight", "moss height", term)) %>% 
  mutate(test = if_else(
    str_count(term, ":") == 2, "x",
    if_else(str_count(term, ":") ==1, "y", "z")
  )) %>%  
  arrange(desc(test, .by_group = TRUE))

vegCompmod1plotHZOOM <- vegCompMaxmod1H %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_shape_manual(legend.TOD, values = c(21,24)) + #, labels = c("0.6","2.7","6.5","10.5")
  scale_fill_manual(legend.TOD, values = c("grey90", "black")) + #guide = guide_legend(reverse=TRUE)
  scale_x_discrete(limits = c("sunniness:moss height", "vegetationHeight:sunniness", "moss height:Temp", "vegetationHeight:Temp"), labels = c("sunniness:moss height" = "bryophyte x UV", "vegetationHeight:sunniness" = "vascular x UV", "moss height:Temp" = "bryophyte", "vegetationHeight:Temp" = "vascular")) +
  geom_vline(xintercept =  c(1.5,3.5, 5.5, 6.5), colour = "grey90") +
  geom_vline(xintercept =  2.5, colour = "grey60", linetype = "dashed") +
  coord_flip() +
  axis.dimLarge +
  theme(axis.title.y = element_blank()) +
  labs(y = "standardised coefficients")

ggsave(vegCompmod1plotHZOOM, file = "~/Documents/seedclimComm/figures/vegCompmodH2plotZOOM.jpg", dpi = 300, width = 6, height = 4)

#########################################
######## TEMPERTAURE ANOMALIES ##########

maxminAnom <- maxmin %>% 
  left_join(maxmin %>% filter(Treatment == "FGB") %>% ungroup() %>% select(FGBmaxTemp = maxTemp, date, siteID, Block)) %>%
  mutate(maxAnom = maxTemp - FGBmaxTemp) %>% 
  ungroup()

#treatment
maxAnomPlot <- maxminAnom %>% 
  filter(!between(date, ymd("2015-09-01"), ymd("2016-05-31"))) %>%
  mutate(year = year(date)) %>% 
  filter(weather %in% c("cloudy", "sunny")) %>% #, !Treatment %in% c("FGB", "B", "G", "F")
  distinct(siteID, turfID, Treatment, Temp, Precip, maxAnom, sunniness, weather, Block) %>%
  ggplot(aes(x = Treatment, y = maxAnom, colour = Treatment, fill = Treatment)) +
  geom_boxplot(alpha = 0.8) +
  #geom_bar(stat = "summary", alpha = 0.8) +
  scale_x_discrete(limits = c("GF", "GB", "FB", "G", "F", "B", "C"), labels = c("B", "F", "G", "FB", "GB", "GF", "FGB")) +
  scale_colour_manual("Functional groups",values = cbPalette[c(7,5,6,2,4,10,1)], limits = c("GF", "GB", "FB", "G", "F", "B", "C"), labels = c("Bryophytes", "Forbs","Graminoids", "Bryophytes and forbs", "Graminoids and bryophytes", "Forbs and graminoids", "Bryophytes, forbs \nand graminoids")) +
  scale_fill_manual("Functional groups", values = cbPalette[c(7,5,6,2,4,10,1)], limits = c("GF", "GB", "FB", "G", "F", "B", "C"), labels = c("Bryophytes", "Forbs","Graminoids", "Bryophytes and forbs", "Graminoids and bryophytes", "Forbs and graminoids", "Bryophytes, forbs \nand graminoids")) +
  facet_grid(. ~ weather) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 3.5, linetype = "dashed") +
  labs(y = "temperature anomaly from bare ground") +
  theme(axis.title.x = element_blank())

ggsave(maxAnomPlot, file = "~/Documents/seedclimComm/figures/maxAnomPlot1.jpg", dpi = 300, width = 10, height = 4)



#cover
coverPlot1 <- maxminAnom %>% 
  #filter(between(date, ymd("2015-08-01"), ymd("2015-09-30"))) %>%
  filter(!is.na(bryophyteCov)) %>% 
  gather(key = response, value = value, graminoidCov, vegetationHeight, forbCov, mossHeight, bryophyteCov) %>% 
  mutate(response = factor(response, levels = c("graminoidCov", "forbCov", "bryophyteCov", "vegetationHeight", "mossHeight"))) %>% 
  filter(response %in% c("graminoidCov", "forbCov", "bryophyteCov")) %>% 
  ggplot(aes(x = value, y = maxAnom, colour = sunniness, fill = sunniness)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_point() +
  #stat_summary(geom = "point", fun.y = "mean", alpha = 0.7) +
  #scale_colour_manual(values = c("black", "grey60")) +
  #scale_fill_manual(values = c("black", "grey60")) +
  #facet_wrap( ~ Temperature_level, scales = "free_x") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "temperature anomaly from bare ground",
       x = "sunniness") +
  facet_grid(.~response)

#ggsave(coverPlot1, file = "~/Documents/seedclimComm/figures/coverPlot2.jpg", dpi = 300, width = 9, height = 5)

heightPlot1 <- maxminAnom %>% 
  #filter(between(date, ymd("2015-08-01"), ymd("2015-09-30"))) %>%
  filter(!Treatment == "FGB") %>% 
  gather(key = response, value = value, bryophyteCov, graminoidCov, vegetationHeight) %>% 
  #filter(response %in% c("bryophyteCov"), value < 580) %>% 
  ggplot(aes(x = value, y = maxTemp, colour = Treatment, fill = Treatment, group = Treatment)) +
  geom_smooth(method = "lm", se = TRUE) +
  stat_summary(geom = "point", fun.y = "mean", alpha = 0.7) +
  scale_colour_manual("", values = cbPalette) +
  scale_fill_manual("", values = cbPalette) +
  facet_wrap(response ~ weather, scales = "free_x") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("t") +
  xlab("vegetation height (mm)") +
  theme(axis.title.y = element_text(colour = "white"))

#ggsave(heightPlot1, file = "~/Documents/seedclimComm/figures/heightPlot1.jpg", dpi = 300, width = 6, height = 4)
legend <- get_legend(heightPlot1)

xplot <- plot_grid(vegCompmod1plotC, coverPlot1 + theme(legend.position = "none"), vegCompmod1plotH, heightPlot1  + theme(legend.position = "none"), labels = c("A", "B", "C", "D"), rel_widths = c(0.83, 0.95))


coverPlot1grid <- plot_grid(xplot, legend, rel_widths = c(3, 0.3))

ggsave(coverPlot1grid, file = "~/Documents/seedclimComm/figures/responsePlotGrid1Temp.jpg", dpi = 300, width = 9, height = 7)

# sum of degrees above 0 in August, September and October
DDsum <- vegComp %>% 
  distinct(turfID, date, TOD, Value, .keep_all = TRUE) %>% 
  filter(between(date, ymd("2015-08-01"), ymd("2015-12-31"))) %>%
  mutate(month = month(date)) %>% 
  filter(Value > 5) %>% 
  group_by(siteID, month, blockID, Treatment, Temperature_level) %>% 
  summarise(tempSum = sum(Value))

frostDayMod <- vegComp %>%
  distinct(turfID, date, TOD, Value, .keep_all = TRUE) %>% 
  filter(between(date, ymd("2015-07-15"), ymd("2016-06-01"))) %>%
  #mutate(Treatment = factor(Treatment, levels = c("C", "FB", "GB", "GF", "FGB"))) %>% 
  group_by(date, siteID, turfID, Treatment, Block, sunniness, Temperature_level, Precipitation_level, litter, graminoidCov, forbCov, bryophyteCov, graminoid, forb, mossHeight, vegetationHeight) %>% 
  summarise(maxTemp = max(Value),
            minTemp = min(Value)) %>% 
  mutate(x = minTemp < 0) %>%
  distinct(turfID, date, .keep_all = TRUE) %>% 
  arrange(date) %>% 
  group_by(Treatment, siteID, turfID) %>% 
  mutate(sum = cumsum(x), n = n()) %>%
  mutate(Temp = recode(siteID, Ulvhaugen=6.17, Lavisdalen =6.45, Gudmedalen =5.87, Skjellingahaugen =6.58, Alrust =9.14, Hogsete =9.17, Rambera =8.77, Veskre =8.67, Fauske =10.3, Vikesland =10.55, Arhelleren =10.60, Ovstedal=10.78),
         Precip= recode(siteID, Ulvhaugen =596, Lavisdalen =1321, Gudmedalen =1925, Skjellingahaugen =2725, Alrust =789, Hogsete =1356, Rambera =1848, Veskre =3029, Fauske =600, Vikesland =1161, Arhelleren =2044, Ovstedal=2923)) %>% 
  filter(!turfID == "Lav3GF") %>% 
  filter(date == ymd("2016-05-01"))

frostDayMod <- frostDayMod %>% 
  group_by(turfID, Treatment, Block, siteID) %>%
  left_join(frostDayMod %>% filter(Treatment == "FGB") %>% ungroup() %>% select(FGBsum = sum, date, siteID, Block)) %>%
  mutate(sumAnom = sum - FGBsum) %>% 
  ungroup() %>% 
  filter(date == "2016-05-30")

x <- frostDayMod %>% 
  filter(!Treatment == "temp200cm", siteID == "Lavisdalen") %>% #work in progress
  ungroup() %>% 
  ggplot(aes(x = date, y = sum, colour = Treatment)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "line", size = 0.8) +
  #geom_point(size = 0.8) +
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

frostDayMod1T <- frostDayMod %>% 
  ungroup() %>% 
  mutate(Treatment = recode(Treatment, "FGB" = "aFGB")) %>% 
  do({
    mod <- lmer(sum ~ Treatment*scale(Precip) + (1|siteID/Block), REML = FALSE, data = .)
    tidy(mod)}) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

frostDayMod1Tt <- frostDayMod1T %>% 
  filter(!term == "(Intercept)") %>% 
  filter(!grepl("^sd_", term),
         !term == "scale(climVal)",
         !term == "sunniness") %>% 
  #filter(!grepl("_|sunn", term)) %>% 
  mutate(term = gsub("scale\\(|\\)", "", term),
         #term = gsub("graminoid", "graminoid height", term),
         #term = gsub("forb", "forb height", term),
         term = gsub("mossHeight", "moss height", term),
         term = gsub("bryophyteCov", "moss cover", term)) %>% 
  mutate(term2 = if_else(grepl("Precip:", term), "PFG x t",
                         if_else(grepl(":Precip", term), "PFG x t", "PFG")))

frostDayMod1H <- frostDayMod %>% 
  ungroup() %>% 
  do({
    mod <- lmer(maxTemp ~ vegetationHeight*scale(Precip) + mossHeight*scale(Precip) + (1|siteID/Block), REML = FALSE, data = .)
    tidy(mod)}) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))


frostDayMod1Ht <- frostDayMod1H %>% 
  filter(!term == "(Intercept)") %>% 
  filter(!grepl("^sd_", term),
         !term == "scale(climVal)",
         !term == "sunniness") %>% 
  #filter(!grepl("_|sunn", term)) %>% 
  mutate(term = gsub("scale\\(|\\)", "", term),
         #term = gsub("graminoid", "graminoid height", term),
         #term = gsub("forb", "forb height", term),
         term = gsub("mossHeight", "moss height", term),
         term = gsub("bryophyteCov", "moss cover", term)) %>% 
  mutate(term2 = if_else(grepl("Precip:", term), "PFG x t",
                         if_else(grepl(":Precip", term), "PFG x t", "PFG")))
  
frostDayMod1C <- frostDayMod %>% 
  ungroup() %>% 
  do({
    mod <- lmer(maxTemp ~ bryophyteCov*scale(Temp) + forbCov*scale(Temp) + graminoidCov*scale(Temp) + (1|siteID/Block), REML = FALSE, data = .)
    tidy(mod)}) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

frostDayMod1Ct <- frostDayMod1C %>% 
  filter(!term == "(Intercept)") %>% 
  filter(!grepl("^sd_", term),
         !term == "scale(climVal)",
         !term == "sunniness") %>% 
  #filter(!grepl("_|sunn", term)) %>% 
  mutate(term = gsub("scale\\(|\\)", "", term),
         #term = gsub("graminoid", "graminoid height", term),
         #term = gsub("forb", "forb height", term),
         term = gsub("mossHeight", "moss height", term),
         term = gsub("bryophyteCov", "moss cover", term)) %>% 
  mutate(term2 = if_else(grepl("Temp:", term), "PFG x t",
                         if_else(grepl(":Temp", term), "PFG x t", "PFG")))



frostDayModplot <- frostDayMod1C %>% 
  filter(!term %in% c("(Intercept)", "scale(Temp)"), !(grepl("sd", term))) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = term, y = estimate, ymin = lower, ymax = upper)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_vline(xintercept =  c(1.5,2.5,3.5,4.5,5.5,6.5), colour = "grey90") +
  coord_flip() +
  axis.dimLarge

ggsave(frostDayModplot, filename = "~/Documents/seedclimComm/figures/frostDayMod2Cover.jpg", dpi = 300, width = 6, height = 4)

