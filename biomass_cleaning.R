#### #### #### #### #### 
#### biomass #### 
bio <- read.csv(file = "/Volumes/fja062/PhD/Data/biomass/Removal_Biomass_151617.csv", sep = ";")
bio15 <- read_excel("/Volumes/fja062/PhD/Data/biomass/biomass_2015.xlsx")

bio15 <- bio15 %>% 
  mutate(siteID = plyr::mapvalues(Site, from = dict_Site$v2, to = dict_Site$new)) %>% 
  select(-Site)



biomass <- bio %>%
  mutate(siteID = plyr::mapvalues(Site, from = dict_Site$old, to = dict_Site$new)) %>% 
  bind_rows(bio15) %>% 
  group_by(Year, siteID, Treatment, Func_group, Block) %>% 
  mutate(biomass_sum = sum(Biomass_g)) %>%
  group_by(Year, siteID, Treatment, Func_group) %>% 
  mutate(biomass_mean = mean(biomass_sum)) %>% 
  mutate(Block = as.character(Block)) %>% 
  select(-Site, -Date) %>% 
  ungroup() %>% 
  mutate(Temperature_level = if_else(siteID %in% c("Lavisdalen", "Gudmedalen", "Skjellingahaugen", "Ulvhaugen"), "6.5",
                                     if_else(siteID %in% c("Alrust", "Hogsete", "Rambera", "Veskre"), "8.5", "10.5"))) %>% 
  mutate(Precipitation_level = if_else(siteID %in% c("Alrust", "Fauske", "Ulvhaugen"), "0.6",
                                       if_else(siteID %in% c("Lavisdalen","Hogsete", "Vikesland"), "1.2",
                                               ifelse(siteID %in% c("Rambera", "Gudmedalen", "Arhelleren"), "2.0", "2.7")))) %>%
  mutate(Precipitation_level = as.numeric(Precipitation_level), Temperature_level = as.numeric(Temperature_level)) %>% 
  distinct(Year, Block, Treatment, Func_group, .keep_all = TRUE)



biomass %>% 
  filter(Year == 2015) %>%
  #filter(Treatment %in% c("C", "FGB", "F", "G", "B")) %>%
  ggplot(aes(x = Treatment, y = Biomass_g, fill = Func_group)) +
  geom_col() +
  facet_grid(Precipitation_level ~ Temperature_level) +
  #scale_fill_manual(values = cbPalette[c(3, 8, 4)], labels = c("Bryophyte", "Forb", "Graminoid"), name = "Removal") +
  labs(y = "Removed biomass (g)") +
  theme_classic() +
  axis.dim 
ggsave(filename = "removed_biomass_2016.jpg", path = "/Users/fja062/Documents/seedclimComm/figures/", dpi = 300, width = 6.5, height = 6.5)

