#### #### #### #### #### 
#### biomass #### 
# load packages
library(broom)

# read in biomass files
bio15 <- read_excel("~/OneDrive - University of Bergen/Research/FunCaB/Data/veg_biomass/biomass_removals_2015.xlsx")

biomass <- bio15 %>%
  filter(!Treatment %in% c("RTC", "RTC2nd")) %>% 
  left_join(dict_Site, by = c("Site" = "old")) %>% 
  select(-c(Site, Round, Year, v2, v3), "siteID" = new, "blockID" = Block, "turfID" = TurfID, "functionalGroup" = Func_group)

# vegetation data
comp2 <- comp2 %>% 
  filter(!is.na(Treatment), !Treatment == "XC")

# FIX THIS!!!
# set covers and heights to zero in removed plots
comp2 <- comp2 %>% 
  mutate(mossCov = if_else(grepl("B", Treatment) & Year > 2015, 0, mossCov),
         forbCov = if_else(grepl("F", Treatment) & Year > 2015, 0, forbCov),
         graminoidCov = if_else(grepl("G", Treatment) & Year > 2015, 0, graminoidCov),
         vegetationHeight = if_else(Treatment == "FGB" & Year > 2015, 0, vegetationHeight),
         mossHeight = if_else(Treatment == "FGB" & Year > 2015, 0, mossHeight))

composition2015 <- comp2 %>% 
  filter(Year == 2015) %>% 
  left_join(mossHeight, by = "turfID", suffix = c("", ".new")) %>% 
  mutate(mossHeight = if_else(is.na(mossHeight), mossHeight.new, mossHeight),
         blockID = as.character(blockID)) %>%
  distinct(siteID, Treatment, turfID, blockID, vegetationHeight, mossHeight, litter, mossCov, forbCov, graminoidCov) %>% 
  ungroup()

composition2015 <- composition2015 %>%
  group_by(siteID, blockID) %>% 
  mutate(mossHeight = mean(mossHeight, na.rm = TRUE))

# gather vegetation into columns for join with biomass
biomassComp <- composition2015 %>% 
  gather(graminoidCov, forbCov, mossCov, key = "functionalGroup", value = "covValue") %>% 
  mutate(functionalGroup = recode(functionalGroup, "mossCov" = "bryophyte", "graminoidCov" = "graminoid", "forbCov" = "forb"))
  
# align naming conventions among vegetation and biomass datasets
# join biomass data to composition data
biomassReg <- biomass %>% 
  mutate(blockID = as.character(if_else(siteID == "Gudmedalen", recode(blockID, "1" = 5, "2" = 12, "3" = 13, "4" = 15), blockID)),
         turfID = if_else(siteID == "Gudmedalen", paste0(substr(siteID, 1, 3), blockID, Treatment), turfID),
         turfID = recode(turfID, "Alr4FGB" = "Alr5C"),
         functionalGroup = recode(functionalGroup, "B" = "bryophyte", "G" = "graminoid", "F" = "forb")) %>% 
  left_join(composition2015)

# turn biomass from g to g/m^2
biomassReg <- biomassReg %>% 
  mutate(Biomass_gm = Biomass_g/0.0625) %>% 
  spread(key = functionalGroup, value = Biomass_gm) %>% 
  select(-Biomass_g, -litter)

# run and extract regressions for each Functional group with zero intercept
# forb
forbRegCoef <- biomassReg %>% 
  lm(forb ~ 0 + forbCov + vegetationHeight, data = .)
forbRegCoef <- tidy(forbRegCoef)

# graminoid
gramRegCoef <- biomassReg %>% 
  lm(graminoid ~ 0 + graminoidCov + vegetationHeight, data = .)
gramRegCoef <- tidy(gramRegCoef)

# moss
mossRegCoef <- biomassReg %>% 
  lm(bryophyte ~ 0 + mossCov + mossHeight, data = .)
mossRegCoef <- tidy(mossRegCoef)

# bind model estimates together
regCoef <- bind_rows("forb" = forbRegCoef, "graminoid" = gramRegCoef, "bryophyte" = mossRegCoef, .id = "functionalGroup") %>% 
  select(estimate, term, functionalGroup)

heightMoss <- regCoef %>% filter(term %in% c("mossHeight")) %>% 
  select(-functionalGroup) %>% 
  spread(key = term, value = estimate)

heightOther <- regCoef %>% filter(term == "vegetationHeight") %>% 
  select(-term) %>% 
  spread(key = functionalGroup, value = estimate)

cover <- regCoef %>% filter(term %in% c("forbCov", "graminoidCov", "mossCov")) %>% 
  select(-functionalGroup) %>% 
  spread(key = term, value = estimate)

# create biomass regressions
biomassReg <- biomassReg %>% 
  group_by(turfID) %>% 
  mutate(forbCov = forbCov*cover$forbCov,
         graminoidCov = graminoidCov*cover$graminoidCov,
         mossCov = mossCov*cover$mossCov,
         mossHeight = mossHeight*heightMoss$mossHeight) %>% 
  left_join(heightOther)

  left_join(regCoef, by = c("term", "functionalGroup"), suffix = c("", ".coef")) %>% 
  group_by(functionalGroup, term, turfID) %>% 
  mutate(coefVal = value*estimate) %>% 
  select(-estimate, -value, -litter, -Biomass_gm, -Biomass_g)

# figures #
biomassReg %>% ggplot(aes(x = cover, y = height)) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y ~ -1 + x") +
  facet_grid(. ~ functionalGroup, scales = "free_x")
