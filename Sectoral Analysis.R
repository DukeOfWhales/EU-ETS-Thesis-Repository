rm(list = ls())
cat("\014") 
graphics.off()
setwd("C:/Users/Tobyd/OneDrive - Aarhus universitet/Uni-livet/Kandidatgrad/10. Semester/Specialet/Data/Final data/")

#Install all packages

library(dplyr)
library(ggplot2)
library(broom)
library(estimatr)
library(fixest)
library(stringr)
library(readr)

##################CH4

load("matched_data.Ch4.RData")

matched_data <- matched_data %>%
  filter(year >= 2008, year <= 2017) %>%
  mutate(Sector2 = factor(Sector))

did_ETS_SectorCh4 <- feols(
  emission_log ~ Sector2:ETS:phase3_post|
    Firm + year + Sector:year,
  data     = matched_data,
  weights  = ~weights,
  cluster  = ~subclass
) %>% tidy()

did_ETS_SectorCh4

WeightsCh4 <- matched_data %>%
  group_by(Sector) %>%
  summarise(n = sum(weights)) %>%
  arrange(Sector)

##################SOx

load("matched_data.Sox.RData")

matched_data <- matched_data %>%
  filter(year >= 2008, year <= 2017) %>%
  mutate(Sector2 = factor(Sector))

did_ETS_SectorSox <- feols(
  emission_log ~ Sector2:ETS:phase3_post|
    Firm + year + Sector:year,
  data     = matched_data,
  weights  = ~weights,
  cluster  = ~subclass
) %>% tidy()

did_ETS_SectorSox

WeightsSox <- matched_data %>%
  group_by(Sector) %>%
  summarise(n = sum(weights)) %>%
  arrange(Sector)

##################Co2

load("matched_data.Co2.RData")

matched_data <- matched_data %>%
  filter(year >= 2008, year <= 2017) %>%
  mutate(Sector2 = factor(Sector))

did_ETS_SectorCo2 <- feols(
  emission_log ~ Sector2:ETS:phase3_post|
    Firm + year + Sector:year,
  data     = matched_data,
  weights  = ~weights,
  cluster  = ~subclass
) %>% tidy()

did_ETS_SectorCo2

#Count the number of observations per sector
WeightsCo2 <- matched_data %>%
  group_by(Sector) %>%
  summarise(n = sum(weights)) %>%
  arrange(Sector)

##################Zn_water

load("matched_data.Zn_Water.RData")

matched_data <- matched_data %>%
  filter(year >= 2008, year <= 2017) %>%
  mutate(Sector2 = factor(Sector))

did_ETS_SectorZn_Water <- feols(
  emission_log ~ Sector2:ETS:phase3_post|
    Firm + year + Sector:year,
  data     = matched_data,
  weights  = ~weights,
  cluster  = ~subclass
) %>% tidy()

did_ETS_SectorZn_Water

WeightsZn_Water <- matched_data %>%
  group_by(Sector) %>%
  summarise(n = sum(weights)) %>%
  arrange(Sector)

##################N2o

load("matched_data.N2o.RData")

matched_data <- matched_data %>%
  filter(year >= 2008, year <= 2016) %>%
  mutate(Sector2 = factor(Sector))

did_ETS_SectorN2o <- feols(
  emission_log ~ Sector2:ETS:phase3_post|
    Firm + year+ Sector:year,
  data     = matched_data,
  weights  = ~weights,
  cluster  = ~subclass
) %>% tidy()

did_ETS_SectorN2o

WeightsN2o <- matched_data %>%
  group_by(Sector) %>%
  summarise(n = sum(weights)) %>%
  arrange(Sector)

##################NH3

load("matched_data.Nh3.RData")

matched_data <- matched_data %>%
  filter(year >= 2008, year <= 2017) %>%
  mutate(Sector2 = factor(Sector))

did_ETS_SectorNh3 <- feols(
  emission_log ~ Sector2:ETS:phase3_post|
    Firm + year+ Sector:year,
  data     = matched_data,
  weights  = ~weights,
  cluster  = ~subclass
) %>% tidy()

did_ETS_SectorNh3

WeightsNh3 <- matched_data %>%
  group_by(Sector) %>%
  summarise(n = sum(weights)) %>%
  arrange(Sector)


print(did_ETS_SectorCh4)
print(did_ETS_SectorSox)
print(did_ETS_SectorCo2)
print(did_ETS_SectorZn_Water)
print(did_ETS_SectorN2o)
print(did_ETS_SectorNh3)


print(WeightsCh4)
print(WeightsSox)
print(WeightsCo2)
print(WeightsZn_Water)
print(WeightsN2o)
print(WeightsNh3)