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
  mutate(Region = factor(Region))

did_ETS_RegionCh4 <- feols(
  emission_log ~ Region:ETS:phase3_post |
    Firm + year + Sector:year,
  data     = matched_data,
  weights  = ~weights,
  cluster  = ~subclass
) %>% tidy()

did_ETS_RegionCh4

WeightsCh4 <- matched_data %>%
  group_by(Region) %>%
  summarise(n = sum(weights)) %>%
  arrange(Region)

##################SOx

load("matched_data.Sox.RData")

matched_data <- matched_data %>%
  filter(year >= 2008, year <= 2017) %>%
  mutate(Region = factor(Region))

did_ETS_RegionSox <- feols(
  emission_log ~ Region:ETS:phase3_post|
    Firm + year + Sector:year,
  data     = matched_data,
  weights  = ~weights,
  cluster  = ~subclass
) %>% tidy()

did_ETS_RegionSox

WeightsSox <- matched_data %>%
  group_by(Region) %>%
  summarise(n = sum(weights)) %>%
  arrange(Region)

##################Co2

load("matched_data.Co2.RData")

matched_data <- matched_data %>%
  filter(year >= 2008, year <= 2017) %>%
  mutate(Region = factor(Region))

did_ETS_RegionCo2 <- feols(
  emission_log ~ Region:ETS:phase3_post|
    Firm + year + Sector:year,
  data     = matched_data,
  weights  = ~weights,
  cluster  = ~subclass
) %>% tidy()

did_ETS_RegionCo2

#Count the number of observations per Region
WeightsCo2 <- matched_data %>%
  group_by(Region) %>%
  summarise(n = sum(weights)) %>%
  arrange(Region)

##################Zn_water

load("matched_data.Zn_Water.RData")

matched_data <- matched_data %>%
  filter(year >= 2008, year <= 2017) %>%
  mutate(Region = factor(Region))

did_ETS_RegionZn_Water <- feols(
  emission_log ~ Region:ETS:phase3_post|
    Firm + year + Sector:year,
  data     = matched_data,
  weights  = ~weights,
  cluster  = ~subclass
) %>% tidy()

did_ETS_RegionZn_Water

WeightsZn_Water <- matched_data %>%
  group_by(Region) %>%
  summarise(n = sum(weights)) %>%
  arrange(Region)

##################N2o

load("matched_data.N2o.RData")

matched_data <- matched_data %>%
  filter(year >= 2008, year <= 2016) %>%
  mutate(Region = factor(Region))

did_ETS_RegionN2o <- feols(
  emission_log ~ Region:ETS:phase3_post|
    Firm + year+ Sector:year,
  data     = matched_data,
  weights  = ~weights,
  cluster  = ~subclass
) %>% tidy()

did_ETS_RegionN2o

WeightsN2o <- matched_data %>%
  group_by(Region) %>%
  summarise(n = sum(weights)) %>%
  arrange(Region)

##################NH3

load("matched_data.Nh3.RData")

matched_data <- matched_data %>%
  filter(year >= 2008, year <= 2017) %>%
  mutate(Region = factor(Region))

did_ETS_RegionNh3 <- feols(
  emission_log ~ Region:ETS:phase3_post|
    Firm + year+ Sector:year,
  data     = matched_data,
  weights  = ~weights,
  cluster  = ~subclass
) %>% tidy()

did_ETS_RegionNh3

WeightsNh3 <- matched_data %>%
  group_by(Region) %>%
  summarise(n = sum(weights)) %>%
  arrange(Region)


print(did_ETS_RegionCh4)
print(did_ETS_RegionSox)
print(did_ETS_RegionCo2)
print(did_ETS_RegionZn_Water)
print(did_ETS_RegionN2o)
print(did_ETS_RegionNh3)


print(WeightsCh4)
print(WeightsSox)
print(WeightsCo2)
print(WeightsZn_Water)
print(WeightsN2o)
print(WeightsNh3)