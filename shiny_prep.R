
options(scipen = 999)
library(fs)
library(shinythemes)
library(tidyverse)

dir_create("data")

zillow_data <- read.csv("Metro_Zhvi_SingleFamilyResidence.csv")

cities <- zillow_data %>%
  select(RegionName) %>%
  filter(RegionName != "United States")

citiesVector <- as.vector(cities$RegionName)

