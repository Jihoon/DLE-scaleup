library(countrycode)
library(tidyverse)

source('DLE_integration_data_structure.R')

# Example for health dimension
countries <- codelist$iso3c[!is.na(codelist$iso3c)]
group <- c('adult', 'child')

idx <- expand.grid(iso=countries, grp=group) %>% arrange(iso, grp)
thres <- data.frame(idx, val=2000)
gap <- data.frame(idx, val=2000)

# Initiate an inherited, specific dimension object
DLE.Health <- DLE.dimension.health(indicator = 'kcal/day', thres = thres)

# Initiate a general dimension object
DLE.A <- DLE.dimension(indicator = 'xxx', thres = thres)

# Do whatever necessary for the initial setup
DLE.Health$DeriveThreshold()
DLE.Health$IdentifyGap()

# Build a list of dimensions
dim.list <- list(health = DLE.Health, A = DLE.A)

# Initiate a scenario object
DLE.BAU <- DLE.scenario(scenario = "BAU", dims = dim.list)
DLE.BAU$SetupRollout()
