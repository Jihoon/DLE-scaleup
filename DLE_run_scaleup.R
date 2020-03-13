library(countrycode)
library(tidyverse)

# Example for health dimension
countries <- codelist$iso3c[!is.na(codelist$iso3c)]
group <- c('adult', 'child')

idx <- expand.grid(iso=countries, grp=group) %>% arrange(iso, grp)
thres <- data.frame(idx, val=2000)
gap <- data.frame(idx, val=2000)

DLE.Health <- DLE.dimension.health(indicator = 'kcal/day', thres = thres)

DLE.Health$IdentifyGap()
DLE.Health
