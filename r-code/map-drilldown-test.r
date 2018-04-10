# Drilldown map for happiness in 2015

# importing packages
library(pacman)
pacman::p_load(tidyr, highcharter, dplyr, rjson)

# reading 2015 happiness data file
df <- tidyr::read_csv(file='~/Documents/GitHub/fcdc-overflow-project-1/data-raw/2015.csv')
reset.var <- c('Country','Region','HappinessRank','HappinessScore','StandardError','EconomyGDP','Family','LifeExpectancy'
               ,'Freedom','GovernmentCorruption','Generosity','DystopiaResidual')
colnames(df) <- reset.var

# reading geo json file
geo.continent <- rjson::fromJSON(file='')