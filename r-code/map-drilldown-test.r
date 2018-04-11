# Drilldown map for happiness in 2015

# importing packages
library(pacman)
pacman::p_load(tidyr, highcharter, dplyr, rjson)

# reading 2015 happiness data file
# df <- readr::read_csv(file='~/Documents/GitHub/fcdc-overflow-project-1/data-raw/2015.csv')
# reset.var <- c('Country','Region','HappinessRank','HappinessScore','StandardError','EconomyGDP','Family','LifeExpectancy'
#                ,'Freedom','GovernmentCorruption','Generosity','DystopiaResidual')
# colnames(df) <- reset.var

# reading geo json file
json.continent <- 'https://raw.githubusercontent.com/Jinwooooo/fcdc-overflow-project-1/master/data-raw/continents.json'
json.country <- 'https://raw.githubusercontent.com/Jinwooooo/fcdc-overflow-project-1/master/data-raw/country.json'
geo.continent <- rjson::fromJSON(paste(readLines(json.continent), collapse=''))
geo.country <- rjson::fromJSON(paste(readLines(json.country), collapse=''))

# initializing global variables
continent.json.list <- json_get_feature_properties(geo.continent); continent.json.list <- 
country.json.list <- json_get_feature_properties(geo.country)


# test <- unlist((geo.country$features[[1]])$properties)

json_get_feature_properties <- function(df) {
  result.list <- c()
  for(i in 1:length(df$features)) {
    result.list <- c(result.list, unlist((df$features[[i]])$properties))
  }
  return(result.list)
}

list.continent <- json_get_feature_properties(geo.continent)[1:6]

highchart(type = 'map') %>%
  hc_add_series(mapData = usgeojson, borderWidth = 0.8, tooltip = list(useHTML = TRUE)) %>%
  hc_title('test') %>%
  hc_colorAxis(colors = c("red", "#1874CD")) %>%
  hc_legend(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE)
