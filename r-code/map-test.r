library(highcharter)
library(dplyr)
library(purrr)
library(readr)

mapdata <- get_data_from_map(download_map_data('custom/world-continents'))

df <- read_csv(file='~/Documents/GitHub/fcdc-overflow-project-1/data-raw/2015.csv')
reset.var <- c('Country','Region','HappinessRank','HappinessScore','StandardError','EconomyGDP','Family','LifeExpectancy'
               ,'Freedom','GovernmentCorruption','Generosity','DystopiaResidual')
colnames(df) <- reset.var

region.happiness <- c()

eu <- c('Western Europe','Central and Eastern Europe')
oc <- c('Australia and New Zealand')
af <- c('Sub-Saharan Africa','Middle East and Northern Africa')
as <- c('Eastern Asia','Southeastern Asia','Southern Asia')
na <- c('North America')
sa <- c('Latin America and Caribbean')
continent <- list(eu,oc,af,as,na,sa)

happiness.continent <- c()
for(i in continent) {
  temp <- 0
  if(length(i) == 1) {
    happiness.continent <- c(happiness.continent, mean(unlist((df[df$Region == i,])['HappinessScore'])))
  } else {
    for(j in i) {
      temp <- temp + mean(unlist((df[df$Region == j,])['HappinessScore']))
    }
    temp <- temp / length(i)
    happiness.continent <- c(happiness.continent, temp)
    temp <- 0
  }
}

df.continent <- data_frame(
  code = mapdata$name,
  value = happiness.continent
)

hcmap("custom/world-continents", data = df.continent, value = "value",
      joinBy = c('name','code'), name = 'Happiness Score',
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#000000", borderWidth = 0.1,
      tooltip = list(valueDecimals = 3))
