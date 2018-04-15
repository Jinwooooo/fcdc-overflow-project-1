# install.pacakges('pacman')
library(pacman)
pacman::p_load(highcharter,dplyr,purr,readr,Hmisc,shiny,gridExtra)

# Import data
map.data <- get_data_from_map(download_map_data('custom/world-robinson-highres'))

# Remove white space in column names
reset.col <- c('country','year','life.ladder','log.gdp.per.cap','social.support','healthy.life.expectancy.at.birth',
               'freedom.to.make.life.choices','generosity','perception.of.corruption','positive.affect',
               'negative.affect','confidence.in.govt','democratic.quality','delivery.quality','sd.of.ladder.by.country',
               'sd.mean.ladder.by.country','gini.index','gini.index.past.mean','gini.house.hold.income')
colnames(df) <- reset.col

# assuming df.train is already run and is available in memory (try to use RData...)
df.train.map <- df.train %>% 
  dplyr::group_by(country) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  dplyr::select(-year)

# values that are different were brute force searched and then substituted
# missing.countries <- (df.train.map[!(df.train.map$country %in% map.data$name),])[,1]
# excluded.countries <- (map.data[!(map.data$name %in% df.train.map$country),])$name
df.train.map[df.train.map$country == "Congo (Brazzaville)",1] = 'Democratic Republic of the Congo'
df.train.map[df.train.map$country == "Congo (Kinshasa)",1] = 'Republic of Congo'
df.train.map[df.train.map$country == "North Cyprus",1] = 'Northern Cyprus'
df.train.map[df.train.map$country == "Serbia",1] = 'Republic of Serbia'
df.train.map[df.train.map$country == "Somaliland region",1] = 'Somaliland'
df.train.map[df.train.map$country == "Taiwan Province of China",1] = 'Taiwan'
df.train.map[df.train.map$country == "Tanzania",1] = 'United Republic of Tanzania'
df.train.map[df.train.map$country == "United States",1] = 'United States of America'

map.life.ladder <- hcmap("custom/world-robinson-highres", data = df.train.map, value = "life.ladder",
                         joinBy = c('name','country'), name = 'Life Ladder',
                         dataLabels = list(enabled = TRUE, format = '{point.name}'),
                         borderColor = "#000000", borderWidth = 0.1,
                         tooltip = list(valueDecimals = 3),
                         nullColor = '#FF0000') %>% 
                         hc_title(text = 'Life Ladder World Map')

map.log.gdp.per.cap <- hcmap("custom/world-robinson-highres", data = df.train.map, value = "log.gdp.per.cap",
                          joinBy = c('name','country'), name = 'log(GDP per Capital)',
                          dataLabels = list(enabled = TRUE, format = '{point.name}'),
                          borderColor = "#000000", borderWidth = 0.1,
                          tooltip = list(valueDecimals = 3),
                          nullColor = '#FF0000') %>%
                          hc_title(text = 'log(GDP per Capital) World Map')

map.social.support <- hcmap("custom/world-robinson-highres", data = df.train.map, value = "social.support",
                                joinBy = c('name','country'), name = 'Social Support',
                                dataLabels = list(enabled = TRUE, format = '{point.name}'),
                                borderColor = "#000000", borderWidth = 0.1,
                                tooltip = list(valueDecimals = 3),
                                nullColor = '#FF0000') %>%
                                hc_title(text = 'Social Support World Map')

map.healthy.life.expectancy.at.birth <- hcmap("custom/world-robinson-highres", data = df.train.map, 
                                                  value = "healthy.life.expectancy.at.birth",
                                                  joinBy = c('name','country'), name = 'Freedom to Make Choices',
                                                  dataLabels = list(enabled = TRUE, format = '{point.name}'),
                                                  borderColor = "#000000", borderWidth = 0.1,
                                                  tooltip = list(valueDecimals = 3),
                                                  nullColor = '#FF0000') %>% 
                                                  hc_title(text = 'Healthy Life Expectancy at Birth World Map')

map.freedom.to.make.choices <- hcmap("custom/world-robinson-highres", data = df.train.map, 
                                         value = "freedom.to.make.life.choices",
                                         joinBy = c('name','country'), name = 'Freedom to Make Choices',
                                         dataLabels = list(enabled = TRUE, format = '{point.name}'),
                                         borderColor = "#000000", borderWidth = 0.1,
                                         tooltip = list(valueDecimals = 3),
                                         nullColor = '#FF0000') %>%
                                         hc_title(text = 'Freedom to Make Choice World Map')

map.geneorsity <- hcmap("custom/world-robinson-highres", data = df.train.map, 
                        value = "geneoristy",
                        joinBy = c('name','country'), name = 'Geneorsity',
                        dataLabels = list(enabled = TRUE, format = '{point.name}'),
                        borderColor = "#000000", borderWidth = 0.1,
                        tooltip = list(valueDecimals = 3),
                        nullColor = '#FF0000') %>%
                        hc_title(text = 'Geneorsity World Map')

map.perception.of.corruption <- hcmap("custom/world-robinson-highres", data = df.train.map, 
                                      value = "perception.of.corruption",
                                      joinBy = c('name','country'), name = 'Perception of Corruption',
                                      dataLabels = list(enabled = TRUE, format = '{point.name}'),
                                      borderColor = "#000000", borderWidth = 0.1,
                                      tooltip = list(valueDecimals = 3),
                                      nullColor = '#FF0000') %>%
                                      hc_title(text = 'Perception of Corruption World Map')

map.positive.affect <- hcmap("custom/world-robinson-highres", data = df.train.map, 
                             value = "positive.affect",
                             joinBy = c('name','country'), name = 'Positive Affect',
                             dataLabels = list(enabled = TRUE, format = '{point.name}'),
                             borderColor = "#000000", borderWidth = 0.1,
                             tooltip = list(valueDecimals = 3),
                             nullColor = '#FF0000') %>%
                             hc_title(text = 'Positive Affect World Map')

map.negative.affect <- hcmap("custom/world-robinson-highres", data = df.train.map, 
                             value = "negative.affect",
                             joinBy = c('name','country'), name = 'Negative Affect',
                             dataLabels = list(enabled = TRUE, format = '{point.name}'),
                             borderColor = "#000000", borderWidth = 0.1,
                             tooltip = list(valueDecimals = 3),
                             nullColor = '#FF0000') %>%
                             hc_title(text = 'Negative Affect World Map')

map.confidence.in.govt <- hcmap("custom/world-robinson-highres", data = df.train.map, 
                               value = "confidence.in.govt",
                               joinBy = c('name','country'), name = 'Confidence in Government',
                               dataLabels = list(enabled = TRUE, format = '{point.name}'),
                               borderColor = "#000000", borderWidth = 0.1,
                               tooltip = list(valueDecimals = 3),
                               nullColor = '#FF0000') %>%
                               hc_title(text = 'Confidence in Government World Map')

map.gini.index.past.mean <- hcmap("custom/world-robinson-highres", data = df.train.map, 
                                  value = "gini.index.past.mean",
                                  joinBy = c('name','country'), name = 'Gini Index : Past Mean',
                                  dataLabels = list(enabled = TRUE, format = '{point.name}'),
                                  borderColor = "#000000", borderWidth = 0.1,
                                  tooltip = list(valueDecimals = 3),
                                  nullColor = '#FF0000') %>%
                                  hc_title(text = 'Gini Index : Past Mean World Map')

map.gini.house.hold.income <- hcmap("custom/world-robinson-highres", data = df.train.map, 
                                    value = "gini.house.hold.income",
                                    joinBy = c('name','country'), name = 'Gini House Hold Income',
                                    dataLabels = list(enabled = TRUE, format = '{point.name}'),
                                    borderColor = "#000000", borderWidth = 0.1,
                                    tooltip = list(valueDecimals = 3),
                                    nullColor = '#FF0000') %>%
                                    hc_title(text = 'Gini House Hold Income World Map')

map.life.ladder
map.log.gdp.per.cap
map.social.support
map.healthy.life.expectancy.at.birth
map.freedom.to.make.choices
map.geneorsity
map.perception.of.corruption
map.negative.affect
map.positive.affect
map.confidence.in.govt
map.gini.index.past.mean
map.gini.house.hold.income
