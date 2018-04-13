# install.pacakges('pacman')
library(pacman)
pacman::p_load(highcharter,dplyr,purr,readr)

df.train.map <- df.train %>% 
  dplyr::group_by(country) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  dplyr::select(-year)

af.data <- list(region='Africa',
                country=(get_data_from_map(download_map_data('custom/africa')))$name)
asia.data <- list(region='Asia',
                  country=(get_data_from_map(download_map_data('custom/asia')))$name)
eu.data <- list(region='Europe',
                country=(get_data_from_map(download_map_data('custom/europe')))$name)
na.data <- list(region='North America',
                country=(get_data_from_map(download_map_data('custom/north-america')))$name)
oc.data <- list(region='Oceania',
                country=(get_data_from_map(download_map_data('custom/oceania')))$name)
sa.data <- list(region='South America',
                country=(get_data_from_map(download_map_data('custom/south-america')))$name)

world.data <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                     list(af.data, asia.data, eu.data, na.data, oc.data, sa.data))

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


df.final <- merge(df.train.map, world.data, by='country', all.x=TRUE); df.final$region <- as.character(df.final$region)

region.happiness <- c()
for(i in unique(df.final$region)) {
  summary <- df.final[df.final$region == i,]
  region.happiness <- c(region.happiness,round(mean(summary$life.ladder,na.rm=TRUE),digits=3))
}

df.outer <- (data_frame(name = unique(df.final$region),
                       y = round(region.happiness,digits=3),
                       drilldown=tolower(unique(df.final$region))))[1:6,]
df.outer <- df.outer[order(df.outer$y, decreasing=TRUE),]

hc <- highchart() %>% 
  hc_chart(type='column') %>% 
  hc_title(text='Drilldown by Life Ladder for Regions') %>% 
  hc_xAxis(type='category') %>% 
  hc_legend(enabled=FALSE) %>% 
  hc_plotOptions(series=list(borderWidth=0,dataLabels=list(enabled=TRUE))) %>%
  hc_add_series(data=df.outer, name="Life Ladder",colorByPoint=TRUE)

africa <- df.final[df.final$region == 'Africa',][c('country','life.ladder')]
colnames(africa) <- c('name','value')
africa <- africa[!is.na(africa$name),]
africa <- africa[order(africa$value,decreasing=TRUE),]
africa$value <- round(africa$value, digits=3)
asia <- df.final[df.final$region == 'Asia',][c('country','life.ladder')]
colnames(asia) <- c('name','value')
asia <- asia[!is.na(asia$name),]
asia <- asia[order(asia$value,decreasing=TRUE),]
asia$value <- round(asia$value, digits=3)
europe <- df.final[df.final$region == 'Europe',][c('country','life.ladder')]
colnames(europe) <- c('name','value')
europe <- europe[!is.na(europe$name),]
europe <- europe[order(europe$value,decreasing=TRUE),]
europe$value <- round(europe$value, digits=3)
north.america <- df.final[df.final$region == 'North America',][c('country','life.ladder')]
colnames(north.america) <- c('name','value')
north.america <- north.america[!is.na(north.america$name),]
north.america <- north.america[order(north.america$value,decreasing=TRUE),]
north.america$value <- round(north.america$value, digits=3)
oceania <- df.final[df.final$region == 'Oceania',][c('country','life.ladder')]
colnames(oceania) <- c('name','value')
oceania <- oceania[!is.na(oceania$name),]
oceania <- oceania[order(oceania$value,decreasing=TRUE),]
oceania$value <- round(oceania$value, digits=3)
south.america <- df.final[df.final$region == 'South America',][c('country','life.ladder')]
colnames(south.america) <- c('name','value')
south.america <- south.america[!is.na(south.america$name),]
south.america <- south.america[order(south.america$value,decreasing=TRUE),]
south.america$value <- round(south.america$value, digits=3)

hc %>%
  hc_drilldown(allowPointDrilldown = TRUE,
               series = list(
                 list(id = 'africa', data = list_parse2(africa)),
                 list(id = 'asia', data = list_parse2(asia)),
                 list(id = 'europe', data = list_parse2(europe)),
                 list(id = 'north america', data = list_parse2(north.america)),
                 list(id = 'oceania', data = list_parse2(oceania)),
                 list(id = 'south america', data = list_parse2(south.america))
                 )
               )
