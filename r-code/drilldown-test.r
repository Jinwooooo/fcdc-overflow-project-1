### ----------------------------------------- NOTE ----------------------------------------- ###
# for highchart, you need columns to be name and y in order to show in graph, weird...         #
### ---------------------------------------------------------------------------------------- ###

suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("purrr"))
suppressPackageStartupMessages(library("highcharter"))

df <- read_csv(file='~/Documents/GitHub/fcdc-overflow-project-1/data-raw/2015.csv')
reset.var <- c('Country','Region','HappinessRank','HappinessScore','StandardError','EconomyGDP','Family','LifeExpectancy'
               ,'Freedom','GovernmentCorruption','Generosity','DystopiaResidual')
colnames(df) <- reset.var

region.happiness <- c()
for(i in unique(df$Region)) {
  summary <- df[df$Region == i,]
  region.happiness <- c(region.happiness,round(mean(summary$HappinessScore),digits=3))
}

df.region <- data_frame(
  name = unique(df$Region),
  y = region.happiness,
  drilldown=tolower(unique(df$Region))
)

hc <- highchart() %>% 
  hc_chart(type='column') %>% 
  hc_title(text='Drilldown by Happiness Score for Regions') %>% 
  hc_xAxis(type='category') %>% 
  hc_legend(enabled=FALSE) %>% 
  hc_plotOptions(series=list(borderWidth=0,dataLabels=list(enabled=TRUE))) %>%
  hc_add_series(data=df.region, name="Happiness Score",colorByPoint=TRUE)

we.eu <- (df[df$Region == 'Western Europe',])[c('Country','HappinessScore')]; colnames(we.eu) <- c('name','value')
no.am <- (df[df$Region == 'North America',])[c('Country','HappinessScore')]; colnames(no.am) <- c('name','value')
au.nz <- (df[df$Region == 'Australia and New Zealand',])[c('Country','HappinessScore')]; colnames(au.nz) <- c('name','value')
me.na <- (df[df$Region == 'Middle East and Northern Africa',])[c('Country','HappinessScore')]; colnames(me.na) <- c('name','value')
la.ca <- (df[df$Region == 'Latin America anc Caribbean',])[c('Country','HappinessScore')]; colnames(la.ca) <- c('name','value')
se.as <- (df[df$Region == 'Southeastern Asia',])[c('Country','HappinessScore')]; colnames(se.as) <- c('name','value')
ce.eu <- (df[df$Region == 'Central and Eastern Europe',])[c('Country','HappinessScore')]; colnames(ce.eu) <- c('name','value')
ea.as <- (df[df$Region == 'Eastern Asia',])[c('Country','HappinessScore')]; colnames(ea.as) <- c('name','value')
ss.af <- (df[df$Region == 'Sub-Saharan Africa',])[c('Country','HappinessScore')]; colnames(ss.af) <- c('name','value')
se.as <- (df[df$Region == 'Southern Asia',])[c('Country','HappinessScore')]; colnames(se.as) <- c('name','value')

hc <- hc %>%
  hc_drilldown(allowPointDrilldown = TRUE,
    series = list(
      list(id = "western europe",data = list_parse2(we.eu)),
      list(id = "north america",data = list_parse2(no.am)),
      list(id = "austrilia and new zealand",data = list_parse2(au.nz)),
      list(id = "middle east and northern africa",data = list_parse2(me.na)),
      list(id = "latin america and caribbean",data = list_parse2(la.ca)),
      list(id = "southeastern asia",data = list_parse2(se.as)),
      list(id = "central and eastern europe",data = list_parse2(ce.eu)),
      list(id = "eastern asia",data = list_parse2(ea.as)),
      list(id = "sub-saharan africa",data = list_parse2(ss.af)),
      list(id = "southern asia",data = list_parse2(se.as))
    )
  )

hc


