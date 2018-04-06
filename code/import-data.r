library(readxl)

setwd('~/Documents/GitHub/fcdc-overflow-project-1/')

# due to some data having different area this group will be used for all data so they are compatiable to each other
seoul.area <- c('강남','강동','강북','강서','관악','광진','구로','금천','노원','도봉','동대문','동작','마포','서대문','서초',
                '성동','성북','송파','양천','영등포','용산','은평','종로','중구','중랑')

df.cctv <- read.csv(file='processed-data/cctv-merged.csv')
  df.cctv$'오차' <- NULL
df.crime <- read.csv(file='processed-data/seoul-crime-merged.csv')
  df.crime$lat <- NULL
  df.crime$lng <- NULL


##### CCTV DATA #####
# cctvTotal : 2013 = # of cctv up to 2013, 2014 ~ 2016 = # of cctv added
# cctvIncTotal : same as cctvTotal but each year represent total # cctv till the end of the year
# cctvIncProb : each year represent how much % of cctv increased compared to last year (2013 is excluded, due 2012 data being NA)
df.cctvTotal <- read.csv(file='raw-data/seoul-cctv/seoul_cctv.csv')
  colnames(df.cctvTotal) <- c('area','total','2013','2014','2015','2016')
  df.cctvTotal$area <- as.character(df.cctvTotal$area)
  df.cctvTotal$area <- seoul.area
df.cctvIncTotal <- df.cctvTotal
  for(i in 4:6) { df.cctvIncTotal[i] <- df.cctvIncTotal[i-1] + df.cctvIncTotal[i] }
df.cctvIncProb <- df.cctvIncTotal 
  for(i in 4:6) { df.cctvIncProb[i] <- round(((df.cctvIncTotal[i]-df.cctvIncTotal[i-1])/df.cctvIncTotal[i]), digits = 3) }
  df.cctvIncProb$'2013' <- NULL

##### CRIME DATA #####
df.crime2013 <- read_excel(path='raw-data/seoul-crime/2013.xlsx')
  colnames(df.crime2013) <- c('area','classification','occurORarrest','freq')
df.temp <- modify_crime_df(df.crime2013,'종합','발생')
df.temp <- df.temp[(order(df.temp$area)),]
df.temp <- df.temp[(df.temp$area %in% seoul.area),]

##### USER DEFINED FUNCTION #####
# df = dataframe; classify = c(분류) or '종합'; status = ['발생','검거']
modify_crime_df <- function(df,classify,status) {
  # initial output dataframe
  df.output = df[0,]
  
  # reducing df by occur or arrest
  if(status == '발생') {
    df <- df[df['occurORarrest'] == '발생',]
  } else {
    df <- df[df['occurORarrest'] == '검거',]
  }
  
  # reducing df by classification
  if(classify != '종합') {
    df.output <- dplyr::filter(df, classification == classify)
  } else {
    temp.area <- unique(df$area)
    for(i in temp.area) {
      temp.df <- dplyr::filter(df, area == i)
      temp.freq <- sum(temp.df$freq)
      df.output[nrow(df.output) + 1,] <- list(i,'종합',status,temp.freq)
    }
  }
  
  return(df.output)
}
