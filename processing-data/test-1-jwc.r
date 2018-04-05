library(readxl)
library(dplyr)

# importing data into memory
df.cctvTotal <- read.csv(file='~/Documents/GitHub/fcdc-overflow-project-1/raw-data/seoul-cctv/seoul_cctv.csv'); colnames(df.cctvTotal) <- c('area','total','2013','2014','2015','2016')
df.cctvIncTotal <- df.cctvTotal; for(i in 4:6) { df.cctvIncTotal[i] <- df.cctvIncTotal[i-1] + df.cctvIncTotal[i] }
df.cctvIncProb <- df.cctvIncTotal; for(i in 4:6) { df.cctvIncProb[i] <- round(((df.cctvIncTotal[i]-df.cctvIncTotal[i-1])/df.cctvIncTotal[i]), digits = 3) }
df.crime2013 <- read_excel(path='~/Documents/GitHub/fcdc-overflow-project-1/raw-data/seoul-crime/2013.xlsx'); colnames(df.crime2013) <- c('area','classification','occurORarrest','freq')
df.crime2014 <- read_excel(path='~/Documents/GitHub/fcdc-overflow-project-1/raw-data/seoul-crime/2014.xlsx'); colnames(df.crime2013) <- c('area','classification','occurORarrest','freq')
df.crime2015 <- read_excel(path='~/Documents/GitHub/fcdc-overflow-project-1/raw-data/seoul-crime/2015.xlsx'); colnames(df.crime2013) <- c('area','classification','occurORarrest','freq')
df.crime2016 <- read_excel(path='~/Documents/GitHub/fcdc-overflow-project-1/raw-data/seoul-crime/2016.xlsx'); colnames(df.crime2013) <- c('area','classification','occurORarrest','freq')

# testing
df.temp <- modify_crime_df(df.crime2013,c('강간','절도'),'발생')

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
  