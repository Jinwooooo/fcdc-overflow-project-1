library(tidyverse)

df.2015 <- read_csv(file='~/Documents/GitHub/fcdc-overflow-project-1/data-raw/2015.csv')
df.2016 <- read_csv(file='~/Documents/GitHub/fcdc-overflow-project-1/data-raw/2016.csv')
df.2017 <- read_csv(file='~/Documents/GitHub/fcdc-overflow-project-1/data-raw/2017.csv')

# initializing global variables
reset.var <- c('Country','Region','HappinessRank','HappinessScore','StandardError','EconomyGDP','Family','LifeExpectancy'
               ,'Freedom','GovernmentCorruption','Generosity','DystopiaResidual')
dependent.index <- 4
independent.index <- 6:12

lm.test <- basic_linear_reg_test(df.2015,reset.var,dependent.index,independent.index)
summary(lm.test)

basic_linear_reg_test <- function(df,set.colname,dep.colindex,indep.colindex) {
  colnames(df) <- set.colname
  lm.testvar <- paste(colnames(df)[dep.colindex],'~')
  lm.testcol <- colnames(df)[indep.colindex]
  
  for(i in lm.testcol) {
    if(i != lm.testcol[length(lm.testcol)]) {
      lm.testvar <- paste(lm.testvar, i, '+')
    } else {
      lm.testvar <- paste(lm.testvar, i)
    }
  }
  
  return(lm(lm.testvar,data=df,na.action=na.exclude))
}
