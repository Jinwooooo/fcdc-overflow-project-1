library(tidyverse)

# getting data
df.2015 <- readr::read_csv(file='~/Documents/GitHub/fcdc-overflow-project-1/data-raw/2015.csv')
df.2016 <- readr::read_csv(file='~/Documents/GitHub/fcdc-overflow-project-1/data-raw/2016.csv')
df.2017 <- readr::read_csv(file='~/Documents/GitHub/fcdc-overflow-project-1/data-raw/2017.csv')

# initializing global variables
reset.var <- c('Country','Region','HappinessRank','HappinessScore','StandardError','EconomyGDP','Family','LifeExpectancy'
               ,'Freedom','GovernmentCorruption','Generosity','DystopiaResidual')
dependent.index <- 4 # 'HappinessScore'
independent.index <- 6:12 # 'EconomyGDP','Family','LifeExpectancy','Freedom','GovernmentCorruption','Generosity','DystopiaResidual'

lm.2015test <- basic_linear_reg_test(df.2015,reset.var,dependent.index,independent.index)
lm.2016test <- basic_linear_reg_test(df.2016,reset.var,dependent.index,independent.index)
lm.2017test <- basic_linear_reg_test(df.2017,reset.var,dependent.index,independent.index)
summary(lm.2015test)
summary(lm.2016test)
summary(lm.2017test)

basic_linear_reg_test <- function(df,set.colname,dep.colindex,indep.colindex) {
  colnames(df) <- set.colname # personally dont like white spaces in colnames, so I made a colnames w/o white spaces
  lm.testvar <- paste(colnames(df)[dep.colindex],'~') # basically returns => 'dependent.variable ~'
  lm.testcol <- colnames(df)[indep.colindex] # character vector of all the required dependent variable
  
  # loop to make a complete string that is suitable for lm argument
  for(i in lm.testcol) {
    # if the concatenating independent variable isn't final
    if(i != lm.testcol[length(lm.testcol)]) {
      # concatenate 'independent.variablex + '
      lm.testvar <- paste(lm.testvar, i, '+')
    } else {
      # last independent variable doesn't need to have '+' at the end, so no need to add another '+'
      lm.testvar <- paste(lm.testvar, i)
    }
  }
  # returns the lm test result back
  return(lm(lm.testvar,data=df,na.action=na.exclude))
}


