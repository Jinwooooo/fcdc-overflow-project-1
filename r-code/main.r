# importing libraries to memory
library(readr)
library(readxl)
library(dplyr)
library(GGally)
library(gvlma)
library(lmtest)

# getting happiness data
df.main <- readxl::read_excel(path='~/Documents/GitHub/fcdc-overflow-project-1/data-raw/WHR2018Chapter2OnlineData.xls',
                              sheet=1,col_names=TRUE)
reset.col <- c('country','year','life.ladder','log.gdp.per.cap','social.support','healthy.life.expectancy.at.birth'
               ,'freedom.to.make.life.choices','generosity','perception.of.corruption','positive.affect'
               ,'negative.affect','confidence.in.govt','democratic.quality','delivery.quality','sd.of.ladder.by.country'
               ,'sd.mean.ladder.by.country','gini.index','gini.index.past.mean','gini.house.hold.income')

df.past <- df.main[(df.main$year %in% (2008:2014)),]
df.future <- df.main[(df.main$year %in% (2015:2016)),]
colnames(df.future) <- reset.col

dependent.index <- 3
# pre.independent.index <- c(4:12,15:16,18:19)
# 
# pre.lm.past.result <- basic_linear_reg_test(df.past,reset.col,dependent.index,pre.independent.index)
# pre.lm.future.result <- basic_linear_reg_test(df.future,reset.col,dependent.index,pre.independent.index)
# 
# summary(pre.lm.past.result) # has relevance c('log.gdp.per.cap','freedom.to.make.life.choices','generosity','perception.of.corruption'
#                             # ,'positive.affect','confidence.in.govt')
# summary(pre.lm.future.result)


post.independent.index <- c(4,7:10,12)
tempcol1 <- df.past['freedom.to.make.life.choices']
df.past['freedom.to.make.life.choices'] <- log10(df.past['freedom.to.make.life.choices'])
df.past['freedom.to.make.life.choices'] <- tempcol1
tempcol2 <- df.past['generosity']
df.past['generosity'] <- log10(df.past['generosity'])
df.past['geneorsity'] <- tempcol2
tempcol3 <- df.past['perception.of.corruption']
df.past['perception.of.corruption'] <- log10(df.past['perception.of.corruption'])
df.past['perception.of.corruption'] <- tempcol3
tempcol4 <- df.past['positive.affect']
df.past['positive.affect'] <- log10(df.past['positive.affect'])
df.past['positive.affect'] <- tempcol4
tempcol5 <- df.past['confidence.in.govt']
df.past['confidence.in.govt'] <- log10(df.past['confidence.in.govt'])
df.past['confidence.in.govt'] <- tempcol5


post.lm.past.result <- basic_linear_reg_test(df.past,reset.col,dependent.index,post.independent.index)
summary(post.lm.past.result)

# residual plot
par(mfrow=c(2,2))
plot(post.lm.past.result)
par(mfrow=c(1,1))

# multicolinearity test
corr.test.col <- df.past[,post.independent.index]
GGally::ggpairs(corr.test.col)

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

# normality test
shapiro.test(post.lm.past.result$residuals)
# homoscedasticity test
bptest(post.lm.past.result)
# independence test
lmtest::dwtest(post.lm.past.result)

gvlma(post.lm.past.result)

predicted.value <- predict(post.lm.past.result, newdata=data.frame(log.gdp.per.cap = df.future$log.gdp.per.cap,
                                                freedom.to.make.life.choices= df.future$freedom.to.make.life.choices,
                                                generosity = df.future$generosity,
                                                perception.of.corruption = df.future$perception.of.corruption,
                                                positive.affect = df.future$positive.affect,
                                                confidence.in.govt = df.future$confidence.in.govt),
                           interval = 'predict')


temp <- as.data.table(predicted.value)
temp$result <- ifelse( (temp$lwr <= temp$fit) & (temp$upr >= temp$fit), 1, 0) 
table(temp$result)



true.value <- df.future$life.ladder
compared <- data.frame(predicted = predicted.value,
                       true = true.value,
                       diff = abs(predicted.value - true.value))
temp <- data.frame(predicted = predicted.value,
                   true = true.value)
plot(temp)
mean(compared$diff,na.rm=TRUE)
