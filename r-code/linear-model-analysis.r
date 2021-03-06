# importing libraries to local memory
library(pacman)
pacman::p_load(readr,dplyr,gvlma,data.table,car,ggplot2,lm.beta,
               corrplot,dplyr,MASS,Hmisc,r2glmm,olsrr,DAAG,highcharter)

# Import data
df <- data.table::fread('https://raw.githubusercontent.com/Jinwooooo/fcdc-overflow-project-1/master/data/raw-data.csv')
df$V1 <- NULL

# Remove white space in column names
reset.col <- c('country','year','life.ladder','log.gdp.per.cap','social.support','healthy.life.expectancy.at.birth',
               'freedom.to.make.life.choices','generosity','perception.of.corruption','positive.affect',
               'negative.affect','confidence.in.govt','democratic.quality','delivery.quality','sd.of.ladder.by.country',
               'sd.mean.ladder.by.country','gini.index','gini.index.past.mean','gini.house.hold.income')
colnames(df) <- reset.col

# Removing unused columns and filling na values with mean values (in order to actually use values as predict at the end)
df.main <- df %>%
  dplyr::select(-c(sd.of.ladder.by.country, sd.mean.ladder.by.country)) %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate_at(vars(-country),funs(replace(., which(is.nan(.)), mean(., na.rm = TRUE)))) %>%
  dplyr::mutate_at(vars(-country), funs(replace(., which(is.na(.)), mean(., na.rm = TRUE))))

df.main <- df.main %>% 
  dplyr::group_by(country) %>%
  dplyr::mutate_all(funs(replace(., which(is.nan(.)), mean(., na.rm = TRUE)))) %>% 
  dplyr::mutate_all(funs(replace(., which(is.na(.)), mean(., na.rm = TRUE))))

df.main$gini.house.hold.income <- Hmisc::impute(df.main$gini.house.hold.income,mean)
# [debug] : checking for na values
# colSums(is.na(df.main))

# Box-whisker plot to see life ladder's mean and IQR values per year
ggplot2::ggplot(df.main, aes(x = year, y = life.ladder, group = year)) + ggplot2::geom_boxplot()
ggplot2::ggplot(df.main, aes(x = year, y = log.gdp.per.cap, group = year)) + ggplot2::geom_boxplot()
# to see if it's somewhere near normal distribution
ggplot2::ggplot(df.main, aes(life.ladder)) +
  ggplot2::geom_histogram(aes(y = ..density..),
                          bins = 20,
                          col = 4,
                          fill = "blue", 
                          alpha = 0.4) +
  ggplot2::geom_density(col = 6) +
  ggplot2::labs(x = "life.ladder", y = "count")

# Split into train and test data
df.train <- df.main %>% dplyr::filter(year >= 2005 & year <= 2014)
df.test <- df.main %>% dplyr::filter(year > 2014 & year < 2017)

# Reduce each set to means
df.train.mean <- df.train %>% 
  dplyr::group_by(country) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  dplyr::select(-country)
df.test.mean <- df.test %>% 
  dplyr::group_by(country) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  dplyr::select(-country)

# EDA
df.train.map <- df.train %>% 
  dplyr::group_by(country) %>%
  dplyr::summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  dplyr::select(-year)

# data is from highcharter map library, the grouping for continents were off, but wasn't a problem in overall EDA
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

df.final <- merge(df.train.map, world.data, by='country', all.x=TRUE)
df.final$region <- as.character(df.final$region)

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

# due to too many maps, other variables have been exlcuded from this report
# for other map visualization on other variables that's not life.ladder, see map-visualization.r file
map.life.ladder <- hcmap("custom/world-robinson-highres", data = df.train.map, value = "life.ladder",
                         joinBy = c('name','country'), name = 'Life Ladder',
                         dataLabels = list(enabled = TRUE, format = '{point.name}'),
                         borderColor = "#000000", borderWidth = 0.1,
                         tooltip = list(valueDecimals = 3),
                         nullColor = '#FF0000') %>% 
  hc_title(text = 'Life Ladder World Map')
map.life.ladder

# Initial Linear Model
lm.train.mean <- lm(life.ladder ~. -year, data = df.train.mean)
summary(lm.train.mean)
#################################################################################
# Coefficients:                                                                 #
#                                    Estimate Std. Error t value Pr(>|t|)       #
#   (Intercept)                      -0.69787    0.72224   -0.97   0.3354       #
#   log.gdp.per.cap                   0.31510    0.06043    5.21  5.9e-07 ***   #
#   social.support                    1.52761    0.50443    3.03   0.0029 **    #
#   healthy.life.expectancy.at.birth  0.01208    0.00858    1.41   0.1611       #
#   freedom.to.make.life.choices      0.74193    0.41665    1.78   0.0770 .     #
#   generosity                        0.18288    0.30192    0.61   0.5456       #
#   perception.of.corruption         -1.07614    0.29068   -3.70   0.0003 ***   #
#   positive.affect                   3.52175    0.67423    5.22  5.7e-07 ***   #
#   negative.affect                   1.15256    0.63674    1.81   0.0723 .     #
#   confidence.in.govt               -0.90373    0.30466   -2.97   0.0035 **    #
#   gini.index.past.mean             -0.71392    0.62546   -1.14   0.2555       #
#   gini.house.hold.income           -1.23801    0.56359   -2.20   0.0296 *     #
#   ---                                                                         #
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1              #
#                                                                               #
# Residual standard error: 0.449 on 152 degrees of freedom                      #
# Multiple R-squared:  0.84,	Adjusted R-squared:  0.829                        #
# F-statistic: 72.7 on 11 and 152 DF,  p-value: <2e-16                          #
#################################################################################
gvlma::gvlma(lm.train.mean)
#################################################################################
#                       Value   p-value                   Decision              #
#   Global Stat        25.5898 3.828e-05 Assumptions NOT satisfied!             #
#   Skewness            2.4249 1.194e-01    Assumptions acceptable.             #
#   Kurtosis            0.7587 3.837e-01    Assumptions acceptable.             #
#   Link Function      21.7913 3.040e-06 Assumptions NOT satisfied!             #
#   Heteroscedasticity  0.6150 4.329e-01    Assumptions acceptable.             #
#################################################################################

# Selecting independent variable that have relation to the dependent variable (stepwise)
step(lm.train.mean, df.train.mean, direction = "both")
#################################################################################
# Step:  AIC=-252.94                                                            #
#                                                                               #
#                                    Df Sum of Sq    RSS     AIC                #
# <none>                                          30.674 -252.94                #
# - gini.index.past.mean              1    0.4040 31.078 -252.79                #
# - healthy.life.expectancy.at.birth  1    0.4165 31.090 -252.73                #
# - freedom.to.make.life.choices      1    0.6637 31.337 -251.43                #
# - negative.affect                   1    0.7986 31.472 -250.72                #
# - gini.house.hold.income            1    0.9373 31.611 -250.00                #
# - confidence.in.govt                1    1.7163 32.390 -246.01                #
# - social.support                    1    1.8316 32.505 -245.43                #
# - perception.of.corruption          1    2.9299 33.603 -239.98                #
# - log.gdp.per.cap                   1    5.4738 36.147 -228.01                #
# - positive.affect                   1    7.4832 38.157 -219.14                #
#################################################################################

# Build new model
lm.train.step <- lm(life.ladder ~ log.gdp.per.cap + social.support + healthy.life.expectancy.at.birth + 
                   freedom.to.make.life.choices + perception.of.corruption + 
                   positive.affect + negative.affect + confidence.in.govt + 
                   gini.index.past.mean + gini.house.hold.income, data = df.train.mean)
summary(lm.train.step)
#################################################################################
# Coefficients:                                                                 #
#                                     Estimate Std. Error t value Pr(>|t|)      #
#   (Intercept)                      -0.797457   0.684808  -1.164 0.246059      #
#   log.gdp.per.cap                   0.309807   0.058584   5.288 4.26e-07 ***  #
#   social.support                    1.402717   0.519521   2.700 0.007725 **   #
#   healthy.life.expectancy.at.birth  0.012355   0.008535   1.447 0.149845      #
#   freedom.to.make.life.choices      0.882231   0.454913   1.939 0.054324 .    #
#   perception.of.corruption         -1.088818   0.286007  -3.807 0.000204 ***  #
#   positive.affect                   3.610201   0.607964   5.938 1.91e-08 ***  #
#   negative.affect                   1.151392   0.622203   1.851 0.066194 .    #
#   confidence.in.govt               -0.897060   0.308365  -2.909 0.004173 **   #
#   gini.index.past.mean             -0.689780   0.597285  -1.155 0.249973      #
#   gini.house.hold.income           -1.096220   0.460742  -2.379 0.018597 *    #
#   ---                                                                         #
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1              #
#                                                                               #
# Residual standard error: 0.4466 on 151 degrees of freedom                     #
# (2 observations deleted due to missingness)                                   #
# Multiple R-squared:  0.8425,	Adjusted R-squared:  0.8321                     #
# F-statistic: 80.78 on 10 and 151 DF,  p-value: < 2.2e-16                      #
#################################################################################
gvlma::gvlma(lm.train.step)
#################################################################################
#                        Value   p-value                   Decision             #
#   Global Stat        25.3801 4.219e-05 Assumptions NOT satisfied!             #
#   Skewness            1.8321 1.759e-01    Assumptions acceptable.             #
#   Kurtosis            0.5609 4.539e-01    Assumptions acceptable.             #
#   Link Function      22.3452 2.278e-06 Assumptions NOT satisfied!             #
#   Heteroscedasticity  0.6419 4.230e-01    Assumptions acceptable.             #
#################################################################################

# Removing variables with little impact (healthy.life, gini.index.past.mean)
lm.train.step2 <- lm(life.ladder ~ log.gdp.per.cap + social.support +
                    freedom.to.make.life.choices + perception.of.corruption + 
                    positive.affect + negative.affect + confidence.in.govt + gini.house.hold.income, 
                    data = df.train.mean)
summary(lm.train.step2)
#################################################################################
# Coefficients:                                                                 #
#                                Estimate Std. Error t value Pr(>|t|)           #
#   (Intercept)                  -0.52021    0.65047  -0.800 0.425103           #
#   log.gdp.per.cap               0.37435    0.04702   7.961 3.59e-13 ***       #
#   social.support                1.40026    0.52229   2.681 0.008145 **        #
#   freedom.to.make.life.choices  0.98169    0.45543   2.156 0.032684 *         #
#   perception.of.corruption     -1.20868    0.28250  -4.278 3.30e-05 ***       #
#   positive.affect               3.44207    0.55740   6.175 5.69e-09 ***       #
#   negative.affect               1.17269    0.60926   1.925 0.056113 .         #
#   confidence.in.govt           -0.99422    0.30707  -3.238 0.001477 **        #
#   gini.house.hold.income       -1.54129    0.41282  -3.734 0.000266 ***       #
#   ---                                                                         #
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1              #
#                                                                               #
# Residual standard error: 0.4502 on 153 degrees of freedom                     #
# (2 observations deleted due to missingness)                                   #
# Multiple R-squared:  0.8379,	Adjusted R-squared:  0.8294                     #
# F-statistic: 98.83 on 8 and 153 DF,  p-value: < 2.2e-16                       #
#################################################################################
gvlma(lm.train.step2)
#################################################################################
#                         Value   p-value                   Decision            #
#   Global Stat        26.90536 2.077e-05 Assumptions NOT satisfied!            #
#   Skewness            1.50762 2.195e-01    Assumptions acceptable.            #
#   Kurtosis            0.01427 9.049e-01    Assumptions acceptable.            #
#   Link Function      24.05676 9.354e-07 Assumptions NOT satisfied!            #
#   Heteroscedasticity  1.32671 2.494e-01    Assumptions acceptable.            #
#################################################################################

# ------------------------------------ TESTING IF TIME SERIES MODEL FITS BETTER ------------------------------------ #
# Removing healthy.life.expectany.at.birth, gini.index.past.mean, generosity columns from dataset
df.train.mean2 <- df.train.mean %>% 
  dplyr::select(-c(healthy.life.expectancy.at.birth, gini.index.past.mean, generosity))

# PQL (Penalized Quasi-Likelihood) <<< this is for one of the models from time series
# Random Intercept (random = ~1|year)
glm.train.mean2 <- MASS::glmmPQL(life.ladder ~ log.gdp.per.cap + social.support + freedom.to.make.life.choices + 
                     perception.of.corruption + positive.affect + negative.affect + 
                     confidence.in.govt + gini.house.hold.income, ~1 | year, family = Gamma,
                     data = df.test.mean, verbose = FALSE)
summary(glm.train.mean2)
r2glmm::r2beta(glm.train.mean2)
#################################################################################
#                         Effect   Rsq upper.CL lower.CL                        #                      
# 1                        Model 0.796    0.841    0.751                        #
# 2              log.gdp.per.cap 0.249    0.366    0.143                        #
# 3               social.support 0.125    0.233    0.044                        #
# 4 freedom.to.make.life.choices 0.066    0.159    0.010                        # 
# 6              positive.affect 0.059    0.150    0.008                        #
# 8           confidence.in.govt 0.052    0.140    0.005                        #
# 9       gini.house.hold.income 0.033    0.111    0.001                        #
# 7              negative.affect 0.015    0.077    0.000                        #
# 5     perception.of.corruption 0.008    0.061    0.000                        #
#################################################################################
# ------------------------------------ TESTING IF TIME SERIES MODEL FITS BETTER ------------------------------------ #

# comparing models
anova(lm.train.mean, lm.train.step, lm.train.step2)
#################################################################################
#   Res.Df  RSS Df Sum of Sq    F Pr(>F)                                        #
# 1    152 30.6                                                                 #
# 2    153 30.7 -1    -0.074 0.37  0.546                                        #
# 3    155 31.8 -2    -1.086 2.70  0.071 .                                      #
################################################################################# 

# testing if there are outliers that are influencing the R^2 value
car::outlierTest(lm.train.step2, data = df.train.mean2)
#################################################################################
#  No Studentized residuals with Bonferonni p < 0.05                            #
#  Largest |rstudent|:                                                          #
#      rstudent unadjusted p-value Bonferonni p                                 #
# 137 -2.928325          0.0039326      0.63709                                 #
#################################################################################
par(mfrow = c(2, 2))
plot(lm.train.step2)
par(mfrow = c(1,1))
# Conclusion : Sri Lanka is an outlier, but not omitted, due to being 1 value out of many


# Correlation plot (visually checking for multicollinearity)
lm.corr <- cor(df.train.mean2[3:ncol(df.train.mean2)])
corrplot::corrplot(lm.corr, method = "number")

# Multicollinearity test
car::vif(lm.train.step2)
#############################################################################################################
# log.gdp.per.cap               social.support freedom.to.make.life.choices     perception.of.corruption    #
#        2.581713                     2.913823                     2.920392                     1.758410    #
# positive.affect              negative.affect           confidence.in.govt       gini.house.hold.income    #
#        2.461076                     1.481359                     1.757102                     1.391640    #
#############################################################################################################

# Check for homoscedasticity
car::ncvTest(lm.train.step2)
#################################################################################
# Non-constant Variance Score Test                                              #
# Variance formula: ~ fitted.values                                             #
# Chisquare = 0.1370489    Df = 1     p = 0.7112326                             #
#################################################################################
spreadLevelPlot(lm.train.step2)
# Suggested power transformation:  1.289758 
# Conclusion : No change required

# Testing if residuals are normally distributed
olsrr::ols_test_normality(lm.train.step2)
#################################################################################
# -----------------------------------------------                               #
#   Test                   Statistic       pvalue                               #  
# -----------------------------------------------                               #
# Shapiro-Wilk              0.9896         0.2830                               #
# Kolmogorov-Smirnov        0.0494         0.8234                               #
# Cramer-von Mises         21.4515         0.0000                               #
# Anderson-Darling          0.467          0.2481                               #
# -----------------------------------------------                               #
#################################################################################
qqp(lm.train.step2, data = df.train.mean2)

# Testing for autocorrelation of errors
#################################################################################
# car::durbinWatsonTest(lm.train.step2)                                         #
# lag Autocorrelation D-W Statistic p-value                                     #
#   1     0.008946716       1.98158    0.92                                     #
# Alternative hypothesis: rho != 0                                              #
#################################################################################
olsrr::ols_test_correlation(lm.train.step2) # 0.995226

# Check for cross validation of model
DAAG::cv.lm(df.train.mean2, lm.train.step2, m = 3)

# ------------------------------------ Predicting with the test data  ------------------------------------ #
# matching train colnames to test colnames
train.colnames <- colnames(df.train.mean2)
test.colnames <- colnames(df.test.mean)
predict.colnames <- test.colnames[train.colnames %in% test.colnames]

pred_probs <- predict(lm.train.step2, newdata = df.test.mean[, predict.colnames], interval = "predict", type = "response")

dt.pred_prob <- as.data.table(pred_probs)
dt.pred_prob$result <- ifelse((dt.pred_prob$lwr <= dt.pred_prob$fit) & (dt.pred_prob$upr >= dt.pred_prob$fit), 1, 0)
table(dt.pred_prob$result)

# MSE of our model
mean(lm.train.step2$residuals^2)
# ------------------------------------ Predicting with the test data  ------------------------------------ #