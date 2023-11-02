# read_data
marketing_df <- read.csv("/Users/wootaekkim/Desktop/youtube learning/marketing_sales_data.csv")
library("tidyverse")

# view data
View(marketing_df)

# first 5 and last 5 rows of data
head(marketing_df, 5)
tail(marketing_df, 5)

# closer look into basic statistics and data types of data
library(skimr)
skim(marketing_df)

#check any n/a values
sum(is.na(marketing_df))

# check correlation coefficients
cor(subset(marketing_df, select = -c(TV,Influencer)))
attach(marketing_df)
pairs(Sales ~ Radio + Social.Media)

#model
m1 <- lm(Sales ~ Radio + Social.Media + TV + Influencer)
summary(m1)

m2 <- lm(Sales ~ Radio + TV + Influencer )
summary(m2)

m3 <- lm(Sales ~ Radio + TV)
summary(m3)

anova(m2, m1)
# this implies that reduced model is better than full model 
anova(m3, m2)
# again implies that further reduced model is better than m2

# residual plot
plot(m3, which=1)
# we can see constant variance around 0 yet there are three seperate groups
# QQ plot and histogram of residuals
plot(m3, which=2)
ggplot(data = marketing_df, aes(x = m3$residuals)) +
  geom_histogram(fill = 'steelblue', color = 'black') +
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')
# points distributed closely around the mean so that we can confirm normality
# assumption of residuals
#cook's distance
plot(m3, which=4)
# none of the points are over 0.5 so we can say that there is no leverage point

library(car)
vif(m3)
# here we can see there is not vif value over 5 so that we can confirm that
# there is no multicolineraity issue with the model and data

# confidence interval of predictor variables
confint(m3, 'Radio', level=0.95)
confint(m3, 'TVLow', level=0.95)
confint(m3, 'TVMedium', level=0.95)
 