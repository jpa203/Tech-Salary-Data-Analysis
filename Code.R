library(dplyr)
library(ggplot2)
library(forecast)
library(stats)
library(visualize)
#naniar

options(scipen = 999)
# Question 1 -  loading data set

df <- read.csv('/Users/jazzopardi/Desktop/R/699/tech_salary_data.csv')

df['level'] = NULL # file description says we won't be using this for analysis, so removing from df


# Question 2 - Numerical/Categorical Data

glimpse(df) # gives details on rows and column names

str(df) 

# Question 3 - Data Partition 

# seed value - 10 

set.seed(10)

0.6 * nrow(df) # 37585 is 60% (for training)

sampler <- sample_n(df, nrow(df))

train.df <- slice(sampler, 1:37585) # we have selected our data for training

valid.df <- slice(sampler, 37586: nrow(df)) # we have 40% for testing 

# Data partioning is important because we need to keep a subset of the available
# data so that we can use it to verify the model we create with the training
# data. As such, the data we don't use is appropriately called training data.

# Question 4

ggplot(train.df, aes(x= yearsofexperience, 
                     y= totalyearlycompensation)) + geom_point(
                       size = 2, shape = 23) + geom_smooth(method = lm)


# According to the best line fit, as years of experience, so does yearly compensation
# This makes sense given that, the more experience you have, the more expertise, and thus
# the more you get compensated. 

# Question 5

names(train.df)

cor.data <- train.df[, c('yearsofexperience', 'totalyearlycompensation')]

cor(cor.data) # correlation matrix = 0.4135291 

cor.test(train.df$yearsofexperience, train.df$totalyearlycompensation)

# The p value is close to 0 which means that it is statistically significant
# The correlation is 0.413 which suggest a low/medium correlation. 

# Question 6

            # dependent variable      #  independent variable
model <- lm(totalyearlycompensation ~ yearsofexperience, data = train.df) # is the right way?

summary(model)


# Question 7

# Minimum residual is -718441 and the max residual is 4569490 

# calculating standardized residuals 

View(train.df)

train.df$resid <- model$residuals

train.df$expected <- model$fitted.values

View(train.df)  # highest and lowest residuals

max(model$fitted.values) # 

4950000 - 380509
  
102000 - 820441.3


summary(model)


# Question 8 

# Getting the Regression Equation

summary(model)


# intercept is a constant - here it is 145879
# the slope of the line is 'yearsofexperience'  - 9776
# so our equation is y = 145879 + 9776(x)

# input is years of experience, so let's say 15.
# this equation will predict total yearly compensation based on
# 15 years of work experience

res <- 145879 + (9776*15)
res # The predicted outcome, in this case yearly compensation, is 292519

# Question 9

pred <- predict(model, train.df)
accuracy(pred, train.df$totalyearlycompensation)

pred_two <- predict(model, valid.df)
accuracy(pred_two, valid.df$totalyearlycompensation)

# Question 10

sd(train.df$totalyearlycompensation) # 137989

summary(model) # Residual standard error (rmse) - 125600


# Multiple Linear Regression 

train.df.two <- read.csv('/Users/jazzopardi/Desktop/R/699/tech_salary_data.csv')

train.df.two$level <- NULL

# Question 1

# a 

anyNA(train.df.two) # there are NA values

which(colSums(is.na(train.df.two)) > 0)
# level # tag # gender # otherdetails # dmaid # race # education

# b

# NA values can produce biased estimates and lead to invalid conclusions.

# c

perc <- colSums(is.na(train.df.two)) #miss_var_summary()

perc

x <- sort(perc, decreasing = TRUE)

names(x[(x/nrow(train.df.two)*100) > 20])# gender, other details, race, education 

train.df.two <- subset(train.df.two, select = -c(gender, otherdetails, Race, Education))

# d

names(x[(x/nrow(train.df.two)*100) < 20 & (x/nrow(train.df.two)*100) > 0]) # tag # dmaid

train.df.two$tag[is.na(train.df.two$tag)] <- sort(table(train.df.two$tag), decreasing = TRUE)[1]

train.df.two$dmaid[is.na(train.df.two$dmaid)] <- sort(table(train.df.two$dmaid), decreasing = TRUE)[1]

# e

str(train.df.two)

# timestamp is categorical

train.df.two <- subset(train.df.two, select = -c(rowNumber)) # removing time stamp


# f

# Location, City ID and Dmaid

as.factor(train.df.two$location)# 1050
as.factor(train.df.two$cityid) # 1045
as.factor(train.df.two$dmaid) # 150

train.df.two <- subset(train.df.two, select = -c(location, cityid))

train.df.two$dmaid <- as.factor(train.df.two$dmaid)

train.df.two <- train.df.two[train.df.two$dmaid %in% names(sort(table(train.df.two$dmaid), decreasing = TRUE)[1:8]),]

# g

check <- as.data.frame(sort(table(train.df.two$company), decreasing=TRUE)[1:8])


check <- c('Amazon','Microsoft','Google','Facebook','Apple','Oracle',
           'Salesforce', 'Cisco')

saldat3 <- filter(train.df.two, company %in% check)

nrow(saldat3) #24593

# h

check_two <- as.data.frame(sort(table(train.df.two$title), decreasing=TRUE)[1:8])

check_two <- c('Software Engineer', 'Product Manager','Software Engineering Manager',
               'Data Scientist','Hardware Engineer','Product Designer','Technical Program Manager',
               'Solution Architect')

saldat3 <- filter(saldat3, title %in% check_two)

nrow(saldat3) # 23323

# i 

check_three <- as.data.frame(sort(table(train.df.two$tag), decreasing=TRUE)[1:8])


check_three<- c('Distributed Systems (Back-End)', 'Full Stack','API Development (Back-End)',
               'ML / AI','Web Development (Front-End)','Product','Data',
               'DevOps')

saldat3 <- filter(saldat3, tag %in% check_three)

nrow(saldat3) # 215751

# j 

saldat3$level <- NULL
saldat3$timestamp <- NULL

# k

saldat3$basesalary <- NULL
saldat3$bonus <- NULL
saldat3$stockgrantvalue <- NULL


# Question 2

set.seed(10)

0.6 * nrow(saldat3) # 9450

sampler <- sample_n(saldat3, nrow(saldat3))

new.train.df <- slice(sampler, 1:9450) # we have selected our data for training

new.valid.df <- slice(sampler, 9451: nrow(saldat3)) # we have 40% for testing 

# Question 3


cor.data <- new.train.df[ , c('totalyearlycompensation', 'yearsofexperience',
                             'yearsatcompany')]

cor(cor.data)

# Nothing above 0.9 

# Question 4

salary.lm <- lm(totalyearlycompensation ~ . , data = new.train.df) 

salary.lm.step <- step(salary.lm, direction = 'backward')

summary(salary.lm.step)

# Question 5

sst <- new.train.df$totalyearlycompensation - mean(new.train.df$totalyearlycompensation)
sst <- sst^2
sst <- sum(sst) # the difference between the mean and the observed value 

sst

# Question 6

ssr <- salary.lm.step$fitted.values - mean(new.train.df$totalyearlycompensation)

ssr <- ssr^2

ssr <- sum(ssr)

ssr


# Question 7


ssr / sst # 0.7159

summary(salary.lm.step) # Shows itself in the Multiple R-Squared

# Question 8

visualize.t(stat = c(-1.619, 1.619), df = 9415, section = 'bounded')


# 99.4 % of the curve is shaded. The remaining 0.6 % is the p value
# This is the unspoken area under the curve which is left for chance.
# A 99.4% confidence is far above the 95% cut off in statistics.

# Question 9 

p <- length(salary.lm.step$coefficients)

n <- nrow(new.train.df)

sse <- sum(salary.lm.step$residuals^2)

numerator <- ssr / p

denominator <- sse / (n - p - 1)

numerator/denominator  

# F statistic indicates as a whole if the model is statistically significant


# Question 10 

newframe <- data.frame(company = 'Facebook',
                       title = 'Product Designer',
                       yearsofexperience = 5,
                       yearsatcompany = 2,
                       tag = 'Data',
                       dmaid = '501',
                       Masters_Degree = 1,
                       Bachelors_Degree = 1,
                       Doctorate_Degree = 0,
                       rowNumber = 0)

predict(salary.lm.step, newframe)

#241782 

# Question 11

pred <- predict(salary.lm.step, new.train.df)
accuracy(pred, new.train.df$totalyearlycompensation)


pred_two <- predict(salary.lm.step, new.valid.df)
accuracy(pred_two, new.valid.df$totalyearlycompensation)

#                ME     RMSE      MAE       MPE     MAPE
# Test set 2757.326 101512.9 36212.95 -4.642061 16.95429
#


View(new.train.df)
